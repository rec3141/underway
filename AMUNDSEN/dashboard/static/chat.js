// Ship chat: a drawer in the corner of every tab that can open out into a
// right-hand side bar (the Chat tab button does the same). Messages go
// through the page's own server (api/chat, a SQLite file behind it); the
// drawer polls every few seconds while open and less often while collapsed.
//
// Rooms: Ship (everyone; a crew member answers only when @mentioned), Crew
// (the AI crew's room, where they also speak unprompted while it is open),
// Ada (the librarian's reading room: every message is a question answered
// from the History wiki with the pages read), and direct messages with a
// person or a crew member. A name belongs to the first device that uses it,
// by a token kept on the device; a direct message reaches only that device.
(() => {
  "use strict";
  const $ = (s) => document.querySelector(s);
  const store = window.UW?.store || { get: (k, d) => { try { const v = localStorage.getItem("uw." + k); return v == null ? d : JSON.parse(v); } catch { return d; } }, set: (k, v) => { try { localStorage.setItem("uw." + k, JSON.stringify(v)); } catch {} } };
  const esc = (s) => String(s ?? "").replace(/[&<>"]/g, (c) => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;" }[c]));
  const el = $("#chat"), log = $("#chatlog"), who = $("#chatwho"), unread = $("#chatunread"), dot = $("#chatdot");
  const nameIn = $("#chatname"), textIn = $("#chattext"), emojiBtn = $("#chatemoji"), pick = $("#emojipick"), typing = $("#chattyping"), crewEl = $("#chatcrew");
  const roomsEl = $("#chatrooms"), pickerEl = $("#chatpicker");
  const EMOJI = ["🙂", "😎", "🤓", "🥶", "🧊", "🐧", "🐻‍❄️", "🦭", "🐋", "🐟", "🦑", "🐙", "🦀", "🌊", "⚓", "🚢", "🛶", "🧭", "🔭", "🧪", "🧬", "☕", "🍩", "🎣", "🌌", "❄️", "🌬️", "⛈️", "🛰️", "🐾"];
  // the device's token: the first device to use a name owns it
  let token = store.get("chat.token", "");
  if (!token) { token = Array.from(crypto.getRandomValues(new Uint8Array(18)), (b) => b.toString(16).padStart(2, "0")).join(""); store.set("chat.token", token); }
  const st = { open: store.get("chat.open", false), side: store.get("chat.side", false), timer: null,
    room: store.get("chat.room", "ship"),
    rooms: [],                                               // what the server lists for this name
    lastId: {},                                              // the newest message shown, per room
    seen: store.get("chat.seenRooms", {}),                   // the newest message read, per room
    hidden: store.get("chat.hiddenRooms", {}),               // a cleared shared room: ids up to here stay hidden on this device
    closed: store.get("chat.closedRooms", {}),               // a closed direct message: gone from the row until something newer arrives
    latest: {},                                              // the newest message that exists, per room
    unread: 0, error: "", roomBots: [], modelOn: false,
    myName: store.get("chat.name", ""), myEmoji: store.get("chat.emoji", "🙂"), crew: [], online: [], noai: store.get("chat.noai", false) };
  nameIn.value = st.myName; emojiBtn.textContent = st.myEmoji;
  const phone = matchMedia("(max-width: 640px)");
  phone.addEventListener?.("change", () => layout());

  const isDM = (ch) => ch.startsWith("dm:");
  // a private room with a crew member is named for where they are found
  const partnerTitle = (n) => { if (!n.startsWith("@")) return n; const c = st.crew.find((x) => x.handle === n.slice(1)); return c ? (c.room || c.name) : n; };
  const roomInfo = (ch) => st.rooms.find((r) => r.channel === ch) || { channel: ch, title: ch === "ship" ? "Ship" : ch === "crew" ? "Crew" : ch === "ada" ? "Library" : ch.replace(/^dm:/, "").split("|").filter((n) => n !== st.myName.toLowerCase()).map(partnerTitle).join(", ") || "Me", kind: isDM(ch) ? "dm" : "room" };
  const roomTitle = (ch) => roomInfo(ch).title;
  const placeholder = (ch) => ch === "ada" ? "ask Ada, the librarian · Enter to send"
    : ch === "crew" ? "talk to the crew · Enter to send"
    : isDM(ch) ? `message ${roomTitle(ch)} · Enter to send` : "message · Enter to send";

  function layout() {
    el.hidden = !st.open;                                     // closed is gone; the Chat tab brings it back
    el.classList.toggle("noai", st.noai && st.room === "ship");
    el.dataset.room = st.room;
    $("#chattitle").textContent = roomTitle(st.room);
    textIn.placeholder = placeholder(st.room);
    renderRooms();
    el.classList.toggle("collapsed", !st.open);
    el.classList.toggle("sidebar", st.side && st.open);
    document.documentElement.classList.toggle("chat-side", st.side && st.open);
    $("#tabchat")?.classList.toggle("on", st.side && st.open);
    $("#chatsidebtn").textContent = st.side ? "⇥" : "⇤";
    $("#chatsidebtn").title = st.side ? "back to the corner" : "open as a side bar";
    setTimeout(() => {
      if (st.open) log.scrollTop = log.scrollHeight;
      for (const p of document.querySelectorAll(".plot, #map")) if (p.data && p.offsetParent) window.Plotly?.Plots.resize(p);
    }, 80);
  }
  // the room bar: the three rooms, then the direct messages, a dot on any with news
  function renderRooms() {
    const fixed = ["ship", "crew", "ada"].map((ch) => roomInfo(ch));
    const dms = st.rooms.filter((r) => r.kind === "dm" && !(st.closed[r.channel] && (r.latest || 0) <= st.closed[r.channel]));
    if (isDM(st.room) && !dms.some((r) => r.channel === st.room)) dms.unshift(roomInfo(st.room));   // a room just opened, empty so far
    roomsEl.innerHTML = [...fixed, ...dms].map((r) => {
      const fresh = r.channel !== st.room && (st.latest[r.channel] || 0) > (st.seen[r.channel] || 0);
      return `<button type="button" data-ch="${esc(r.channel)}" class="${r.channel === st.room ? "on" : ""} ${r.kind}" title="${esc(r.kind === "dm" ? "direct messages with " + r.title : r.channel === "ship" ? "the ship's room: everyone; the crew answer when @mentioned" : r.channel === "crew" ? "the AI crew's room" : "the Library: ask Ada about the region's past")}"><span class="rdot" ${fresh ? "" : "hidden"}></span>${esc(r.title)}</button>`;
    }).join("") + `<button type="button" id="chatnew" title="a direct message with someone here, or with a crew member">+</button>` +
      // the room's own tools sit at the right end of the row, out of the head
      `<span class="tools">${st.room === "ship" ? `<button type="button" id="chataibtn" title="${st.noai ? "show the AI crew's messages again" : "hide the AI crew's messages and names"}">${st.noai ? "show AI" : "hide AI"}</button>` : ""}` +
      (isDM(st.room)
        ? `<button type="button" id="chatclearbtn" title="close this conversation: its history is erased and the room leaves the row">close</button>`
        : `<button type="button" id="chatclearbtn" title="clear this room on this device; others keep their copy">clear</button>`) + `</span>`;
    for (const b of roomsEl.querySelectorAll("button[data-ch]")) b.onclick = () => setRoom(b.dataset.ch);
    $("#chatnew").onclick = () => togglePicker();
    $("#chatclearbtn").onclick = clearRoom;
    const ai = $("#chataibtn"); if (ai) ai.onclick = () => { st.noai = !st.noai; store.set("chat.noai", st.noai); layout(); poll(); };
  }
  // who a direct message can be with: people here, and the crew
  function togglePicker(force) {
    const show = force ?? pickerEl.hidden;
    if (!show) { pickerEl.hidden = true; return; }
    const me = st.myName.toLowerCase();
    const people = st.online.filter((n) => n.name.toLowerCase() !== me);
    const crew = st.crew.map((c) => ({ name: "@" + c.handle, label: `${c.emoji} ${c.name}`, sub: c.room ? `the ${c.room}` : c.beat }));
    pickerEl.innerHTML = `<div class="pickhead">Rooms</div>` +
      ["ship", "crew", "ada"].map((ch) => `<button type="button" data-room="${ch}">${esc(roomTitle(ch))}</button>`).join("") +
      `<div class="pickhead">Message…</div>` +
      (people.length ? people.map((n) => `<button type="button" data-with="${esc(n.name)}">${esc(n.emoji || "•")} ${esc(n.name)}</button>`).join("") : `<div class="muted small">nobody else has the page open</div>`) +
      `<div class="pickhead">The crew</div>` + crew.map((c) => `<button type="button" data-with="${esc(c.name)}">${esc(c.label)} <span class="muted">${esc(c.sub)}</span></button>`).join("");
    for (const b of pickerEl.querySelectorAll("button[data-with]")) b.onclick = () => { pickerEl.hidden = true; openDM(b.dataset.with); };
    for (const b of pickerEl.querySelectorAll("button[data-room]")) b.onclick = () => { pickerEl.hidden = true; setRoom(b.dataset.room); };
    pickerEl.hidden = false;
  }
  function openDM(withName) {
    if (!st.myName) { nameIn.focus(); nameIn.placeholder = "name first"; return; }
    const ch = "dm:" + [st.myName.toLowerCase(), withName.toLowerCase()].sort().join("|");
    if (st.closed[ch]) { delete st.closed[ch]; store.set("chat.closedRooms", st.closed); }
    setRoom(ch);
  }

  const fmtT = (t) => { const d = new Date(t * 1000); const now = new Date();
    return (d.toDateString() === now.toDateString() ? "" : d.toLocaleDateString(undefined, { month: "short", day: "numeric" }) + " ") + d.toLocaleTimeString(undefined, { hour: "2-digit", minute: "2-digit" }); };
  // links: Markdown links to History pages, bare URLs, and @handles
  const linkify = (s) => esc(s)
    .replace(/\[(\d{1,2})\]\(#history\/([^)\s]+)\)/g, (m, n, slug) => `<sup><a href="#history/${slug}" data-slug="${slug}" class="ref" title="reference ${n}">${n}</a></sup>`)
    .replace(/\[([^\]]+)\]\(#history\/([^)\s]+)\)/g, (m, t, slug) => `<a href="#history/${slug}" data-slug="${slug}" class="cite">${t}</a>`)
    .replace(/\[([^\]]+)\]\(((?:artifact|person|place|event|source|topic|vessel|animal|kind)\/[^)\s]+)\)/g, (m, t, slug) => `<a href="#history/${slug}" data-slug="${slug}" class="cite">${t}</a>`)   // a page written without the #history/ prefix
    .replace(/\[([^\]]+)\]\((https?:\/\/[^)\s]+)\)/g, '<a href="$2" target="_blank" rel="noopener">$1</a>')
    .replace(/(^|[^"'>])(https?:\/\/[^\s<]+)/g, '$1<a href="$2" target="_blank" rel="noopener">$2</a>')
    .replace(/(^|\s)@(\w+)/g, '$1<span class="at">@$2</span>');
  const isCrew = (name) => st.crew.some((c) => c.name === name);
  // an artifact the answer rests on, as a chip: a picture with its title, or the words of a quote
  const chipHTML = (c) => `<a class="artchip ${esc(c.type)} ${c.thumb ? "" : "nomedia"}" href="#history/${esc(c.slug)}" data-slug="${esc(c.slug)}" title="${esc(c.title)}">${c.thumb ? `<img src="${esc(c.thumb)}" alt="" loading="lazy">` : ""}<span class="body"><span class="kind">${esc(c.type)}${c.year ? " · " + esc(c.year) : ""}${c.credit ? " · " + esc(c.credit) : ""}</span>${c.quote ? `<q>${esc(c.quote)}</q>` : `<b>${esc(c.title)}</b>`}</span></a>`;
  // the text with the chips Ada chose after the paragraphs she chose them for
  const bodyHTML = (m) => {
    const chips = m.meta?.chips || [];
    if (!chips.length) return linkify(m.text);
    const paras = String(m.text || "").split(/\n\s*\n/);
    const at = (i) => chips.filter((c, k) => Math.min(c.para ?? k, paras.length - 1) === i);
    return paras.map((p, i) => linkify(p) + (at(i).length ? `<div class="chips">${at(i).map(chipHTML).join("")}</div>` : i < paras.length - 1 ? "\n\n" : "")).join("");
  };
  const pagesHTML = (m) => m.meta?.refs?.length
    ? `<ol class="refs">${m.meta.refs.map((p) => `<li value="${p.n}"><a href="#history/${esc(p.slug)}" data-slug="${esc(p.slug)}">${esc(p.title)}</a></li>`).join("")}</ol>`
    : m.meta?.pages?.length
    ? `<div class="pages"><span class="lbl">read</span>${m.meta.pages.map((p) => `<a href="#history/${esc(p.slug)}" data-slug="${esc(p.slug)}">${esc(p.title)}</a>`).join("")}</div>` : "";

  function append(msgs, room) {
    if (!msgs.length) return;
    const atBottom = log.scrollHeight - log.scrollTop - log.clientHeight < 40;
    const hideUpTo = st.hidden[room] || 0;
    for (const m of msgs) {
      st.lastId[room] = Math.max(st.lastId[room] || 0, m.id);
      if (m.id <= hideUpTo) continue;                            // cleared on this device
      const mine = st.myName && m.name === st.myName;
      const d = document.createElement("div");
      d.className = "msg" + (mine ? " mine" : "") + (isCrew(m.name) ? " bot" : "");
      d.innerHTML = `<span class="av">${esc(m.emoji || (isCrew(m.name) ? "" : "•"))}</span><span class="who" data-name="${esc(m.name)}" title="${mine ? "" : "message " + esc(m.name) + " directly"}">${esc(m.name)}</span><span class="when">${fmtT(m.t)}</span><div class="txt">${bodyHTML(m)}${pagesHTML(m)}</div>`;
      log.appendChild(d);
    }
    while (log.children.length > 300) log.firstChild.remove();
    if (atBottom || st.open) log.scrollTop = log.scrollHeight;
    markSeen(room);
  }
  function markSeen(room) {
    if (st.open && room === st.room) { st.seen[room] = Math.max(st.seen[room] || 0, st.lastId[room] || 0, st.latest[room] || 0); store.set("chat.seenRooms", st.seen); }
    st.unread = 0;
    for (const r of Object.keys(st.latest)) if (!(st.open && r === st.room)) st.unread += Math.max(0, (st.latest[r] || 0) - (st.seen[r] || 0));
    unread.hidden = !st.unread; unread.textContent = st.unread;
  }

  async function poll() {
    try {
      const present = st.open && !document.hidden;
      const room = st.room;
      const r = await fetch(`api/chat?channel=${encodeURIComponent(room)}&since=${st.lastId[room] || 0}&name=${encodeURIComponent(st.myName)}&token=${encodeURIComponent(token)}&leave=${present ? 0 : 1}&emoji=${encodeURIComponent(st.myEmoji)}&t=${Date.now()}`, { cache: "no-store" });
      if (!r.ok) throw new Error(r.status);
      const j = await r.json();
      if (room !== st.room) return;                            // the room changed while this was in flight
      dot.className = "dot on";
      st.crew = j.crew || []; st.online = j.online || []; st.roomBots = j.room_bots || []; st.modelOn = !!j.model_online; st.error = j.error || "";
      st.rooms = j.rooms || [];
      for (const rm of st.rooms) st.latest[rm.channel] = rm.latest || 0;
      append(j.messages || [], room);
      const others = st.online.filter((n) => n.name !== st.myName);
      if (st.error) { who.textContent = st.error; who.classList.add("warn"); }
      else {
        who.classList.remove("warn");
        who.textContent = isDM(room) ? (st.roomBots.length ? "private room with a crew member" : `private with ${roomTitle(room)}`)
          : room === "ada" ? "the Library: ask Ada about the region's past"
          : st.online.length ? `${st.online.length} here${others.length ? ": " + others.slice(0, 4).map((n) => `${n.emoji || ""}${n.name}`).join(", ") + (others.length > 4 ? "…" : "") : ""}` : "nobody else here";
      }
      who.title = st.online.map((n) => n.name).join(", ");
      const t = (j.typing || []).map((h) => st.crew.find((c) => c.handle === h)).filter(Boolean);
      const noai = st.noai && room === "ship";
      typing.hidden = !t.length || noai; typing.textContent = t.length ? `${t.map((c) => `${c.emoji} ${c.name}`).join(", ")} ${t.length > 1 ? "are" : "is"} ${room === "ada" || t.some((c) => c.handle === "ada") ? "looking it up…" : "typing…"}` : "";
      // the crew line: who belongs to this room, and whether their model is up
      const on = st.modelOn;
      const light = `<span class="mdot ${on ? "on" : "off"}" title="${on ? "model online: " + esc(j.model || "") : "no model loaded: the crew cannot answer (" + esc(j.model || "") + "); the operator has been told"}"></span>`;
      const members = st.crew.filter((c) => st.roomBots.includes(c.handle));
      crewEl.hidden = !st.crew.length || noai;
      crewEl.innerHTML = !st.crew.length ? "" : room === "ship"
        ? `${light}AI crew${on ? "" : " offline"}, answer when mentioned: ` + st.crew.map((c) => `<button type="button" class="mention" data-h="${esc(c.handle)}" title="${esc(c.name)}">${esc(c.emoji)} @${esc(c.handle)}</button>`).join(" ")
        : `${light}${on ? "" : "offline: no model loaded, and the chat never loads one itself. "}${members.map((c) => `${esc(c.emoji)} ${esc(c.name)}`).join(", ")}${members.length ? (room === "ada" ? " is here" : room === "crew" ? " are here and may speak first" : " is here and may speak first") : ""}`;
      for (const b of crewEl.querySelectorAll(".mention")) b.onclick = () => { textIn.value = (textIn.value ? textIn.value.replace(/\s*$/, " ") : "") + `@${b.dataset.h} `; textIn.focus(); };
      el.classList.toggle("model-off", !on);
      layout();
    } catch { dot.className = "dot"; who.textContent = "offline"; }
    clearTimeout(st.timer);
    st.timer = setTimeout(poll, st.open ? 4000 : 20000);
  }

  function toggle(open) {
    st.open = open ?? !st.open; store.set("chat.open", st.open);
    if (st.open) markSeen(st.room);
    layout(); if (st.open) textIn.focus(); poll();
  }
  function setRoom(room) {
    st.room = room; store.set("chat.room", room);
    log.innerHTML = ""; st.lastId[room] = 0; pickerEl.hidden = true;
    if (!st.open) { st.open = true; store.set("chat.open", true); }
    layout(); textIn.focus(); poll();
  }
  // close a direct message: its history is erased (on the server when the
  // other member is a crew member, who then forgets it; on this device when
  // it is a person, who keeps their copy) and the room leaves the row until
  // something newer arrives. A shared room is only cleared on this device.
  async function clearRoom() {
    const room = st.room, dm = isDM(room);
    const warn = dm
      ? (st.roomBots.length ? `Close this conversation with ${roomTitle(room)}? Its history will be erased, and ${roomTitle(room)} will not remember it.`
                            : `Close this conversation with ${roomTitle(room)}? Its history will be erased on this device; ${roomTitle(room)} keeps theirs.`)
      : `Clear ${roomTitle(room)} on this device? Others keep their copy.`;
    if (!confirm(warn)) return;
    try {
      const r = await fetch("api/chat/clear", { method: "POST", headers: { "Content-Type": "application/json" }, body: JSON.stringify({ name: st.myName, token, channel: room }) });
      const j = await r.json();
      if (!r.ok) { who.textContent = j.error || "not closed"; return; }
      const upTo = Math.max(st.lastId[room] || 0, st.latest[room] || 0);
      st.hidden[room] = upTo; store.set("chat.hiddenRooms", st.hidden);
      log.innerHTML = ""; st.lastId[room] = 0;
      if (dm) { st.closed[room] = upTo; store.set("chat.closedRooms", st.closed); setRoom("ship"); }
      else poll();
    } catch { who.textContent = "offline"; }
  }
  $("#chathead").onclick = () => toggle();
  $("#chatclosebtn").onclick = () => toggle(false);
  $("#chatsidebtn").onclick = () => { st.side = !st.side; store.set("chat.side", st.side); if (!st.open) st.open = true; layout(); poll(); };
  // a name in the log opens a direct message; a page link opens the History tab
  log.addEventListener("click", (e) => {
    const a = e.target.closest("a[data-slug]"); if (a) { e.preventDefault(); window.UW?.historyOpen?.(a.dataset.slug); return; }
    const w = e.target.closest(".who[data-name]"); if (!w || w.dataset.name === st.myName) return;
    if (isCrew(w.dataset.name)) { const c = st.crew.find((x) => x.name === w.dataset.name); if (c) openDM("@" + c.handle); }
    else openDM(w.dataset.name);
  });
  window.UW = Object.assign(window.UW || {}, {
    chatToggle: () => { if (st.open) { toggle(false); } else { st.side = true; store.set("chat.side", true); toggle(true); } },
    // a room by name, or a crew member's private room by handle (Ada has a room of her own; the others are met one to one)
    chatRoom: (room) => { if (!st.side) { st.side = true; store.set("chat.side", true); } if (room !== "ada" && st.crew.some((c) => c.handle === room)) openDM("@" + room); else setRoom(room === "historian" ? "ada" : room); },
  });

  nameIn.onchange = async () => {
    const next = nameIn.value.trim().slice(0, 24);
    if (st.myName && next !== st.myName) {            // let the old name go so another device may take it
      try { await fetch("api/chat/release", { method: "POST", headers: { "Content-Type": "application/json" }, body: JSON.stringify({ name: st.myName, token }) }); } catch {}
    }
    st.myName = next; store.set("chat.name", st.myName); poll();
  };
  pick.innerHTML = EMOJI.map((e) => `<button type="button">${e}</button>`).join("");
  emojiBtn.onclick = () => { pick.hidden = !pick.hidden; };
  for (const b of pick.querySelectorAll("button")) b.onclick = () => { st.myEmoji = b.textContent; store.set("chat.emoji", st.myEmoji); emojiBtn.textContent = st.myEmoji; pick.hidden = true; poll(); };
  $("#chatform").onsubmit = async (ev) => {
    ev.preventDefault();
    const text = textIn.value.trim(); if (!text) return;
    if (!st.myName) { nameIn.focus(); nameIn.placeholder = "name first"; return; }
    if (st.error) { nameIn.focus(); return; }
    textIn.disabled = true;
    try {
      const body = { name: st.myName, token, emoji: st.myEmoji, text, channel: st.room };
      if (st.room === "ada" || st.roomBots.includes("ada")) body.slug = window.UW?.historyContext?.() || "";   // the page being read, as context
      else if (st.roomBots.includes("doc")) body.slug = window.UW?.natureContext?.() || "";                    // the subject or observation open on the Nature tab
      const r = await fetch("api/chat", { method: "POST", headers: { "Content-Type": "application/json" }, body: JSON.stringify(body) });
      if (r.ok) { textIn.value = ""; await poll(); } else { const j = await r.json().catch(() => ({})); who.textContent = j.error || "not sent"; }
    } catch { who.textContent = "offline"; }
    textIn.disabled = false; textIn.focus();
  };
  document.addEventListener("visibilitychange", () => { if (!document.hidden) poll(); });
  if (st.room === "historian" || st.room === "crew" && !store.get("chat.rooms.v2")) { st.room = st.room === "historian" ? "ada" : "ship"; store.set("chat.rooms.v2", true); }
  layout(); poll();
})();
