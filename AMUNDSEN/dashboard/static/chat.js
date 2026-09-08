// Ship chat: a drawer in the corner of every tab that can open out into a
// right-hand side bar (the Chat tab button does the same). Messages go
// through the page's own server (api/chat, a SQLite file behind it); the
// drawer polls every few seconds while open and less often while collapsed.
// Everyone picks a name and an emoji, kept on the device. The crew members
// (@capn, @doc, @ada, @polly) are played by a local model and answer when
// mentioned. A second room, the Historian's, takes questions about the
// region's past and answers them from the History wiki, naming the pages it
// read; the room switcher in the drawer head moves between the two.
(() => {
  "use strict";
  const $ = (s) => document.querySelector(s);
  const store = window.UW?.store || { get: (k, d) => { try { const v = localStorage.getItem("uw." + k); return v == null ? d : JSON.parse(v); } catch { return d; } }, set: (k, v) => { try { localStorage.setItem("uw." + k, JSON.stringify(v)); } catch {} } };
  const esc = (s) => String(s ?? "").replace(/[&<>"]/g, (c) => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;" }[c]));
  const el = $("#chat"), log = $("#chatlog"), who = $("#chatwho"), unread = $("#chatunread"), dot = $("#chatdot");
  const nameIn = $("#chatname"), textIn = $("#chattext"), emojiBtn = $("#chatemoji"), pick = $("#emojipick"), typing = $("#chattyping"), crewEl = $("#chatcrew");
  const EMOJI = ["🙂", "😎", "🤓", "🥶", "🧊", "🐧", "🐻‍❄️", "🦭", "🐋", "🐟", "🦑", "🐙", "🦀", "🌊", "⚓", "🚢", "🛶", "🧭", "🔭", "🧪", "🧬", "☕", "🍩", "🎣", "🌌", "❄️", "🌬️", "⛈️", "🛰️", "🐾"];
  const ROOMS = { crew: { title: "Chat", placeholder: "message · Enter to send", who: "" },
                  historian: { title: "Historian", placeholder: "ask about the region's past · Enter to send", who: "answers from the History wiki" } };
  const st = { open: store.get("chat.open", false), side: store.get("chat.side", false), timer: null,
    room: ROOMS[store.get("chat.room", "crew")] ? store.get("chat.room", "crew") : "crew",
    lastId: { crew: 0, historian: 0 },                       // the newest message shown, per room
    seen: Object.assign({ crew: 0, historian: 0 }, store.get("chat.seenRooms", {})),   // the newest message read, per room
    latest: { crew: 0, historian: 0 },                       // the newest message that exists, per room (the server says)
    unread: 0,
    myName: store.get("chat.name", ""), myEmoji: store.get("chat.emoji", "🙂"), crew: [], noai: store.get("chat.noai", false) };
  // an older browser kept one seen counter for the one room there was
  if (!store.get("chat.seenRooms")) st.seen.crew = store.get("chat.seen", 0);
  nameIn.value = st.myName; emojiBtn.textContent = st.myEmoji;
  // on a phone the drawer stays out of the way until the Chat tab opens it
  const phone = matchMedia("(max-width: 640px)");
  phone.addEventListener?.("change", () => layout());

  function layout() {
    el.hidden = !st.open;                                     // closed is gone; the Chat tab brings it back
    el.classList.toggle("noai", st.noai && st.room === "crew");   // the historian's answers are the point of that room
    el.dataset.room = st.room;
    $("#chattitle").textContent = ROOMS[st.room].title;
    textIn.placeholder = ROOMS[st.room].placeholder;
    for (const b of $("#chatrooms").querySelectorAll("button")) {
      b.classList.toggle("on", b.dataset.ch === st.room);
      const other = b.dataset.ch !== st.room && st.latest[b.dataset.ch] > (st.seen[b.dataset.ch] || 0);
      const d = b.querySelector(".rdot"); if (d) d.hidden = !other; else b.classList.toggle("fresh", other);
    }
    const ai = $("#chataibtn"); ai.hidden = st.room !== "crew"; ai.textContent = st.noai ? "show AI" : "hide AI"; ai.title = st.noai ? "show the AI crew's messages again" : "hide the AI crew's messages and names";
    el.classList.toggle("collapsed", !st.open);
    el.classList.toggle("sidebar", st.side && st.open);
    document.documentElement.classList.toggle("chat-side", st.side && st.open);
    $("#tabchat")?.classList.toggle("on", st.side && st.open);
    $("#chatsidebtn").textContent = st.side ? "⇥" : "⇤";
    $("#chatsidebtn").title = st.side ? "back to the corner" : "open as a side bar";
    // the page just changed width: every Plotly graph, the map included,
    // has to be told (a container change is not a window resize)
    setTimeout(() => {
      if (st.open) log.scrollTop = log.scrollHeight;
      for (const p of document.querySelectorAll(".plot, #map")) if (p.data && p.offsetParent) window.Plotly?.Plots.resize(p);
    }, 80);
  }
  const fmtT = (t) => { const d = new Date(t * 1000); const now = new Date();
    return (d.toDateString() === now.toDateString() ? "" : d.toLocaleDateString(undefined, { month: "short", day: "numeric" }) + " ") + d.toLocaleTimeString(undefined, { hour: "2-digit", minute: "2-digit" }); };
  const linkify = (s) => esc(s).replace(/(https?:\/\/[^\s<]+)/g, '<a href="$1" target="_blank" rel="noopener">$1</a>').replace(/(^|\s)@(\w+)/g, '$1<span class="at">@$2</span>');
  const isCrew = (name) => st.crew.some((c) => c.name === name);
  // the historian's answers name the pages they drew on; each opens on the History tab
  const pagesHTML = (m) => m.meta?.pages?.length
    ? `<div class="pages"><span class="lbl">read</span>${m.meta.pages.map((p) => `<a href="#history/${esc(p.slug)}" data-slug="${esc(p.slug)}">${esc(p.title)}</a>`).join("")}</div>` : "";
  // a page cited in the answer as [Title] becomes a link to it
  const cite = (html, m) => { for (const p of (m.meta?.pages || [])) html = html.split(`[${esc(p.title)}]`).join(`<a href="#history/${esc(p.slug)}" data-slug="${esc(p.slug)}" class="cite">${esc(p.title)}</a>`); return html; };

  function append(msgs, room = st.room) {
    if (!msgs.length) return;
    const atBottom = log.scrollHeight - log.scrollTop - log.clientHeight < 40;
    for (const m of msgs) {
      const mine = st.myName && m.name === st.myName;
      const d = document.createElement("div");
      d.className = "msg" + (mine ? " mine" : "") + (isCrew(m.name) ? " bot" : "");
      d.innerHTML = `<span class="av">${esc(m.emoji || (isCrew(m.name) ? "" : "•"))}</span><span class="who">${esc(m.name)}</span><span class="when">${fmtT(m.t)}</span><div class="txt">${cite(linkify(m.text), m)}${pagesHTML(m)}</div>`;
      log.appendChild(d);
      st.lastId[room] = Math.max(st.lastId[room], m.id);
    }
    while (log.children.length > 300) log.firstChild.remove();
    if (atBottom || st.open) log.scrollTop = log.scrollHeight;
    markSeen(msgs, room);
  }
  function markSeen(msgs, room) {
    if (st.open) { st.seen[room] = Math.max(st.seen[room] || 0, st.lastId[room]); store.set("chat.seenRooms", st.seen); }
    // the badge on the drawer counts what is unread in every room
    st.unread = 0;
    for (const r of Object.keys(ROOMS)) st.unread += Math.max(0, (st.latest[r] || 0) - (st.seen[r] || 0)) * (r === st.room && st.open ? 0 : 1);
    if (!st.open && room === st.room) st.unread = Math.max(st.unread, msgs.filter((m) => m.id > (st.seen[room] || 0) && !(st.noai && isCrew(m.name))).length);
    unread.hidden = !st.unread; unread.textContent = st.unread;
  }

  async function poll() {
    try {
      // only a chat that is open (and a page that is visible) counts as "here"; a collapsed one polls anonymously and drops its presence
      const present = st.open && !document.hidden;
      const room = st.room;
      const r = await fetch(`api/chat?channel=${room}&since=${st.lastId[room]}&name=${encodeURIComponent(present ? st.myName : "")}&leave=${encodeURIComponent(present ? "" : st.myName)}&emoji=${encodeURIComponent(st.myEmoji)}&t=${Date.now()}`, { cache: "no-store" });
      if (!r.ok) throw new Error(r.status);
      const j = await r.json();
      if (room !== st.room) return;                            // the room changed while this was in flight
      dot.className = "dot on";
      st.crew = j.crew || [];
      st.latest = Object.assign(st.latest, j.latest || {});
      append(j.messages, room);
      const others = (j.online || []).filter((n) => n.name !== st.myName);
      who.textContent = room === "historian" ? ROOMS.historian.who
        : j.online?.length ? `${j.online.length} here${others.length ? ": " + others.slice(0, 4).map((n) => `${n.emoji || ""}${n.name}`).join(", ") + (others.length > 4 ? "…" : "") : ""}` : "nobody else here";
      who.title = (j.online || []).map((n) => n.name).join(", ");
      const t = (j.typing || []).map((h) => st.crew.find((c) => c.handle === h)).filter(Boolean);
      const noai = st.noai && room === "crew";
      typing.hidden = !t.length || noai; typing.textContent = t.length ? (room === "historian" ? "📜 The historian is reading the wiki…" : `${t.map((c) => `${c.emoji} ${c.name}`).join(", ")} ${t.length > 1 ? "are" : "is"} typing…`) : "";
      crewEl.hidden = !st.crew.length || noai;
      // one model serves the whole crew: the dot says whether it is reachable
      const on = !!j.model_online;
      const light = `<span class="mdot ${on ? "on" : "off"}" title="${on ? "model online: " + esc(j.model || "") : "no model loaded: the crew cannot answer (" + esc(j.model || "") + "); the operator has been told"}"></span>`;
      crewEl.innerHTML = !st.crew.length ? "" : room === "historian"
        ? `${light}📜 The historian ${on ? `(${esc(j.model || "local model")}) answers from the History wiki and names the pages it read; every question here is answered.` : `is offline: no model is loaded, and the chat never loads one itself.`}`
        : `${light}AI crew${on ? ` (${esc(j.model || "local model")})` : " offline, no model loaded"}: ` + st.crew.map((c) => `<button type="button" class="mention" data-h="${esc(c.handle)}" title="${esc(c.name)}${on ? "" : " (offline)"}">${esc(c.emoji)} @${esc(c.handle)}</button>`).join(" ");
      el.classList.toggle("model-off", !on);
      for (const b of crewEl.querySelectorAll(".mention")) b.onclick = () => { textIn.value = (textIn.value ? textIn.value.replace(/\s*$/, " ") : "") + `@${b.dataset.h} `; textIn.focus(); };
      layout();
    } catch { dot.className = "dot"; who.textContent = "offline"; }
    clearTimeout(st.timer);
    st.timer = setTimeout(poll, st.open ? 4000 : 20000);
  }

  function toggle(open) {
    st.open = open ?? !st.open; store.set("chat.open", st.open);
    if (st.open) markSeen([], st.room);
    layout(); if (st.open) textIn.focus(); poll();
  }
  // the room switcher: the log is refilled from the server for the new room
  function setRoom(room) {
    if (!ROOMS[room]) return;
    st.room = room; store.set("chat.room", room);
    log.innerHTML = ""; st.lastId[room] = 0;
    if (!st.open) { st.open = true; store.set("chat.open", true); }
    layout(); textIn.focus(); poll();
  }
  for (const b of $("#chatrooms").querySelectorAll("button")) b.onclick = () => setRoom(b.dataset.ch);
  // an answer's page links open on the History tab
  log.addEventListener("click", (e) => { const a = e.target.closest("a[data-slug]"); if (!a) return; e.preventDefault(); window.UW?.historyOpen?.(a.dataset.slug); });
  $("#chathead").onclick = () => toggle();
  $("#chatclosebtn").onclick = () => toggle(false);
  $("#chataibtn").onclick = () => { st.noai = !st.noai; store.set("chat.noai", st.noai); layout(); poll(); };
  $("#chatsidebtn").onclick = () => { st.side = !st.side; store.set("chat.side", st.side); if (!st.open) st.open = true; layout(); poll(); };
  // the Chat tab button: open as a side bar, or put it away
  window.UW = Object.assign(window.UW || {}, {
    chatToggle: () => { if (st.open) { toggle(false); } else { st.side = true; store.set("chat.side", true); toggle(true); } },
    chatRoom: (room) => { if (!st.side) { st.side = true; store.set("chat.side", true); } setRoom(room); },
  });

  nameIn.onchange = () => { st.myName = nameIn.value.trim().slice(0, 24); store.set("chat.name", st.myName); poll(); };
  pick.innerHTML = EMOJI.map((e) => `<button type="button">${e}</button>`).join("");
  emojiBtn.onclick = () => { pick.hidden = !pick.hidden; };
  for (const b of pick.querySelectorAll("button")) b.onclick = () => { st.myEmoji = b.textContent; store.set("chat.emoji", st.myEmoji); emojiBtn.textContent = st.myEmoji; pick.hidden = true; poll(); };
  $("#chatform").onsubmit = async (ev) => {
    ev.preventDefault();
    const text = textIn.value.trim(); if (!text) return;
    if (!st.myName) { nameIn.focus(); nameIn.placeholder = "name first"; return; }
    textIn.disabled = true;
    try {
      const body = { name: st.myName, emoji: st.myEmoji, text, channel: st.room };
      if (st.room === "historian") body.slug = window.UW?.historyContext?.() || "";   // the page being read, as context
      const r = await fetch("api/chat", { method: "POST", headers: { "Content-Type": "application/json" }, body: JSON.stringify(body) });
      if (r.ok) { textIn.value = ""; await poll(); } else { const j = await r.json().catch(() => ({})); who.textContent = j.error || "not sent"; }
    } catch { who.textContent = "offline"; }
    textIn.disabled = false; textIn.focus();
  };
  document.addEventListener("visibilitychange", () => { if (!document.hidden) poll(); });
  layout(); poll();
})();
