// Ship chat: a drawer in the corner of every tab that can open out into a
// right-hand side bar (the Chat tab button does the same). Messages go
// through the page's own server (api/chat, a SQLite file behind it); the
// drawer polls every few seconds while open and less often while collapsed.
// Everyone picks a name and an emoji, kept on the device. The crew members
// (@capn, @polly, @doc) are played by a local model and answer when
// mentioned.
(() => {
  "use strict";
  const $ = (s) => document.querySelector(s);
  const store = window.UW?.store || { get: (k, d) => { try { const v = localStorage.getItem("uw." + k); return v == null ? d : JSON.parse(v); } catch { return d; } }, set: (k, v) => { try { localStorage.setItem("uw." + k, JSON.stringify(v)); } catch {} } };
  const esc = (s) => String(s ?? "").replace(/[&<>"]/g, (c) => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;" }[c]));
  const el = $("#chat"), log = $("#chatlog"), who = $("#chatwho"), unread = $("#chatunread"), dot = $("#chatdot");
  const nameIn = $("#chatname"), textIn = $("#chattext"), emojiBtn = $("#chatemoji"), pick = $("#emojipick"), typing = $("#chattyping"), crewEl = $("#chatcrew");
  const EMOJI = ["🙂", "😎", "🤓", "🥶", "🧊", "🐧", "🐻‍❄️", "🦭", "🐋", "🐟", "🦑", "🐙", "🦀", "🌊", "⚓", "🚢", "🛶", "🧭", "🔭", "🧪", "🧬", "☕", "🍩", "🎣", "🌌", "❄️", "🌬️", "⛈️", "🛰️", "🐾"];
  const st = { open: store.get("chat.open", false), side: store.get("chat.side", false), lastId: 0, seen: store.get("chat.seen", 0), unread: 0, timer: null,
    myName: store.get("chat.name", ""), myEmoji: store.get("chat.emoji", "🙂"), crew: [] };
  nameIn.value = st.myName; emojiBtn.textContent = st.myEmoji;

  function layout() {
    el.classList.toggle("collapsed", !st.open);
    el.classList.toggle("sidebar", st.side && st.open);
    document.documentElement.classList.toggle("chat-side", st.side && st.open);
    $("#tabchat")?.classList.toggle("on", st.side && st.open);
    $("#chatsidebtn").textContent = st.side ? "⇥" : "⇤";
    $("#chatsidebtn").title = st.side ? "back to the corner" : "open as a side bar";
    if (st.open) setTimeout(() => { log.scrollTop = log.scrollHeight; for (const p of document.querySelectorAll(".plot")) if (p.data) window.Plotly?.Plots.resize(p); }, 60);
  }
  const fmtT = (t) => { const d = new Date(t * 1000); const now = new Date();
    return (d.toDateString() === now.toDateString() ? "" : d.toLocaleDateString(undefined, { month: "short", day: "numeric" }) + " ") + d.toLocaleTimeString(undefined, { hour: "2-digit", minute: "2-digit" }); };
  const linkify = (s) => esc(s).replace(/(https?:\/\/[^\s<]+)/g, '<a href="$1" target="_blank" rel="noopener">$1</a>').replace(/(^|\s)@(\w+)/g, '$1<span class="at">@$2</span>');
  const isCrew = (name) => st.crew.some((c) => c.name === name);

  function append(msgs) {
    if (!msgs.length) return;
    const atBottom = log.scrollHeight - log.scrollTop - log.clientHeight < 40;
    for (const m of msgs) {
      const mine = st.myName && m.name === st.myName;
      const d = document.createElement("div");
      d.className = "msg" + (mine ? " mine" : "") + (isCrew(m.name) ? " bot" : "");
      d.innerHTML = `<span class="av">${esc(m.emoji || (isCrew(m.name) ? "" : "•"))}</span><span class="who">${esc(m.name)}</span><span class="when">${fmtT(m.t)}</span><div class="txt">${linkify(m.text)}</div>`;
      log.appendChild(d);
      st.lastId = Math.max(st.lastId, m.id);
    }
    while (log.children.length > 300) log.firstChild.remove();
    if (atBottom || st.open) log.scrollTop = log.scrollHeight;
    if (st.open) { st.seen = st.lastId; store.set("chat.seen", st.seen); st.unread = 0; }
    else st.unread += msgs.filter((m) => m.id > st.seen).length;
    unread.hidden = !st.unread; unread.textContent = st.unread;
  }

  async function poll() {
    try {
      const r = await fetch(`api/chat?since=${st.lastId}&name=${encodeURIComponent(st.myName)}&emoji=${encodeURIComponent(st.myEmoji)}&t=${Date.now()}`, { cache: "no-store" });
      if (!r.ok) throw new Error(r.status);
      const j = await r.json();
      dot.className = "dot on";
      st.crew = j.crew || [];
      append(j.messages);
      const others = (j.online || []).filter((n) => n.name !== st.myName);
      who.textContent = j.online?.length ? `${j.online.length} here${others.length ? ": " + others.slice(0, 4).map((n) => `${n.emoji || ""}${n.name}`).join(", ") + (others.length > 4 ? "…" : "") : ""}` : "nobody else here";
      who.title = (j.online || []).map((n) => n.name).join(", ");
      const t = (j.typing || []).map((h) => st.crew.find((c) => c.handle === h)).filter(Boolean);
      typing.hidden = !t.length; typing.textContent = t.length ? `${t.map((c) => `${c.emoji} ${c.name}`).join(", ")} ${t.length > 1 ? "are" : "is"} typing…` : "";
      crewEl.hidden = !st.crew.length;
      crewEl.innerHTML = st.crew.length ? "crew: " + st.crew.map((c) => `<button type="button" class="mention" data-h="${esc(c.handle)}" title="${esc(c.name)}">${esc(c.emoji)} @${esc(c.handle)}</button>`).join(" ") : "";
      for (const b of crewEl.querySelectorAll(".mention")) b.onclick = () => { textIn.value = (textIn.value ? textIn.value.replace(/\s*$/, " ") : "") + `@${b.dataset.h} `; textIn.focus(); };
    } catch { dot.className = "dot"; who.textContent = "offline"; }
    clearTimeout(st.timer);
    st.timer = setTimeout(poll, st.open ? 4000 : 20000);
  }

  function toggle(open) {
    st.open = open ?? !st.open; store.set("chat.open", st.open);
    if (st.open) { st.seen = st.lastId; store.set("chat.seen", st.seen); st.unread = 0; unread.hidden = true; }
    layout(); if (st.open) textIn.focus(); poll();
  }
  $("#chathead").onclick = () => toggle();
  $("#chatsidebtn").onclick = () => { st.side = !st.side; store.set("chat.side", st.side); if (!st.open) st.open = true; layout(); poll(); };
  // the Chat tab button: open as a side bar, or put it away
  window.UW = Object.assign(window.UW || {}, { chatToggle: () => { if (st.side && st.open) { toggle(false); } else { st.side = true; store.set("chat.side", true); toggle(true); } } });

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
      const r = await fetch("api/chat", { method: "POST", headers: { "Content-Type": "application/json" }, body: JSON.stringify({ name: st.myName, emoji: st.myEmoji, text }) });
      if (r.ok) { textIn.value = ""; await poll(); } else { const j = await r.json().catch(() => ({})); who.textContent = j.error || "not sent"; }
    } catch { who.textContent = "offline"; }
    textIn.disabled = false; textIn.focus();
  };
  document.addEventListener("visibilitychange", () => { if (!document.hidden) poll(); });
  layout(); poll();
})();
