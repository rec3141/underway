// Ship chat: a drawer in the corner of every tab. Messages go through the
// page's own server (api/chat, a SQLite file behind it); the drawer polls
// every few seconds while open and less often while collapsed, so everyone
// with the page open sees the same thread within moments. The name is kept
// on the device.
(() => {
  "use strict";
  const $ = (s) => document.querySelector(s);
  const store = window.UW?.store || { get: (k, d) => { try { const v = localStorage.getItem("uw." + k); return v == null ? d : JSON.parse(v); } catch { return d; } }, set: (k, v) => { try { localStorage.setItem("uw." + k, JSON.stringify(v)); } catch {} } };
  const esc = (s) => String(s ?? "").replace(/[&<>"]/g, (c) => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;" }[c]));
  const el = $("#chat"), log = $("#chatlog"), who = $("#chatwho"), unread = $("#chatunread"), dot = $("#chatdot");
  const nameIn = $("#chatname"), textIn = $("#chattext");
  const st = { open: store.get("chat.open", false), lastId: 0, seen: store.get("chat.seen", 0), unread: 0, timer: null, ok: false, myName: store.get("chat.name", "") };
  nameIn.value = st.myName;
  el.classList.toggle("collapsed", !st.open);

  const fmtT = (t) => { const d = new Date(t * 1000); const now = new Date();
    return (d.toDateString() === now.toDateString() ? "" : d.toLocaleDateString(undefined, { month: "short", day: "numeric" }) + " ") + d.toLocaleTimeString(undefined, { hour: "2-digit", minute: "2-digit" }); };
  const linkify = (s) => esc(s).replace(/(https?:\/\/[^\s<]+)/g, '<a href="$1" target="_blank" rel="noopener">$1</a>');

  function append(msgs) {
    if (!msgs.length) return;
    const atBottom = log.scrollHeight - log.scrollTop - log.clientHeight < 40;
    for (const m of msgs) {
      const mine = m.name === st.myName && st.myName;
      const d = document.createElement("div");
      d.className = "msg" + (mine ? " mine" : "");
      d.innerHTML = `<span class="who">${esc(m.name)}</span><span class="when">${fmtT(m.t)}</span><div class="txt">${linkify(m.text)}</div>`;
      log.appendChild(d);
      st.lastId = Math.max(st.lastId, m.id);
    }
    while (log.children.length > 300) log.firstChild.remove();
    if (atBottom || st.open) log.scrollTop = log.scrollHeight;
    if (st.open) { st.seen = st.lastId; store.set("chat.seen", st.seen); st.unread = 0; }
    else st.unread = msgs.filter((m) => m.id > st.seen).length + st.unread;
    unread.hidden = !st.unread; unread.textContent = st.unread;
  }

  async function poll() {
    try {
      const r = await fetch(`api/chat?since=${st.lastId}&name=${encodeURIComponent(st.myName)}&t=${Date.now()}`, { cache: "no-store" });
      if (!r.ok) throw new Error(r.status);
      const j = await r.json();
      st.ok = true; dot.className = "dot on";
      append(j.messages);
      const others = j.online.filter((n) => n !== st.myName);
      who.textContent = j.online.length ? `${j.online.length} here${others.length ? ": " + others.slice(0, 4).join(", ") + (others.length > 4 ? "…" : "") : ""}` : "nobody else here";
      who.title = j.online.join(", ");
    } catch { st.ok = false; dot.className = "dot"; who.textContent = "offline"; }
    clearTimeout(st.timer);
    st.timer = setTimeout(poll, st.open ? 4000 : 20000);
  }

  $("#chathead").onclick = () => {
    st.open = !st.open; store.set("chat.open", st.open);
    el.classList.toggle("collapsed", !st.open);
    if (st.open) { st.seen = st.lastId; store.set("chat.seen", st.seen); st.unread = 0; unread.hidden = true; log.scrollTop = log.scrollHeight; textIn.focus(); }
    poll();
  };
  nameIn.onchange = () => { st.myName = nameIn.value.trim().slice(0, 24); store.set("chat.name", st.myName); };
  $("#chatform").onsubmit = async (ev) => {
    ev.preventDefault();
    const text = textIn.value.trim(); if (!text) return;
    if (!st.myName) { nameIn.focus(); nameIn.placeholder = "name first"; return; }
    textIn.disabled = true;
    try {
      const r = await fetch("api/chat", { method: "POST", headers: { "Content-Type": "application/json" }, body: JSON.stringify({ name: st.myName, text }) });
      if (r.ok) { textIn.value = ""; await poll(); } else { const j = await r.json().catch(() => ({})); who.textContent = j.error || "not sent"; }
    } catch { who.textContent = "offline"; }
    textIn.disabled = false; textIn.focus();
  };
  document.addEventListener("visibilitychange", () => { if (!document.hidden) poll(); });
  poll();
})();
