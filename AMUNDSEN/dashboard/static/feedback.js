/* Page feedback is saved on the dashboard server, independently of wiki flags. */
(() => {
  "use strict";
  const $ = s => document.querySelector(s), UW = window.UW;
  const dialog = $("#feedback-dialog"), form = $("#feedback-form");
  const message = $("#feedback-message"), name = $("#feedback-name");
  const status = $("#feedback-status"), send = $("#feedback-send"), close = $("#feedback-close");
  let context, submission, sending = false, saved = false;
  const id = () => {
    // crypto.randomUUID is unavailable on the ship's plain HTTP origins.
    const b = crypto.getRandomValues(new Uint8Array(16));
    b[6] = (b[6] & 15) | 64; b[8] = (b[8] & 63) | 128;
    const h = [...b].map(x => x.toString(16).padStart(2, "0")).join("");
    return `${h.slice(0,8)}-${h.slice(8,12)}-${h.slice(12,16)}-${h.slice(16,20)}-${h.slice(20)}`;
  };
  $("#feedback-open").onclick = () => {
    if (saved) { message.value = ""; saved = false; context = null; }
    if (!context || !message.value.trim()) {
      context = {url: location.href, title: document.title,
        tab: $("#tabs button.on")?.dataset.tab || "underway", wiki: UW.wikiContext?.() || "",
        window: UW.state.win, hiddenLegs: [...UW.state.hidden], colour: UW.state.colour,
        mapView: UW.state.view, viewport: {width: innerWidth, height: innerHeight},
        generated: UW.M.generated_utc};
      submission = id();
    }
    $("#feedback-page").textContent = `${context.tab} · ${context.wiki || context.window || ""}`;
    status.textContent = ""; close.textContent = "Cancel"; send.disabled = false;
    dialog.showModal(); message.focus();
  };
  close.onclick = () => dialog.close();
  // An edited message is a new submission; an unchanged retry keeps its ID.
  for (const field of [message, name]) field.addEventListener("input", () => { submission = id(); });
  form.onsubmit = async e => {
    e.preventDefault(); if (sending || saved || !form.reportValidity()) return;
    sending = true; send.disabled = true; close.disabled = true;
    message.readOnly = name.readOnly = true;
    status.textContent = "Saving feedback…";
    const controller = new AbortController(), timer = setTimeout(() => controller.abort(), 15000);
    try {
      const response = await fetch("api/feedback", {method:"POST", headers:{"Content-Type":"application/json"},
        body:JSON.stringify({id:submission, message:message.value, name:name.value, context}), signal:controller.signal});
      const result = await response.json();
      if (!response.ok || !result.ok) throw Error(result.error || "Could not save feedback.");
      saved = true; status.textContent = "Thank you—your feedback has been saved."; close.textContent = "Close";
    } catch (error) {
      status.textContent = error.name === "AbortError" ? "The request timed out. Your feedback is still here; please retry." : `${error.message} Your feedback is still here; please retry.`;
    } finally {
      clearTimeout(timer); sending = false; send.disabled = saved; close.disabled = false;
      message.readOnly = name.readOnly = false;
    }
  };
  dialog.addEventListener("cancel", e => { if (sending) e.preventDefault(); });
})();
