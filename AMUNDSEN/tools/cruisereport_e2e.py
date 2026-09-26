"""Drive report.html end to end in headless Chrome against a running server.

    python tools/cruisereport_e2e.py [http://127.0.0.1/report] [outdir]

Ticks the CTD-rosette instrument and a team, imports a logsheet, adds a table
of each kind and a map, writes some text and downloads the .docx; screenshots
go to ``outdir``. Needs the ship's shares mounted (it reads 2026 Leg 3).
"""

import sys
from pathlib import Path

from playwright.sync_api import sync_playwright

BASE = sys.argv[1] if len(sys.argv) > 1 else "http://127.0.0.1/report"
OUT = Path(sys.argv[2] if len(sys.argv) > 2 else "e2e-out")
OUT.mkdir(parents=True, exist_ok=True)
LOGSHEET = "/mnt/ship/Share/2026/2026_LEG_03/Nets/Gustavo_Samples_LEG3.xlsx"

errors = []
with sync_playwright() as p:
    b = p.chromium.launch(channel="chrome", headless=True)
    pg = b.new_page(viewport={"width": 1280, "height": 900}, accept_downloads=True)
    pg.on("console", lambda m: m.type == "error" and errors.append(m.text))
    pg.on("pageerror", lambda e: errors.append(str(e)))
    pg.goto(f"{BASE}/report.html")
    pg.evaluate("localStorage.clear()")
    pg.reload()
    pg.wait_for_selector("#groups .chip")
    pg.select_option("#leg", "2026_LEG_03") if pg.input_value("#leg") != "2026_LEG_03" else None
    pg.wait_for_selector("#groups .chip")
    pg.fill("#team", "Collins")
    pg.fill("#title", "Microbial communities of the Canadian Arctic Archipelago")
    pg.locator("#leaders input").nth(0).fill("R. Eric Collins")
    pg.locator("#leaders input").nth(2).fill("University of Alaska Fairbanks")
    pg.locator("#groups .chip", has_text="CTD-Rosette").locator("input").check()
    pg.locator("#groups .chip", has_text="Plankton nets").locator("input").check()
    pg.click("#teams-box summary")
    pg.locator("#teams .chip", has_text="Collins").locator("input").check()
    pg.click("#log-box summary")
    pg.set_input_files("#log-file", LOGSHEET)
    pg.wait_for_selector("#logsheets .how", timeout=30000)
    pg.screenshot(path=str(OUT / "1-work.png"), full_page=False)
    pg.click("#cond-preview")
    pg.wait_for_selector("#narratives p:not(.hint)", timeout=60000)
    for preset in ("conditions", "bottles"):
        pg.click(f"[data-preset={preset}]")
    pg.locator("#tables .tbl").nth(1).locator("button", has_text="Preview").click()
    pg.wait_for_selector("#tables .tbl >> nth=1 >> .preview table", timeout=60000)
    pg.click("[data-fig=map]")
    pg.wait_for_function("document.querySelector('#figures img')?.naturalWidth > 0", timeout=60000)
    pg.locator("#texts textarea").nth(0).fill("Why we sampled.\n\nWhat we wanted.")
    pg.locator("#s-tables").scroll_into_view_if_needed()
    pg.screenshot(path=str(OUT / "2-tables.png"), full_page=False)
    pg.set_viewport_size({"width": 390, "height": 844})
    pg.goto(f"{BASE}/report.html")
    pg.wait_for_selector("#groups .chip")
    overflow = pg.evaluate("document.documentElement.scrollWidth > window.innerWidth")
    pg.screenshot(path=str(OUT / "3-phone.png"), full_page=False)
    pg.set_viewport_size({"width": 1280, "height": 900})
    with pg.expect_download(timeout=120000) as dl:
        pg.click("#download")
    path = OUT / dl.value.suggested_filename
    dl.value.save_as(path)
    words = pg.inner_text("#words")
    b.close()

print("download:", path, path.stat().st_size, "bytes")
print("words:", words)
print("phone horizontal overflow:", overflow)
print("console errors:", errors or "none")
sys.exit(1 if errors or overflow else 0)
