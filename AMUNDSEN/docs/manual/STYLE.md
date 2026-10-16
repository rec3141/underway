# Writing the manual

The manual is built from `user/` and `admin/`: one chapter per file, named
`NN-topic.md`, read in that order; the first line is the chapter's `# Title`.
`python -m dashboard manual --root <web root>` renders it into `<web root>/manual/`
(every full build does this when a source changed). This file is not part of it.

## Both parts

- Describe what the dashboard does now. No history of how it came to be, no
  "used to", no stories of what went wrong; those belong in commits and issues.
- Use the words the page uses, in **bold**, exactly as they appear: **Legs**,
  **Colour by**, **Show last**.
- Headings: `##` for sections, `###` below that. No deeper.
- Notes and tips are admonitions:

  ```
  !!! tip
      Hold the mouse over a point to see its value.

  !!! note "Aboard only"
      Alerts work only on the ship's network.
  ```

- Pictures go in `img/` as WebP, named `<chapter>-<what>.webp`
  (`05-map-colour-by.webp`), cropped to what the text is about, at most 1280
  pixels wide, and referenced as `![What it shows](img/05-map-colour-by.webp)`.
  Every picture is introduced by the sentence before it.
- Say when something works only aboard (live data, alerts, chat, photo
  uploads, the ship's intranet) or only ashore.

## The user manual

Written for someone who has never seen the dashboard and is not technical:
a scientist, a crew member, a student, a visitor.

- Second person, short sentences, one idea per sentence. Explain every word a
  newcomer would not know the first time it appears (and add it to the
  glossary).
- Slow and complete: every button, menu and pill on a tab gets a sentence.
  Tasks are numbered steps, each saying what to click and what you then see.
- Show, then tell: a screenshot for each tab and each important step.
- Examples with real cruise data: "colour the track by **Salinity (PSU)** to
  see the fresh water off the Mackenzie", "pick two casts at the same station
  to compare them".
- End a chapter with a short "In short" list when it is long.

## The administrator manual

Written for a busy system administrator who knows Linux and needs to keep this
running.

- Lead with the answer: what it is, where it is, the command. Tables for
  settings, services and files; code blocks for commands.
- Explain how the parts fit only as far as needed to operate and repair them.
- Real paths and names (`/etc/underway/site.env`, `underway.service`), and
  what a setting's default is.
