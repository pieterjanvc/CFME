# Plan: fix paraphrased / unlocatable AI competency quotes

Status: **not started** — scoped 2026-09-08, to be tackled in a later session.
Companion to the rule-2 conflict work committed in `b25be1e`.

---

## 1. Problem

`dbCompExtraction()` stores each AI-extracted quote in `competency_text` with a
`start`/`end` character offset produced by `mod_highlight_locate()`. When the
model's `text_match` string is **not a verbatim substring** of the evaluation
text, the locator can't place it and the row is written with
`start = NULL, end = NULL`.

Consequences:

- The review app (`inst/review_app.R`) filters `!is.na(start), !is.na(end)`
  when seeding highlights, so a human auditor sees **no highlight** for that
  piece of AI evidence — they can't check where it came from.
- A paraphrased quote violates extraction **rule 3 ("Verbatim only")** — it is
  a data-quality defect regardless of highlighting.
- Some paraphrases are near-duplicates of another competency's quote (a rule-2
  violation that the exact-substring check in
  `dbCompExtractionCheckConflicts()` cannot catch).

This is the residue left over from the rule-2 reprocess: that work only fixed
`NA` rows that were *overlap-losers* (a verbatim quote whose span was already
claimed). The rows addressed here are a different failure — the text genuinely
isn't in the evaluation.

---

## 2. What is already done (do not redo)

- `mod_highlight_locate()` (committed in `b25be1e`) now has a
  **whitespace-tolerant fallback**: if the exact `fixed = TRUE` search fails it
  retries with every whitespace run allowed to match `[[:space:]]*`. This
  recovers quotes lost only to whitespace differences (e.g. `<br>` between two
  words stripped to nothing). It runs **only at extraction time** — it does not
  retroactively fix rows already stored as `NA`.

---

## 3. Sizing (rerun before starting — numbers drift)

Query: for every `competency_text` row with `start IS NULL` under a
rubric-3 AI review at `statusCode 5`, rebuild the canonical locate text
(`mod_highlight_strip_tags(dbGetEvals(..., html = TRUE, subtitleTag = "b"))`)
and test whether `text_match` is findable (exact **or** whitespace-tolerant).

As of 2026-09-08 (post rule-2 reprocess):

| bucket | count | rows | reviews |
|---|---|---|---|
| total `NA`-position rows | 235 | — | 150 |
| findable now (locator quirk — pre-dated the ws fallback) | **71** | no LLM needed | — |
| genuinely not in text (paraphrase / fabrication) | **164** | needs LLM or drop | — |

There will also be `NA` rows outside rubric 3 / outside `statusCode 5`; decide
whether they are in scope.

A ready-made sizing script exists at
`/tmp/.../scratchpad/` from the scoping session — reproduce it as
`dev/size_paraphrase_na.R` if useful.

---

## 4. Root causes

### 4a. Locator quirk (the 71) — mechanical, no LLM

These `text_match` strings *are* verbatim (or whitespace-equivalent) but were
extracted before the ws-tolerant fallback existed, so they were stored `NA`.
Fixable by a one-off relocate pass.

### 4b. Text-source mismatch bug — makes 4a worse, still live

The model and the locator read **different renderings** of the same
evaluation:

| consumer | call | result |
|---|---|---|
| model input | `db_fetch_review_extract()` → `dbGetEvals(id, conn)` (`html = FALSE`) then `dedupe_repeated_paragraphs()` | keeps `---question` headers, `\n`, `\n\n` |
| locator | `dbCompExtraction()` → `mod_highlight_strip_tags(dbGetEvals(..., html = TRUE, subtitleTag = "b"))` | `<br>` → `""`, headers gone, blocks concatenated |

So a quote that spans a question boundary, or includes an internal newline, or
sits next to a header, is not a verbatim substring of the locator's text even
though the model copied it faithfully.

**Constraint:** `competency_text.start/end` must stay in the
`strip_tags(dbGetEvals(html = TRUE, subtitleTag = "b"))` coordinate system —
the review app renders highlights against exactly that
(`inst/review_app.R` `evalText()` + `mod_highlight_server`). Changing the
coordinate system means re-locating **every** existing AI `competency_text`
row (human rows carry offsets from the browser module and must not shift).

### 4c. Genuine paraphrase / fabrication (the 164) — needs judgement

The model rewrote, truncated, inserted `…`, or invented lead-in words
("there was one specific patient who…"). No mechanical fix; the true source
span has to be found (LLM) or the quote discarded.

---

## 5. Proposed approach (phased, each shippable alone)

### Phase 0 — relocate pass for the mechanically-recoverable rows (the ~71)

New dev function, e.g. `dbRelocateCompText(conn, review_ids = NULL)`:

1. For each target review, rebuild the canonical locate text once.
2. For every `competency_text` row (whole review, not just `NA` ones — so the
   non-overlapping claim order is preserved), re-run `mod_highlight_locate()`
   over all `text_match` values in insertion order.
3. Write back `start`/`end` where the new result differs; **never** overwrite a
   good offset with `NA`.
4. Report per review: rows filled, rows changed, rows still `NA`.

No LLM, fully reversible via backup. Run DB-wide.

### Phase 1 — fix the text-source mismatch (4b) so it stops recurring

Pick one:

- **1a (lightest):** feed the model the exact locator text
  (`mod_highlight_strip_tags(dbGetEvals(html = TRUE, subtitleTag = "b"))`,
  deduped) instead of the `html = FALSE` rendering. Downside: questions run
  straight into answers ("…specific examples.[name_redact] was…"). Measure
  extraction quality impact on a sample first.
- **1b:** change `dbGetEvals(html = TRUE)` so tag-stripping leaves a readable
  separator (e.g. emit `\n` alongside `<br>`, header on its own line), then
  **re-locate all existing AI `competency_text` rows once** as a migration
  (coordinate-system change — verify the app still lines up, and that human
  rows are untouched).
- **1c:** keep the ws-tolerant locator as the only defence and accept the
  residual. (Cheapest, leaves ~some ongoing paraphrase-independent misses.)

Recommendation: try **1a**, fall back to **1c** if extraction quality drops.
Do **not** do 1b unless 1a is unacceptable — the migration is the risky part.

### Phase 2 — LLM re-anchor for genuine paraphrases (the ~164)

New step, batch + live, mirroring the resolve step's shape:

- Input per `NA` row: the evaluation text + the model's paraphrased
  `text_match` + its competency name.
- Ask: *return the single exact verbatim span from the evaluation that this
  evidence refers to, or `NONE` if it does not correspond to any actual text.*
- Output contract: `{"anchor": "<verbatim span>"}` or `{"anchor": null}`.
- Apply: validate the returned span is a verbatim substring (ws-tolerant),
  `mod_highlight_locate()` it, update `text_match` + `start`/`end`. On `null`
  or a non-verbatim return → Phase 3.
- Group by review so one call can re-anchor all of a review's `NA` rows and
  respect non-overlapping claims.

Keep it a **separate** operation from extraction and from resolve — same
reasoning as in the rule-2 work: a tight, verifiable contract, no competency
re-selection.

### Phase 3 — handle the unanchorable remainder

For rows Phase 2 returns `null` for (true fabrications) or that stay
non-verbatim after a retry:

- Option D1: delete the `competency_text` row; delete the `competency_score`
  if it is then orphaned (matches `dbCompConflictResolve()` cleanup). Loses
  the competency.
- Option D2: keep the row, set a flag / note, and surface it in the app as
  "AI evidence, source not located" so a human can accept or reject.

Recommendation: **D2** — don't silently drop AI judgements; let a human
adjudicate. Needs a small schema addition (a nullable `note` or status on
`competency_text`, or a review-level flag) — decide during implementation.

### Phase 4 — re-score affected reviews

Any review whose `competency_text` changed in Phases 0/2/3 must be re-scored
(`statusCode 5 → 3 → score`), same as the rule-2 reprocess. Reuse
`dev/reprocess_rule2_conflicts.R` structure (it already does reset → batch →
notify → re-score, resumable, PushOver per step).

---

## 6. Decisions needed before coding

1. **Scope:** rubric 3 / `statusCode 5` only, or all AI `competency_text`
   `NA` rows DB-wide?
2. **Phase 1 choice:** 1a vs 1c (vs 1b).
3. **Phase 3 choice:** drop (D1) vs flag-for-human (D2). D2 needs a schema
   touch.
4. **Re-score everything touched, or only Phase 2/3 changes?** (Phase 0 only
   adds highlights, doesn't change `text_match` — arguably no re-score needed,
   but the earlier decision was "full re-score is fine, it re-interprets the
   whole evaluation".)
5. **Model / batch vs live** for Phase 2 (default: `gpt-5.1-batch`, same as
   the rest of the pipeline).

---

## 7. Files in play

- `R/shiny_mod_highlight.R` — `mod_highlight_locate()` (already ws-tolerant),
  `mod_highlight_strip_tags()`
- `R/dbOperations.R` — `dbCompExtraction()` locate call (~L853), new
  `dbRelocateCompText()` / re-anchor apply function
- `R/dbGetEvals` (in `dbOperations.R`, L289) — Phase 1b would change `html`
  rendering here
- `R/review_helper.R` — `db_fetch_review_extract()` (L412), `db_record_batch`,
  new fetch helper for the re-anchor step
- `R/review.R` — new `llm_comp_reanchor_run()` / `_batch_submit()` /
  `batch_reanchor_process()` alongside the resolve equivalents
- `inst/prompt_comp_reanchor.md` — new prompt template
- `inst/review_app.R` — L434-448 `evalText()`, L463-487 `highlightInitVals()`
  (Phase 1b coordinate check; Phase 3 D2 display)
- `inst/narrate.sql` — only if Phase 3 D2 needs a column
- `dev/reprocess_rule2_conflicts.R` — template for the Phase 4 driver

---

## 8. Testing

- Phase 0: run on a 10-review sample, assert every filled row's `text_match`
  equals the substring at its new `[start, end)` (the check used at the end of
  the rule-2 reprocess), and no previously-good offset became `NA`.
- Phase 1a: extract a ~20-review sample both ways, diff competency sets and
  `NA`-row counts.
- Phase 2: dry-run the apply on a handful with hand-written anchors before any
  live call; then live on ~10, eyeball each re-anchored span in context.
- Phase 4: same before/after comparison script used for the rule-2 batches
  (competency counts, specificity/utility/sentiment deltas, position
  integrity, 0 residual conflicts).

---

## 9. Backups

Always `cp local/narrate.db local/backup/narrate_pre-paraphrase-<step>_<ts>.db`
before any write step, as was done for every rule-2 batch.
