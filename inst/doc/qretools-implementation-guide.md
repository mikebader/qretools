# qretools Implementation Reference Guide

**Project:** Baltimore Area Survey (BAS) — `qretools` R package  
**Purpose:** Reference document for implementation decisions. Use this when writing code, adding features, or resolving design questions.  
**Last updated:** 2026-03-05 (rev 2)  

---

## 1. Project Overview

`qretools` (questionnaire research tools) is a private R package (GPL-3) developed at Johns Hopkins 21st Century Cities Initiative to manage the Baltimore Area Survey. It replaces manual, copy-paste workflows with a metadata-driven system.

**Core problems it solves:**
- Consistent variable naming across years
- Tracking question changes over time (~65% of ~150 questions change annually)
- Preventing copy/paste errors in questionnaire creation
- Enabling automation for questionnaire generation and DDI codebook export

---

## 2. Directory Structure

```
project-root/
├── banks/
│   ├── questions/          # Fielded survey questions (YAML by topic)
│   ├── generated/          # Variables constructed from other variables
│   └── control/
│       ├── project.yml     # Stable parameters (JURISDICTION, ADDRESS)
│       └── surveys/
│           ├── bas-2023.yml
│           └── ...
│
├── value-labels/           # Shared label definitions (NOT inside banks/)
│   ├── common.yml
│   ├── demographic.yml
│   └── ...
│
├── surveys/
│   └── bas-YYYY/
│       ├── survey-config.yml
│       ├── survey-metadata.yml
│       └── design/
│           ├── build-survey-YYYY.R     # R script (human-readable design)
│           ├── survey-YYYY-YYYYMMDD.yml  # date-stamped drafts
│           ├── survey-YYYY-FINAL.yml
│           └── candidates/
│               ├── new-question.yml
│               ├── promoted/
│               └── unused/
│
└── project-config.yml
```

**Key rules:**
- `banks/` = permanent variable definitions (never edited during design)
- `value-labels/` is at project root, not inside `banks/`, because it is shared across all bank types
- `surveys/bas-YYYY/design/candidates/` = staging area for new/revised questions
- Survey config files reference variables by varname; they do not redefine them

---

## 3. Three-Tier Architecture

| Tier | Name | Location | Purpose | Edited When |
|------|------|----------|---------|-------------|
| 1 | Question Bank | `banks/questions/` | Single source of truth for all fielded questions | Only after survey finalization |
| 2 | Candidates | `surveys/bas-YYYY/design/candidates/` | Staging area for questions under consideration | During design phase |
| 3 | Survey Config | `surveys/bas-YYYY/design/` | Which questions, in what order, for a specific year | During design phase |

**Principle:** Questions are defined in exactly one place at any time — either the bank OR candidates, never both simultaneously.

---

## 4. Question Bank YAML Schema (v2.1)

### Canonical field names

- Use `response_type:` (not `type:`) to specify how the question collects responses
- Use `surveys_used:` with survey ID strings (not `years_used:` with integers), because surveys are not always annual

### Required Fields (every question)

```yaml
variable_id: nhd_sat
title: Neighborhood Satisfaction
response_type: factor     # factor | character | integer | number | ranking | select_all | loop
vargroup: nhd
question_text: "How satisfied are you with your neighborhood as a place to live?"
surveys_used: [bas-2023, bas-2024, bas-2025]
```

### Highly Recommended

```yaml
source: "Original"        # or citation
variable_role: substantive  # substantive | parameter | administrative | generated | weight
```

### For `response_type: factor`

```yaml
value_label_id: satisfied5   # Must exist in value-labels/
```

### Optional Fields

```yaml
description: "Extended description for complex questions"
universe: "Homeowners only"
instruction: "Select all that apply"
help_text: "Pop-up text shown to respondent"
note: "Internal implementation note — not shown to respondent"
if_condition: "nhd_mvlkly >= 2"    # Display/skip logic (not display_logic)
rotation_set: ["rotation_A"]        # Array, not string
related_questions: [nhd_safe, nhd_clean]
first_year: 2021
```

### Version Tracking (questions that changed)

```yaml
versions:
  - version: 1
    years: [2021, 2022]
    question_text: "Original wording..."
    value_label_id: satisfied4
    changes_from_previous: []
  - version: 2
    years: [2023, 2024, 2025]
    question_text: "Revised wording..."
    value_label_id: satisfied5
    changes_from_previous:
      - type: response_scale
        description: "Changed from 4-point to 5-point scale"
```

Omit `versions` entirely if the question has never changed.

### Access Control Fields

```yaml
restricted_access: true
restriction_reason: "Disclosure risk — allows re-identification"
public_alternative: dem_income     # varname of public version
derived_from: dem_income_exact     # for generated public alternatives
derivation_method: "Collapsed to brackets"
```

### Generated Variables (`banks/generated/`)

Same schema, with:
```yaml
variable_role: generated
derived_from: [dem_income_70k, dem_income_lo, dem_income_hi]
derivation_method: "Combines low/high income based on 70k filter"
derivation_script: "construct-data.Rmd"
```

### Multiple Response Questions

```yaml
type: multiple_response
instruction: "Select all that apply"
creates_variables: [tsp_pubreas_cost, tsp_pubreas_time]
variable_parts:
  tsp_pubreas_cost:
    part_label: "Cost"
    part_text: "Too expensive"
    type: integer
    value_label_id: yesno
  tsp_pubreas_time:
    part_label: "Time"
    part_text: "Takes too long"
    type: integer
    value_label_id: yesno
```

---

## 5. Value Labels YAML Schema

**Location:** `value-labels/` (project root, shared across all banks)

### Structure

```yaml
satisfied5:
  tags: [satisfaction]
  labels:
    "Very satisfied": 1
    "Satisfied": 2
    "Neither satisfied nor dissatisfied": 3
    "Dissatisfied": 4
    "Very dissatisfied": 5

yesno:
  tags: [binary, yesno]
  labels:
    "Yes": 1
    "No": 2
  note: "Standard yes/no — do not use for agree/disagree"
```

### Rules
- Missing value codes (-6, -7, -8, -9) are defined globally in `missing-value-labels.yml` — do NOT include in individual label sets
- Optional fields: `note`, `related_to`, `extends`
- Tags are required for all label sets

---

## 6. Candidates (Staging Area)

**Location:** `surveys/bas-YYYY/design/candidates/`

### Schema
Candidates use the **same YAML schema as the question bank**. No separate candidate schema.

### Lifecycle
1. Create candidate file in `candidates/`
2. Reference in survey config with `source: candidate`
3. After survey finalized:
   - Made it to field → move to `candidates/promoted/`, then integrate into bank
   - Did not make it → move to `candidates/unused/`

### Parallel Structure
Candidates directory mirrors the banks structure where relevant:
- `candidates/` — question candidates (most common)
- Survey-specific parameters (e.g., randomization) live in `candidates/` as `control.yml`

### Promotion
- Promotion is function-based, not manual cut-and-paste (to prevent data loss)
- A `promoted.yml` tracking file is generated by a function at finalization
- **Deferred:** Detailed candidate management features (promotion tracking, metadata about why it's a candidate) are not yet implemented — use basic file movement for now

---

## 7. Survey Config

**Location:** `surveys/bas-YYYY/design/`
**Naming:** `survey-YYYY-YYYYMMDD.yml` for drafts; `survey-YYYY-FINAL.yml` for final
**Full schema:** `inst/schemas/survey-config-yaml-schema.md`

### Root Structure

```yaml
meta:           { id, title, status, ... }
questionnaire:
  preload:      [ compute items only — no questions ]
  items:        [ sections, questions, modules, logic, loops, ... ]
  postload:     [ ]   # future feature
```

### Key Principles

- Survey config files reference questions by `variable_id` — they do not redefine them
- One config includes all variables (public and restricted); output functions filter by access level
- All items and containers share an `item_type:` field — `section`, `question`, `module`, `loop`, `split`, `logic`, `compute`, `statement`, `randomize`, `display_together`
- `preload` is restricted to `compute` items (no respondent-facing content)
- `surveys_used` on a question during design should use a placeholder like `bas-2026-draft`; replace with final ID at finalization

### Design Workflow

The long-term goal is an R-script-as-design-document workflow: a `build-survey-YYYY.R` script uses builder functions (`add_section()`, `add_question()`, etc.) to generate the YAML programmatically, with the script serving as the human-readable record of design decisions. **Builder functions are not yet implemented.**

Until builder functions exist, the YAML file is the working design document. Hand-edit the YAML carefully; validate it by calling `qt_read_survey_config()` after each change.

The intended workflow once builder functions exist:
1. Edit `build-survey-YYYY.R` to change the survey structure
2. Re-run the script to regenerate the YAML
3. Commit both files — the script as the design record, the YAML as the machine-readable spec

### Versioning
- Date-stamped files for each draft snapshot
- Git tags for major milestones: `bas-YYYY-initial-draft`, `bas-YYYY-team-review`, `bas-YYYY-firm-review`, `bas-YYYY-FINAL`

---

## 8. R Function Naming Conventions

**Prefix:** `qt_` for all package functions

| Function | Alias | Conventional object name | Purpose |
|----------|-------|--------------------------|---------|
| `qt_read_question_bank()` | `qt_qbank()` | `qbank` | Read `banks/questions/` |
| `qt_read_generated_variables()` | `qt_genbank()` | `genbank` | Read `banks/generated/` |
| `qt_read_control_parameters()` | `qt_ctrlbank()` | `ctrlbank` | Read `banks/control/` |
| `qt_read_module_bank()` | `qt_mbank()` | `mbank` | Read `banks/modules/` |
| `qt_read_value_labels()` | `qt_vlabs()` | `vlabs` | Read `value-labels/` |
| `qt_read_survey_config()` | — | `qre` | Read survey config YAML → `qt_qreconfig` |
| `qt_config()` | — | `config` | Read `project-config.yml` |

**Conventions:**
- All read functions accept `config` parameter (defaults to cached config)
- All read functions accept `path` parameter override for testing
- All read functions return S3 objects with consistent structure
- Return objects include `$variables` (named list) and `$meta` (indices, source info)

---

## 9. Versioning

### Question Versioning

Questions use integer version numbers (1, 2, 3...) — not semantic versioning. Versions are only needed when a question changes; omit the `versions` block entirely for stable questions.

Each version records the years it was used, the question text, and (for factors) the value label set. The `ddi_version_date` field (ISO date) maps to the DDI `verStmt` element and should be populated to support future DDI export.

```yaml
versions:
  - version: 1
    years: [2021, 2022]
    question_text: "Original wording..."
    value_label_id: satisfied4
    ddi_version_date: "2021-03-01"
    changes_from_previous: []
  - version: 2
    years: [2023, 2024, 2025]
    question_text: "Revised wording..."
    value_label_id: satisfied5
    ddi_version_date: "2023-02-15"
    changes_from_previous:
      - type: response_scale
        description: "Changed from 4-point to 5-point scale"
```

**Validation rules:**
- Version years must not overlap
- All years in `years_used` must be covered by version years

### Data Release Versioning

Release versions are tracked in `metadata-YYYY.yml` under a `versions` block. This maps to the DDI `verStmt` element at the study level.

**Numbering convention:**
- Public releases: `1.0`, `1.1`, `1.2`, ... (increment minor for corrections)
- Restricted releases: `2.0`, `2.1`, ... or `1.0-restricted`

```yaml
versions:
  - version: "1.0"
    release_date: 2026-09-15
    status: "Released"
    access_level: "public"
    description: "Initial public use file"
    variables_excluded: [dem_income_exact, geo_tract]
  - version: "2.0"
    release_date: 2027-01-15
    status: "Released"
    access_level: "restricted"
    description: "Restricted use file with geographic and income detail"
```

### File-Level Versioning

| Item | Convention |
|------|-----------|
| Survey config drafts | Date-stamped: `survey-YYYY-YYYYMMDD.yml` |
| Final survey config | `survey-YYYY-FINAL.yml` |
| Git milestone tags | `bas-YYYY-milestone-name` (e.g., `bas-2026-FINAL`) |

---

## 10. Access Control

- Binary model: `restricted_access: true/false`
- Restricted variables have `restriction_reason` (required)
- Public alternatives use `public_alternative` (varname) and `derived_from`
- Generation functions filter by access level at output time — the single config includes all variables

---

## 11. Implementation Roadmap

### Phase 1 — Questionnaire reader (current focus)
1. `qt_read_survey_config()` → `qt_qreconfig` object: reads and validates survey config YAML, resolves all `variable_id` references against banks and candidates, expands modules inline, unrolls loops, tags items with container context
2. Human-readable questionnaire preview from a `qt_qreconfig` object
3. Candidate question handling (`source: candbank`)

### Phase 2 — DDI Codebook export
- Variable-level metadata (name, label, type, categories) from the bank objects
- Study-level metadata from `meta` blocks
- The variable bank work is already most of the way there

### Phase 3 — DDI Lifecycle export
- Instrument layer: `qt_qreconfig` → DDI `ControlConstructScheme` XML
- `section` → DDI `Sequence`; `loop` → DDI `Loop`; `logic` → DDI `IfThenElse`
- `if_condition` R expressions → DDI `CommandCode` with `programLanguage="R"` (no translation needed — DDI Lifecycle supports this natively)
- The `qt_qreconfig` data structure is designed to mirror DDI Lifecycle concepts, so Phase 3 is a serialization step, not a structural redesign

### Phase 4 — Builder functions (deferred)
- `add_section()`, `add_question()`, `add_module()`, etc.
- Generate survey config YAML programmatically from an R script
- R script becomes the authoritative design record; YAML is the generated artifact
- Do not implement before discussing — confirm design before starting

---

## 12. DDI Approach

**Target:** DDI Lifecycle (with DDI Codebook as an intermediate milestone).

**`if_condition` and DDI compliance:**
The `if_condition` field accepts R expressions (e.g., `"dem_child == 1"`). For DDI Lifecycle export, these are emitted as `CommandCode` elements tagged `programLanguage="R"` inside `IfThenElse` constructs. This is fully DDI 3.x compliant without requiring a separate expression language. To preserve this compatibility, `if_condition` values must reference only `variable_id` and control parameter IDs — no arbitrary R function calls.

**Structural mapping (survey config → DDI Lifecycle):**

| qretools | DDI Lifecycle |
|----------|---------------|
| `questionnaire` | `Instrument` / `ControlConstructScheme` |
| `item_type: section` | `Sequence` |
| `item_type: loop` | `Loop` |
| `item_type: logic` | `IfThenElse` |
| `item_type: question` | `QuestionConstruct` → `QuestionItem` |
| `item_type: statement` | `StatementItem` |
| `if_condition` | `CommandCode` (R, inside `IfThenElse`) |
| `compute` block | `GenerationInstruction` |
| `versions` block | `verStmt` |

---

## 13. Deferred Decisions

These were intentionally deferred — do not implement without discussing:

- Detailed candidate management features (tracking why something is a candidate, multi-person workflow)
- Formal promotion workflow function (`promote_candidates_to_bank()`)
- Full `promoted.yml` tracking file format
- Qualtrics QSF export
- Survey config builder functions (`add_section()`, `add_question()`, etc.) — see Phase 4 above
- `postload` block implementation

---

## 14. What NOT to Do

- Do not edit `banks/` files during questionnaire design phase
- Do not include missing value codes in individual value label sets
- Do not use `display_logic` — use `if_condition`
- Do not use `type:` or `years_used:` — use `response_type:` and `surveys_used:`
- Do not make `rotation_set` a string — it must be an array
- Do not store candidates in `banks/` — they go in `surveys/bas-YYYY/design/candidates/`
- Do not duplicate question definitions between bank and candidates
- Do not use `qt_varbank()` — it is retired; use individual bank readers or process via survey config
