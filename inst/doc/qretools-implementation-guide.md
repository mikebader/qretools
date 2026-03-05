# qretools Implementation Reference Guide

**Project:** Baltimore Area Survey (BAS) — `qretools` R package  
**Purpose:** Reference document for implementation decisions. Use this when writing code, adding features, or resolving design questions.  
**Last updated:** 2026-03-05  

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

### Required Fields (every question)

```yaml
varname: nhd_sat
title: Neighborhood Satisfaction
type: factor              # integer | numeric | factor | character | composite | multiple_response
vargroup: nhd
question_text: "How satisfied are you with your neighborhood as a place to live?"
years_used: [2023, 2024, 2025]
```

### Highly Recommended

```yaml
source: "Original"        # or citation
variable_role: substantive  # substantive | parameter | administrative | generated | weight
```

### For `type: factor`

```yaml
value_labels_name: satisfied5   # Must exist in value-labels/
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
    value_labels_name: satisfied4
    changes_from_previous: []
  - version: 2
    years: [2023, 2024, 2025]
    question_text: "Revised wording..."
    value_labels_name: satisfied5
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
    value_labels_name: yesno
  tsp_pubreas_time:
    part_label: "Time"
    part_text: "Takes too long"
    type: integer
    value_labels_name: yesno
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

### Key Principle
Survey config files reference questions by varname — they do not redefine them. One config includes all variables (public and restricted); generation functions filter by access level at output time.

### Design Workflow (status: partially decided)
The schema documents describe survey configs as **generated by R scripts** using builder functions (`add_section()`, `add_question()`, etc.), with the R script as the human-readable design document and the YAML as the machine-readable artifact. These builder functions are not yet implemented. Until that decision is confirmed and the functions exist, treat the YAML as the working design document.

**Why R-script generation is the goal (for context):**
Hand-editing survey config YAML directly is technically possible but strongly discouraged for several reasons. First, survey config files are complex and verbose — structural errors from typos are easy to introduce and hard to spot. Second, and more importantly, the R build script is meant to serve as the authoritative, readable record of design decisions. If someone edits the YAML directly and not the script, the script no longer accurately describes the survey, creating a documentation gap and a potential source of confusion or error in future years.

The intended workflow once builder functions exist:
1. Edit `build-survey-YYYY.R` to change the survey structure
2. Re-run the script to regenerate the YAML
3. Commit both files — the script as the design record, the YAML as the machine-readable spec

When implementing features that touch survey configs, do not write code that encourages or assumes direct YAML editing. Validate YAML on read; generate it from R.

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
| `qt_read_all_variables()` | `qt_varbank()` | `varbank` | Combined view of all banks |
| `qt_read_value_labels()` | `qt_vlabs()` | `vlabs` | Read `value-labels/` |
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
    value_labels_name: satisfied4
    ddi_version_date: "2021-03-01"
    changes_from_previous: []
  - version: 2
    years: [2023, 2024, 2025]
    question_text: "Revised wording..."
    value_labels_name: satisfied5
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

## 11. Deferred Decisions

These were intentionally deferred — do not implement without discussing:

- Detailed candidate management features (tracking why something is a candidate, multi-person workflow)
- Formal promotion workflow function (`promote_candidates_to_bank()`)
- Full `promoted.yml` tracking file format
- Qualtrics QSF export
- DDI codebook generation (beyond basic structure)
- Survey config builder functions (`add_section()`, `add_question()`, etc.) and R-script-as-design-document workflow (vs. hand-edited YAML) — confirm before implementing

---

## 12. What NOT to Do

- Do not edit `banks/` files during questionnaire design phase
- Do not include missing value codes in individual value label sets
- Do not use `display_logic` — use `if_condition`
- Do not make `rotation_set` a string — it must be an array
- Do not store candidates in `banks/` — they go in `surveys/bas-YYYY/design/candidates/`
- Do not duplicate question definitions between bank and candidates
