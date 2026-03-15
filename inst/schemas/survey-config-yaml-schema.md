# Survey Configuration Specification

## Root Structure

A survey configuration file has two top-level keys:

```yaml
meta:         { ... }   # Survey metadata (required)
questionnaire:          # Survey content (required)
  preload:    [ ... ]   # Optional: items loaded before survey starts
  items:      [ ... ]   # Required: ordered survey content
  postload:   [ ... ]   # Future feature: post-survey processing
```

---

## Metadata (`meta`)

Survey design specifications for the entire survey.

```yaml
id: string                     # REQUIRED: Survey identifier (should match
                               #   directory)
title: string                  # REQUIRED: Survey title
status: enum                   # REQUIRED: One of: draft, final
short_title: string            # Abbreviated title or name
version: string                # Version of the survey
organization: string           # Name of organization fielding survey
author: string                 # Name of creator
irb:                           # IRB information
    - institution: string      # Name of institution that gave IRB approval
      number: string           # IRB protocol number for project
description: string            # Longer description or information about the
                               #   survey
controls_required: array[str]  # List of control parameter IDs that must be
                               #   loaded for use in the survey
rotation_sets:
    include_sets: array[str]   # List of names for rotation_sets to confirm
                               #    variables in rotation set are included
    exclude_variables: array[str] # List of variable IDs to exclude
candidates_path: string        # Path to file or directory storing candidate
                               #    question bank (defaults to "candidates")
programmer_note: string        # Note to survey programmer
note: string                   # Implementation notes
```

---

## Questionnaire (`questionnaire`)

### `preload`

Items loaded before any questions are presented to the respondent (background
operations). **Restricted to `compute` items only.** No `question`, `module`,
`statement`, or container types are permitted in `preload`.

```yaml
preload:
  - item_type: compute
    id: assign_condition
    description: "Assign respondent to experimental condition"
    command: "condition <- sample(c('A', 'B'), 1)"
    provides: [condition]
```

### `items`

Ordered list of survey content. May contain any item or container type.
All elements are optional unless specified as `REQUIRED`.

### `postload`

**Future feature.** Will hold post-survey operations (e.g., generating derived
variables from responses). Not yet implemented; reserved for schema
forward-compatibility.

---

## Item Types

All items have an `item_type` field. Simple items hold content; container items
hold other items in their `items` field.

### Statement (`item_type: statement`)

```yaml
item_type: statement
id: string                      # REQUIRED: Unique identifier
text: string                    # REQUIRED: Text of statement to display
keep_with_next: boolean         # Display on same screen as next item?
if_condition: string            # Display logic (R expression)
display_class: string           # How to display the text
programmer_note: string         # Note to survey programmer
note: string                    # Implementation notes
```

### Question (`item_type: question`)

```yaml
item_type: question
variable_id: str                # REQUIRED: Variable key from bank
source: enum                    # Bank to use: qbank (default) or candbank
hide_question: bool             # Hide question text? (useful for display_together)
if_condition: string            # Display logic for this survey only (R expression)
programmer_note: string         # Note to survey programmer
```

### Module (`item_type: module`)

```yaml
item_type: module
module_id: str                  # REQUIRED: Module key from module bank
if_condition: string            # Display logic for this survey only (R expression)
programmer_note: string         # Note to survey programmer
```

### Logic Block (`item_type: logic`)

```yaml
item_type: logic
id: string                      # REQUIRED: Unique identifier
condition: string               # REQUIRED: Condition to evaluate (R expression)
then: array[object]             # REQUIRED: Items if condition is true
  - item_type: ...
else: array[object]             # Items if condition is false
  - item_type: ...
note: string                    # Implementation notes
programmer_note: string         # Note to survey programmer
```

### Compute Block (`item_type: compute`)

Used in `preload` for pre-survey setup. Not for respondent-facing content.

```yaml
item_type: compute
id: string                      # REQUIRED: Unique identifier
description: string             # REQUIRED: What this computes/sets
command: string                 # REQUIRED: R expression to execute
requires: array[string]         # Parameter names required by command
provides: array[string]         # Parameter names created/set (required if
                                #    parameters used later in survey flow)
note: string                    # Implementation notes
programmer_note: string         # Note to survey programmer
```

---

## Container Item Types

Containers hold other items in their `items` field and may appear anywhere
in `questionnaire.items` (and within other containers).

### Section (`item_type: section`)

Ordered container of items. All BAS surveys use sections as the top-level
organizer, but sections are not required — items may appear directly in
`questionnaire.items` for simpler surveys.

```yaml
item_type: section
id: string                     # REQUIRED: Unique identifier
title: string                  # REQUIRED: Short name of section
items: array[object]           # REQUIRED: List of item elements
description: string            # Longer description of section contents
note: string                   # Implementation notes
```

### Randomization (`item_type: randomize`)

Group of items whose order will be randomized for each respondent.

```yaml
item_type: randomize
id: string                     # REQUIRED: Unique identifier
items: array[object]           # REQUIRED: List of item elements
description: string            # Description of randomization block
note: string                   # Implementation notes
programmer_note: string        # Note to survey programmer
```

### Split (`item_type: split`)

Route respondents down different item sequences based on a control parameter.

```yaml
item_type: split
id: string                     # REQUIRED: Unique identifier
description: string            # REQUIRED: Description of split
control_id: string             # REQUIRED: Control parameter determining path
paths: array[object]           # REQUIRED: Alternative paths (minimum 2)
  - id: string                 # REQUIRED: Unique identifier for path
    control_value: string      # REQUIRED: Parameter value triggering this path
    items: array[object]       # REQUIRED: Items for this path
note: string                   # Implementation notes
programmer_note: string        # Note to survey programmer
```

### Loop (`item_type: loop`)

Repeat a set of items multiple times, substituting `{i}` in variable names
and titles with values from `id_fills`.

```yaml
item_type: loop
id: string                     # REQUIRED: Unique identifier
id_fills: array[str]           # REQUIRED: Values to substitute for {i} in
                               #   variable names (e.g., ["1", "2", "3"])
items: array[object]           # REQUIRED: Items to repeat (questions must
                               #   have response_type: loop and {i} in varname)
title_fills: array[str]        # Text to substitute for {i} in titles
                               #   (defaults to id_fills values)
note: string                   # Implementation notes
programmer_note: string        # Note to survey programmer
```

### Display Together (`item_type: display_together`)

Display multiple items on the same screen or as a single response block.

```yaml
item_type: display_together
id: string                     # REQUIRED: Unique identifier
items: array[object]           # REQUIRED: Items to display together
note: string                   # Implementation notes
programmer_note: string        # Note to survey programmer
```

---

## Notes on `if_condition`

The `if_condition` field accepts R expressions referencing variable IDs and
control parameter IDs. Expressions should use only comparisons and logical
operators — no arbitrary function calls.

```yaml
if_condition: "nhd_sat <= 2"
if_condition: "dem_child == 1 & dem_childyng > 0"
if_condition: "condition == 'A'"
```

For DDI Lifecycle export, `if_condition` values are tagged with
`programLanguage="R"` and emitted as `CommandCode` elements inside
`IfThenElse` constructs. This is fully DDI-compliant without requiring
translation to a separate expression language.

For complex branching that routes to different item sequences, use the
`logic` item type instead of `if_condition`.
