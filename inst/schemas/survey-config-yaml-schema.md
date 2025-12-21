# Survey Configuration Specification

## Metadata (`meta`)

Survey design specifications for the entire survey

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
programmer_note: string         # Note to survey programmer
note: string                    # Implementation notes
```

## Item-Type Specifications

The following will be types of `item` elements. All elements are optional unless specified as `REQUIRED`. 

### Statement

`item.item_type = statement`

```yaml
item_type: statement
text: string                    # REQUIRED: Text of statement to print
keep_with_next: boolean         # Should element be displayed on same screen/
                                # text as next item?
if_condition: string            # Display logic
display_class: string           # How should the text be displayed 
programmer_note: string         # Note to survey programmer
note: string                    # Implementation notes

```

### Question

`item.item_type = question`

```yaml
item_type: question
variable_id: str                # REQUIRED: Variable key from bank
source: enum                    # Identify bank to use, one of: 
                                # qbank (default) or candbank
hide_question: bool             # Should question text be hidden? 
                                #   (Useful for displaying questions together
                                #    e.g., address fields)
programmer_note: string         # Note to survey programmer
if_condition: string            # Display logic (for this survey only)
```

### Module

`item.item_type = module`

```yaml
item_type: module
module_id: str                  # REQUIRED: Module key used from bank
programmer_note: string         # Note to survey programmer
if_condition: string            # Display logic (for this survey only)
```

### Logic Block

`item.item_type = logic`

```yaml
item_type: logic
condition: string              # REQUIRED - Condition to evaluate (R expression)
then: array[object]           # REQUIRED - Items to execute if condition true
  - item_type: ...            # Nested items 
else: array[object]           # Items to execute if condition false
  - item_type: ...            # Nested items
note: string                  # Implementation notes
programmer_note: string       # Note to survey programmer
```

### Compute Block (Internal)

`item.item_type = compute`

```yaml
item_type: compute
description: string            # REQUIRED - What this computes/sets
command: string                # REQUIRED - R expression to execute
requires: array[string]        # Parameter names required in command
provides: array[string]        # Parameter names created/set (required if
                               # parameters used later in survey flow)
note: string                   # Implementation notes
programmer_note: string        # Note to survey programmer
```

## Container Specifications

### Section (`section`)

Ordered container of items. Nested inside `sections` object.

```yaml
id: string                     # REQUIRED: Unique identifier for section
title: string                  # REQUIRED: Short name of section
items: array[object]           # REQUIRED: List of item elements
description: string            # Longer description of section contents
note: string                   # Implementation notes
```

### Randomization (`randomize`)

Group of variables with order to be randomized

```yaml
id: string                     # REQUIRED: Unique identifier for randomization
                               #   block
items: array[object]           # REQUIRED: List of item elements
description: string            # Longer description of section contents
note: string                   # Implementation notes
programmer_note: string        # Note to survey programmer
```

### Split (`split`)

Provide two or more sequences for items in survey

```yaml
id: string                     # REQUIRED: Unique identifier
description: string            # REQUIRED: Description of section contents
control_id: string             # REQUIRED: Unique id of control parameter 
                               #   that determines split
paths: array[object]           # REQUIRED: Alternative paths (min = 2)
  - id: string                 # REQUIRED: Unique identifier of path
    control_value: string      # REQUIRED: Value of parameter for path to be 
                               #   followed
    items: array[object]       # REQUIRED: List of item elements
note: string                   # Implementation notes
programmer_note: string        # Note to survey programmer
```

### Loop (`loop`) 

Creates logic to loop over same quesitons multiple times. Questions included in `loop` objects must have `response_type: loop`

```yaml
id: string                     # REQUIRED: Unique identifier
id_fills: array[str]           # REQUIRED - List of strings to fill {i} in
                               #            variable name
items: array[object]           # REQUIRED - Items to be repeated through loops
title_fills: array[str]        # Text to fill title in constructed variables
                               #    (defaults to items in id_fills)
note: string                   # Implementation notes
programmer_note: string        # Note to survey programmer
```

### Display Together (`display_together`)

Creates containers that display multiple variables together

```yaml
id: string                     # REQUIRED: Unique identifier
items: array[object]           # REQUIRED: List of items to display on same 
                               #   screen/in same response
note: string                   # Implementation notes
programmer_note: string        # Note to survey programmer
```



