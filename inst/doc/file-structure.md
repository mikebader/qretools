# qretools File Structure

**Version:** 1.0  
**Date:** December 17, 2025

---

## Overview

The qretools system uses a structured directory organization to separate different types of variables and metadata. This document describes the standard file structure and the conventions used for organizing survey project files.

---

## Directory Structure

```
project-root/
|
+-- banks/
|   |
|   +-- questions/              # Fielded survey questions
|   |   +-- demographics.yml
|   |   +-- neighborhoods.yml
|   |   +-- housing.yml
|   |   +-- ...
|   |
|   +-- generated/              # Variables constructed from other variables
|   |   +-- demographics-generated.yml
|   |   +-- ...
|   |
|   +-- control/                # Control parameters
|       +-- project.yml         # Stable parameters (JURISDICTION, ADDRESS)
|       +-- surveys/
|           +-- bas-2023.yml    # Survey-specific parameters
|           +-- bas-2024.yml
|           +-- ...
|
+-- value-labels/               # Value label definitions (shared across all banks)
|   +-- common.yml
|   +-- demographic.yml
|   +-- ...
|
+-- surveys/
|   +-- bas-2023/
|   |   +-- survey-config.yml
|   |   +-- survey-metadata.yml
|   |   +-- design/
|   |       +-- candidates/
|   |           +-- new-question.yml
|   |           +-- promoted/
|   |           +-- unused/
|   |
|   +-- bas-2024/
|   |   +-- ...
|   |
|   +-- bas-2025/
|       +-- ...
|
+-- project-config.yml
```

---

## Directory Purposes

### `banks/` - Variable Storage

All permanent variable storage is consolidated under the `banks/` directory, organized by variable type:

#### `banks/questions/`
- **Purpose:** Fielded survey questions
- **Content:** Questions that have been asked of respondents in actual surveys
- **File format:** YAML files organized by topic (e.g., `demographics.yml`, `neighborhoods.yml`)
- **Function:** `qt_read_question_bank()` / `qt_qbank()`

#### `banks/generated/`
- **Purpose:** Variables constructed from other variables
- **Content:** Variables created through data processing (e.g., combining split questions, recoding)
- **File format:** Same YAML schema as questions, with `variable_role: generated`
- **Example:** Combined income variable from separate low/high income questions
- **Function:** `qt_read_generated_variables()` / `qt_genbank()`

#### `banks/control/`
- **Purpose:** Control parameters and preloaded values
- **Content:** 
  - `project.yml` - Stable parameters used across surveys (JURISDICTION, ADDRESS)
  - `surveys/` - Survey-specific parameters (random assignments, computed values)
- **File format:** Same YAML schema as questions, with `variable_role: parameter`
- **Function:** `qt_read_control_parameters()` / `qt_ctrlbank()`

### `value-labels/` - Shared Label Definitions

- **Purpose:** Value label definitions used by all variable banks
- **Content:** Label sets for categorical variables (e.g., `satisfied5`, `yesno`, `education`)
- **Location:** Project root (not inside `banks/`) because labels are shared across all banks
- **File format:** Can be single file (`value-labels.yml`) or directory with multiple files
- **Function:** `qt_read_value_labels()` / `qt_vlabs()`

### `surveys/` - Survey Instances

Each subdirectory represents a specific survey instance (e.g., `bas-2023`, `bas-2024`):

- **Purpose:** Survey-specific configuration and design work
- **Key files:**
  - `survey-config.yml` - Which questions, in what order, for this survey
  - `survey-metadata.yml` - Survey-level metadata (title, PI, funding, etc.)
  - `design/candidates/` - Questions under consideration for this survey
- **Naming:** Directory names must match values in `surveys_used` field of questions

### `project-config.yml` - Project Configuration

- **Purpose:** Project-wide settings and paths
- **Content:** 
  - Paths to all banks and resources
  - Project metadata
  - IRB information
  - Custom settings
- **Function:** `qt_config()`

---

## Design Rationale

### Why `banks/` organization?

1. **Conceptual clarity:** All "banks" (permanent variable storage) grouped together
2. **Separation of concerns:** Clear distinction between questions (fielded), generated (constructed), and control (parameters)
3. **Extensibility:** Easy to add new bank types if needed
4. **Consistent naming:** Natural alignment between directory names and function names

### Why value labels at root?

1. **Shared resource:** Used by questions, generated variables, and parameters
2. **Not bank-specific:** Doesn't belong to any single bank
3. **Easy discovery:** Simple, predictable location

### Why `surveys/` separate from `banks/`?

1. **Different purpose:** Surveys reference variables; banks define variables
2. **Temporal organization:** Surveys are time-bound instances; banks are permanent repositories
3. **Design workflow:** Survey design happens separately from variable definition

---

## Path Configuration

Paths are configured in `project-config.yml`:

```yaml
paths:
  question_bank: "banks/questions"
  generated_bank: "banks/generated"
  control_bank: "banks/control"
  value_labels: "value-labels"
```

These can be customized per project, but the defaults follow the structure above.

---

## Reading Functions

Each directory has a corresponding read function:

| Directory | Function | Alias | Returns |
|-----------|----------|-------|---------|
| `banks/questions/` | `qt_read_question_bank()` | `qt_qbank()` | Questions from surveys |
| `banks/generated/` | `qt_read_generated_variables()` | `qt_genbank()` | Constructed variables |
| `banks/control/` | `qt_read_control_parameters()` | `qt_ctrlbank()` | Control parameters |
| All banks | `qt_read_all_variables()` | `qt_varbank()` | Combined view |
| `value-labels/` | `qt_read_value_labels()` | `qt_vlabs()` | Value label sets |

All read functions:
- Accept a `config` parameter (defaults to cached config)
- Can override with explicit `path` parameter for testing
- Return S3 objects with consistent structure
- Include metadata about source files

---

## Usage Examples

### Reading questions
```r
library(qretools)

# Read question bank
qbank <- qt_qbank()

# Access specific question
qbank$variables$nhd_sat

# List questions by file
qbank$meta$indices$by_file

# List questions alphabetically  
qbank$meta$indices$by_varname
```

### Reading all variables
```r
# Read all banks at once
allvars <- qt_varbank()

# Access by type
allvars$meta$indices$by_type$questions
allvars$meta$indices$by_type$generated
allvars$meta$indices$by_type$control
```

### Reading value labels
```r
# Read value labels
vlabs <- qt_vlabs()

# Access specific label set
vlabs$labels$satisfied5
```

---

## File Naming Conventions

### YAML files in banks

- **Descriptive names:** Use clear topic names (e.g., `demographics.yml`, `neighborhoods.yml`)
- **Multiple files OK:** Split questions into multiple files by topic
- **Generated suffix:** Use `-generated` suffix for generated variable files (e.g., `demographics-generated.yml`)
- **All `.yml` files read:** All `.yml` files in a directory are automatically read and combined

### Survey directories

- **Must match surveys_used:** Directory name must match exactly what's in question `surveys_used` field
- **Any naming scheme:** Can use years (`bas-2023`), waves (`wave-01`), cohorts, etc.
- **Consistency required:** Be consistent within a project

---

## Migration from Other Structures

If you have existing `question-bank/` structure:

```bash
# Old structure
question-bank/
  demographics.yml
  neighborhoods.yml
  value-labels.yml

# New structure - move files
mkdir -p banks/questions banks/generated banks/control/surveys
mv question-bank/*.yml banks/questions/
mv banks/questions/value-labels.yml value-labels.yml

# Update project-config.yml paths
```

---

## Best Practices

1. **One source of truth:** Questions defined in banks, surveys just reference them
2. **Meaningful names:** Use descriptive file and directory names
3. **Logical grouping:** Group related questions in same file
4. **Generated separate:** Keep generated variables in separate files from questions
5. **Document derivations:** Use `derivation_method` field to document how variables are constructed
6. **Validate early:** Use validation functions before fielding surveys

---

## See Also

- `question-bank-yaml-schema-v2_1.md` - Question/variable YAML structure
- `value-labels-yaml-schema.md` - Value label YAML structure
- `survey-config-yaml-schema.md` - Survey configuration structure
- `parameters-yaml-schema.md` - Parameter-specific documentation
