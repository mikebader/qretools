# qretools Near-Term Features

**Last Updated:** December 18, 2025

This document tracks features planned for near-term implementation. For longer-term planning, see the Implementation Phase Checklist in the project root.

---

## Priority: High (Usability - Before Wider Use)

Essential for good user experience:

- [ ] **Print methods for bank objects**
  - `print.qt_qbank()` - Brief summary of question bank
  - `print.qt_genbank()` - Brief summary of generated variables
  - `print.qt_ctrlbank()` - Brief summary of control parameters
  - `print.qt_vlabs()` - Brief summary of value labels
  
- [ ] **Summary methods for detailed views**
  - `summary.qt_qbank()` - Detailed question bank info
  - `summary.qt_genbank()` - Detailed generated variables info
  - `summary.qt_ctrlbank()` - Detailed control parameters info
  - `summary.qt_vlabs()` - Detailed value labels info
  
- [ ] **Print method for individual variables**
  - Nice display when accessing `qbank$variables$nhd_sat`
  - Show key fields formatted nicely

---

## Priority: Medium (Enhancement)

Nice to have but not blocking:

- [ ] **Custom user-defined indices for question banks**
  - Allow users to define custom groupings/sorts
  - Example: by topic, by data type, by source
  
- [ ] **Survey-specific naming convention validation**
  - Project-specific rules (e.g., BAS requires 3-letter prefix)
  - Configurable in project-config.yml
  - Warning vs. error levels
  
- [ ] **User-extensible validation rules**
  - Allow projects to add custom validation checks
  - Hook system for pre/post validation
  - Example: check against project-specific requirements

---

## Completed During Implementation ✅

Features that were planned but already implemented:

- ✅ **Multi-part question validation** (creates_variables / variable_parts)
  - Implemented in `.qt_validate_variables()`
  - Validates all variables in creates_variables exist in variable_parts
  - Validates child variable structure

---

## Implementation Notes

### Print Methods
```r
# Desired output format
print.qt_qbank(qbank)
# qretools Question Bank
# 
# Variables: 5
# Source: banks/questions/ (directory)
# Files: test-questions.yml
# 
# Variables: nhd_cohes1, nhd_cohes2, nhd_nyrs, nhd_pbviol, nhd_sat
```

### Summary Methods
```r
# More detailed than print
summary.qt_qbank(qbank)
# Should show:
# - Breakdown by vargroup
# - Breakdown by type (factor, integer, etc.)
# - Source files with counts
# - Survey usage statistics
```

---

## Adding New Features

When implementing features from this list:

1. Create feature branch
2. Implement with tests
3. Update this document (move to completed section)
4. Merge to main
5. Update version number

---

## See Also

- `/implementation_phase_checklist.md` - Full project implementation plan
- `/inst/doc/file-structure.md` - System file organization
- `/question-bank-yaml-schema-v2_1.md` - YAML schema reference
