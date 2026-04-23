# TODO:

* *** Need to figure out how to add candidate questions into qbank for use in 
  survey construction (maybe include an option in the `qt_qre()` method?)
* Need to add `restriction_reason` to YAML specification for variables
* .qt_resolve_path() should handle paths with .yml extension
* Missing value codes (skiplabs) - add to _qretools.yml config and survey 
  metadata before implementing clean() functions
* Module YAML schema needs a `versions` field (analogous to question bank
  versions). Module versions would control module-level fields (e.g.,
  `intro_text`, `questions` list) but would NOT propagate version selection
  to individual questions. Once schema is defined, extend `qt_add_module()`
  with a `version` parameter backed by `.qt_resolve_version()`.

# Solved:
* *** `qt_add_display_together()` method is not including the first item in `...`
