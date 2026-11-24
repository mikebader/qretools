# qretools (development version)

## Data documentation

* New `qt_apply_codings()`: applies declared storage types, value labels and
  missing codes to collected data. Missing codes stay in the data (factor
  levels, integer/numeric values, character strings) and are tagged for
  `qt_zap_missing()`; multiple-response options use `yesno_01`. Problems are
  collected in `attr(, "qt_codings_report")`. Drops unused `purrr` from Imports.
* New `qt_read_variable_groups()` and `paths: variable_groups:` setting.
  `_variable_groups.yml` (project root by default) lists the `vargroup` codes
  with display names and section order, plus the label of the catch-all section.
* `qt_codebook()` gains `variable_groups` (default `FALSE`). With `TRUE` the
  codebook is topical: sections by `vargroup` in the order of
  `_variable_groups.yml`, instrument order within a section, child variables
  following their parent, and a final "Not classified" section.
* New `qt_zap_missing()`: turns the missing codes left by `qt_apply_codings()`
  into `NA`.

* New `qt_data_summary()`: joins a survey (`qt_qre`/`qt_qreconfig`) to collected
  data, resolves the question version used by the survey, reconciles
  instrument and data columns, and summarizes each variable by storage type
  (factor counts by code and label, numeric min/max/mean/sd, multiple-response
  options, coded missing values counted by type).
* New `qt_read_missing_codes()` and `paths: missing_codes:` setting. Project
  missing-value codes (skip, item non-response, breakoff, ...) are defined by
  type in `_missing_codes.yml` (the default; override the path in
  `_qretools.yml`). No file means no codes. `qt_data_summary()` and
  `qt_codebook()` read them through the `missing_codes` argument.
* New `qt_codebook(qre, data, ...)`: text/HTML/PDF codebook for one survey's
  data with response counts. Takes `qre` first so it works in a pipe.
* **Breaking:** the bank-based `qt_codebook(bank, ...)` is renamed
  `qt_dictionary(bank, ...)`. `qt_codebook()` now takes a survey and data.

## version 0.0.1

* Completes entire pipeline for design and production of questionnaires
* Takes questions and control parameters from banks and produces question
* Allows modules
* Minimal level of version control
* Produces markdown questionnaire and formatted HTML questionnaire

## version 0.0.0.9000
* Initial CRAN submission.
