# qretools (development version)

## Data documentation

* New `qt_apply_codings()`: applies declared storage types, value labels and
  missing codes to collected data. Factors keep missing codes as labelled
  levels; integer/numeric variables become `haven::labelled_spss()` with the
  codes registered as user-missing; multiple-response options use `yesno_01`.
  Problems are collected in `attr(, "qt_codings_report")`. Adds `haven` to
  Imports and drops unused `purrr`.

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
