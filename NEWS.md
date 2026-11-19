# qretools (development version)

## Data documentation

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
* HTML codebooks and dictionaries now use semantic `qt-` CSS classes
  (`qt-variable`, `qt-question-text`, `qt-response-table`, `qt-missing-row`,
  ...) and ship a default stylesheet. Styles apply in this order: package
  default, `css/codebook.css` in the project (folder set by `paths: css:`), then
  files passed to the new `css` argument. `qt_copy_css()` copies the default
  into the project for editing.
* `qt_codebook()` and `qt_dictionary()` gain `style` (`"gitbook"`, a
  `bookdown`-like page and the default, or `"document"`) and
  `group_by = "vargroup"` to group variables under `vargroup` headings (HTML,
  PDF and text). The R Markdown template `inst/templates/codebook.Rmd` is
  replaced by Markdown generated in R.

## version 0.0.1

* Completes entire pipeline for design and production of questionnaires
* Takes questions and control parameters from banks and produces question
* Allows modules
* Minimal level of version control
* Produces markdown questionnaire and formatted HTML questionnaire

## version 0.0.0.9000
* Initial CRAN submission.
