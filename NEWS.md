# qretools (development version)

## Data documentation

* New `qt_data_summary()`: joins a survey (`qt_qre`/`qt_qreconfig`) to collected
  data, resolves the question version used by the survey, reconciles
  instrument and data columns, and summarizes each variable by storage type
  (factor counts by code and label, numeric min/max/mean/sd, multiple-response
  options, skip codes via `skip_labels`).
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
