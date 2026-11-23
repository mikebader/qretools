# Apply Codings
#
# Apply the storage types, value labels and missing codes defined in a
# questionnaire's metadata to collected data. Type resolution reuses
# .qt_ds_lookup() so it is identical to qt_data_summary() and qt_codebook().

#' Apply Codings to Survey Data
#'
#' Convert each column of collected data to the type its question definition
#' declares, label factors from the value-label sets, and mark missing-value
#' codes.
#'
#' @inheritParams qt_data_summary
#' @param value_labels A value-label object from [qt_read_value_labels()].
#'   Required.
#'
#' @return `data` with instrument variables recoded (columns not in the
#'   instrument are returned unchanged). A data frame of problems found is
#'   attached as `attr(, "qt_codings_report")`, with columns `variable_id`,
#'   `issue` and `detail`; each kind of problem is also signalled by one
#'   warning.
#'
#' @details
#' Treatment by `storage_type`:
#'
#' * **factor**: a factor whose levels are the full labels from the label set,
#'   followed by the labels of every missing code in the project's missing-codes
#'   file (so levels are the same across variables). Missing codes stay in the
#'   data as levels. Values with no label are kept as levels named by their
#'   code, and reported as `unlabeled_values`. The code behind each level is
#'   kept in `attr(x, "qt_codes")`. Factors cannot carry `haven`'s user-missing
#'   flag; use [qt_zap_missing()] to turn their missing levels into `NA`.
#' * **integer**, **numeric**: coerced to the declared type, then wrapped with
#'   [haven::labelled_spss()]. Declared missing codes, and any other negative
#'   value (reported as `negative_not_in_codes`), are registered as user-missing
#'   values but stay in the data. An integer variable holding non-whole values
#'   stays numeric (`non_integer_values`). Use [haven::zap_missing()] to turn
#'   user-missing values into `NA` before analysis.
#' * **character**: unchanged (a missing code such as `"-8"` stays as is).
#' * **multiple_response** options: factors labelled with the `yesno_01` label
#'   set.
#' * Composite variables, containers and control parameters are left alone.
#'
#' @seealso [qt_data_summary()], [qt_read_missing_codes()]
#'
#' @examples
#' \dontrun{
#' bas <- qt_apply_codings(qre, raw, value_labels = vallabs)
#' attr(bas, "qt_codings_report")
#' }
#'
#' @export
qt_apply_codings <- function(qre, data, value_labels, missing_codes = NULL,
                             survey_id = NULL, config = NULL) {
  if (!is.data.frame(data))
    stop("'data' must be a data frame", call. = FALSE)
  if (missing(value_labels) || is.null(value_labels))
    stop("'value_labels' is required to label factor variables", call. = FALSE)

  if (inherits(qre, "qt_qre")) qre <- qt_process(qre, config = config)
  if (!inherits(qre, "qt_qreconfig"))
    stop("'qre' must be a qt_qre or a processed survey (class 'qt_qreconfig')",
         call. = FALSE)

  survey_id <- survey_id %||% qre$meta$id
  if (is.null(survey_id) || !nzchar(survey_id))
    stop("Cannot determine survey_id; supply 'survey_id'", call. = FALSE)

  mc     <- .qt_resolve_missing_codes(missing_codes, config)
  lookup <- .qt_ds_lookup(qre, survey_id)

  issues <- list()
  add <- function(variable_id, issue, detail = "") {
    issues[[length(issues) + 1]] <<- data.frame(
      variable_id = variable_id, issue = issue, detail = detail,
      stringsAsFactors = FALSE)
  }

  for (r in lookup) {
    if (r$role %in% c("container", "composite", "composite_child")) next
    vid <- r$variable_id
    if (!vid %in% names(data)) { add(vid, "not_in_data"); next }

    st <- r$storage_type
    if (is.na(st)) { add(vid, "no_storage_type"); next }
    if (st == "character") next

    if (st %in% c("integer", "numeric")) {
      res <- .qt_ac_numeric(data[[vid]], st, mc)
      data[[vid]] <- res$x
      if (res$n_failed > 0)
        add(vid, "coercion_failed", paste(res$n_failed, "value(s) became NA"))
      if (res$n_fractional > 0)
        add(vid, "non_integer_values", "kept as numeric")
      if (length(res$undeclared) > 0)
        add(vid, "negative_not_in_codes",
            paste(res$undeclared, collapse = ", "))

    } else if (st %in% c("factor", "multiple_response")) {
      vl_id <- if (st == "multiple_response") "yesno_01" else r$value_label_id
      set   <- if (!is.na(vl_id)) value_labels$labels[[vl_id]] else NULL
      if (is.null(set)) {
        add(vid, "no_label_set", if (is.na(vl_id)) "" else vl_id)
        next
      }
      res <- .qt_ac_factor(data[[vid]], set, mc)
      data[[vid]] <- res$x
      if (length(res$unlabeled) > 0)
        add(vid, "unlabeled_values", paste(res$unlabeled, collapse = ", "))

    } else {
      add(vid, "unrecognized_storage_type", st)
    }
  }

  # Data columns the instrument does not account for
  known    <- vapply(lookup, function(r) r$variable_id, character(1))
  controls <- qre$indices$controls %||% character()
  for (nm in setdiff(names(data), c(known, controls)))
    add(nm, "not_in_instrument")

  report <- if (length(issues) > 0) do.call(rbind, issues) else
    data.frame(variable_id = character(), issue = character(),
               detail = character(), stringsAsFactors = FALSE)

  labels <- c(
    not_in_data               = "instrument variable(s) not in data",
    not_in_instrument         = "data column(s) not in instrument",
    no_storage_type           = "variable(s) with no storage_type",
    no_label_set              = "factor variable(s) with no label set",
    unrecognized_storage_type = "variable(s) with unrecognized storage_type",
    coercion_failed           = "numeric variable(s) with non-numeric values",
    non_integer_values        = "integer variable(s) with non-whole values",
    negative_not_in_codes     = "variable(s) with negative values not in missing codes",
    unlabeled_values          = "factor variable(s) with values lacking labels"
  )
  for (k in unique(report$issue)) {
    v <- report$variable_id[report$issue == k]
    warning(sprintf("%s: %d (%s%s)", labels[[k]], length(v),
                    paste(utils::head(v, 10), collapse = ", "),
                    if (length(v) > 10) ", ..." else ""),
            call. = FALSE)
  }

  attr(data, "qt_codings_report") <- report
  data
}


# Internal: integer/numeric column -> typed, user-missing-aware vector -------
.qt_ac_numeric <- function(x, storage, mc = NULL) {
  num    <- if (is.numeric(x)) as.numeric(x) else
    suppressWarnings(as.numeric(as.character(x)))
  failed <- !is.na(x) & is.na(num)
  n_frac <- sum(!is.na(num) & num != round(num))

  codes <- numeric(); txt <- character()
  if (!is.null(mc) && nrow(mc) > 0) {
    codes <- suppressWarnings(as.numeric(mc$value))
    txt   <- mc$label
    keep  <- !is.na(codes)
    codes <- codes[keep]; txt <- txt[keep]
  }

  neg        <- unique(num[!is.na(num) & num < 0])
  undeclared <- sort(setdiff(neg, codes))

  conv <- if (storage == "integer" && n_frac == 0) as.integer else as.numeric
  out  <- conv(num)

  na_vals <- unique(conv(c(codes, undeclared)))
  if (length(na_vals) > 0) {
    labs <- if (length(codes) > 0) stats::setNames(conv(codes), txt) else NULL
    out  <- haven::labelled_spss(out, labels = labs, na_values = na_vals)
  }

  list(x = out, n_failed = sum(failed), n_fractional = n_frac,
       undeclared = undeclared)
}


# Internal: column -> factor labelled from a label set plus missing codes ----
.qt_ac_factor <- function(x, set, mc = NULL) {
  lab_codes <- as.character(set$values)
  lab_text  <- as.character(unname(set$labels))

  mc_codes <- character(); mc_text <- character()
  if (!is.null(mc) && nrow(mc) > 0) {
    keep     <- !mc$value %in% lab_codes
    mc_codes <- mc$value[keep]
    mc_text  <- mc$label[keep]
  }

  codes     <- as.character(x)
  all_codes <- c(lab_codes, mc_codes)
  all_text  <- c(lab_text, mc_text)

  unlabeled <- sort(setdiff(unique(codes[!is.na(codes)]), all_codes))
  all_codes <- c(all_codes, unlabeled)
  all_text  <- c(all_text, unlabeled)

  out <- factor(codes, levels = all_codes, labels = all_text)
  attr(out, "qt_codes")   <- stats::setNames(all_codes, all_text)
  attr(out, "qt_missing") <- mc_text   # level names that are missing codes
  list(x = out, unlabeled = unlabeled)
}


#' Turn Missing Codes into NA
#'
#' Convert the missing codes in data from [qt_apply_codings()] to `NA`, so the
#' data can be analyzed directly.
#'
#' @param data A data frame, usually the result of [qt_apply_codings()].
#' @inheritParams qt_data_summary
#'
#' @return `data` with
#'   * user-missing values in `haven` columns set to `NA`
#'     ([haven::zap_missing()]),
#'   * missing-code levels of factors set to `NA` and dropped from the levels,
#'   * character values equal to a missing code (e.g., `"-8"`) set to `NA`.
#'
#' @seealso [qt_apply_codings()]
#' @export
qt_zap_missing <- function(data, missing_codes = NULL, config = NULL) {
  if (!is.data.frame(data))
    stop("'data' must be a data frame", call. = FALSE)
  mc   <- .qt_resolve_missing_codes(missing_codes, config)
  data <- haven::zap_missing(data)

  for (nm in names(data)) {
    x <- data[[nm]]
    if (is.factor(x) && !is.null(attr(x, "qt_missing"))) {
      data[[nm]] <- .qt_zap_factor(x)
    } else if (is.character(x) && !is.null(mc)) {
      x[x %in% mc$value] <- NA_character_
      data[[nm]] <- x
    }
  }
  data
}

# Internal: drop missing-code levels from a factor made by .qt_ac_factor() ---
.qt_zap_factor <- function(x) {
  codes <- attr(x, "qt_codes")
  keep  <- setdiff(levels(x), attr(x, "qt_missing"))
  out   <- factor(as.character(x), levels = keep)
  attr(out, "qt_codes") <- codes[names(codes) %in% keep]
  out
}
