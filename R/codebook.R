# Codebook Functions
#
# A codebook documents one survey's collected data: the questions as asked
# (the version used by that survey) with response counts from the data. For a
# cross-survey dictionary of variables that needs no data, see qt_dictionary().
# Rendering is shared with qt_dictionary() (R/dictionary.R).

#' Generate a Codebook for a Survey's Data
#'
#' Document the data collected for one survey. Summarizes `data` against the
#' survey instrument with [qt_data_summary()] and renders each variable with
#' its question text, storage type and response counts. Supports plain-text
#' output to the console or a file, and HTML/PDF output via R Markdown.
#'
#' @inheritParams qt_data_summary
#' @param missing_in_table Logical. If `TRUE` (default), coded missing values
#'   appear as rows of each factor or multiple-response variable's response
#'   table, after the substantive values, using their labels from the
#'   missing-codes file. If `FALSE`, they are listed in a separate "Coded
#'   missing values" table. Numeric variables list them as extra rows of their
#'   statistics table.
#' @param missing_zeros Logical. If `TRUE` (default), every defined missing code
#'   gets a row for each factor, multiple-response and numeric variable, with a
#'   count of 0 where the code does not occur. If `FALSE`, only codes that occur
#'   in the data are shown.
#' @param format Character string. Output format: \code{"text"} (default),
#'   \code{"html"}, or \code{"pdf"}. HTML and PDF require the \pkg{rmarkdown}
#'   package to be installed.
#' @param output_file Character string or \code{NULL}. Path for the output
#'   file. For \code{format = "text"}, if \code{NULL} the result is printed to
#'   the console; if a path is given, output is written to that file. For
#'   \code{format = "html"} or \code{"pdf"}, if \code{NULL} a temporary file
#'   is used.
#' @param open Logical. If \code{TRUE}, the output file is opened after
#'   rendering (HTML/PDF only). Default: \code{FALSE}.
#' @param title Character string. Title for the codebook document. Default:
#'   \code{"Codebook: <survey_id>"}.
#' @param ... Additional arguments passed to \code{rmarkdown::render()} for
#'   HTML/PDF output.
#'
#' @return For \code{format = "text"} with no \code{output_file}: invisibly
#'   returns the character vector of text lines. For all other cases: invisibly
#'   returns the path to the output file.
#'
#' @details
#' Each variable in the data is documented in instrument order, followed by
#' data columns that are not in the instrument. A variable entry shows:
#' \itemize{
#'   \item variable ID, title, storage type and universe
#'   \item question text from the version the survey used
#'   \item counts of substantive responses, coded missing values by type (see
#'     `missing_codes`) and `NA`
#'   \item factor variables: counts for every value label, plus any values with
#'     no label; numeric variables: minimum, maximum, mean and standard
#'     deviation
#'   \item multiple-response questions: one entry per option column
#'   \item character variables are marked as character, with nothing else
#'     documented
#'   \item composite variables, control parameters and columns not in the
#'     instrument are marked as needing compilation
#' }
#' Instrument variables with no column in the data are listed in the header
#' rather than documented.
#'
#' @seealso [qt_data_summary()], [qt_read_missing_codes()], [qt_dictionary()]
#'
#' @examples
#' \dontrun{
#' vl <- qt_read_value_labels()
#'
#' # missing codes come from the project's _missing_codes.yml, if it has one
#' my_qre |>
#'   qt_codebook(dat, value_labels = vl,
#'               format = "html", output_file = "codebook-bas-2026.html")
#' }
#'
#' @export
qt_codebook <- function(qre, data, value_labels = NULL, missing_codes = NULL,
                        missing_in_table = TRUE, missing_zeros = TRUE,
                        survey_id = NULL,
                        format = c("text", "html", "pdf"),
                        output_file = NULL, open = FALSE, title = NULL,
                        config = NULL, ...) {
  format <- match.arg(format)

  if (inherits(qre, "qt_bank")) {
    stop("qt_codebook() documents one survey's data and takes a qt_qre as ",
         "its first argument. To document a bank without data, use ",
         "qt_dictionary().", call. = FALSE)
  }
  if (missing(data)) {
    stop("'data' is required. To document variables without data, use ",
         "qt_dictionary().", call. = FALSE)
  }

  mc <- .qt_resolve_missing_codes(missing_codes, config)
  ds <- qt_data_summary(qre, data, value_labels = value_labels,
                        missing_codes = mc %||% FALSE, survey_id = survey_id,
                        config = config)
  survey_id <- attr(ds, "survey_id")

  entries <- .qt_codebook_entries(ds, mc = mc,
                                  missing_in_table = missing_in_table,
                                  missing_zeros = missing_zeros)
  if (length(entries) == 0)
    stop("No variables to document", call. = FALSE)

  not_in_data <- ds$variable_id[ds$status == "missing_from_data"]
  title <- title %||% paste0("Codebook: ", survey_id)

  if (format == "text") {
    .qt_codebook_text(entries, survey = survey_id, title = title,
                      output_file = output_file, not_in_data = not_in_data)
  } else {
    .qt_codebook_rmd(entries, format = format, survey = survey_id,
                     title = title, output_file = output_file, open = open,
                     not_in_data = not_in_data, ...)
  }
}


# Internal: Build codebook entries from a qt_data_summary --------------------------
#
# One entry per summary row, except instrument variables missing from the data.
# Fields shared with dictionary entries keep the same names so both functions
# use the same renderers; data fields (status, flags, n, n_coded, n_na, values,
# stats, missing, parent_id, options, version_id) are added.
#
# @param ds A qt_data_summary object from qt_data_summary()
# @param mc A qt_missing_codes object or NULL
# @param missing_in_table Logical. Append coded missing values to the response
#   table of entries that have one (factor, multiple-response)
# @param missing_zeros Logical. List every defined code, with n = 0 where the
#   code does not occur, instead of only the codes present in the data
# @return List of entry lists
.qt_codebook_entries <- function(ds, mc = NULL, missing_in_table = TRUE,
                                 missing_zeros = TRUE) {
  resolved <- attr(ds, "resolved") %||% list()
  na_null  <- function(x) if (length(x) == 0 || is.na(x)) NULL else x

  lapply(which(ds$status != "missing_from_data"), function(i) {
    id  <- ds$variable_id[i]
    res <- resolved[[id]] %||% list()
    sm  <- ds$summary[[i]]

    vals <- sm[["values"]]
    miss <- sm[["missing"]]

    # Documented variables (those with a response table or numeric stats) list
    # missing codes in the order defined; absent codes get n = 0 if requested.
    if (!is.null(mc) && nrow(mc) > 0 && (!is.null(vals) || !is.null(sm[["stats"]]))) {
      n_found <- if (!is.null(miss)) stats::setNames(miss$n, miss$value) else integer()
      n_all   <- unname(n_found[mc$value])
      n_all[is.na(n_all)] <- 0L
      miss <- data.frame(type = mc$type, value = mc$value, label = mc$label,
                         n = as.integer(n_all), stringsAsFactors = FALSE)
      if (!missing_zeros) miss <- miss[miss$n > 0, , drop = FALSE]
      if (nrow(miss) == 0) miss <- NULL
    }

    if (missing_in_table && !is.null(vals) && !is.null(miss)) {
      if (!"label" %in% names(vals)) vals$label <- rep("", nrow(vals))
      vals <- rbind(
        vals[, c("value", "label", "n"), drop = FALSE],
        data.frame(value = miss$value, label = miss$label, n = miss$n,
                   stringsAsFactors = FALSE)
      )
      miss <- NULL
    }

    # Numeric variables: one table with statistics as rows; coded missing
    # values follow as rows when they are listed in the table.
    stats_table <- NULL
    st <- sm[["stats"]]
    if (!is.null(st)) {
      fmt <- function(v) {
        if (is.na(v)) "NA" else format(round(v, 2), trim = TRUE, scientific = FALSE)
      }
      stats_table <- data.frame(
        statistic = c("Valid responses", "Minimum", "Maximum", "Mean",
                      "Standard deviation"),
        value     = c(as.character(ds$n[i]), fmt(st[["min"]]), fmt(st[["max"]]),
                      fmt(st[["mean"]]), fmt(st[["sd"]])),
        stringsAsFactors = FALSE
      )
      if (missing_in_table && !is.null(miss)) {
        stats_table <- rbind(
          stats_table,
          data.frame(statistic = paste0(miss$label, " (", miss$value, ")"),
                     value = as.character(miss$n), stringsAsFactors = FALSE)
        )
        miss <- NULL
      }
    }

    list(
      variable_id    = id,
      title          = res$title %||% res$option_title %||% "",
      storage_type   = na_null(ds$storage_type[i]) %||% "unclassified",
      text           = res$question_text %||% res$option_text %||% res$description,
      value_labels   = NULL,
      value_label_id = na_null(ds$value_label_id[i]),
      surveys_used   = NULL,
      universe       = res$universe,
      vargroup       = res$vargroup,
      cite           = res$cite %||% res$source,
      note           = res$note,
      status         = ds$status[i],
      flags          = ds$flags[i],
      n              = na_null(ds$n[i]),
      n_coded        = na_null(ds$n_coded[i]),
      n_na           = na_null(ds$n_na[i]),
      values         = vals,
      stats          = st,
      stats_table    = stats_table,
      missing        = miss,
      parent_id      = na_null(ds$parent_id[i]),
      options        = {
        kids <- ds$variable_id[!is.na(ds$parent_id) & ds$parent_id == id]
        if (length(kids) > 0) kids else NULL
      },
      version_id     = na_null(ds$version_id[i])
    )
  })
}


# Internal: Plain-text lines for the data part of a codebook entry -----------------
#
# Returns character(0) for entries without data fields (dictionary entries).
#
# @param entry A prepared entry list
# @return Character vector of lines
.qt_text_data_lines <- function(entry) {
  status <- entry[["status"]]
  if (is.null(status)) return(character())

  lines <- character()
  if (!is.null(entry[["parent_id"]]))
    lines <- c(lines, "", paste0("Option of: ", entry[["parent_id"]]))
  if (!is.null(entry[["options"]]))
    lines <- c(lines, "",
               paste0("Option variables: ",
                      paste(entry[["options"]], collapse = ", ")))

  if (status == "needs_compilation") {
    lines <- c(lines, "", paste0("Needs compilation: ", entry[["flags"]]))
    if (!is.null(entry[["n"]]))
      lines <- c(lines, paste0("Non-missing values: ", entry[["n"]]))
    return(lines)
  }
  if (status != "ok") return(lines)

  lines <- c(lines, "",
             sprintf("Responses: %d valid, %d coded missing, %d NA",
                     entry[["n"]] %||% 0L, entry[["n_coded"]] %||% 0L,
                     entry[["n_na"]] %||% 0L))

  vals <- entry[["values"]]
  if (!is.null(vals) && nrow(vals) > 0) {
    for (i in seq_len(nrow(vals))) {
      lab <- if ("label" %in% names(vals) && !is.na(vals$label[i]) &&
                nzchar(vals$label[i]))
               paste0(": ", vals$label[i]) else ""
      lines <- c(lines, sprintf("  %s%s  (n = %d)", vals$value[i], lab, vals$n[i]))
    }
  }

  tab <- entry[["stats_table"]]
  if (!is.null(tab) && nrow(tab) > 0) {
    for (i in seq_len(nrow(tab))) {
      lines <- c(lines, sprintf("  %s: %s", tab$statistic[i], tab$value[i]))
    }
  }

  mi <- entry[["missing"]]
  if (!is.null(mi) && nrow(mi) > 0) {
    lines <- c(lines, "", "Coded missing values")
    for (i in seq_len(nrow(mi))) {
      lines <- c(lines, sprintf("  %s (%s): %s  (n = %d)",
                                mi$value[i], mi$type[i], mi$label[i], mi$n[i]))
    }
  }

  if (nzchar(entry[["flags"]] %||% ""))
    lines <- c(lines, "", paste0("Flags: ", entry[["flags"]]))

  lines
}
