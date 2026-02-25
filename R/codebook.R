# Codebook Generation Functions
#
# Functions for producing documentation of survey variables in multiple
# output formats: plain text (to console or file), HTML, and PDF.
# HTML and PDF outputs use an R Markdown template bundled with the package.

#' Generate a Variable Codebook
#'
#' Produce documentation for variables in a bank, optionally filtered to a
#' single survey wave. Supports plain-text output to the console or a file,
#' and HTML/PDF output via R Markdown.
#'
#' @param bank A bank object: \code{qt_qbank}, \code{qt_genbank}, or
#'   \code{qt_ctrlbank}.
#' @param survey Character string or \code{NULL}. If supplied, only variables
#'   whose \code{surveys_used} field includes this survey ID are included in
#'   the codebook. If \code{NULL} (the default), all variables are included
#'   and each variable entry shows which surveys it appears in.
#' @param format Character string. Output format: \code{"text"} (default),
#'   \code{"html"}, or \code{"pdf"}. HTML and PDF require the \pkg{rmarkdown}
#'   package to be installed.
#' @param output_file Character string or \code{NULL}. Path for the output
#'   file. For \code{format = "text"}, if \code{NULL} the result is printed to
#'   the console; if a path is given, output is written to that file. For
#'   \code{format = "html"} or \code{"pdf"}, if \code{NULL} a temporary file
#'   is used.
#' @param value_labels A \code{qt_vlabs} object or \code{NULL}. If supplied,
#'   value labels are resolved and included in the codebook for factor
#'   variables. If \code{NULL}, factor variables show only the label-set name.
#' @param open Logical. If \code{TRUE}, the output file is opened after
#'   rendering (HTML/PDF only). Default: \code{FALSE}.
#' @param title Character string. Title for the codebook document. Default:
#'   \code{"Codebook"}.
#' @param ... Additional arguments passed to \code{rmarkdown::render()} for
#'   HTML/PDF output.
#'
#' @return For \code{format = "text"} with no \code{output_file}: invisibly
#'   returns the character vector of text lines. For all other cases: invisibly
#'   returns the path to the output file.
#'
#' @details
#' Each variable entry in the codebook includes:
#' \itemize{
#'   \item Variable ID, title, and storage type
#'   \item Question text (for question variables) or description (for generated
#'     and control variables)
#'   \item Value labels if the variable is a factor and \code{value_labels} is
#'     supplied
#'   \item Universe, variable group, citation/source, and notes
#'   \item Surveys used (for cross-survey codebooks only)
#' }
#'
#' @seealso \code{\link{qt_qbank}}, \code{\link{qt_genbank}},
#'   \code{\link{qt_ctrlbank}}, \code{\link{qt_read_value_labels}}
#'
#' @examples
#' \dontrun{
#' config <- qt_config(project_root = "inst/examples")
#' qbank  <- qt_qbank(config)
#' vl     <- qt_read_value_labels(config)
#'
#' # Cross-survey text codebook to console
#' qt_codebook(qbank, value_labels = vl)
#'
#' # Single-survey text codebook
#' qt_codebook(qbank, survey = "bas-2024", value_labels = vl)
#'
#' # HTML codebook to file
#' qt_codebook(qbank, format = "html", output_file = "codebook.html",
#'             value_labels = vl)
#'
#' # PDF codebook
#' qt_codebook(qbank, format = "pdf", output_file = "codebook.pdf",
#'             value_labels = vl)
#' }
#'
#' @export
qt_codebook <- function(bank,
                        survey       = NULL,
                        format       = c("text", "html", "pdf"),
                        output_file  = NULL,
                        value_labels = NULL,
                        open         = FALSE,
                        title        = "Codebook",
                        ...) {
  format <- match.arg(format)

  # 1. Prepare standardized variable entries
  entries <- .qt_prepare_codebook_entries(bank, survey, value_labels)

  if (length(entries) == 0) {
    if (!is.null(survey)) {
      stop("No variables found for survey '", survey, "'", call. = FALSE)
    } else {
      stop("The bank contains no variables", call. = FALSE)
    }
  }

  # 2. Build title
  if (title == "Codebook" && !is.null(survey)) {
    title <- paste0("Codebook: ", survey)
  }

  # 3. Dispatch by format
  if (format == "text") {
    .qt_codebook_text(entries, survey = survey, title = title,
                      output_file = output_file)
  } else {
    .qt_codebook_rmd(entries, format = format, survey = survey, title = title,
                     output_file = output_file, open = open, ...)
  }
}

# Internal: Prepare standardized codebook entries from a bank
#
# Filters by survey (if specified) and resolves value labels for each variable.
#
# @param bank A qt_qbank, qt_genbank, or qt_ctrlbank object
# @param survey Character string or NULL
# @param value_labels qt_vlabs object or NULL
# @return List of standardized entry lists
.qt_prepare_codebook_entries <- function(bank, survey, value_labels) {
  vars <- bank$variables

  # Filter by survey if requested
  if (!is.null(survey)) {
    vars <- Filter(function(v) survey %in% v$surveys_used, vars)
  }

  # Build entries in file position order (preserves YAML order)
  var_ids_ordered <- bank$meta$indices$by_file_position
  var_ids_ordered <- var_ids_ordered[var_ids_ordered %in% names(vars)]

  lapply(var_ids_ordered, function(var_id) {
    v <- vars[[var_id]]

    # Resolve text field (question_text for qvar, description for gen/ctrl)
    text <- v$question_text %||% v$description

    # Resolve value labels
    resolved_labels <- NULL
    vl_name <- v$value_labels_name %||% v$value_label_id
    if (!is.null(vl_name) && !is.null(value_labels)) {
      resolved_labels <- value_labels$labels[[vl_name]]
    }

    # Resolve citation (schema uses 'cite' or 'source')
    cite <- v$cite %||% v$source

    list(
      variable_id      = var_id,
      title            = v$title,
      storage_type     = v$storage_type,
      text             = text,
      value_labels     = resolved_labels,
      value_labels_name = vl_name,
      surveys_used     = v$surveys_used,
      universe         = v$universe,
      vargroup         = v$vargroup,
      cite             = cite,
      note             = v$note
    )
  })
}

# Internal: Write plain-text codebook
#
# @param entries List of prepared codebook entries
# @param survey Character string or NULL
# @param title Character string
# @param output_file Character string or NULL
# @return Invisibly returns the lines or file path
.qt_codebook_text <- function(entries, survey, title, output_file) {
  lines <- character()

  # Header
  lines <- c(lines,
             title,
             strrep("=", min(nchar(title), 70)),
             "")

  if (!is.null(survey)) {
    lines <- c(lines, paste0("Survey wave: ", survey), "")
  }
  lines <- c(lines, paste0("Total variables: ", length(entries)), "", "")

  for (entry in entries) {
    # Variable heading
    lines <- c(lines, paste0(entry$variable_id, "  [", entry$storage_type, "]  ",
                             entry$title))
    lines <- c(lines, strrep("-", min(70, nchar(entry$variable_id) +
                                        nchar(entry$storage_type) +
                                        nchar(entry$title) + 6)))

    # Type / group / universe
    meta_parts <- character()
    if (!is.null(entry$vargroup))
      meta_parts <- c(meta_parts, paste0("Group: ", entry$vargroup))
    if (!is.null(entry$universe))
      meta_parts <- c(meta_parts, paste0("Universe: ", entry$universe))
    if (length(meta_parts) > 0)
      lines <- c(lines, paste(meta_parts, collapse = "  |  "))

    # Question / description text
    if (!is.null(entry$text) && nchar(trimws(entry$text)) > 0) {
      lines <- c(lines, "", .qt_wrap_text(entry$text, width = 70, indent = 2))
    }

    # Value labels
    if (!is.null(entry$value_labels) && length(entry$value_labels$values) > 0) {
      vl_header <- "Value labels"
      if (!is.null(entry$value_labels_name))
        vl_header <- paste0(vl_header, " (", entry$value_labels_name, ")")
      lines <- c(lines, "", vl_header)
      for (i in seq_along(entry$value_labels$values)) {
        lines <- c(lines,
                   sprintf("  %s: %s",
                           entry$value_labels$values[i],
                           entry$value_labels$labels[i]))
      }
    }

    # Surveys used (cross-survey only)
    if (is.null(survey) && !is.null(entry$surveys_used)) {
      lines <- c(lines, "",
                 paste0("Surveys: ", paste(entry$surveys_used, collapse = ", ")))
    }

    # Citation
    if (!is.null(entry$cite) && nchar(trimws(entry$cite)) > 0) {
      lines <- c(lines, paste0("Source: ", entry$cite))
    }

    # Notes
    if (!is.null(entry$note) && nchar(trimws(entry$note)) > 0) {
      lines <- c(lines, "", paste0("Note: ", entry$note))
    }

    lines <- c(lines, "", "")
  }

  if (is.null(output_file)) {
    cat(paste(lines, collapse = "\n"))
    invisible(lines)
  } else {
    writeLines(lines, output_file)
    invisible(output_file)
  }
}

# Internal: Render R Markdown codebook
#
# @param entries List of prepared codebook entries
# @param format Character string: "html" or "pdf"
# @param survey Character string or NULL
# @param title Character string
# @param output_file Character string or NULL
# @param open Logical
# @param ... Passed to rmarkdown::render()
# @return Invisibly returns the output file path
.qt_codebook_rmd <- function(entries, format, survey, title, output_file,
                              open, ...) {
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    stop(
      "Package 'rmarkdown' is required for HTML/PDF codebook output.\n",
      "Install it with: install.packages(\"rmarkdown\")",
      call. = FALSE
    )
  }

  # Locate template
  template <- system.file("templates", "codebook.Rmd", package = "qretools")
  if (!nzchar(template)) {
    stop("codebook.Rmd template not found in the qretools package installation",
         call. = FALSE)
  }

  # Default output file
  if (is.null(output_file)) {
    ext <- if (format == "html") ".html" else ".pdf"
    output_file <- tempfile(pattern = "codebook_", fileext = ext)
  }

  # Save entries to a temp RDS so the template can read them
  entries_path <- tempfile(pattern = "qretools_entries_", fileext = ".rds")
  on.exit(unlink(entries_path), add = TRUE)
  saveRDS(entries, entries_path)

  # Build rmarkdown output format spec
  output_format <- if (format == "html") {
    rmarkdown::html_document(toc = TRUE, toc_float = TRUE, theme = "flatly")
  } else {
    rmarkdown::pdf_document(toc = TRUE, latex_engine = "xelatex")
  }

  rmarkdown::render(
    input         = template,
    output_format = output_format,
    output_file   = output_file,
    params        = list(
      entries = entries,
      title   = title,
      survey  = survey
    ),
    quiet = TRUE,
    ...
  )

  if (open) {
    utils::browseURL(output_file)
  }

  invisible(output_file)
}

# Internal: Wrap a long text string to a given width
#
# @param text Character string
# @param width Integer - max line width
# @param indent Integer - spaces of indentation for each line
# @return Character vector of wrapped lines
.qt_wrap_text <- function(text, width = 70, indent = 0) {
  prefix <- strrep(" ", indent)
  usable_width <- width - indent
  words <- strsplit(trimws(text), "\\s+")[[1]]

  lines <- character()
  current <- character()
  current_len <- 0

  for (word in words) {
    word_len <- nchar(word)
    if (current_len == 0) {
      current <- word
      current_len <- word_len
    } else if (current_len + 1 + word_len <= usable_width) {
      current <- paste(current, word)
      current_len <- current_len + 1 + word_len
    } else {
      lines <- c(lines, paste0(prefix, current))
      current <- word
      current_len <- word_len
    }
  }
  if (nchar(current) > 0) {
    lines <- c(lines, paste0(prefix, current))
  }
  lines
}
