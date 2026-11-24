# Variable Dictionary Functions
#
# Functions for producing documentation of survey variables in multiple
# output formats: plain text (to console or file), HTML, and PDF.
# HTML and PDF outputs use an R Markdown template bundled with the package.

#' Generate a Variable Dictionary
#'
#' Produce documentation for variables in a bank, optionally filtered to a
#' single survey wave. Supports plain-text output to the console or a file,
#' and HTML/PDF output via R Markdown.
#'
#' @param bank A bank object: \code{qt_qbank}, \code{qt_genbank}, or
#'   \code{qt_ctrlbank}.
#' @param survey Character string or \code{NULL}. If supplied, only variables
#'   whose \code{surveys_used} field includes this survey ID are included in
#'   the dictionary. If \code{NULL} (the default), all variables are included
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
#'   value labels are resolved and included in the dictionary for factor
#'   variables. If \code{NULL}, factor variables show only the label-set name.
#' @param open Logical. If \code{TRUE}, the output file is opened after
#'   rendering (HTML/PDF only). Default: \code{FALSE}.
#' @param title Character string. Title for the dictionary document. Default:
#'   \code{"Dictionary"}.
#' @param variable_groups Logical (default `FALSE`). If `TRUE`, variables are
#'   grouped under a heading for each `vargroup`. With a project variable-groups
#'   file (see [qt_read_variable_groups()], read from the current project),
#'   headings use its names and order and variables with no group go last under
#'   its "unclassified" label; otherwise headings are the raw codes in order of
#'   first appearance, with variables that have no group last under "Other
#'   variables".
#' @param style Character string. HTML only. `"gitbook"` (default) renders a
#'   `bookdown`-like page with a sidebar table of contents (needs the
#'   \pkg{bookdown} package; falls back to `"document"` if it is not
#'   installed); `"document"` renders a plain `rmarkdown` HTML document with a
#'   floating table of contents.
#' @param css Character vector of CSS file paths or `NULL`. Applied after the
#'   package's default stylesheet and the project's `css/codebook.css` (if it
#'   exists; see [qt_copy_css()]), so later rules override earlier ones.
#' @param ... Additional arguments passed to \code{rmarkdown::render()} for
#'   HTML/PDF output.
#'
#' @return For \code{format = "text"} with no \code{output_file}: invisibly
#'   returns the character vector of text lines. For all other cases: invisibly
#'   returns the path to the output file.
#'
#' @details
#' HTML output carries semantic `qt-` CSS classes (for example `qt-variable`,
#' `qt-question-text`, `qt-response-table`, `qt-missing-row`) that the default
#' stylesheet and your own CSS can target.
#'
#' Each variable entry in the dictionary includes:
#' \itemize{
#'   \item Variable ID, title, and storage type
#'   \item Question text (for question variables) or description (for generated
#'     and control variables)
#'   \item Value labels if the variable is a factor and \code{value_labels} is
#'     supplied
#'   \item Universe, variable group, citation/source, and notes
#'   \item Surveys used (for cross-survey dictionaries only)
#' }
#'
#' @seealso \code{\link{qt_qbank}}, \code{\link{qt_genbank}},
#'   \code{\link{qt_ctrlbank}}, \code{\link{qt_read_value_labels}},
#'   [qt_copy_css()]
#'
#' @examples
#' \dontrun{
#' config <- qt_config(project_root = "inst/examples")
#' qbank  <- qt_qbank(config)
#' vl     <- qt_read_value_labels(config)
#'
#' # Cross-survey text dictionary to console
#' qt_dictionary(qbank, value_labels = vl)
#'
#' # Single-survey text dictionary
#' qt_dictionary(qbank, survey = "bas-2024", value_labels = vl)
#'
#' # HTML dictionary to file
#' qt_dictionary(qbank, format = "html", output_file = "dictionary.html",
#'             value_labels = vl)
#'
#' # PDF dictionary
#' qt_dictionary(qbank, format = "pdf", output_file = "dictionary.pdf",
#'             value_labels = vl)
#' }
#'
#' @export
qt_dictionary <- function(bank,
                        survey       = NULL,
                        format       = c("text", "html", "pdf"),
                        output_file  = NULL,
                        value_labels = NULL,
                        open         = FALSE,
                        title        = "Dictionary",
                        variable_groups = FALSE,
                        style        = c("gitbook", "document"),
                        css          = NULL,
                        ...) {
  format   <- match.arg(format)
  group_by <- .qt_group_by_arg(variable_groups)
  style    <- match.arg(style)

  # 1. Prepare standardized variable entries
  entries <- .qt_prepare_codebook_entries(bank, survey, value_labels)

  if (length(entries) == 0) {
    if (!is.null(survey)) {
      stop("No variables found for survey '", survey, "'", call. = FALSE)
    } else {
      stop("The bank contains no variables", call. = FALSE)
    }
  }

  entries <- .qt_label_entry_groups(entries, group_by)

  # 2. Build title
  if (title == "Dictionary" && !is.null(survey)) {
    title <- paste0("Dictionary: ", survey)
  }

  # 3. Dispatch by format
  if (format == "text") {
    .qt_codebook_text(entries, survey = survey, title = title,
                      output_file = output_file, group_by = group_by)
  } else {
    .qt_codebook_rmd(entries, format = format, survey = survey, title = title,
                     output_file = output_file, open = open,
                     group_by = group_by, style = style, css = css, ...)
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

    # Resolve to most recent version before reading any display fields.
    v <- .qt_resolve_version(v)

    # Resolve text field (question_text for qvar, description for gen/ctrl)
    text <- v$question_text %||% v$description

    # Resolve value labels
    resolved_labels <- NULL
    vl_name <- v$value_label_id %||% v$value_labels_name
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
      value_label_id   = vl_name,
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
.qt_codebook_text <- function(entries, survey, title, output_file,
                              not_in_data = NULL, group_by = "none") {
  lines <- character()

  # Header
  lines <- c(lines,
             title,
             strrep("=", min(nchar(title), 70)),
             "")

  if (!is.null(survey)) {
    lines <- c(lines, paste0("Survey wave: ", survey), "")
  }
  lines <- c(lines, paste0("Total variables: ", length(entries)))
  if (length(not_in_data) > 0) {
    lines <- c(lines, "",
               paste0("Instrument variables not in data (", length(not_in_data), "):"),
               .qt_wrap_text(paste(not_in_data, collapse = ", "), width = 70, indent = 2))
  }
  lines <- c(lines, "", "")

  for (grp in .qt_group_entries(entries, group_by)) {
    if (!is.null(grp$name)) {
      lines <- c(lines, strrep("=", 70), toupper(grp$name), strrep("=", 70), "")
    }
    for (entry in grp$entries) {
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
        if (!is.null(entry$value_label_id))
          vl_header <- paste0(vl_header, " (", entry$value_label_id, ")")
        lines <- c(lines, "", vl_header)
        for (i in seq_along(entry$value_labels$values)) {
          lines <- c(lines,
                     sprintf("  %s: %s",
                             entry$value_labels$values[i],
                             entry$value_labels$labels[i]))
        }
      }

      # Response counts and flags (codebook entries only)
      lines <- c(lines, .qt_text_data_lines(entry))

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
# The Markdown is generated in R (see R/codebook-render.R) and rendered from a
# temporary folder, so nothing is written next to the package or the output.
#
# @param entries List of prepared codebook entries
# @param format Character string: "html" or "pdf"
# @param survey Character string or NULL
# @param title Character string
# @param output_file Character string or NULL
# @param open Logical
# @param not_in_data Character vector of instrument variables absent from the
#   data (codebook only), or NULL
# @param group_by "none" or "vargroup"
# @param style "gitbook" or "document" (HTML only)
# @param css Character vector of extra CSS files (HTML only) or NULL
# @param config qt_config or NULL; used to find the project CSS
# @param ... Passed to rmarkdown::render()
# @return Invisibly returns the output file path
.qt_codebook_rmd <- function(entries, format, survey, title, output_file,
                             open, not_in_data = NULL, group_by = "none",
                             style = "gitbook", css = NULL, config = NULL,
                             ...) {
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    stop(
      "Package 'rmarkdown' is required for HTML/PDF codebook output.\n",
      "Install it with: install.packages(\"rmarkdown\")",
      call. = FALSE
    )
  }

  # Default output file
  if (is.null(output_file)) {
    ext <- if (format == "html") ".html" else ".pdf"
    output_file <- tempfile(pattern = "codebook_", fileext = ext)
  }

  # rmarkdown resolves a relative output_file against the input's folder, not
  # the working directory, so make the path absolute here.
  out_dir <- dirname(output_file)
  if (!dir.exists(out_dir)) {
    stop("Output directory does not exist: ", out_dir, call. = FALSE)
  }
  output_file <- file.path(normalizePath(out_dir, winslash = "/", mustWork = TRUE),
                           basename(output_file))

  # Single-survey documents do not repeat the surveys each variable appears in
  if (!is.null(survey)) {
    entries <- lapply(entries, function(e) { e["surveys_used"] <- list(NULL); e })
  }

  # Build the document
  yaml_title <- gsub("\\", "\\\\", title, fixed = TRUE)
  yaml_title <- gsub("\"", "\\\"", yaml_title, fixed = TRUE)
  rmd <- c("---",
           paste0("title: \"", yaml_title, "\""),
           paste0("date: \"", format(Sys.Date(), "%B %d, %Y"), "\""),
           "---", "",
           .qt_codebook_body(entries, format = format, survey = survey,
                             not_in_data = not_in_data, group_by = group_by,
                             style = style))

  work_dir <- tempfile(pattern = "qretools_render_")
  dir.create(work_dir)
  on.exit(unlink(work_dir, recursive = TRUE), add = TRUE)
  rmd_path <- file.path(work_dir, "codebook.Rmd")
  writeLines(rmd, rmd_path, useBytes = TRUE)

  render <- function(output_format) {
    rmarkdown::render(
      input         = rmd_path,
      output_format = output_format,
      output_file   = output_file,
      quiet         = TRUE,
      ...
    )
  }

  if (format == "pdf") {
    render(rmarkdown::pdf_document(toc = TRUE, latex_engine = "xelatex"))
  } else {
    css_files <- .qt_css_files(css, config)
    document  <- function() {
      rmarkdown::html_document(toc = TRUE, toc_float = TRUE, theme = "flatly",
                               css = css_files)
    }
    if (identical(style, "gitbook") && requireNamespace("bookdown", quietly = TRUE)) {
      gitbook <- bookdown::gitbook(split_by = "none", number_sections = FALSE,
                                   css = css_files,
                                   config = list(toc = list(collapse = "section")))
      tryCatch(
        render(gitbook),
        error = function(e) {
          warning("bookdown::gitbook rendering failed (", conditionMessage(e),
                  "); using style = \"document\" instead", call. = FALSE)
          render(document())
        }
      )
    } else {
      if (identical(style, "gitbook"))
        message("Package 'bookdown' is not installed; using style = \"document\"")
      render(document())
    }
  }

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
