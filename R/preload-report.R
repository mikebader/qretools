# Preload Variable Reporting Functions
#
# Generates compact tables of control/preload variables showing factor levels,
# value labels, and descriptions — replacing the manual Word-document tables
# previously used for survey documentation.

#' Generate a Preload Variable Table
#'
#' Produce a compact table of variables showing their values, value labels
#' (for factor variables), and descriptions. Designed for documenting
#' control and preload variables in survey questionnaires. Factor variables
#' are expanded to one row per level; all other types show their storage type
#' as the value.
#'
#' @param bank A bank object: \code{qt_ctrlbank}, \code{qt_genbank}, or
#'   \code{qt_qbank}.
#' @param variables Character vector or \code{NULL}. Variable IDs to include
#'   in the specified order. \code{NULL} (default) includes all variables in
#'   bank file order.
#' @param survey Character string or \code{NULL}. If supplied, only variables
#'   whose \code{surveys_used} includes this survey ID are shown.
#' @param value_labels A \code{qt_vlabs} object or \code{NULL}. Required to
#'   expand factor levels; factor variables fall back to showing the label-set
#'   name when \code{NULL}.
#' @param format Character string. Output format: \code{"markdown"} (default)
#'   for a GFM table, \code{"text"} for a plain fixed-width table.
#' @param output Character string. Output destination: \code{"print"}
#'   (default) sends to console; \code{"return"} returns the character vector
#'   invisibly without printing.
#' @param title Character string or \code{NULL}. Heading above the table.
#'   Default: \code{"Preload Variables"}. Use \code{NULL} to omit.
#' @param group_by_input Logical. When variables have an \code{input} field
#'   set and multiple distinct \code{input} values are present, split into
#'   labelled sub-tables per input type. Default: \code{FALSE}.
#'
#' @return Invisibly returns a character vector of formatted lines.
#'
#' @details
#' The \code{input} field on control and generated variables specifies how
#' the variable is populated before the survey starts:
#' \describe{
#'   \item{\code{"sample"}}{Loaded from a sample / preload data file (e.g.,
#'     address, jurisdiction).}
#'   \item{\code{"system"}}{Auto-populated by the survey system (e.g.,
#'     current date, unique response ID).}
#'   \item{\code{"computed"}}{Derived or randomised within the survey
#'     instrument (e.g., experimental conditions, composite scores).}
#' }
#' When \code{group_by_input = TRUE} and variables carry multiple distinct
#' \code{input} values, each group gets its own labelled sub-table. If only
#' one \code{input} type is present the grouping header is suppressed.
#'
#' @seealso \code{\link{qt_ctrlbank}}, \code{\link{qt_read_value_labels}},
#'   \code{\link{qt_codebook}}
#'
#' @examples
#' \dontrun{
#' config <- qt_config(project_root = "inst/examples")
#' ctrl   <- qt_ctrlbank(config)
#' vl     <- qt_read_value_labels(config)
#'
#' # Print Markdown table to console
#' qt_preload_table(ctrl, value_labels = vl)
#'
#' # Filter to a specific survey wave
#' qt_preload_table(ctrl, survey = "bas-2024", value_labels = vl)
#'
#' # Specific variables in a given order
#' qt_preload_table(ctrl,
#'                  variables = c("JURISDICTION", "RICHPOORORD"),
#'                  value_labels = vl)
#'
#' # Group by input source, return lines for embedding
#' lines <- qt_preload_table(ctrl, value_labels = vl,
#'                           group_by_input = TRUE, output = "return")
#' }
#'
#' @export
qt_preload_table <- function(
    bank,
    variables      = NULL,
    survey         = NULL,
    value_labels   = NULL,
    format         = c("markdown", "text"),
    output         = c("print", "return"),
    title          = "Preload Variables",
    group_by_input = FALSE
) {
  format <- match.arg(format)
  output <- match.arg(output)

  # 1. Extract and filter variables -------------------------------------------
  vars <- bank$variables

  if (!is.null(variables)) {
    missing_ids <- setdiff(variables, names(vars))
    if (length(missing_ids) > 0) {
      warning("Variables not found in bank: ",
              paste(missing_ids, collapse = ", "), call. = FALSE)
    }
    keep <- variables[variables %in% names(vars)]
    vars <- vars[keep]
  }

  if (!is.null(survey)) {
    vars <- Filter(
      function(v) survey %in% (v$surveys_used %||% character()),
      vars
    )
  }

  if (length(vars) == 0) {
    warning("No variables to report", call. = FALSE)
    return(invisible(character()))
  }

  # 2. Prepare display entries ------------------------------------------------
  entries <- .qt_preload_prepare_entries(vars, value_labels)

  # 3. Build lines ------------------------------------------------------------
  any_input <- any(sapply(entries, function(e) !is.null(e$input)))

  lines <- if (group_by_input && any_input) {
    .qt_preload_grouped_lines(entries, format, title)
  } else {
    .qt_preload_flat_lines(entries, format, title, show_input = any_input)
  }

  # 4. Output -----------------------------------------------------------------
  if (output == "print") {
    cat(paste(lines, collapse = "\n"), "\n")
  }
  invisible(lines)
}


# Internal helpers -------------------------------------------------------------

# Prepare display entries from a named list of variable objects.
# Resolves value labels, description text, and the input field.
#
# @param vars Named list of variable objects (qt_qvar, qt_ctrlvar, qt_genvar)
# @param value_labels qt_vlabs object or NULL
# @return List of entry lists
# @keywords internal
# @noRd
.qt_preload_prepare_entries <- function(vars, value_labels) {
  lapply(vars, function(v) {
    # Resolve to most recent version before extracting any display fields.
    # variable_id is stable across versions so capture it before resolving.
    var_id <- v$variable_id
    v      <- .qt_resolve_version(v)

    description <- v$question_text %||% v$description %||% ""

    resolved_labels <- NULL
    if (!is.null(v$value_label_id) && !is.null(value_labels)) {
      resolved_labels <- value_labels$labels[[v$value_label_id]]
    }

    list(
      variable_id    = var_id,
      storage_type   = v$storage_type %||% "character",
      description    = description,
      value_label_id = v$value_label_id,
      labels         = resolved_labels,
      input          = v$input  # NULL if not defined in YAML
    )
  })
}


# Convert storage_type to a human-readable Value column string.
# @keywords internal
# @noRd
.qt_preload_storage_display <- function(storage_type) {
  switch(storage_type,
    character         = "String",
    integer           = "Integer",
    numeric           = "Number",
    logical           = "Logical",
    boolean           = "Logical",
    factor            = "Factor",
    multiple_response = "Multiple",
    composite         = "Composite",
    toupper(storage_type)
  )
}


# Build flat table rows from prepared entries.
# Returns a list of row lists: variable_id, value, label, description, input.
# Factor variables yield one row per level; non-factors yield a single row.
#
# @param entries List from .qt_preload_prepare_entries()
# @param show_input Logical — whether to include an input cell
# @keywords internal
# @noRd
.qt_preload_build_rows <- function(entries, show_input) {
  rows <- list()
  for (entry in entries) {
    if (entry$storage_type == "factor" && !is.null(entry$labels) &&
        length(entry$labels$values) > 0) {
      for (i in seq_along(entry$labels$values)) {
        row <- list(
          variable_id = if (i == 1) entry$variable_id else "",
          value       = as.character(entry$labels$values[[i]]),
          label       = entry$labels$labels[[i]],
          description = if (i == 1) entry$description else ""
        )
        if (show_input) row$input <- if (i == 1) entry$input %||% "" else ""
        rows <- c(rows, list(row))
      }
    } else {
      # Non-factor or factor with no resolved labels
      value_str <- if (entry$storage_type == "factor" &&
                       !is.null(entry$value_label_id)) {
        paste0("Factor (", entry$value_label_id, ")")
      } else {
        .qt_preload_storage_display(entry$storage_type)
      }
      row <- list(
        variable_id = entry$variable_id,
        value       = value_str,
        label       = "",
        description = entry$description
      )
      if (show_input) row$input <- entry$input %||% ""
      rows <- c(rows, list(row))
    }
  }
  rows
}


# Build a Markdown (GFM) table from prebuilt rows.
# @keywords internal
# @noRd
.qt_preload_markdown_table <- function(rows, show_input) {
  headers <- c("Variable name", "Value", "Value label", "Description")
  if (show_input) headers <- c(headers, "Input")
  n <- length(headers)

  header_line <- paste0("| ", paste(headers, collapse = " | "), " |")
  sep_line    <- paste0("|", paste(rep(":---|", n), collapse = ""))

  data_lines <- vapply(rows, function(r) {
    cells <- c(r$variable_id, r$value, r$label, r$description)
    if (show_input) cells <- c(cells, r$input)
    paste0("| ", paste(cells, collapse = " | "), " |")
  }, character(1))

  c(header_line, sep_line, data_lines)
}


# Build a plain fixed-width text table from prebuilt rows.
# @keywords internal
# @noRd
.qt_preload_text_table <- function(rows, show_input) {
  col_names <- c("Variable name", "Value", "Value label", "Description")
  if (show_input) col_names <- c(col_names, "Input")

  get_cells <- function(r) {
    cells <- c(r$variable_id, r$value, r$label, r$description)
    if (show_input) cells <- c(cells, r$input %||% "")
    cells
  }
  all_cells <- lapply(rows, get_cells)

  # Compute column widths from headers and data
  col_widths <- sapply(seq_along(col_names), function(j) {
    max(nchar(col_names[j]),
        sapply(all_cells, function(cells) nchar(cells[j])))
  })

  fmt_row <- function(cells) {
    paste(
      mapply(function(cell, w) formatC(cell, width = -w), cells, col_widths),
      collapse = "  "
    )
  }

  header    <- fmt_row(col_names)
  separator <- paste(sapply(col_widths, strrep, x = "-"), collapse = "  ")
  data_lines <- vapply(all_cells, fmt_row, character(1))

  c(header, separator, data_lines)
}


# Build lines for a flat (ungrouped) table with an optional section title.
# @keywords internal
# @noRd
.qt_preload_flat_lines <- function(entries, format, title, show_input) {
  lines <- character()

  if (!is.null(title) && nzchar(title)) {
    if (format == "markdown") {
      lines <- c(lines, paste0("### ", title), "")
    } else {
      lines <- c(lines, toupper(title), strrep("=", nchar(title)), "")
    }
  }

  rows <- .qt_preload_build_rows(entries, show_input)

  table_lines <- if (format == "markdown") {
    .qt_preload_markdown_table(rows, show_input)
  } else {
    .qt_preload_text_table(rows, show_input)
  }

  c(lines, table_lines)
}


# Input-type labels for grouped table headers.
.qt_preload_input_labels <- c(
  sample   = "Sample File Variables",
  system   = "System Variables",
  computed = "Computed Variables"
)

# Build lines for a grouped table, splitting by input type.
# Falls back to flat when only one group is present.
# @keywords internal
# @noRd
.qt_preload_grouped_lines <- function(entries, format, title) {
  all_inputs   <- sapply(entries, function(e) e$input %||% "other")
  input_order  <- c("sample", "system", "computed", "other")
  extra_inputs <- setdiff(unique(all_inputs), input_order)
  present      <- c(
    input_order[input_order %in% unique(all_inputs)],
    extra_inputs
  )

  # Only one group — no point adding a group header
  if (length(present) <= 1) {
    return(.qt_preload_flat_lines(entries, format, title, show_input = FALSE))
  }

  lines <- character()
  if (!is.null(title) && nzchar(title)) {
    if (format == "markdown") {
      lines <- c(lines, paste0("### ", title), "")
    } else {
      lines <- c(lines, toupper(title), strrep("=", nchar(title)), "")
    }
  }

  for (input_type in present) {
    group_entries <- Filter(
      function(e) (e$input %||% "other") == input_type,
      entries
    )
    if (length(group_entries) == 0) next

    group_label <- .qt_preload_input_labels[[input_type]] %||%
      paste0(
        toupper(substr(input_type, 1, 1)),
        substr(input_type, 2, nchar(input_type)),
        " Variables"
      )

    if (format == "markdown") {
      lines <- c(lines, paste0("**", group_label, "**"), "")
    } else {
      lines <- c(lines, group_label, strrep("-", nchar(group_label)), "")
    }

    group_rows  <- .qt_preload_build_rows(group_entries, show_input = FALSE)
    table_lines <- if (format == "markdown") {
      .qt_preload_markdown_table(group_rows, show_input = FALSE)
    } else {
      .qt_preload_text_table(group_rows, show_input = FALSE)
    }

    lines <- c(lines, table_lines, "")
  }

  lines
}
