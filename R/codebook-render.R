# Codebook / Dictionary Rendering (HTML and PDF)
#
# Entries (see .qt_codebook_entries() and .qt_prepare_codebook_entries()) are
# turned into a Markdown document that is rendered with rmarkdown. For HTML
# output each entry is wrapped in a div with semantic `qt-` classes and its
# tables are raw HTML, so every part (and every missing-code row) can be styled
# by CSS. PDF output uses plain Markdown tables.
#
# Class reference (HTML):
#   .qt-codebook-header   survey line, variable count, variables not in data
#   .qt-group             group heading (`# <vargroup>`)
#   .qt-variable          one entry; modifiers .qt-type-<storage-type> and
#                         .qt-status-<status> (codebooks only)
#   .qt-variable-id       variable name, beneath the heading (title)
#   .qt-meta              type / group / universe line
#     .qt-meta-item .qt-storage-type | .qt-vargroup | .qt-universe
#   .qt-question-text, .qt-option-of, .qt-options, .qt-needs-compilation
#   .qt-response-counts   with .qt-count-valid | .qt-count-coded | .qt-count-na
#   table.qt-response-table      rows .qt-response-row | .qt-unlabeled-row |
#                                .qt-missing-row
#   table.qt-statistics-table    rows .qt-statistic-row | .qt-missing-row
#   table.qt-missing-table       rows .qt-missing-row (when not in the table)
#   table.qt-label-table         dictionary value labels
#   .qt-surveys-used, .qt-source, .qt-note, .qt-flags


# CSS -------------------------------------------------------------------------

#' Copy the Default Codebook CSS into a Project
#'
#' Copies the package's default codebook stylesheet to `css/codebook.css` in
#' the project (the folder is set by `paths: css:` in `_qretools.yml`) so it can
#' be edited. HTML codebooks and dictionaries use the package default, then
#' this file if it exists, then any file passed to `css =`.
#'
#' @param config A `qt_config` object (default: `qt_config()`).
#' @param overwrite Logical. Replace an existing `codebook.css`. Default
#'   `FALSE`.
#'
#' @return Invisibly, the path of the copied file.
#'
#' @seealso [qt_codebook()], [qt_dictionary()]
#'
#' @export
qt_copy_css <- function(config = qt_config(), overwrite = FALSE) {
  src <- system.file("templates", "codebook.css", package = "qretools")
  if (!nzchar(src))
    stop("Default codebook.css not found in the qretools installation",
         call. = FALSE)

  dir  <- file.path(config$meta$project_root,
                    config$paths$css %||% .qt_defaults()$paths$css)
  dest <- file.path(dir, "codebook.css")
  if (file.exists(dest) && !overwrite)
    stop("File already exists: ", dest, "\nUse overwrite = TRUE to replace it",
         call. = FALSE)

  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  file.copy(src, dest, overwrite = TRUE)
  message("Copied default codebook CSS to: ", dest)
  invisible(dest)
}

# Internal: CSS files for an HTML render, in the order they are applied
#
# 1. the package default; 2. <project>/<paths$css>/codebook.css if it exists;
# 3. files passed in `css`.
.qt_css_files <- function(css = NULL, config = NULL) {
  default <- system.file("templates", "codebook.css", package = "qretools")
  if (!nzchar(default))
    stop("Default codebook.css not found in the qretools installation",
         call. = FALSE)
  files <- default

  cfg <- config %||% tryCatch(qt_config(), error = function(e) NULL)
  if (!is.null(cfg)) {
    dir  <- file.path(cfg$meta$project_root,
                      cfg$paths$css %||% .qt_defaults()$paths$css)
    proj <- file.path(dir, "codebook.css")
    if (file.exists(proj)) files <- c(files, proj)
  }

  if (!is.null(css)) {
    absent <- css[!file.exists(css)]
    if (length(absent) > 0)
      stop("CSS file(s) not found: ", paste(absent, collapse = ", "),
           call. = FALSE)
    files <- c(files, css)
  }

  normalizePath(files, winslash = "/", mustWork = TRUE)
}


# Grouping --------------------------------------------------------------------

# Internal: split entries into groups by vargroup
#
# @return List of list(name, entries). name is NULL when not grouping. Groups
#   are in order of first appearance; entries without a vargroup come last
#   under "Other variables".
.qt_group_entries <- function(entries, group_by = "none") {
  if (!identical(group_by, "vargroup")) {
    return(list(list(name = NULL, entries = entries)))
  }
  keys <- vapply(entries,
                 function(e) as.character(e[["vargroup"]] %||% NA_character_),
                 character(1))
  groups <- lapply(unique(keys[!is.na(keys)]), function(k) {
    list(name = k, entries = entries[which(keys == k)])
  })
  if (anyNA(keys)) {
    groups <- c(groups, list(list(name = "Other variables",
                                  entries = entries[which(is.na(keys))])))
  }
  groups
}


# Escaping --------------------------------------------------------------------

.qt_esc <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  gsub("\"", "&quot;", x, fixed = TRUE)
}

# Backslash-escape Markdown specials in text placed in headings and tables
.qt_md_esc <- function(x) {
  gsub("([\\\\`*_\\[\\]<>|])", "\\\\\\1", as.character(x), perl = TRUE)
}

.qt_css_class <- function(prefix, x) {
  paste0(prefix, gsub("[^A-Za-z0-9]+", "-", as.character(x)))
}


# HTML pieces -----------------------------------------------------------------

.qt_html_values_table <- function(v) {
  has_label <- "label" %in% names(v)
  kind <- if ("kind" %in% names(v)) v$kind else rep("response", nrow(v))
  head <- paste0(
    "<thead><tr><th class=\"qt-col-value\">Value</th>",
    if (has_label) "<th class=\"qt-col-label\">Label</th>",
    "<th class=\"qt-col-n\">N</th></tr></thead>"
  )
  rows <- vapply(seq_len(nrow(v)), function(i) {
    lab <- if (has_label) v$label[i] else ""
    cls <- if (kind[i] == "missing") "qt-missing-row"
           else if (has_label && is.na(lab)) "qt-unlabeled-row"
           else "qt-response-row"
    lab_cell <- if (!has_label) ""
                else if (is.na(lab)) "<td class=\"qt-col-label\"><em>(no label)</em></td>"
                else paste0("<td class=\"qt-col-label\">", .qt_esc(lab), "</td>")
    paste0("<tr class=\"", cls, "\"><td class=\"qt-col-value\">",
           .qt_esc(v$value[i]), "</td>", lab_cell,
           "<td class=\"qt-col-n\">", v$n[i], "</td></tr>")
  }, character(1))
  c("<table class=\"qt-response-table\">", head, "<tbody>", rows, "</tbody>",
    "</table>")
}

.qt_html_stats_table <- function(t) {
  kind <- if ("kind" %in% names(t)) t$kind else rep("statistic", nrow(t))
  rows <- vapply(seq_len(nrow(t)), function(i) {
    cls <- if (kind[i] == "missing") "qt-missing-row" else "qt-statistic-row"
    paste0("<tr class=\"", cls, "\"><td class=\"qt-col-statistic\">",
           .qt_esc(t$statistic[i]), "</td><td class=\"qt-col-value\">",
           .qt_esc(t$value[i]), "</td></tr>")
  }, character(1))
  c("<table class=\"qt-statistics-table\">",
    "<thead><tr><th class=\"qt-col-statistic\">Statistic</th><th class=\"qt-col-value\">Value</th></tr></thead>",
    "<tbody>", rows, "</tbody>", "</table>")
}

.qt_html_missing_table <- function(m) {
  rows <- vapply(seq_len(nrow(m)), function(i) {
    paste0("<tr class=\"qt-missing-row\"><td class=\"qt-col-value\">",
           .qt_esc(m$value[i]), "</td><td class=\"qt-col-type\">",
           .qt_esc(m$type[i]), "</td><td class=\"qt-col-label\">",
           .qt_esc(m$label[i]), "</td><td class=\"qt-col-n\">", m$n[i],
           "</td></tr>")
  }, character(1))
  c("<table class=\"qt-missing-table\">",
    "<thead><tr><th class=\"qt-col-value\">Value</th><th class=\"qt-col-type\">Type</th><th class=\"qt-col-label\">Label</th><th class=\"qt-col-n\">N</th></tr></thead>",
    "<tbody>", rows, "</tbody>", "</table>")
}

.qt_html_label_table <- function(vl) {
  rows <- vapply(seq_along(vl$values), function(i) {
    paste0("<tr class=\"qt-label-row\"><td class=\"qt-col-value\">",
           .qt_esc(vl$values[i]), "</td><td class=\"qt-col-label\">",
           .qt_esc(vl$labels[i]), "</td></tr>")
  }, character(1))
  c("<table class=\"qt-label-table\">",
    "<thead><tr><th class=\"qt-col-value\">Value</th><th class=\"qt-col-label\">Label</th></tr></thead>",
    "<tbody>", rows, "</tbody>", "</table>")
}

# Internal: raw HTML for the body of one entry (everything below the heading)
.qt_entry_html <- function(e) {
  out <- character()
  add <- function(...) out <<- c(out, ...)
  has_text <- function(x) !is.null(x) && length(x) == 1 && !is.na(x) &&
    nzchar(trimws(x))

  # Variable name beneath the title
  add(paste0("<p class=\"qt-variable-id\"><code>", .qt_esc(e[["variable_id"]]), "</code></p>"))

  # Meta line
  meta <- character()
  if (has_text(e[["storage_type"]]))
    meta <- c(meta, paste0("<span class=\"qt-meta-item qt-storage-type\">",
                           "<span class=\"qt-meta-label\">Type:</span> <code>",
                           .qt_esc(e[["storage_type"]]), "</code></span>"))
  if (has_text(e[["vargroup"]]))
    meta <- c(meta, paste0("<span class=\"qt-meta-item qt-vargroup\">",
                           "<span class=\"qt-meta-label\">Group:</span> <code>",
                           .qt_esc(e[["vargroup"]]), "</code></span>"))
  if (has_text(e[["universe"]]))
    meta <- c(meta, paste0("<span class=\"qt-meta-item qt-universe\">",
                           "<span class=\"qt-meta-label\">Universe:</span> ",
                           .qt_esc(e[["universe"]]), "</span>"))
  if (length(meta) > 0)
    add(paste0("<p class=\"qt-meta\">", paste(meta, collapse = " "), "</p>"))

  # Question / description text
  if (has_text(e[["text"]]))
    add(paste0("<p class=\"qt-question-text\">", .qt_esc(e[["text"]]), "</p>"))

  # Multi-part relationships (codebook)
  if (!is.null(e[["parent_id"]]))
    add(paste0("<p class=\"qt-option-of\">Option of <code>",
               .qt_esc(e[["parent_id"]]), "</code></p>"))
  if (!is.null(e[["options"]]))
    add(paste0("<p class=\"qt-options\">Option variables: ",
               paste0("<code>", .qt_esc(e[["options"]]), "</code>",
                      collapse = ", "), "</p>"))

  # Value labels (dictionary)
  vl <- e[["value_labels"]]
  if (!is.null(vl) && length(vl$values) > 0) {
    add(paste0("<p class=\"qt-label-heading\">Value labels",
               if (has_text(e[["value_label_id"]]))
                 paste0(" (<code>", .qt_esc(e[["value_label_id"]]), "</code>)"),
               "</p>"))
    add(.qt_html_label_table(vl))
  }

  # Data (codebook)
  status <- e[["status"]]
  if (!is.null(status)) {
    if (status == "needs_compilation") {
      add(paste0("<p class=\"qt-needs-compilation\"><strong>Needs compilation:</strong> ",
                 .qt_esc(e[["flags"]]), "</p>"))
      if (!is.null(e[["n"]]))
        add(paste0("<p class=\"qt-response-counts\"><span class=\"qt-count qt-count-valid\">",
                   e[["n"]], " non-missing values</span></p>"))
    } else if (status == "ok") {
      add(paste0(
        "<p class=\"qt-response-counts\">",
        "<span class=\"qt-count qt-count-valid\">", e[["n"]] %||% 0L, " valid</span> ",
        "<span class=\"qt-count qt-count-coded\">", e[["n_coded"]] %||% 0L, " coded missing</span> ",
        "<span class=\"qt-count qt-count-na\">", e[["n_na"]] %||% 0L, " NA</span></p>"))
      if (!is.null(e[["values"]]) && nrow(e[["values"]]) > 0)
        add(.qt_html_values_table(e[["values"]]))
      if (!is.null(e[["stats_table"]]) && nrow(e[["stats_table"]]) > 0)
        add(.qt_html_stats_table(e[["stats_table"]]))
      if (!is.null(e[["missing"]]) && nrow(e[["missing"]]) > 0) {
        add("<p class=\"qt-label-heading\">Coded missing values</p>")
        add(.qt_html_missing_table(e[["missing"]]))
      }
      if (has_text(e[["flags"]]))
        add(paste0("<p class=\"qt-flags\"><strong>Flags:</strong> ",
                   .qt_esc(e[["flags"]]), "</p>"))
    }
  }

  # Surveys used, source, note
  if (is.null(status) && !is.null(e[["surveys_used"]]) && length(e[["surveys_used"]]) > 0)
    add(paste0("<p class=\"qt-surveys-used\"><strong>Surveys used:</strong> ",
               .qt_esc(paste(e[["surveys_used"]], collapse = ", ")), "</p>"))
  if (has_text(e[["cite"]]))
    add(paste0("<p class=\"qt-source\"><strong>Source:</strong> ",
               .qt_esc(e[["cite"]]), "</p>"))
  if (has_text(e[["note"]]))
    add(paste0("<blockquote class=\"qt-note\"><strong>Note:</strong> ",
               .qt_esc(e[["note"]]), "</blockquote>"))

  out
}


# Markdown pieces (PDF) -------------------------------------------------------

.qt_md_table <- function(header, rows) {
  c(paste0("| ", paste(header, collapse = " | "), " |"),
    paste0("|", paste(rep("---", length(header)), collapse = "|"), "|"),
    vapply(rows, function(r) paste0("| ", paste(r, collapse = " | "), " |"),
           character(1)),
    "")
}

# Internal: Markdown body of one entry (everything below the heading)
.qt_entry_md <- function(e) {
  out <- character()
  add <- function(...) out <<- c(out, ...)
  has_text <- function(x) !is.null(x) && length(x) == 1 && !is.na(x) &&
    nzchar(trimws(x))

  add(paste0("`", e[["variable_id"]], "`"), "")
  meta <- paste0("**Type:** `", e[["storage_type"]] %||% "unclassified", "`")
  if (has_text(e[["vargroup"]]))
    meta <- paste0(meta, "   **Group:** `", e[["vargroup"]], "`")
  if (has_text(e[["universe"]]))
    meta <- paste0(meta, "   **Universe:** ", .qt_md_esc(e[["universe"]]))
  add(meta, "")

  if (has_text(e[["text"]])) add(.qt_md_esc(e[["text"]]), "")
  if (!is.null(e[["parent_id"]]))
    add(paste0("**Option of:** `", e[["parent_id"]], "`"), "")
  if (!is.null(e[["options"]]))
    add(paste0("**Option variables:** ",
               paste0("`", e[["options"]], "`", collapse = ", ")), "")

  vl <- e[["value_labels"]]
  if (!is.null(vl) && length(vl$values) > 0) {
    add(paste0("**Value labels**",
               if (has_text(e[["value_label_id"]]))
                 paste0(" (`", e[["value_label_id"]], "`)"), ":"), "")
    add(.qt_md_table(c("Value", "Label"),
                     lapply(seq_along(vl$values), function(i)
                       c(.qt_md_esc(vl$values[i]), .qt_md_esc(vl$labels[i])))))
  }

  status <- e[["status"]]
  if (!is.null(status)) {
    if (status == "needs_compilation") {
      add(paste0("> **Needs compilation:** ", e[["flags"]]), "")
    } else if (status == "ok") {
      add(paste0("**Responses:** ", e[["n"]] %||% 0L, " valid, ",
                 e[["n_coded"]] %||% 0L, " coded missing, ",
                 e[["n_na"]] %||% 0L, " NA"), "")
      v <- e[["values"]]
      if (!is.null(v) && nrow(v) > 0) {
        has_label <- "label" %in% names(v)
        add(.qt_md_table(
          c("Value", if (has_label) "Label", "N"),
          lapply(seq_len(nrow(v)), function(i) c(
            .qt_md_esc(v$value[i]),
            if (has_label) if (is.na(v$label[i])) "*(no label)*" else .qt_md_esc(v$label[i]),
            v$n[i]))))
      }
      t <- e[["stats_table"]]
      if (!is.null(t) && nrow(t) > 0)
        add(.qt_md_table(c("Statistic", "Value"),
                         lapply(seq_len(nrow(t)), function(i)
                           c(.qt_md_esc(t$statistic[i]), t$value[i]))))
      m <- e[["missing"]]
      if (!is.null(m) && nrow(m) > 0) {
        add("**Coded missing values:**", "")
        add(.qt_md_table(c("Value", "Type", "Label", "N"),
                         lapply(seq_len(nrow(m)), function(i)
                           c(.qt_md_esc(m$value[i]), m$type[i],
                             .qt_md_esc(m$label[i]), m$n[i]))))
      }
      if (has_text(e[["flags"]]))
        add(paste0("> **Flags:** ", e[["flags"]]), "")
    }
  }

  if (is.null(status) && !is.null(e[["surveys_used"]]) && length(e[["surveys_used"]]) > 0)
    add(paste0("**Surveys used:** ", paste(e[["surveys_used"]], collapse = ", ")), "")
  if (has_text(e[["cite"]])) add(paste0("**Source:** ", .qt_md_esc(e[["cite"]])), "")
  if (has_text(e[["note"]])) add(paste0("> **Note:** ", .qt_md_esc(e[["note"]])), "")
  out
}


# Document body ---------------------------------------------------------------

# Internal: the Markdown body of the whole document
#
# @param entries Prepared entries
# @param format "html" or "pdf"
# @param survey Survey ID or NULL
# @param not_in_data Instrument variables absent from the data, or NULL
# @param group_by "none" or "vargroup"
# @param style "gitbook" or "document" (HTML only)
# @return Character vector of Markdown lines
.qt_codebook_body <- function(entries, format, survey, not_in_data = NULL,
                              group_by = "none", style = "gitbook") {
  html     <- identical(format, "html")
  grouped  <- identical(group_by, "vargroup")
  # gitbook builds its sidebar from level-1 headings
  var_level <- if (grouped) 2L else if (html && identical(style, "gitbook")) 1L else 2L

  lines <- character()

  # Header
  if (html) {
    hdr <- character()
    if (!is.null(survey))
      hdr <- c(hdr, paste0("<p class=\"qt-survey\"><strong>Survey wave:</strong> ",
                           .qt_esc(survey), "</p>"))
    hdr <- c(hdr, paste0("<p class=\"qt-total\"><strong>Total variables:</strong> ",
                         length(entries), "</p>"))
    if (length(not_in_data) > 0)
      hdr <- c(hdr, paste0("<div class=\"qt-not-in-data\"><p><strong>Instrument variables not in data (",
                           length(not_in_data), "):</strong> ",
                           paste0("<code>", .qt_esc(not_in_data), "</code>",
                                  collapse = ", "), "</p></div>"))
    lines <- c(lines, "::: {.qt-codebook-header}", "", "```{=html}", hdr, "```",
               "", ":::", "")
  } else {
    if (!is.null(survey)) lines <- c(lines, paste0("> **Survey wave:** ", survey), "")
    lines <- c(lines, paste0("**Total variables:** ", length(entries)), "")
    if (length(not_in_data) > 0)
      lines <- c(lines, paste0("**Instrument variables not in data (",
                               length(not_in_data), "):** ",
                               paste0("`", not_in_data, "`", collapse = ", ")), "")
  }

  for (grp in .qt_group_entries(entries, group_by)) {
    if (!is.null(grp$name)) {
      lines <- c(lines,
                 paste0("# ", .qt_md_esc(grp$name), " {#",
                        .qt_css_class("group-", grp$name), " .qt-group}"), "")
    }
    for (e in grp$entries) {
      id    <- e[["variable_id"]]
      title <- e[["title"]] %||% ""
      # the title is the heading; the variable name is shown beneath it
      heading <- paste0(strrep("#", var_level), " ",
                        if (nzchar(title)) .qt_md_esc(title) else paste0("`", id, "`"),
                        " {#", id, "}")
      if (html) {
        classes <- c(".qt-variable",
                     .qt_css_class(".qt-type-", e[["storage_type"]] %||% "unclassified"),
                     if (!is.null(e[["status"]])) .qt_css_class(".qt-status-", e[["status"]]))
        lines <- c(lines, paste0("::: {", paste(classes, collapse = " "), "}"), "",
                   heading, "", "```{=html}", .qt_entry_html(e), "```", "",
                   ":::", "")
      } else {
        lines <- c(lines, heading, "", .qt_entry_md(e), "---", "")
      }
    }
  }
  lines
}
