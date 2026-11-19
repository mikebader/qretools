# Missing Codes
#
# Project-level codes recorded in the data for responses that are missing for a
# stated reason (skip pattern, item non-response, breakoff, ...). The codes are
# defined once per project in a YAML file whose location is set in
# `paths: missing_codes:` (default `_missing_codes.yml` in the project root).
# No file means no codes.

#' Read Missing Codes
#'
#' Read the project's missing-value codes: the codes used in the data for
#' responses that are missing for a stated reason, such as a skip pattern or a
#' breakoff.
#'
#' @param config A `qt_config` object (default: `qt_config()`).
#' @param path Character string or `NULL`. Explicit path to a missing-codes
#'   file, overriding the config. A file that does not exist is an error.
#'
#' @return `NULL` if the project has no missing-codes file at the default
#'   path. Otherwise an object of class `qt_missing_codes`: a data frame with
#'   one row per type and columns `type`, `value` (the code as a string) and
#'   `label`.
#'
#' @details
#' The file location is `paths: missing_codes:` in `_qretools.yml`; the default
#' is `_missing_codes.yml` in the project root. If that file does not exist
#' the project has no missing codes. If the location is changed in
#' `_qretools.yml` and the file does not exist, an error is raised.
#'
#' The file defines types, each with a `value` and a `label`:
#'
#' ```yaml
#' missing_codes:
#'   skip:
#'     value: "-7"
#'     label: "Missing: Question not presented due to skip pattern"
#'   breakoff:
#'     value: "-9"
#'     label: "Missing: Question not presented due to breakoff"
#' ```
#'
#' Types are names such as `skip` or `item_nonresponse`; values must be unique.
#' Quote the values so YAML reads them as text.
#'
#' @seealso [qt_data_summary()], [qt_codebook()]
#'
#' @export
qt_read_missing_codes <- function(config = qt_config(), path = NULL) {
  default <- .qt_defaults()$paths$missing_codes

  if (is.null(path)) {
    rel      <- config$paths$missing_codes %||% default
    explicit <- !identical(rel, default)
    path     <- file.path(config$meta$project_root, rel)
  } else {
    explicit <- TRUE
  }
  if (!grepl("\\.ya?ml$", path)) path <- paste0(path, ".yml")

  if (!file.exists(path)) {
    if (explicit)
      stop("Missing-codes file not found: ", path, call. = FALSE)
    return(NULL)
  }

  .qt_parse_missing_codes(.read_yaml_safe(path, basename(path)),
                          source = basename(path))
}

# Internal: validate parsed YAML and build a qt_missing_codes object
.qt_parse_missing_codes <- function(y, source = "missing codes") {
  mc <- y[["missing_codes"]]
  if (!is.list(mc) || length(mc) == 0 || is.null(names(mc)))
    stop(source, ": expected a 'missing_codes:' map of types, each with ",
         "'value' and 'label'", call. = FALSE)

  types <- names(mc)
  if (anyNA(types) || !all(grepl("^[A-Za-z][A-Za-z0-9_]*$", types)))
    stop(source, ": type names must start with a letter and contain only ",
         "letters, digits and underscores", call. = FALSE)
  if (anyDuplicated(types))
    stop(source, ": duplicate type names: ",
         paste(unique(types[duplicated(types)]), collapse = ", "), call. = FALSE)

  values <- character(length(mc))
  labels <- character(length(mc))
  for (i in seq_along(mc)) {
    e <- mc[[i]]
    v <- if (is.list(e)) e[["value"]] else NULL
    l <- if (is.list(e)) e[["label"]] else NULL
    if (is.null(v) || is.null(l))
      stop(source, ": type '", types[i], "' needs both 'value' and 'label'",
           call. = FALSE)
    if (length(v) != 1 || is.na(v) || !nzchar(as.character(v)))
      stop(source, ": type '", types[i], "' needs a single, non-empty 'value'",
           call. = FALSE)
    if (length(l) != 1 || !is.character(l) || is.na(l) || !nzchar(trimws(l)))
      stop(source, ": type '", types[i], "' needs a non-empty text 'label'",
           call. = FALSE)
    values[i] <- as.character(v)
    labels[i] <- l
  }
  if (anyDuplicated(values))
    stop(source, ": duplicate values: ",
         paste(unique(values[duplicated(values)]), collapse = ", "), call. = FALSE)

  .qt_new_missing_codes(types, values, labels)
}

# Internal: constructor
.qt_new_missing_codes <- function(type, value, label) {
  structure(
    data.frame(type = type, value = value, label = label,
               stringsAsFactors = FALSE),
    class = c("qt_missing_codes", "data.frame")
  )
}

# Internal: turn the `missing_codes` argument into a qt_missing_codes or NULL
#
# NULL  -> read from the project config (NULL if the project has no file)
# FALSE -> no missing codes
# qt_missing_codes object, or a data frame with type/value/label -> used as is
.qt_resolve_missing_codes <- function(missing_codes, config = NULL) {
  if (isFALSE(missing_codes)) return(NULL)

  if (is.null(missing_codes)) {
    cfg <- config %||% tryCatch(qt_config(), error = function(e) NULL)
    if (is.null(cfg)) return(NULL)
    return(qt_read_missing_codes(cfg))
  }

  if (inherits(missing_codes, "qt_missing_codes")) return(missing_codes)

  if (is.data.frame(missing_codes) &&
      all(c("type", "value", "label") %in% names(missing_codes))) {
    v <- as.character(missing_codes$value)
    if (anyDuplicated(v) || anyDuplicated(missing_codes$type))
      stop("'missing_codes' has duplicate types or values", call. = FALSE)
    return(.qt_new_missing_codes(as.character(missing_codes$type), v,
                                 as.character(missing_codes$label)))
  }

  stop("'missing_codes' must be NULL (read from the project config), FALSE ",
       "(none), the result of qt_read_missing_codes(), or a data frame with ",
       "columns type, value and label", call. = FALSE)
}

#' @export
print.qt_missing_codes <- function(x, ...) {
  cat("Missing codes\n")
  cat(strrep("=", 50), "\n\n", sep = "")
  for (i in seq_len(nrow(x))) {
    cat(sprintf("  %-18s %-6s %s\n", x$type[i], x$value[i], x$label[i]))
  }
  invisible(x)
}
