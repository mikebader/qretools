# Variable Groups
#
# Project-level list of the broad topical groups (the `vargroup` field of each
# variable) with display names and section order, for topical codebooks
# (`group_by = "vargroup"` in qt_codebook() and qt_dictionary()). The
# groups are defined once per project in a YAML file whose location is set in
# `paths: variable_groups:` (default `_variable_groups.yml` in the project
# root). No file means no group names.

#' Read Variable Groups
#'
#' Read the project's variable groups: the display name of each `vargroup`
#' code and the order in which groups appear in a topical codebook (see `group_by = "vargroup"`
#' in [qt_codebook()]).
#'
#' @param config A `qt_config` object (default: `qt_config()`).
#' @param path Character string or `NULL`. Explicit path to a variable-groups
#'   file, overriding the config. A file that does not exist is an error.
#'
#' @return `NULL` if the project has no variable-groups file at the default
#'   path. Otherwise an object of class `qt_variable_groups`: a data frame with
#'   one row per group, in file order, and columns `id`, `label` and
#'   `description` (`NA` if not given). The label of the catch-all section is
#'   stored in `attr(, "unclassified")`.
#'
#' @details
#' The file location is `paths: variable_groups:` in `_qretools.yml`; the
#' default is `_variable_groups.yml` in the project root. If that file does not
#' exist the project has no variable groups. If the location is changed in
#' `_qretools.yml` and the file does not exist, an error is raised.
#'
#' ```yaml
#' variable_groups:
#'   - id: nhd
#'     label: Neighborhoods
#'   - id: fin
#'     label: Finances and Employment
#'     description: Income, assets and debt
#' unclassified:
#'   label: Not classified
#' ```
#'
#' `id` is the `vargroup` code used in the question bank; groups appear in the
#' order listed. `description` is optional. `unclassified` is optional and
#' names the final section for variables with no group (default
#' `"Not classified"`).
#'
#' @seealso [qt_codebook()], [qt_read_missing_codes()]
#'
#' @export
qt_read_variable_groups <- function(config = qt_config(), path = NULL) {
  default <- .qt_defaults()$paths$variable_groups

  if (is.null(path)) {
    rel      <- config$paths$variable_groups %||% default
    explicit <- !identical(rel, default)
    path     <- file.path(config$meta$project_root, rel)
  } else {
    explicit <- TRUE
  }
  if (!grepl("\\.ya?ml$", path)) path <- paste0(path, ".yml")

  if (!file.exists(path)) {
    if (explicit)
      stop("Variable-groups file not found: ", path, call. = FALSE)
    return(NULL)
  }

  .qt_parse_variable_groups(.read_yaml_safe(path, basename(path)),
                            source = basename(path))
}

# Internal: validate parsed YAML and build a qt_variable_groups object
.qt_parse_variable_groups <- function(y, source = "variable groups") {
  vg <- y[["variable_groups"]]
  if (!is.list(vg) || length(vg) == 0 || !is.null(names(vg)))
    stop(source, ": expected a 'variable_groups:' list, each item with 'id' ",
         "and 'label' (start each item with '- id:')", call. = FALSE)

  single_text <- function(v) {
    length(v) == 1 && !is.na(v) && nzchar(trimws(as.character(v)))
  }

  n      <- length(vg)
  ids    <- character(n)
  labels <- character(n)
  descs  <- rep(NA_character_, n)
  for (i in seq_len(n)) {
    e  <- vg[[i]]
    id <- if (is.list(e)) e[["id"]] else NULL
    lb <- if (is.list(e)) e[["label"]] else NULL
    if (!single_text(id) || !grepl("^[A-Za-z0-9][A-Za-z0-9_.-]*$", as.character(id)))
      stop(source, ": item ", i, " needs an 'id' made of letters, digits, ",
           "'_', '.' or '-'", call. = FALSE)
    if (!single_text(lb))
      stop(source, ": group '", id, "' needs a non-empty 'label'", call. = FALSE)
    ids[i]    <- as.character(id)
    labels[i] <- trimws(as.character(lb))
    ds <- e[["description"]]
    if (!is.null(ds)) {
      if (!single_text(ds))
        stop(source, ": group '", id, "' has an invalid 'description'",
             call. = FALSE)
      descs[i] <- as.character(ds)
    }
  }
  if (anyDuplicated(ids))
    stop(source, ": duplicate ids: ",
         paste(unique(ids[duplicated(ids)]), collapse = ", "), call. = FALSE)
  if (anyDuplicated(labels))
    stop(source, ": duplicate labels: ",
         paste(unique(labels[duplicated(labels)]), collapse = ", "), call. = FALSE)

  unc <- "Not classified"
  if (!is.null(y[["unclassified"]])) {
    ul <- if (is.list(y[["unclassified"]])) y[["unclassified"]][["label"]] else NULL
    if (!single_text(ul))
      stop(source, ": 'unclassified' needs a non-empty 'label'", call. = FALSE)
    unc <- trimws(as.character(ul))
  }
  if (unc %in% labels)
    stop(source, ": the 'unclassified' label duplicates a group label: ", unc,
         call. = FALSE)

  .qt_new_variable_groups(ids, labels, descs, unc)
}

# Internal: constructor
.qt_new_variable_groups <- function(id, label,
                                    description = rep(NA_character_, length(id)),
                                    unclassified = "Not classified") {
  structure(
    data.frame(id = id, label = label, description = description,
               stringsAsFactors = FALSE),
    unclassified = unclassified,
    class = c("qt_variable_groups", "data.frame")
  )
}

# Internal: give entries their display group, for `group_by = "vargroup"` ----
#
# Reads the project's variable-groups file. If there is none, entries are left
# alone and .qt_group_entries() groups by raw vargroup code. Otherwise each
# entry gets `group_name` (the label from the file) and `group_rank` (its
# position in the file; the unclassified section is last). A vargroup the file
# does not list is reported and placed in the unclassified section.
.qt_label_entry_groups <- function(entries, group_by = "none", config = NULL) {
  if (!identical(group_by, "vargroup")) return(entries)

  cfg <- config %||% tryCatch(qt_config(), error = function(e) NULL)
  vg  <- if (is.null(cfg)) NULL else qt_read_variable_groups(cfg)
  if (is.null(vg)) return(entries)

  unc <- attr(vg, "unclassified") %||% "Not classified"
  grp <- vapply(entries, function(e) {
    g <- e[["vargroup"]]
    if (length(g) == 1 && !is.na(g) && nzchar(g)) as.character(g) else NA_character_
  }, character(1))

  unknown <- setdiff(unique(grp[!is.na(grp)]), vg$id)
  if (length(unknown) > 0) {
    warning("vargroup(s) not listed in the variable-groups file: ",
            paste(unknown, collapse = ", "),
            " (placed under '", unc, "')", call. = FALSE)
    grp[grp %in% unknown] <- NA_character_
  }

  rank <- match(grp, vg$id)
  rank[is.na(rank)] <- nrow(vg) + 1L
  labs <- c(vg$label, unc)

  for (i in seq_along(entries)) {
    entries[[i]]$group_name <- labs[rank[i]]
    entries[[i]]$group_rank <- rank[i]
  }
  entries
}

#' @export
print.qt_variable_groups <- function(x, ...) {
  cat("Variable groups\n")
  cat(strrep("=", 50), "\n\n", sep = "")
  for (i in seq_len(nrow(x))) {
    cat(sprintf("  %-10s %s\n", x$id[i], x$label[i]))
  }
  cat(sprintf("  %-10s %s\n", "(none)", attr(x, "unclassified") %||% "Not classified"))
  invisible(x)
}
