# Variable Groups
#
# Project-level list of the broad topical groups (the `vargroup` field of each
# variable) with display names and section order, for topical codebooks. The
# groups are defined once per project in a YAML file whose location is set in
# `paths: variable_groups:` (default `_variable_groups.yml` in the project
# root). No file means no group names.

#' Read Variable Groups
#'
#' Read the project's variable groups: the display name of each `vargroup`
#' code and the order in which groups appear in a topical codebook (see the
#' `variable_groups` argument of [qt_codebook()]).
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

# Internal: turn the `variable_groups` argument into a qt_variable_groups or NULL
#
# FALSE/NULL -> NULL (no grouping)
# TRUE       -> read from the project config; if the project has no file, an
#               empty object flagged `fallback` (group by raw vargroup ids)
# qt_variable_groups object -> used as is
.qt_resolve_variable_groups <- function(variable_groups, config = NULL) {
  if (is.null(variable_groups) || isFALSE(variable_groups)) return(NULL)
  if (inherits(variable_groups, "qt_variable_groups")) return(variable_groups)

  if (isTRUE(variable_groups)) {
    cfg <- config %||% tryCatch(qt_config(), error = function(e) NULL)
    vg  <- if (is.null(cfg)) NULL else qt_read_variable_groups(cfg)
    if (is.null(vg)) {
      vg <- .qt_new_variable_groups(character(), character())
      attr(vg, "fallback") <- TRUE
    }
    return(vg)
  }

  stop("'variable_groups' must be TRUE, FALSE, or the result of ",
       "qt_read_variable_groups()", call. = FALSE)
}

# Internal: order codebook entries into topical sections ----------------------
#
# Each entry gets `group_label`. Groups follow the file order; entries with no
# group, or a group the file does not list, go last under the unclassified
# label. Children (multiple-response options, composite parts) take their
# parent's group when they have none. Order within a group is unchanged.
.qt_group_entries <- function(entries, vg) {
  fallback  <- isTRUE(attr(vg, "fallback"))
  unc_label <- attr(vg, "unclassified") %||% "Not classified"

  ids <- vapply(entries, function(e) e$variable_id, character(1))
  own <- lapply(entries, function(e) {
    g <- e$vargroup
    if (length(g) == 1 && !is.na(g) && nzchar(g)) as.character(g) else NULL
  })
  names(own) <- ids

  grp <- vapply(seq_along(entries), function(i) {
    g   <- own[[i]]
    pid <- entries[[i]]$parent_id
    if (is.null(g) && !is.null(pid) && pid %in% ids) g <- own[[pid]]
    g %||% NA_character_
  }, character(1))

  if (fallback) {
    group_ids <- unique(grp[!is.na(grp)])
    labels    <- group_ids
    message("No variable-groups file found for this project; sections use the ",
            "raw vargroup codes in the order they first appear")
  } else {
    group_ids <- vg$id
    labels    <- vg$label
    unknown   <- setdiff(unique(grp[!is.na(grp)]), group_ids)
    if (length(unknown) > 0) {
      warning("vargroup(s) not listed in the variable-groups file: ",
              paste(unknown, collapse = ", "),
              " (placed under '", unc_label, "')", call. = FALSE)
      grp[grp %in% unknown] <- NA_character_
    }
  }

  sect <- match(grp, group_ids)
  sect[is.na(sect)] <- length(group_ids) + 1L
  all_labels <- c(labels, unc_label)

  ord <- order(sect)
  out <- entries[ord]
  for (k in seq_along(out)) out[[k]]$group_label <- all_labels[sect[ord][k]]
  out
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
