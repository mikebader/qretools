# Data Summary Functions
#
# Join a processed survey (qt_qreconfig) to a collected data frame and
# summarize each documented variable according to its storage type. The
# question definition used for each variable is the version that applies to
# the survey (see .qt_effective_version_id() / .qt_resolve_version()).

#' Summarize Collected Data Against a Processed Survey
#'
#' Builds a lookup of the questions in a processed survey, resolves the
#' question version used by the survey, reconciles the lookup with the columns
#' of `data`, and summarizes each variable by storage type.
#'
#' @param data A data frame of collected responses. Column names must equal
#'   instrument variable IDs; factor columns hold numeric codes.
#' @param qre A processed survey: the `qt_qreconfig` object returned by
#'   `qt_process()` (or `qt_read_survey_config()`).
#' @param value_labels A `qt_vlabs` object (see `qt_read_value_labels()`), or
#'   `NULL`. Required to label factor codes; without it factor tables show
#'   observed codes only.
#' @param survey_id Character string. Survey ID used to pick each question's
#'   version. Defaults to `qre$meta$id`.
#' @param strict Logical. If `TRUE`, stop when the data and instrument do not
#'   reconcile (questions missing from the data, or data columns not accounted
#'   for). If `FALSE` (default), warn and report the problems in the result.
#'
#' @return An object of class `qt_data_summary`: a data frame with one row per
#'   variable and columns `variable_id`, `parent_id`, `section_id`, `source`,
#'   `storage_type`, `version_id`, `version_status`, `value_label_id`,
#'   `status`, `n`, `n_missing`, `flags`, and the list-column `summary`.
#'
#' @details
#' `status` is one of:
#' \describe{
#'   \item{ok}{in the instrument and the data; summarized (character
#'     variables are recorded as character and not documented)}
#'   \item{container}{parent of a `multiple_response` question; its options
#'     are the child rows}
#'   \item{missing_from_data}{in the instrument but not in the data}
#'   \item{needs_compilation}{`composite` variables and their children,
#'     control parameters, and data columns not in the instrument}
#' }
#'
#' `version_status` is `"unversioned"`, `"matched"` (a version lists
#' `survey_id` in `surveys_used`), or `"fallback_last"` (no version matched, so
#' the last version was used; check the question bank).
#'
#' Multiple-response options are the child variables listed in the parent's
#' `creates_variables`; each is summarized as its own column.
#'
#' @seealso [qt_process()], [qt_codebook()]
#'
#' @examples
#' \dontrun{
#' qre  <- qt_process(my_qre)
#' vl   <- qt_read_value_labels()
#' dat  <- read.csv("bas-2026.csv")
#' ds   <- qt_data_summary(dat, qre, value_labels = vl)
#' ds
#' ds[ds$status != "ok", ]
#' }
#'
#' @export
qt_data_summary <- function(data, qre, value_labels = NULL,
                            survey_id = NULL, strict = FALSE) {
  if (!is.data.frame(data))
    stop("'data' must be a data frame", call. = FALSE)
  if (!inherits(qre, "qt_qreconfig"))
    stop("'qre' must be a processed survey (class 'qt_qreconfig'); ",
         "run qt_process() first", call. = FALSE)

  survey_id <- survey_id %||% qre$meta$id
  if (is.null(survey_id) || !nzchar(survey_id))
    stop("Cannot determine survey_id; supply 'survey_id'", call. = FALSE)

  # 1. Lookup of instrument variables (version-resolved) ----------------------
  lookup <- .qt_ds_lookup(qre, survey_id)

  # 2. Summarize / reconcile each lookup row ----------------------------------
  recs <- lapply(lookup, .qt_ds_record, data = data, value_labels = value_labels)

  # 3. Data columns not accounted for by the instrument -----------------------
  known    <- vapply(lookup, function(r) r$variable_id, character(1))
  controls <- qre$indices$controls %||% character()
  extra    <- setdiff(names(data), known)
  for (nm in extra) {
    is_ctrl <- nm %in% controls
    recs[[length(recs) + 1]] <- .qt_ds_blank(
      variable_id = nm,
      status      = "needs_compilation",
      flags       = if (is_ctrl) "control parameter" else "in data, not in instrument",
      n           = sum(!is.na(data[[nm]])),
      n_missing   = sum(is.na(data[[nm]]))
    )
  }

  out <- .qt_ds_assemble(recs)

  # 4. Report problems ---------------------------------------------------------
  n_missing_cols <- sum(out$status == "missing_from_data")
  n_unaccounted  <- sum(out$flags == "in data, not in instrument")
  n_fallback     <- sum(out$version_status == "fallback_last", na.rm = TRUE)
  msgs <- character()
  if (n_missing_cols > 0)
    msgs <- c(msgs, sprintf("%d instrument variable(s) not in data", n_missing_cols))
  if (n_unaccounted > 0)
    msgs <- c(msgs, sprintf("%d data column(s) not in instrument", n_unaccounted))
  if (length(msgs) > 0) {
    if (strict) stop(paste(msgs, collapse = "; "), call. = FALSE)
    warning(paste(msgs, collapse = "; "), call. = FALSE)
  }
  if (n_fallback > 0)
    message(n_fallback, " variable(s) matched no version for '", survey_id,
            "' and used the last version (see version_status)")

  attr(out, "survey_id") <- survey_id
  class(out) <- c("qt_data_summary", "data.frame")
  out
}


# Internal: build version-resolved lookup rows from a qt_qreconfig -------------
#
# One row per question variable (first occurrence). Children created by
# `creates_variables` get their own rows.
.qt_ds_lookup <- function(qre, survey_id) {
  rows <- list()
  seen <- character()

  for (it in qre$items) {
    if (!inherits(it, "qt_qitem")) next
    vid <- it$variable_id
    if (vid %in% seen) next
    seen <- c(seen, vid)

    def      <- it$definition
    versions <- def$versions
    if (is.null(versions) || length(versions) == 0) {
      version_id     <- NA_character_
      version_status <- "unversioned"
      eff            <- NULL
    } else {
      eff <- .qt_effective_version_id(def, survey_id)
      if (is.null(eff)) {
        version_status <- "fallback_last"
        version_id     <- versions[[length(versions)]]$version_id %||% NA_character_
      } else {
        version_status <- "matched"
        version_id     <- eff
      }
    }

    res     <- .qt_resolve_version(def, eff)
    storage <- res$storage_type %||% NA_character_
    vl_id   <- res$value_label_id %||% res$value_labels_name %||% NA_character_
    kids    <- .qt_ds_children(res)

    rows[[length(rows) + 1]] <- list(
      variable_id    = vid,
      parent_id      = NA_character_,
      section_id     = it$section_id %||% NA_character_,
      source         = it$source %||% NA_character_,
      storage_type   = storage,
      version_id     = version_id,
      version_status = version_status,
      value_label_id = vl_id,
      role           = if (identical(storage, "multiple_response")) "container"
      else if (identical(storage, "composite")) "composite"
      else "variable"
    )

    # Children of multiple_response / composite parents
    for (kid in names(kids)) {
      kd <- kids[[kid]]
      rows[[length(rows) + 1]] <- list(
        variable_id    = kid,
        parent_id      = vid,
        section_id     = it$section_id %||% NA_character_,
        source         = it$source %||% NA_character_,
        storage_type   = if (identical(storage, "multiple_response"))
          "multiple_response" else (kd$storage_type %||% NA_character_),
        version_id     = version_id,
        version_status = version_status,
        value_label_id = kd$value_label_id %||% NA_character_,
        role           = if (identical(storage, "composite")) "composite_child"
        else "option"
      )
    }
  }
  rows
}

# Internal: child variable definitions from creates_variables (inline or split)
.qt_ds_children <- function(res) {
  cv <- res$creates_variables
  if (is.null(cv) || length(cv) == 0) return(list())

  if (is.list(cv) && !is.null(names(cv))) {          # inline: named mapping
    return(lapply(cv, function(x) if (is.list(x)) x else list()))
  }

  ids   <- as.character(unlist(cv))                   # split: IDs + variable_parts
  parts <- res$variable_parts %||% list()
  stats::setNames(lapply(ids, function(id) {
    p <- parts[[id]]
    if (is.list(p)) p else list()
  }), ids)
}

# Internal: summarize one lookup row against the data --------------------------
.qt_ds_record <- function(r, data, value_labels) {
  rec <- .qt_ds_blank(
    variable_id = r$variable_id, parent_id = r$parent_id,
    section_id = r$section_id, source = r$source,
    storage_type = r$storage_type, version_id = r$version_id,
    version_status = r$version_status, value_label_id = r$value_label_id
  )

  if (r$role == "container") {
    rec$status <- "container"
    return(rec)
  }
  if (r$role %in% c("composite", "composite_child")) {
    rec$status <- "needs_compilation"
    rec$flags  <- "composite variable"
    if (r$variable_id %in% names(data)) {
      rec$n <- sum(!is.na(data[[r$variable_id]]))
      rec$n_missing <- sum(is.na(data[[r$variable_id]]))
    }
    return(rec)
  }

  if (!r$variable_id %in% names(data)) {
    rec$status <- "missing_from_data"
    return(rec)
  }

  x <- data[[r$variable_id]]
  s <- .qt_ds_summarize(x, r$storage_type, r$value_label_id, value_labels)
  rec$status    <- "ok"
  rec$n         <- sum(!is.na(x))
  rec$n_missing <- sum(is.na(x))
  rec$flags     <- s$flags
  rec$summary   <- s$summary
  rec
}

# Internal: type-specific summary ---------------------------------------------
.qt_ds_summarize <- function(x, storage, vl_id, value_labels) {
  flags <- character()
  obs   <- x[!is.na(x)]

  if (is.na(storage)) {
    return(list(summary = NULL, flags = "no storage_type in question definition"))
  }

  if (storage == "character") {
    return(list(summary = NULL, flags = "character variable (not documented)"))
  }

  if (storage %in% c("integer", "numeric")) {
    num <- if (is.numeric(obs)) obs else suppressWarnings(as.numeric(as.character(obs)))
    if (any(is.na(num)))
      flags <- c(flags, "non-numeric values in numeric variable")
    num <- num[!is.na(num)]
    sm <- if (length(num) == 0) {
      c(min = NA_real_, max = NA_real_, mean = NA_real_, sd = NA_real_)
    } else {
      c(min = min(num), max = max(num), mean = mean(num),
        sd = if (length(num) > 1) stats::sd(num) else NA_real_)
    }
    return(list(summary = sm, flags = paste(flags, collapse = "; ")))
  }

  if (storage == "factor") {
    codes <- as.character(obs)
    set   <- if (!is.na(vl_id) && !is.null(value_labels))
      value_labels$labels[[vl_id]] else NULL
    if (is.null(set)) {
      flags <- c(flags, if (is.na(vl_id)) "no value label set"
                 else "value label set not found")
      lab_codes <- character(); lab_text <- character()
    } else {
      lab_codes <- as.character(set$values)
      lab_text  <- as.character(unname(set$labels))
    }
    all_codes <- c(lab_codes, setdiff(sort(unique(codes)), lab_codes))
    tab <- data.frame(
      value = all_codes,
      label = c(lab_text, rep(NA_character_, length(all_codes) - length(lab_codes))),
      n     = vapply(all_codes, function(cd) sum(codes == cd), integer(1)),
      row.names = NULL, stringsAsFactors = FALSE
    )
    unlabeled <- setdiff(unique(codes), lab_codes)
    if (!is.null(set) && length(unlabeled) > 0)
      flags <- c(flags, paste0("values without labels: ",
                               paste(sort(unlabeled), collapse = ", ")))
    return(list(summary = tab, flags = paste(flags, collapse = "; ")))
  }

  if (storage == "multiple_response") {
    vals <- sort(unique(as.character(obs)))
    tab  <- data.frame(
      value = vals,
      n     = vapply(vals, function(v) sum(as.character(obs) == v), integer(1)),
      row.names = NULL, stringsAsFactors = FALSE
    )
    return(list(summary = tab, flags = ""))
  }

  list(summary = NULL, flags = paste0("unrecognized storage_type '", storage, "'"))
}

# Internal: empty record ---------------------------------------------------------
.qt_ds_blank <- function(variable_id, parent_id = NA_character_,
                         section_id = NA_character_, source = NA_character_,
                         storage_type = NA_character_, version_id = NA_character_,
                         version_status = NA_character_,
                         value_label_id = NA_character_,
                         status = NA_character_, flags = "",
                         n = NA_integer_, n_missing = NA_integer_) {
  list(variable_id = variable_id, parent_id = parent_id, section_id = section_id,
       source = source, storage_type = storage_type, version_id = version_id,
       version_status = version_status, value_label_id = value_label_id,
       status = status, n = as.integer(n), n_missing = as.integer(n_missing),
       flags = flags, summary = NULL)
}

# Internal: records -> data frame with summary list-column ----------------------
.qt_ds_assemble <- function(recs) {
  chr <- function(f) vapply(recs, function(r) as.character(r[[f]] %||% NA_character_), character(1))
  int <- function(f) vapply(recs, function(r) as.integer(r[[f]] %||% NA_integer_), integer(1))
  out <- data.frame(
    variable_id    = chr("variable_id"),
    parent_id      = chr("parent_id"),
    section_id     = chr("section_id"),
    source         = chr("source"),
    storage_type   = chr("storage_type"),
    version_id     = chr("version_id"),
    version_status = chr("version_status"),
    value_label_id = chr("value_label_id"),
    status         = chr("status"),
    n              = int("n"),
    n_missing      = int("n_missing"),
    flags          = chr("flags"),
    stringsAsFactors = FALSE
  )
  out$flags[is.na(out$flags)] <- ""
  out$summary <- lapply(recs, function(r) r$summary)
  out
}

#' @export
print.qt_data_summary <- function(x, ...) {
  cat("Data summary: ", attr(x, "survey_id") %||% "", "\n", sep = "")
  cat(strrep("=", 50), "\n\n", sep = "")
  st <- table(x$status)
  for (nm in names(st)) cat(sprintf("  %-18s %d\n", nm, st[[nm]]))
  fb <- sum(x$version_status == "fallback_last", na.rm = TRUE)
  if (fb > 0) cat(sprintf("  %-18s %d\n", "version fallback", fb))

  flagged <- x[x$flags != "" | x$status %in% c("missing_from_data", "needs_compilation"), ,
               drop = FALSE]
  if (nrow(flagged) > 0) {
    cat("\nFlagged variables:\n")
    for (i in seq_len(nrow(flagged))) {
      cat(sprintf("  %-24s %-18s %s\n", flagged$variable_id[i], flagged$status[i],
                  flagged$flags[i]))
    }
  }
  invisible(x)
}
