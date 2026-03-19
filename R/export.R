# CSV Export Functions
#
# Functions for exporting variable bank summaries to CSV format.

#' Export Variable Bank to CSV
#'
#' Create a CSV summary of variables in a bank. The unit of observation is
#' controlled by the \code{rows} parameter: one row per variable (default) or
#' one row per question (collapsing select-all indicators that share
#' \code{question_text} into a single row).
#'
#' @param bank A bank object: \code{qt_qbank}, \code{qt_genbank}, or
#'   \code{qt_ctrlbank}.
#' @param file Character string or \code{NULL}. Path for the output CSV file.
#'   If \code{NULL} (the default), no file is written and the data frame is
#'   returned visibly.
#' @param survey Character string or \code{NULL}. If supplied, only variables
#'   whose \code{surveys_used} field includes this survey ID are included. If
#'   \code{NULL} (the default), all variables are included.
#' @param surveys_sep Character string. Separator used when collapsing multiple
#'   survey IDs or variable IDs into a single cell. Ignored when
#'   \code{survey_cols = TRUE} for the surveys column. Default: \code{";"}.
#' @param survey_cols Logical. If \code{TRUE} (default), each survey wave gets
#'   its own indicator column (one column per unique survey ID found in the
#'   bank, value \code{TRUE}/\code{FALSE}) instead of a single collapsed
#'   \code{surveys_used} string.
#' @param rows Character string. Controls the unit of observation:
#'   \describe{
#'     \item{\code{"variables"}}{(Default) One row per variable. Select-all
#'       questions produce one row per indicator.}
#'     \item{\code{"questions"}}{One row per unique \code{question_text}.
#'       Select-all indicators that share the same question text are collapsed
#'       into a single row; their variable IDs are joined with
#'       \code{surveys_sep}. Variables with an empty \code{question_text}
#'       (e.g., derived indicators) each get their own row.}
#'   }
#'
#' @return A data frame. Column layout depends on \code{rows}:
#'
#'   \strong{\code{rows = "variables"}:}
#'   \describe{
#'     \item{variable_id}{Variable identifier.}
#'     \item{title}{Short descriptive title.}
#'     \item{value_label_id}{Value label set name, or \code{NA}.}
#'     \item{surveys_used}{(\code{survey_cols = FALSE}) Collapsed survey IDs.}
#'     \item{<survey_id>}{(\code{survey_cols = TRUE}) One logical column per
#'       survey wave.}
#'   }
#'
#'   \strong{\code{rows = "questions"}:}
#'   \describe{
#'     \item{question_text}{The question as shown to respondents.}
#'     \item{variable_ids}{Variable IDs that this question produces, joined
#'       with \code{surveys_sep}. Single-variable questions show one ID.}
#'     \item{value_label_id}{First non-\code{NA} value label set name
#'       found among the grouped variables, or \code{NA}.}
#'     \item{surveys_used}{(\code{survey_cols = FALSE}) Union of survey IDs
#'       across all grouped variables, collapsed with \code{surveys_sep}.}
#'     \item{<survey_id>}{(\code{survey_cols = TRUE}) \code{TRUE} when any
#'       variable in the group was used in that wave.}
#'   }
#'
#'   If \code{file} is supplied the data frame is returned invisibly.
#'
#' @examples
#' \dontrun{
#' qbank <- qt_qbank()
#'
#' # One row per variable (default)
#' qt_export_csv(qbank)
#'
#' # One row per question (select-all sets collapsed)
#' qt_export_csv(qbank, rows = "questions")
#'
#' # Write to file
#' qt_export_csv(qbank, file = "variables.csv")
#'
#' # Filter to one survey wave
#' qt_export_csv(qbank, file = "bas-2024-variables.csv", survey = "bas-2024")
#'
#' # Collapsed surveys_used string instead of indicator columns
#' qt_export_csv(qbank, survey_cols = FALSE)
#' qt_export_csv(qbank, rows = "questions", survey_cols = FALSE)
#' }
#'
#' @export
qt_export_csv <- function(bank, file = NULL, survey = NULL,
                          surveys_sep = ";", survey_cols = TRUE,
                          rows = c("variables", "questions")) {
  if (!inherits(bank, "qt_bank")) {
    stop(
      "'bank' must be a qt_qbank, qt_genbank, or qt_ctrlbank object",
      call. = FALSE
    )
  }

  rows <- match.arg(rows)
  vars <- bank$variables

  # Filter by survey if requested
  if (!is.null(survey)) {
    vars <- Filter(function(v) survey %in% v$surveys_used, vars)
    if (length(vars) == 0)
      warning("No variables found for survey '", survey, "'", call. = FALSE)
  }

  df <- if (rows == "questions")
    .qt_export_question_rows(vars, surveys_sep, survey_cols)
  else
    .qt_export_variable_rows(vars, surveys_sep, survey_cols)

  if (!is.null(file)) {
    utils::write.csv(df, file = file, row.names = FALSE)
    invisible(df)
  } else {
    df
  }
}


# Build a data frame with one row per variable.
# @keywords internal
# @noRd
.qt_export_variable_rows <- function(vars, surveys_sep, survey_cols) {
  if (length(vars) == 0) {
    df <- data.frame(variable_id = character(), title = character(),
                     value_label_id = character(),
                     stringsAsFactors = FALSE)
    if (!survey_cols) df$surveys_used <- character()
    return(df)
  }

  all_surveys <- if (survey_cols)
    sort(unique(unlist(lapply(vars, `[[`, "surveys_used"))))

  rows_list <- lapply(vars, function(v) {
    row <- data.frame(
      variable_id       = v$variable_id,
      title             = v$title,
      value_label_id = v$value_label_id %||% NA_character_,
      stringsAsFactors  = FALSE
    )
    if (survey_cols) {
      for (s in all_surveys) row[[s]] <- s %in% v$surveys_used
    } else {
      row$surveys_used <- paste(v$surveys_used, collapse = surveys_sep)
    }
    row
  })

  df <- do.call(rbind, rows_list)
  rownames(df) <- NULL
  df
}


# Build a data frame with one row per unique question_text.
# Variables sharing question_text are collapsed; their IDs are joined with
# surveys_sep. Variables with an empty question_text get individual rows.
# @keywords internal
# @noRd
.qt_export_question_rows <- function(vars, surveys_sep, survey_cols) {
  if (length(vars) == 0) {
    df <- data.frame(question_text = character(), variable_ids = character(),
                     value_label_id = character(),
                     stringsAsFactors = FALSE)
    if (!survey_cols) df$surveys_used <- character()
    return(df)
  }

  all_surveys <- if (survey_cols)
    sort(unique(unlist(lapply(vars, `[[`, "surveys_used"))))

  # Group by question_text, preserving first-appearance order.
  # Variables with empty question_text each form their own singleton group.
  qt_vals <- vapply(vars, function(v) v$question_text %||% "", character(1))
  var_ids <- names(vars)

  seen   <- character()
  groups <- list()
  for (i in seq_along(var_ids)) {
    qt <- qt_vals[[i]]
    id <- var_ids[[i]]
    if (qt == "" || !qt %in% seen) {
      groups <- c(groups, list(list(qt = qt, ids = id)))
      if (qt != "") seen <- c(seen, qt)
    } else {
      idx <- which(vapply(groups, function(g) g$qt == qt, logical(1)))
      groups[[idx]]$ids <- c(groups[[idx]]$ids, id)
    }
  }

  rows_list <- lapply(groups, function(g) {
    group_vars  <- vars[g$ids]
    grp_surveys <- sort(unique(unlist(lapply(group_vars, `[[`, "surveys_used"))))
    first_vlabs <- Filter(Negate(is.null),
                          lapply(group_vars, `[[`, "value_label_id"))

    row <- data.frame(
      question_text     = g$qt,
      variable_ids      = paste(g$ids, collapse = surveys_sep),
      value_label_id = if (length(first_vlabs) > 0)
                            first_vlabs[[1]] else NA_character_,
      stringsAsFactors  = FALSE
    )
    if (survey_cols) {
      for (s in all_surveys) row[[s]] <- s %in% grp_surveys
    } else {
      row$surveys_used <- paste(grp_surveys, collapse = surveys_sep)
    }
    row
  })

  df <- do.call(rbind, rows_list)
  rownames(df) <- NULL
  df
}
