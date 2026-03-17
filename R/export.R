# CSV Export Functions
#
# Functions for exporting variable bank summaries to CSV format.

#' Export Variable Bank to CSV
#'
#' Create a CSV summary of variables in a bank, with one row per variable
#' containing the variable name, title, value labels name (if present), and
#' the surveys in which the variable was asked.
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
#'   survey IDs into a single cell. Ignored when \code{survey_cols = TRUE}.
#'   Default: \code{";"}.
#' @param survey_cols Logical. If \code{TRUE}, each survey wave gets its own
#'   indicator column (one column per unique survey ID found in the bank,
#'   value \code{TRUE}/\code{FALSE}) instead of a single collapsed
#'   \code{surveys_used} string. Default: \code{FALSE}.
#'
#' @return A data frame with columns:
#'   \describe{
#'     \item{variable_id}{Variable name/identifier}
#'     \item{title}{Short descriptive title}
#'     \item{value_labels_name}{Name of the value label set, or \code{NA} if
#'       the variable has no associated labels}
#'     \item{surveys_used}{(\code{survey_cols = FALSE} only) Survey IDs
#'       collapsed into a single string using \code{surveys_sep}.}
#'     \item{<survey_id>}{(\code{survey_cols = TRUE} only) One logical column
#'       per survey wave, \code{TRUE} when the variable was used in that wave.}
#'   }
#'   If \code{file} is supplied the data frame is written to that path and
#'   returned invisibly; otherwise it is returned visibly.
#'
#' @examples
#' \dontrun{
#' qbank <- qt_qbank()
#'
#' # Return as a data frame
#' df <- qt_export_csv(qbank)
#'
#' # Write to file
#' qt_export_csv(qbank, file = "variables.csv")
#'
#' # Filter to a single survey wave
#' qt_export_csv(qbank, file = "bas-2024-variables.csv", survey = "bas-2024")
#'
#' # One column per survey instead of a delimited list
#' qt_export_csv(qbank, survey_cols = TRUE)
#' }
#'
#' @export
qt_export_csv <- function(bank, file = NULL, survey = NULL,
                          surveys_sep = ";", survey_cols = TRUE) {
  if (!inherits(bank, "qt_bank")) {
    stop(
      "'bank' must be a qt_qbank, qt_genbank, or qt_ctrlbank object",
      call. = FALSE
    )
  }

  vars <- bank$variables

  # Filter by survey if requested
  if (!is.null(survey)) {
    vars <- Filter(function(v) survey %in% v$surveys_used, vars)
    if (length(vars) == 0) {
      warning("No variables found for survey '", survey, "'", call. = FALSE)
    }
  }

  # Build data frame
  if (length(vars) == 0) {
    df <- data.frame(
      variable_id       = character(),
      title             = character(),
      value_labels_name = character(),
      stringsAsFactors  = FALSE
    )
    if (!survey_cols)
      df$surveys_used <- character()
  } else if (survey_cols) {
    # Collect all unique survey IDs across the (filtered) variables, sorted
    all_surveys <- sort(unique(unlist(lapply(vars, `[[`, "surveys_used"))))

    rows <- lapply(vars, function(v) {
      row <- data.frame(
        variable_id       = v$variable_id,
        title             = v$title,
        value_labels_name = if (!is.null(v$value_labels_name))
                              v$value_labels_name
                            else
                              NA_character_,
        stringsAsFactors  = FALSE
      )
      for (s in all_surveys)
        row[[s]] <- s %in% v$surveys_used
      row
    })
    df <- do.call(rbind, rows)
    rownames(df) <- NULL
  } else {
    rows <- lapply(vars, function(v) {
      data.frame(
        variable_id       = v$variable_id,
        title             = v$title,
        value_labels_name = if (!is.null(v$value_labels_name))
                              v$value_labels_name
                            else
                              NA_character_,
        surveys_used      = paste(v$surveys_used, collapse = surveys_sep),
        stringsAsFactors  = FALSE
      )
    })
    df <- do.call(rbind, rows)
    rownames(df) <- NULL
  }

  if (!is.null(file)) {
    utils::write.csv(df, file = file, row.names = FALSE)
    invisible(df)
  } else {
    df
  }
}
