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
#'   survey IDs into a single cell. Default: \code{";"}.
#'
#' @return A data frame with columns:
#'   \describe{
#'     \item{variable_id}{Variable name/identifier}
#'     \item{title}{Short descriptive title}
#'     \item{value_labels_name}{Name of the value label set, or \code{NA} if
#'       the variable has no associated labels}
#'     \item{surveys_used}{Survey IDs in which the variable was asked, collapsed
#'       into a single string using \code{surveys_sep}}
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
#' }
#'
#' @export
qt_export_csv <- function(bank, file = NULL, survey = NULL, surveys_sep = ";") {
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
      surveys_used      = character(),
      stringsAsFactors  = FALSE
    )
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
