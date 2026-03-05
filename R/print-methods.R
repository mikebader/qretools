# Print & Summary Methods for qretools Variable Objects
#
# Brief S3 print methods for individual variable objects (qt_qvar, qt_ctrlvar,
# qt_genvar). The default output shows the variable ID, storage type, title,
# and surveys in which the variable was used.

#' Print a qretools Variable Object
#'
#' Display a brief one- or two-line summary of an individual variable object.
#' The output shows the variable ID, storage type in brackets, the title on
#' the first line, and the surveys used on the second line.
#'
#' @param x A variable object (\code{qt_qvar}, \code{qt_ctrlvar}, or
#'   \code{qt_genvar})
#' @param ... Additional arguments (ignored)
#'
#' @return Invisibly returns the original object
#'
#' Summarize a qretoos Variable Object
#'
#' Display more information about an individual variable object. The output
#' shows the information in `print()` methods as well as additional information
#' such as factor labels, full question or description, etc.
#'
#' @examples
#' \dontrun{
#' qbank <- qt_qbank()
#' print(qbank$variables[["nhd_sat"]])
#' # nhd_sat [factor] Neighborhood Satisfaction
#' #   Surveys: bas-2023, bas-2024, bas-2025
#' }
#'
#' @name print-variables
#' @export
print.qt_qvar <- function(x, ...) {
  cat(x$variable_id, " [", x$storage_type, "] ", x$title, "\n", sep = "")
  cat("  Surveys: ", paste(x$surveys_used, collapse = ", "), "\n", sep = "")
  invisible(x)
}

#' @name print-variables
#' @export
summary.qt_qvar <- function(x, ...) {
  cli::cli_text("{.strong {x$variable_id}}: {x$title}")
  cli::cli_text("Variable type: {x$storage_type}")
  if (x$storage_type == "factor") {
    cli::cli_text("Factor label: {.val {x$value_labels_name}}")
  }
  if(!is.null(x$question_text) && nzchar(x$question_text)) {
    full <- paste("Question text:", x$question_text)
  } else {
    full <- paste("Description:", x$description)
  }
  cli::cli_text({full})
  cli::cli_text("Surveys: {paste(x$surveys_used, collapse = ", ")}")
}

#' @rdname print-variables
#' @export
print.qt_ctrlvar <- function(x, ...) {
  cat(x$variable_id, " [", x$storage_type, "] ", x$title, "\n", sep = "")
  cat("  Surveys: ", paste(x$surveys_used, collapse = ", "), "\n", sep = "")
  invisible(x)
}

#' @rdname print-variables
#' @export
print.qt_genvar <- function(x, ...) {
  cat(x$variable_id, " [", x$storage_type, "] ", x$title, "\n", sep = "")
  cat("  Surveys: ", paste(x$surveys_used, collapse = ", "), "\n", sep = "")
  invisible(x)
}


