#' Read Module Bank
#'
#' Read question module definitions from YAML file(s) in the project's module
#' bank directory. Modules group related questions into named sets with metadata
#' about their purpose and survey usage.
#'
#' @param config A qt_config object (default: \code{qt_config()}). Configuration
#'   specifying project paths and settings.
#' @param path Character string or NULL. Explicit path to override the config
#'   path. Primarily for testing. If NULL (default), uses path from config.
#'
#' @return An S3 object of class \code{qt_mbank} containing:
#'   \describe{
#'     \item{modules}{Named list of modules, indexed by module_id}
#'     \item{meta}{Metadata including source files, read time, and indices}
#'   }
#'
#' @details
#' Each module definition must include:
#' \itemize{
#'   \item{\code{title} - Short descriptive title}
#'   \item{\code{description} - Full description of module contents}
#'   \item{\code{surveys_used} - Surveys when this module is fielded}
#'   \item{\code{questions} - Ordered list of question references (each with
#'     \code{variable_id} and optional \code{if_condition})}
#' }
#'
#' **File/Directory Flexibility:**
#' The path can be either a single YAML file or a directory containing multiple
#' YAML files. If a directory, all \code{.yml} files are read and combined.
#'
#' @section Default Path:
#' Default path is \code{banks/modules} (can be overridden in
#' \code{project-config.yml} or \code{_qretools.yml}).
#'
#' @examples
#' \dontrun{
#' # Read module bank
#' mbank <- qt_mbank()
#'
#' # Access specific module
#' mbank$modules$dem_basics
#'
#' # Use explicit path for testing
#' mbank <- qt_read_module_bank(path = "test-data/modules")
#' }
#'
#' @export
qt_read_module_bank <- function(config = qt_config(), path = NULL) {

  # 1. Resolve path
  source_path <- .qt_resolve_path("module_bank", path, config)

  # 2. Read YAML file(s)
  result <- .qt_read_yaml_files(source_path, "modules")

  # 3. Add module_id from dictionary key to each module
  for (module_id in names(result$items)) {
    result$items[[module_id]]$module_id <- module_id
  }

  # 4. Validate modules
  .qt_validate_modules(result$items)

  # 5. Wrap each module in qt_module class
  modules_with_classes <- lapply(result$items, function(mod_data) {
    structure(mod_data, class = "qt_module")
  })
  names(modules_with_classes) <- names(result$items)

  # 6. Build structure with indices
  .qt_build_module_structure(
    modules = modules_with_classes,
    source_files = result$source_files,
    source_path = result$source_path,
    source_type = result$source_type
  )
}

#' @rdname qt_read_module_bank
#' @export
qt_mbank <- qt_read_module_bank

# Internal helper: Validate module structure
#
# @param modules Named list of modules (from YAML)
# @return Invisible TRUE on success, stops with error on validation failure
.qt_validate_modules <- function(modules) {
  errors <- character()

  required_fields <- c("title", "description", "surveys_used", "questions")

  for (module_id in names(modules)) {
    mod <- modules[[module_id]]
    missing <- setdiff(required_fields, names(mod))

    if (length(missing) > 0) {
      errors <- c(errors,
                  sprintf("Module '%s': Missing required fields: %s",
                          module_id, paste(missing, collapse = ", ")))
    }

    # Validate questions list
    if (!is.null(mod$questions)) {
      if (!is.list(mod$questions)) {
        errors <- c(errors,
                    sprintf("Module '%s': 'questions' must be a list", module_id))
      } else {
        for (i in seq_along(mod$questions)) {
          q <- mod$questions[[i]]
          if (is.null(q$variable_id)) {
            errors <- c(errors,
                        sprintf("Module '%s': question[%d] missing required 'variable_id'",
                                module_id, i))
          }
        }
      }
    }
  }

  # Check for duplicate module_ids
  if (any(duplicated(names(modules)))) {
    dups <- names(modules)[duplicated(names(modules))]
    errors <- c(errors,
                sprintf("Duplicate module IDs found: %s", paste(unique(dups), collapse = ", ")))
  }

  if (length(errors) > 0) {
    stop("Validation errors:\n", paste(errors, collapse = "\n"), call. = FALSE)
  }

  invisible(TRUE)
}

# Internal helper: Build module structure with indices
#
# @param modules Named list of qt_module objects
# @param source_files Character vector of file paths read
# @param source_path Character string - original path
# @param source_type Character string - "file" or "directory"
# @return Structured qt_mbank object
.qt_build_module_structure <- function(modules, source_files, source_path, source_type) {
  file_order <- basename(source_files)
  if (source_type == "directory") {
    file_order <- unique(file_order)
  }

  module_ids <- names(modules)

  # by_module_id: alphabetical
  by_module_id <- sort(module_ids)

  # by_file: group by source file
  by_file <- list()
  for (module_id in module_ids) {
    file <- basename(modules[[module_id]]$source_file)
    if (is.null(by_file[[file]])) {
      by_file[[file]] <- character()
    }
    by_file[[file]] <- c(by_file[[file]], module_id)
  }

  # by_file_position: file order then position within file
  by_file_position <- character()
  for (file in file_order) {
    if (!is.null(by_file[[file]])) {
      vars_in_file <- by_file[[file]]
      positions <- sapply(vars_in_file, function(v) modules[[v]]$file_position)
      vars_in_file <- vars_in_file[order(positions)]
      by_file_position <- c(by_file_position, vars_in_file)
    }
  }

  # by_survey: group module IDs by survey wave
  by_survey <- list()
  for (mod_id in module_ids) {
    surveys <- modules[[mod_id]]$surveys_used
    if (!is.null(surveys)) {
      for (s in surveys) {
        if (is.null(by_survey[[s]])) by_survey[[s]] <- character(0)
        by_survey[[s]] <- c(by_survey[[s]], mod_id)
      }
    }
  }

  structure(
    list(
      modules = modules,
      meta = list(
        source_path = source_path,
        source_type = source_type,
        source_files = source_files,
        file_order = file_order,
        n_modules = length(modules),
        read_time = Sys.time(),
        indices = list(
          by_module_id = by_module_id,
          by_file = by_file,
          by_file_position = by_file_position,
          by_survey = by_survey
        )
      )
    ),
    class = "qt_mbank"
  )
}

#' Link Modules to Variable Banks
#'
#' Resolve module question references against a variable bank to attach full
#' variable metadata to each question entry in the module.
#'
#' @param mbank A \code{qt_mbank} object from \code{qt_mbank()}.
#' @param qbank A \code{qt_qbank} object from \code{qt_qbank()}. Used to look
#'   up variable metadata by \code{variable_id}.
#'
#' @return A \code{qt_mbank} object with variable metadata attached to each
#'   question entry under a \code{variable} field. Questions whose
#'   \code{variable_id} is not found in \code{qbank} will have \code{variable}
#'   set to \code{NULL} and a warning is issued.
#'
#' @details
#' For each question entry in each module, this function looks up the
#' \code{variable_id} in the provided question bank and attaches the full
#' variable object under a \code{variable} field. This makes it easy to access
#' question text, storage type, value labels, and other metadata directly from
#' the module structure.
#'
#' @examples
#' \dontrun{
#' mbank <- qt_mbank()
#' qbank <- qt_qbank()
#' mbank_linked <- qt_link_modules(mbank, qbank)
#'
#' # Access linked variable data
#' mbank_linked$modules$dem_basics$questions[[1]]$variable$title
#' }
#'
#' @export
qt_link_modules <- function(mbank, qbank) {
  if (!inherits(mbank, "qt_mbank")) {
    stop("'mbank' must be a qt_mbank object", call. = FALSE)
  }
  if (!inherits(qbank, "qt_bank")) {
    stop("'qbank' must be a qt_bank object (qt_qbank, qt_genbank, or qt_ctrlbank)",
         call. = FALSE)
  }

  missing_ids <- character()

  linked_modules <- lapply(mbank$modules, function(mod) {
    mod$questions <- lapply(mod$questions, function(q) {
      var_id <- q$variable_id
      var <- qbank$variables[[var_id]]

      if (is.null(var)) {
        missing_ids <<- c(missing_ids, var_id)
      }

      q$variable <- var
      q
    })
    mod
  })

  if (length(missing_ids) > 0) {
    warning("The following variable_ids in modules were not found in the bank:\n  ",
            paste(unique(missing_ids), collapse = ", "), call. = FALSE)
  }

  mbank$modules <- linked_modules
  mbank
}

#' Print a qt_mbank Object
#'
#' Display a brief summary of a module bank object.
#'
#' @param x A \code{qt_mbank} object
#' @param ... Additional arguments (ignored)
#'
#' @return Invisibly returns the original object
#'
#' @export
print.qt_mbank <- function(x, ...) {
  surveys <- sort(unique(unlist(lapply(x$modules, `[[`, "surveys_used"))))
  cat("Module bank: ", x$meta$n_modules, " module(s)",
      " across ", length(surveys), " survey wave(s)\n", sep = "")
  cat("  Surveys: ", paste(surveys, collapse = ", "), "\n", sep = "")
  cat("  Source:  ", x$meta$source_path, "\n", sep = "")
  invisible(x)
}

#' Print a qt_module Object
#'
#' Display a brief summary of an individual module object.
#'
#' @param x A \code{qt_module} object
#' @param ... Additional arguments (ignored)
#'
#' @return Invisibly returns the original object
#'
#' @export
print.qt_module <- function(x, ...) {
  n_questions <- length(x$questions)
  cat(x$module_id, ": ", x$title, "\n", sep = "")
  cat("  Questions: ", n_questions, "\n", sep = "")
  cat("  Surveys:   ", paste(x$surveys_used, collapse = ", "), "\n", sep = "")
  invisible(x)
}
