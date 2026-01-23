#' Reader Functions to Load Information
#'
#'
# Internal helper: Read, validate, and build variable bank structure
#
# Common workflow for reading any variable bank (questions, generated, control)
#
# @param config_path_name Character string - name in config$paths
# @param path Character string or NULL - explicit path override
# @param config qt_config object
# @param item_type Character string - for error messages
# @param class_name Character vector - class names to assign
# @param validate_value_labels Logical - whether to validate label references
# @return Structured variable bank object
.qt_read_variable_bank <- function(config_path_name, path, config,
                                   item_type = "variables",
                                   class_name = c("qt_bank"),
                                   validate_value_labels = TRUE) {

  # 1. Resolve path
  source_path <- .qt_resolve_path(config_path_name, path, config)

  # 2. Read YAML file(s)
  result <- .qt_read_yaml_files(source_path, item_type)

  # 2.5 Add varname from dictionary key to each item
  for (variable_id in names(result$items)) {
    result$items[[variable_id]]$variable_id <- variable_id
  }

  # 3. Load value labels for validation (if needed)
  value_labels <- if (validate_value_labels) {
    qt_read_value_labels(config)
  } else {
    NULL
  }

  # 4. Validate variables
  .qt_validate_variables(result$items, value_labels, validate_value_labels)

  # 4.5. Wrap each variable in appropriate class based on bank type
  bank_type_map <- list(
    "questions" = "qt_make_qvar",
    "generated variables" = "qt_make_genvar",
    "control parameters" = "qt_make_ctrlvar"
  )

  constructor_name <- bank_type_map[[item_type]]
  if (is.null(constructor_name)) {
    stop("Unknown item_type: ", item_type, call. = FALSE)
  }

  constructor_fn <- get(constructor_name)

  vars_with_classes <- lapply(result$items, function(var_data) {
    constructor_fn(var_data)
  })
  names(vars_with_classes) <- sapply(result$items, function(x) x$variable_id)

  # Use the typed variables for the rest of the function
  result$items <- vars_with_classes

  # 5. Build structure with indices
  .qt_build_variable_structure(
    variables = result$items,
    source_files = result$source_files,
    source_path = result$source_path,
    source_type = result$source_type,
    class_name = class_name
  )
}
#' Read Variable Banks
#'
#' Read variables from the different variable banks (questions, generated, control).
#' All functions return structured objects with variables and metadata.
#'
#' @param config A qt_config object (default: \code{qt_config()}). Configuration
#'   specifying project paths and settings.
#' @param path Character string or NULL. Explicit path to override the config
#'   path. Primarily for testing. If NULL (default), uses path from config.
#'
#' @return An S3 object of class \code{qt_qbank}, \code{qt_genbank}, or
#'   \code{qt_ctrlbank} (all inherit from \code{qt_bank}). Contains:
#'   \describe{
#'     \item{variables}{Named list of variables, indexed by variable_id}
#'     \item{meta}{Metadata including source files, read time, and indices}
#'   }
#'
#' @details
#' These functions read variable definitions from YAML files in the project's
#' bank directories. Each function reads from a different bank:
#'
#' \itemize{
#'   \item{\code{qt_read_question_bank()} / \code{qt_qbank()}: Fielded survey questions}
#'   \item{\code{qt_read_generated_variables()} / \code{qt_genbank()}: Constructed variables}
#'   \item{\code{qt_read_control_parameters()} / \code{qt_ctrlbank()}: Control parameters}
#' }
#'
#' **File/Directory Flexibility:**
#' Each path can be either a single YAML file or a directory containing multiple
#' YAML files. If a directory, all \code{.yml} files are read and combined.
#'
#' **Validation:**
#' All variables are validated on read:
#' \itemize{
#'   \item{Required fields must be present}
#'   \item{Field types must be correct}
#'   \item{Value label references must exist (if type is "factor")}
#'   \item{Conditional requirements met (e.g., factor variables need value labels)}
#' }
#'
#' **Returned Structure:**
#' Variables are returned in a named list indexed by \code{variable_id}, with metadata
#' providing multiple access patterns:
#' \itemize{
#'   \item{\code{$meta$indices$by_varname}: Alphabetical order}
#'   \item{\code{$meta$indices$by_file}: Grouped by source file}
#'   \item{\code{$meta$indices$by_file_position}: File order, then position within file}
#' }
#'
#' @section Default Paths:
#' Default paths (can be overridden in \code{project-config.yml} or \code{_qretools.yml}):
#' \itemize{
#'   \item{Questions: \code{banks/questions}}
#'   \item{Generated: \code{banks/generated}}
#'   \item{Control: \code{banks/control}}
#' }
#'
#' @examples
#' \dontrun{
#' # Read question bank
#' qbank <- qt_qbank()
#'
#' # Access specific question
#' qbank$variables$nhd_sat
#'
#' # List all questions alphabetically
#' qbank$meta$indices$by_variable_id
#'
#' # Read generated variables
#' genbank <- qt_genbank()
#'
#' # Read control parameters
#' ctrlbank <- qt_ctrlbank()
#'
#' # Use explicit path for testing
#' qbank <- qt_read_question_bank(path = "test-data/questions")
#' }
#'
#' @name read_banks
#' @export
NULL

#' @rdname read_banks
#' @export
qt_read_question_bank <- function(config = qt_config(), path = NULL) {
  .qt_read_variable_bank(
    config_path_name = "question_bank",
    path = path,
    config = config,
    item_type = "questions",
    class_name = c("qt_qbank", "qt_bank"),
    validate_value_labels = TRUE
  )
}

#' @rdname read_banks
#' @export
qt_qbank <- qt_read_question_bank

#' @rdname read_banks
#' @export
qt_read_generated_variables <- function(config = qt_config(), path = NULL) {
  .qt_read_variable_bank(
    config_path_name = "generated_bank",
    path = path,
    config = config,
    item_type = "generated variables",
    class_name = c("qt_genbank", "qt_bank"),
    validate_value_labels = TRUE
  )
}

#' @rdname read_banks
#' @export
qt_genbank <- qt_read_generated_variables

#' @rdname read_banks
#' @export
qt_read_control_parameters <- function(config = qt_config(), path = NULL) {
  .qt_read_variable_bank(
    config_path_name = "control_bank",
    path = path,
    config = config,
    item_type = "control parameters",
    class_name = c("qt_ctrlbank", "qt_bank"),
    validate_value_labels = TRUE
  )
}

#' @rdname read_banks
#' @export
qt_ctrlbank <- qt_read_control_parameters

#' Read Value Labels from YAML
#'
#' Read value label definitions from YAML file(s) and return a validated
#' qt_value_labels object. Value labels can be stored in a single file or
#' split across multiple files in a directory.
#'
#' @param config A qt_config object. Default calls qt_config() which will
#'   use cached configuration if available.
#' @param path Character string. Explicit path to value labels file or directory.
#'   If provided, overrides the path from config. Useful for testing or reading
#'   value labels outside the standard project structure.
#'
#' @return S3 object of class \code{qt_value_labels} containing:
#'   \describe{
#'     \item{labels}{Named list of value label sets, each containing values,
#'       labels, and source_file}
#'     \item{meta}{Metadata about source files, count, and read time}
#'   }
#'
#' @details
#' **File discovery:**
#' \enumerate{
#'   \item{If \code{path} is provided, uses that location}
#'   \item{Otherwise, uses \code{config$paths$value_labels}}
#'   \item{Checks for single file first (e.g., value-labels.yml)}
#'   \item{If not found, checks for directory (e.g., value-labels/)}
#'   \item{If directory, reads all .yml files and merges them}
#' }
#'
#' **Validation:**
#' \itemize{
#'   \item{Each label set must have required fields (id/name and labels)}
#'   \item{Values must be unique within each label set}
#'   \item{Label set IDs must be unique across all files}
#' }
#'
#' **Metadata tracking:**
#' Each label set includes \code{source_file} showing which file it came from.
#' This helps users locate and edit specific label definitions.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Use default configuration
#' vl <- qt_read_value_labels()
#'
#' # Access a specific label set
#' vl$labels$satisfied5
#'
#' # See where it came from
#' vl$labels$satisfied5$source_file
#'
#' # Use explicit path for testing
#' vl <- qt_read_value_labels(path = "test-labels.yml")
#' }
qt_read_value_labels <- function(config = qt_config(), path = NULL) {

  # 1. Resolve path
  source_path <- .qt_resolve_path("value_labels", path, config)

  # 2. Read YAML file(s)
  result <- .qt_read_yaml_files(source_path, "value labels")

  # 3. Validate and process each label set
  all_labels <- result$items

  for (label_id in names(all_labels)) {
    label_set <- all_labels[[label_id]]

    # Check for required 'labels' field
    if (is.null(label_set$labels)) {
      stop("Value label set '", label_id, "' missing required 'labels' field",
           "\nSource: ", label_set$source_file, call. = FALSE)
    }

    # Get values and labels
    values <- names(label_set$labels)
    labels <- unlist(label_set$labels)

    if (length(values) == 0) {
      stop("Value label set '", label_id, "' has no labels defined",
           "\nSource: ", label_set$source_file, call. = FALSE)
    }

    # Check for duplicate values
    if (any(duplicated(values))) {
      dups <- values[duplicated(values)]
      stop("Value label set '", label_id, "' has duplicate values: ",
           paste(dups, collapse = ", "),
           "\nSource: ", label_set$source_file, call. = FALSE)
    }

    # Store in standardized format
    all_labels[[label_id]]$values <- values
    all_labels[[label_id]]$labels <- labels
  }

  # 4. Validate: Check for duplicate label set IDs
  label_ids <- names(all_labels)
  if (any(duplicated(label_ids))) {
    dup_ids <- unique(label_ids[duplicated(label_ids)])

    # Find which files have the duplicates
    dup_sources <- lapply(dup_ids, function(id) {
      indices <- which(label_ids == id)
      sapply(indices, function(i) all_labels[[i]]$source_file)
    })

    error_msg <- paste0(
      "Duplicate value label IDs found:\n",
      paste(sapply(seq_along(dup_ids), function(i) {
        paste0("  '", dup_ids[i], "' in:\n    - ",
               paste(dup_sources[[i]], collapse = "\n    - "))
      }), collapse = "\n"),
      "\nLabel IDs must be unique across all files."
    )
    stop(error_msg, call. = FALSE)
  }

  # 5. Build metadata
  meta <- list(
    source_path = result$source_path,
    source_type = result$source_type,
    source_files = result$source_files,
    n_labels = length(all_labels),
    read_time = Sys.time()
  )

  # 6. Create S3 object
  structure(
    list(
      labels = all_labels,
      meta = meta
    ),
    class = "qt_vlabs"
  )
}

# Alias
qt_vlabs <- qt_read_value_labels

#' Print qretools Value Labels
#'
#' Print a brief summary of value labels object.
#'
#' @param x A qt_value_labels object
#' @param ... Additional arguments (ignored)
#'
#' @return Invisibly returns the original object
#'
#' @export
print.qt_value_labels <- function(x, ...) {
  # Get project name if available (would need config passed, or skip)
  cat("Value Labels\n")
  cat(strrep("=", 50), "\n\n", sep = "")

  cat("Label sets:", x$meta$n_labels, "\n")
  cat("Files:\n")
  for (f in x$meta$source_files) {
    cat("  ", f, "\n", sep = "")
  }

  cat("\n")
  invisible(x)
}

