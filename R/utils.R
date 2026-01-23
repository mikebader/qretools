# Internal Utility Functions for qretools
#
# These are helper functions used across multiple qretools functions.
# They are not exported and are prefixed with . to indicate internal use.

#' Parse YAML File with User-Friendly Error Messages
#'
#' Internal helper to read YAML files and provide clear error messages
#' for common YAML syntax issues.
#'
#' @param path Character string. Full path to YAML file.
#' @param file_description Character string. Description for error messages
#'   (e.g., "project-config.yml", "_qretools.yml")
#'
#' @return Parsed YAML as a list, or NULL if file doesn't exist
#'
#' @keywords internal
#' @noRd
.read_yaml_safe <- function(path, file_description = basename(path)) {
  if (!file.exists(path)) {
    return(NULL)
  }

  tryCatch(
    yaml::yaml.load_file(path),
    error = function(e) {
      stop("Error reading ", file_description, ":\n",
           "  File may have invalid YAML syntax.\n",
           "  Common issues: incorrect indentation, missing colons, unmatched quotes\n",
           "  Technical details: ", e$message,
           call. = FALSE)
    }
  )
}

# Resolve path from config or explicit path argument
#
# @param config_path_name Character string naming the path in config$paths
# @param path Character string or NULL - explicit path override
# @param config qt_config object
# @return Character string - resolved full path
.qt_resolve_path <- function(config_path_name, path, config) {
  # If explicit path provided, use it
  if (!is.null(path)) {
    return(path)
  }

  # Otherwise get from config
  if (is.null(config$paths[[config_path_name]])) {
    stop("No ", config_path_name, " path found in configuration", call. = FALSE)
  }

  # Construct full path from project root
  file.path(config$meta$project_root, config$paths[[config_path_name]])
}

# Read YAML file(s) from path (file or directory)
#
# Handles both single file and directory of YAML files.
# Adds source_file and file_position metadata to each item.
#
# @param source_path Character string - path to file (without .yml) or directory
# @param item_type Character string - type name for error messages (e.g., "questions", "value labels")
# @return List with components:
#   - items: Named list of items read from YAML
#   - source_files: Character vector of file paths read
#   - source_type: "file" or "directory"
#   - source_path: Original path provided
.qt_read_yaml_files <- function(source_path, item_type = "items") {
  # Check if it's a file or directory
  is_file <- file.exists(paste0(source_path, ".yml"))
  is_dir <- dir.exists(source_path)

  if (!is_file && !is_dir) {
    stop(item_type, " not found at: ", source_path,
         "\nLooked for: ", source_path, ".yml (file) or ",
         source_path, "/ (directory)", call. = FALSE)
  }

  all_items <- list()
  source_files <- character()

  # Helper to add metadata to items
  .add_metadata <- function(content, file_path) {
    for (i in seq_along(content)) {
      content[[i]]$source_file <- file_path
      content[[i]]$file_position <- i
    }
    content
  }

  if (is_file) {
    # Single file
    file_path <- paste0(source_path, ".yml")
    content <- .read_yaml_safe(file_path, basename(file_path))

    if (!is.null(content) && length(content) > 0) {
      all_items <- .add_metadata(content, file_path)
    }
    source_files <- file_path
    source_type <- "file"

  } else {
    # Directory - read all .yml files in alphabetical order
    yml_files <- list.files(source_path, pattern = "\\.yml$", full.names = TRUE)
    yml_files <- sort(yml_files)

    if (length(yml_files) == 0) {
      stop("No .yml files found in directory: ", source_path, call. = FALSE)
    }

    for (file_path in yml_files) {
      content <- .read_yaml_safe(file_path, basename(file_path))

      if (!is.null(content) && length(content) > 0) {
        content <- .add_metadata(content, file_path)
        all_items <- c(all_items, content)
      }
    }
    source_files <- yml_files
    source_type <- "directory"
  }

  # Validate: Check for empty result
  if (length(all_items) == 0) {
    stop("No ", item_type, " found in: ", source_path, call. = FALSE)
  }

  list(
    items = all_items,
    source_files = source_files,
    source_type = source_type,
    source_path = source_path
  )
}

# Internal helper: Validate question/variable structure
#
# Performs comprehensive validation of variables read from YAML.
# Checks required fields, types, conditional requirements, and cross-references.
#
# @param variables Named list of variables (from YAML)
# @param value_labels qt_vlabs object or NULL - for validating label references
# @param validate_value_labels Logical - whether to check value label references exist
# @return Invisible TRUE on success, stops with error on validation failure
.qt_validate_variables <- function(variables, value_labels = NULL, validate_value_labels = TRUE) {
  errors <- character()

  # 1. Check required fields
  required_fields <- c("variable_id", "title", "storage_type", "vargroup", "question_text", "surveys_used")

  for (variable_id in names(variables)) {
    var <- variables[[variable_id]]
    missing <- setdiff(required_fields, names(var))

    if (length(missing) > 0) {
      errors <- c(errors,
                  sprintf("Variable '%s': Missing required fields: %s",
                          variable_id, paste(missing, collapse = ", ")))
    }
  }

  # 2. Check valid types
  valid_types <- c("integer", "numeric", "factor", "character", "composite", "multiple_response")

  for (variable_id in names(variables)) {
    var <- variables[[variable_id]]
    if (!is.null(var$storage_type) && !var$storage_type %in% valid_types) {
      errors <- c(errors,
                  sprintf("Variable '%s': Invalid type '%s'. Must be one of: %s",
                          variable_id, var$storage_type, paste(valid_types, collapse = ", ")))
    }
  }

  # 3. Conditional requirements
  for (variable_id in names(variables)) {
    var <- variables[[variable_id]]

    # Factor must have value_labels_name
    if (!is.null(var$storage_type) && var$storage_type == "factor" && is.null(var$value_labels_name)) {
      errors <- c(errors,
                  sprintf("Variable '%s': storage_type='factor' requires value_labels_name", variable_id))
    }

    # Restricted must have reason
    if (isTRUE(var$restricted_access) && is.null(var$restriction_reason)) {
      errors <- c(errors,
                  sprintf("Variable '%s': restricted_access=true requires restriction_reason", variable_id))
    }

    # creates_variables requires variable_parts
    if (!is.null(var$creates_variables)) {
      if (is.null(var$variable_parts)) {
        errors <- c(errors,
                    sprintf("Variable '%s': creates_variables specified but variable_parts missing", variable_id))
      } else {
        # Check all created variables are defined
        created <- var$creates_variables
        defined <- names(var$variable_parts)
        missing <- setdiff(created, defined)

        if (length(missing) > 0) {
          errors <- c(errors,
                      sprintf("Variable '%s': Variables in creates_variables not defined in variable_parts: %s",
                              variable_id, paste(missing, collapse = ", ")))
        }
      }
    }
  }

  # 4. Check for duplicate variable_ids (should already be caught, but double-check)
  if (any(duplicated(names(variables)))) {
    dups <- names(variables)[duplicated(names(variables))]
    errors <- c(errors,
                sprintf("Duplicate variable names found: %s", paste(unique(dups), collapse = ", ")))
  }

  # 5. Version consistency (if versions present)
  for (variable_id in names(variables)) {
    var <- variables[[variable_id]]

    if (!is.null(var$versions)) {
      version_years <- unlist(lapply(var$versions, function(v) v$years))

      # Check for overlapping years
      if (any(duplicated(version_years))) {
        errors <- c(errors,
                    sprintf("Variable '%s': Overlapping years in versions", variable_id))
      }

      # Check version years are subset of years_used
      if (!all(version_years %in% var$years_used)) {
        errors <- c(errors,
                    sprintf("Variable '%s': Version years must be subset of years_used", variable_id))
      }

      # Check all years_used are covered by versions
      if (!all(var$years_used %in% version_years)) {
        errors <- c(errors,
                    sprintf("Variable '%s': Not all years_used are covered by versions", variable_id))
      }
    }
  }

  # 6. Value labels exist (if provided)
  if (validate_value_labels && !is.null(value_labels)) {
    missing_labels <- character()

    for (variable_id in names(variables)) {
      var <- variables[[variable_id]]

      if (!is.null(var$value_labels_name)) {
        label_name <- var$value_labels_name

        if (!label_name %in% names(value_labels$labels)) {
          missing_labels <- c(missing_labels,
                              sprintf("  Variable '%s' references '%s'", variable_id, label_name))
        }
      }
    }

    if (length(missing_labels) > 0) {
      errors <- c(errors,
                  "Value label sets not found:",
                  missing_labels)
    }
  }

  # If any errors, stop with combined message
  if (length(errors) > 0) {
    stop("Validation errors:\n", paste(errors, collapse = "\n"), call. = FALSE)
  }

  invisible(TRUE)
}

# Internal helper: Build variable structure with indices
#
# Takes validated variables and builds the final structured object with metadata and indices.
# Creates indices for different sorting/access patterns.
#
# @param variables Named list of variables
# @param source_files Character vector of file paths read
# @param source_path Character string - original path
# @param source_type Character string - "file" or "directory"
# @param class_name Character vector - class names to assign
# @return Structured list with variables and metadata
.qt_build_variable_structure <- function(variables, source_files, source_path, source_type, class_name = "qt_variables") {
  # Build file order (basenames only)
  file_order <- basename(source_files)
  if (source_type == "directory") {
    file_order <- unique(file_order)  # Remove duplicates, keep order
  }

  # Build indices
  variable_ids <- names(variables)

  # by_variable_id: alphabetical
  by_variable_id <- sort(variable_ids)

  # by_file: group by source file
  by_file <- list()
  for (variable_id in variable_ids) {
    file <- basename(variables[[variable_id]]$source_file)
    if (is.null(by_file[[file]])) {
      by_file[[file]] <- character()
    }
    by_file[[file]] <- c(by_file[[file]], variable_id)
  }

  # by_file_position: sort by file order, then by position within file
  by_file_position <- character()
  for (file in file_order) {
    if (!is.null(by_file[[file]])) {
      # Sort by file_position within this file
      vars_in_file <- by_file[[file]]
      positions <- sapply(vars_in_file, function(v) variables[[v]]$file_position)
      vars_in_file <- vars_in_file[order(positions)]
      by_file_position <- c(by_file_position, vars_in_file)
    }
  }

  # Build final structure
  structure(
    list(
      variables = variables,
      meta = list(
        source_path = source_path,
        source_type = source_type,
        source_files = source_files,
        file_order = file_order,
        n_variables = length(variables),
        read_time = Sys.time(),
        indices = list(
          by_variable_id = by_variable_id,
          by_file = by_file,
          by_file_position = by_file_position
        )
      )
    ),
    class = class_name
  )
}
