# Package environment to track warnings shown this session
.qretools_warned <- new.env(parent = emptyenv())

# Session cache manager (in-memory, per R session)
.session_cache <- local({
  cache_env <- new.env(parent = emptyenv())

  list(
    get = function(cache_key, root) {
      # Check if cache entry exists
      if (!exists(cache_key, envir = cache_env)) {
        return(NULL)
      }

      entry <- get(cache_key, envir = cache_env)

      # Check if config files have been modified since cache
      qretools_path <- file.path(root, "_qretools.yml")
      project_config_path <- file.path(root, "project-config.yml")

      # Get current modification times
      current_qretools_mtime <- if (file.exists(qretools_path)) {
        file.info(qretools_path)$mtime
      } else {
        NA
      }

      current_project_mtime <- if (file.exists(project_config_path)) {
        file.info(project_config_path)$mtime
      } else {
        NA
      }

      # Compare with cached times
      if (!identical(entry$qretools_mtime, current_qretools_mtime) ||
          !identical(entry$project_config_mtime, current_project_mtime)) {
        # Files changed - invalidate cache
        rm(list = cache_key, envir = cache_env)
        return(NULL)
      }

      # Cache is valid - return config
      return(entry$config)
    },

    set = function(cache_key, config) {
      # Get file modification times
      root <- config$meta$project_root
      qretools_path <- file.path(root, "_qretools.yml")
      project_config_path <- file.path(root, "project-config.yml")

      qretools_mtime <- if (file.exists(qretools_path)) {
        file.info(qretools_path)$mtime
      } else {
        NA
      }

      project_config_mtime <- if (file.exists(project_config_path)) {
        file.info(project_config_path)$mtime
      } else {
        NA
      }

      # Store config with file times
      entry <- list(
        config = config,
        qretools_mtime = qretools_mtime,
        project_config_mtime = project_config_mtime
      )

      assign(cache_key, entry, envir = cache_env)
      invisible(TRUE)
    },

    clear = function() {
      rm(list = ls(envir = cache_env), envir = cache_env)
      invisible(TRUE)
    }
  )
})


# Persistent cache manager (disk-based, survives R sessions)
.persistent_cache <- local({

  # Helper to get cache directory
  get_cache_dir <- function() {
    cache_dir <- tools::R_user_dir("qretools", "cache")
    if (!dir.exists(cache_dir)) {
      dir.create(cache_dir, recursive = TRUE)
    }
    return(cache_dir)
  }

  # Helper to get cache file path
  get_cache_file <- function(cache_key) {
    file.path(get_cache_dir(), paste0(cache_key, ".rds"))
  }

  list(
    get = function(cache_key, root) {
      cache_file <- get_cache_file(cache_key)

      # Check if cache file exists
      if (!file.exists(cache_file)) {
        return(NULL)
      }

      # Read cache entry
      entry <- tryCatch(
        readRDS(cache_file),
        error = function(e) {
          # Cache file corrupted - remove it
          unlink(cache_file)
          return(NULL)
        }
      )

      if (is.null(entry)) return(NULL)

      # Check if config files have been modified since cache
      qretools_path <- file.path(root, "_qretools.yml")
      project_config_path <- file.path(root, "project-config.yml")

      # Get current modification times
      current_qretools_mtime <- if (file.exists(qretools_path)) {
        file.info(qretools_path)$mtime
      } else {
        NA
      }

      current_project_mtime <- if (file.exists(project_config_path)) {
        file.info(project_config_path)$mtime
      } else {
        NA
      }

      # Compare with cached times
      if (!identical(entry$qretools_mtime, current_qretools_mtime) ||
          !identical(entry$project_config_mtime, current_project_mtime)) {
        # Files changed - invalidate cache
        unlink(cache_file)
        return(NULL)
      }

      # Cache is valid - return config
      return(entry$config)
    },

    set = function(cache_key, config, root) {
      # Get file modification times
      qretools_path <- file.path(root, "_qretools.yml")
      project_config_path <- file.path(root, "project-config.yml")

      qretools_mtime <- if (file.exists(qretools_path)) {
        file.info(qretools_path)$mtime
      } else {
        NA
      }

      project_config_mtime <- if (file.exists(project_config_path)) {
        file.info(project_config_path)$mtime
      } else {
        NA
      }

      # Store config with file times
      entry <- list(
        config = config,
        qretools_mtime = qretools_mtime,
        project_config_mtime = project_config_mtime
      )

      cache_file <- get_cache_file(cache_key)
      saveRDS(entry, cache_file)
      invisible(TRUE)
    },

    clear = function() {
      cache_dir <- get_cache_dir()
      cache_files <- list.files(cache_dir, pattern = "\\.rds$", full.names = TRUE)
      unlink(cache_files)
      invisible(TRUE)
    }
  )
})

#' Get qretools Package Default Configuration
#'
#' Internal function that returns the default configuration values for qretools.
#' These defaults are used when no _qretools.yml file is present or to fill in
#' missing values from a partial configuration file.
#'
#' @return A named list with default configuration values containing:
#'   \describe{
#'     \item{paths}{Default file/directory paths for project resources}
#'     \item{dirnames}{Standard directory names within survey instances}
#'     \item{settings}{Default behavior settings}
#'   }
#'
#' @details
#' Path values are base names without extensions or trailing slashes.
#' Read functions will check for both .yml files and directories with the same name,
#' except for project_config which must be a .yml file.
#'
#' @keywords internal
#' @noRd
.qt_defaults <- function() {
  list(
    paths = list(
      project_config = "project-config.yml",
      question_bank = "banks/questions",
      generated_bank = "banks/generated",
      control_bank = "banks/control",
      value_labels = "value-labels",
      surveys = "surveys"
    ),
    dirnames = list(
      design = "design",
      candidates = "candidates",
      questionnaires = "questionnaires"
    ),
    settings = list(
      validation_strictness = "strict"
    )
  )
}

#' Get and Normalize Project Root Directory
#'
#' Internal function to get the project root directory, either from an explicit
#' argument or by using the current working directory. Normalizes the path for
#' cross-platform consistency.
#'
#' @param project_root Character string or NULL. If NULL, uses current working
#'   directory. If provided, must be an existing directory.
#'
#' @return Character string with normalized absolute path to project root
#'
#' @keywords internal
#' @noRd
.get_project_root <- function(project_root = NULL) {
  if (is.null(project_root)) {
    root <- getwd()
  } else {
    root <- project_root
  }

  # Normalize path for consistency across platforms
  normalized <- normalizePath(root, winslash = "/", mustWork = TRUE)

  return(normalized)
}

#' Read and Validate project-config.yml
#'
#' Internal function to read the project-config.yml file and validate that
#' required fields are present for DDI compliance.
#'
#' @param path Character string. Full path to project-config.yml file.
#'
#' @return Named list with project metadata, or NULL if file doesn't exist.
#'   Returns an empty list if file exists but is empty.
#'
#' @details
#' Required fields (will error if missing):
#' \itemize{
#'   \item{title - Study title}
#'   \item{organization - Collecting organization}
#'   \item{principal_investigator - PI name (string or list)}
#' }
#'
#' Principal investigator can be:
#' \itemize{
#'   \item{Character string: "Name"}
#'   \item{List: name (required), affiliation (optional), email (optional)}
#'   \item{List of lists: Multiple PIs, each with above structure}
#' }
#'
#' Recommended fields (will warn if missing, once per session):
#' \itemize{
#'   \item{irb$institution - IRB institution name}
#'   \item{irb$protocol_number - IRB protocol number}
#' }
#'
#' @keywords internal
#' @noRd
.read_project_config <- function(path) {
  # Check if file exists
  if (!file.exists(path)) {
    return(NULL)
  }

  # Read YAML file
  config <- .read_yaml_safe(path, "project-config.yml")

  # Handle empty file
  if (is.null(config) || length(config) == 0) {
    return(list())
  }

  # Validate it's a list/map
  if (!is.list(config)) {
    stop("Invalid format in project-config.yml: expected YAML map/dictionary",
         call. = FALSE)
  }

  # Check required fields (look under 'project' section)
  if (is.null(config$project)) {
    stop("project-config.yml must have a 'project:' section with required fields",
         call. = FALSE)
  }

  required_fields <- c("title", "organization", "principal_investigator")
  missing_required <- setdiff(required_fields, names(config$project))

  if (length(missing_required) > 0) {
    stop("project-config.yml missing required fields:\n  - ",
         paste(missing_required, collapse = "\n  - "),
         "\nAdd these fields or create a new project with qt_create_project()",
         call. = FALSE)
  }

  # Validate principal_investigator structure
  pi_info <- config$project$principal_investigator

  if (is.character(pi_info)) {
    # Simple string format - convert to structured format
    config$project$principal_investigator <- list(
      name = pi_info,
      affiliation = NULL,
      email = NULL
    )
  } else if (is.list(pi_info)) {
    # Check if it's a list of PIs or single PI
    if (!is.null(names(pi_info))) {
      # Single PI with structured fields
      if (is.null(pi_info$name)) {
        stop("principal_investigator must have 'name' field", call. = FALSE)
      }
      if (!is.character(pi_info$name)) {
        stop("principal_investigator$name must be a character string",
             call. = FALSE)
      }
      # Set defaults for optional fields
      if (is.null(pi_info$affiliation)) {
        config$project$principal_investigator$affiliation <- NULL
      }
      if (is.null(pi_info$email)) {
        config$project$principal_investigator$email <- NULL
      }

    } else {
      # Multiple PIs - validate each
      for (i in seq_along(pi_info)) {
        if (is.null(pi_info[[i]]$name)) {
          stop("principal_investigator[", i, "] must have 'name' field",
               call. = FALSE)
        }
        if (!is.character(pi_info[[i]]$name)) {
          stop("principal_investigator[", i, "]$name must be a character string",
               call. = FALSE)
        }
        # Set defaults for optional fields
        if (is.null(pi_info[[i]]$affiliation)) {
          config$project$principal_investigator[[i]]$affiliation <- NULL
        }
        if (is.null(pi_info[[i]]$email)) {
          config$project$principal_investigator[[i]]$email <- NULL
        }
      }
    }
  } else {
    stop("principal_investigator must be a character string or list",
         call. = FALSE)
  }

  # Check recommended fields (warnings only, once per session)
  if (is.null(config$irb$institution) &&
      is.null(.qretools_warned$irb_institution)) {
    warning("project-config.yml missing recommended field 'irb$institution' ",
            "(needed for DDI compliance)", call. = FALSE)
    .qretools_warned$irb_institution <- TRUE
  }

  if (is.null(config$irb$protocol_number) &&
      is.null(.qretools_warned$irb_protocol_number)) {
    warning("project-config.yml missing recommended field 'irb$protocol_number' ",
            "(needed for DDI compliance)", call. = FALSE)
    .qretools_warned$irb_protocol_number <- TRUE
  }

  return(config)
}

#' Merge Configuration from Multiple Sources
#'
#' Internal function to merge configuration values from package defaults,
#' _qretools.yml file, and function argument overrides. Uses proper precedence
#' with later sources overriding earlier ones.
#'
#' @param defaults List from .qt_defaults() containing package default values
#' @param qretools_config List from reading _qretools.yml, or NULL if file
#'   doesn't exist
#' @param project_config List from .read_project_config() containing project
#'   metadata
#' @param overrides Named list from ... arguments in qt_config(), or NULL
#'
#' @return Merged configuration list with structure:
#'   \describe{
#'     \item{paths}{File/directory paths (merged)}
#'     \item{dirnames}{Standard directory names (merged)}
#'     \item{settings}{Behavior settings (merged)}
#'     \item{project}{Project metadata (from project_config only, not merged)}
#'   }
#'
#' @details
#' Merge precedence (highest to lowest):
#' \enumerate{
#'   \item{Function argument overrides}
#'   \item{_qretools.yml configuration}
#'   \item{Package defaults}
#' }
#'
#' Project metadata is not merged - it comes only from project-config.yml.
#' NULL values are skipped during merging (won't override existing values).
#'
#' @keywords internal
#' @noRd
.merge_configs <- function(defaults, qretools_config = NULL,
                           project_config = NULL, overrides = NULL) {

  # Start with defaults
  result <- defaults

  # Helper function for deep merging lists
  merge_lists <- function(base, overlay) {
    if (is.null(overlay)) return(base)
    if (is.null(base)) return(overlay)

    for (name in names(overlay)) {
      if (is.list(overlay[[name]]) && is.list(base[[name]])) {
        # Recursively merge nested lists
        base[[name]] <- merge_lists(base[[name]], overlay[[name]])
      } else {
        # Direct override for non-list values
        base[[name]] <- overlay[[name]]
      }
    }
    return(base)
  }

  # Merge _qretools.yml over defaults
  if (!is.null(qretools_config)) {
    result <- merge_lists(result, qretools_config)
  }

  # Merge overrides over everything
  if (!is.null(overrides)) {
    result <- merge_lists(result, overrides)
  }

  # Add project metadata as separate section (not merged)
  if (!is.null(project_config)) {
    result$project <- project_config
  }

  return(result)
}

#' Clear Cached qretools Configurations
#'
#' Remove cached configuration objects from memory (session cache) and/or
#' disk (persistent cache). Useful when configuration files have been modified
#' and you want to force a fresh read.
#'
#' @param persistent Logical. If TRUE, also clear persistent disk cache.
#'   If FALSE (default), only clear session cache.
#'
#' @return Invisibly returns TRUE if cache was cleared, FALSE if no cache existed.
#'
#' @details
#' Session cache is stored in memory and cleared automatically when R restarts.
#' Persistent cache is stored in the user's cache directory and survives R restarts.
#'
#' Cache is automatically invalidated when configuration files are modified,
#' so manual clearing is rarely needed. However, this function is useful if:
#' \itemize{
#'   \item{You want to force a fresh read for debugging}
#'   \item{You've made changes that don't update file modification times}
#'   \item{You want to reclaim disk space from persistent cache}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Clear session cache only
#' qt_clear_cache()
#'
#' # Clear both session and persistent cache
#' qt_clear_cache(persistent = TRUE)
#' }
qt_clear_cache <- function(persistent = FALSE) {

  session_cleared <- FALSE
  persistent_cleared <- FALSE

  # Clear session cache
  .session_cache$clear()
  session_cleared <- TRUE
  message("Session cache cleared")

  # Clear persistent cache if requested
  if (persistent) {
    .persistent_cache$clear()
    persistent_cleared <- TRUE

    cache_dir <- tools::R_user_dir("qretools", "cache")
    message("Persistent cache cleared from: ", cache_dir)
  }

  invisible(session_cleared || persistent_cleared)
}

#' Get qretools Configuration
#'
#' Read and merge configuration from project-config.yml and _qretools.yml,
#' returning a validated configuration object. This is the main entry point
#' for accessing qretools configuration.
#'
#' @param project_root Character string or NULL. Path to project root directory
#'   containing project-config.yml and _qretools.yml. If NULL (default), uses
#'   current working directory.
#'
#' @param cache Character string. Caching behavior: "none" (default, no caching),
#'   "session" (cache in memory for current R session), or "persist" (cache to
#'   disk, survives R restarts).
#'
#' @param ... Named arguments to override specific configuration settings.
#'   These are temporary overrides primarily for testing/debugging.
#'   **Recommended**: Edit YAML files instead for permanent changes.
#'
#' @return S3 object of class \code{qt_config} containing:
#'   \describe{
#'     \item{paths}{File and directory paths for project resources}
#'     \item{dirnames}{Standard directory names within survey instances}
#'     \item{settings}{Behavior settings (e.g., validation strictness)}
#'     \item{project}{Project metadata (title, organization, PI, IRB info)}
#'     \item{meta}{Configuration metadata (file paths, cache info, overrides)}
#'   }
#'
#' @details
#' Configuration sources are merged with the following precedence (highest to lowest):
#' \enumerate{
#'   \item{Function argument overrides (\code{...})}
#'   \item{_qretools.yml (project-specific configuration)}
#'   \item{Package defaults}
#' }
#'
#' Project metadata from project-config.yml is added separately and not merged.
#'
#' **Caching behavior:**
#' \itemize{
#'   \item{\code{cache = "none"}: Always read files fresh (default)}
#'   \item{\code{cache = "session"}: Cache in memory, cleared when R restarts}
#'   \item{\code{cache = "persist"}: Cache to disk, survives R restarts}
#' }
#'
#' Cache is automatically invalidated when configuration files are modified.
#'
#' **Overrides:** Use function arguments to temporarily override configuration
#' values (e.g., \code{qt_config(surveys_path = "instances/")}). However, for
#' permanent changes, editing YAML files is strongly recommended as it makes
#' configuration explicit and version-controllable.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic usage (discovers project automatically)
#' config <- qt_config()
#'
#' # Explicit project path
#' config <- qt_config(project_root = "~/my-project")
#'
#' # With session caching
#' config <- qt_config(cache = "session")
#'
#' # Temporary override (for testing)
#' config <- qt_config(surveys_path = "instances/")
#'
#' # Access configuration
#' config$paths$question_bank
#' config$project$title
#' }
qt_config <- function(project_root = NULL,
                      cache = c("none", "session", "persist"),
                      ...) {

  # Block 1: Validate parameters
  cache <- match.arg(cache)

  # Block 2: Get project root
  root <- .get_project_root(project_root)

  # Block 3: Check cache
  if (cache != "none") {
    cache_key <- digest::digest(root)

    # Try session cache first
    cached <- .session_cache$get(cache_key, root)
    if (!is.null(cached)) {
      return(cached)
    }

    # Try persistent cache if requested
    if (cache == "persist") {
      cached <- .persistent_cache$get(cache_key, root)
      if (!is.null(cached)) {
        # Also store in session cache for faster access
        .session_cache$set(cache_key, cached)
        return(cached)
      }
    }
  }

  # Block 4: Read _qretools.yml (optional)
  qretools_path <- file.path(root, "_qretools.yml")

  if (file.exists(qretools_path)) {
    qretools_config <- .read_yaml_safe(qretools_path, "_qretools.yml")

    # Validate it's a list
    if (!is.null(qretools_config) && !is.list(qretools_config)) {
      stop("Invalid format in _qretools.yml: expected YAML map/dictionary",
           call. = FALSE)
    }
  } else {
    # No _qretools.yml file - use defaults
    if (is.null(.qretools_warned$no_qretools_yml)) {
      message("No _qretools.yml found. Using default configuration.")
      .qretools_warned$no_qretools_yml <- TRUE
    }
    qretools_config <- NULL
  }

  # Block 5: Read project-config.yml (required)
  project_config_path <- file.path(root, "project-config.yml")
  project_config <- .read_project_config(project_config_path)

  if (is.null(project_config)) {
    stop("No project-config.yml found in: ", root, "\n",
         "Use qt_create_project() to initialize a qretools project.",
         call. = FALSE)
  }

  if (length(project_config) == 0) {
    stop("project-config.yml is empty in: ", root, "\n",
         "Add required fields or use qt_create_project() to initialize.",
         call. = FALSE)
  }

  # Block 6: Process function overrides
  overrides <- list(...)
  if (length(overrides) == 0) {
    overrides <- NULL
  }

  # Block 7: Merge all configs
  defaults <- .qt_defaults()
  merged_config <- .merge_configs(
    defaults = defaults,
    qretools_config = qretools_config,
    project_config = project_config,
    overrides = overrides
  )

  # Block 8: Build metadata section
  merged_config$meta <- list(
    project_root = root,
    qretools_config_path = if (file.exists(qretools_path)) qretools_path else NA,
    project_config_path = project_config_path,
    cached = FALSE,
    cache_time = Sys.time(),
    cache_type = cache,
    overrides = overrides
  )

  # Block 9: Create S3 object
  config <- structure(
    merged_config,
    class = "qt_config"
  )

  # Block 10: Cache the result (if enabled)
  if (cache == "session") {
    cache_key <- digest::digest(root)
    .session_cache$set(cache_key, config)
    config$meta$cached <- TRUE

  } else if (cache == "persist") {
    cache_key <- digest::digest(root)
    .session_cache$set(cache_key, config)
    .persistent_cache$set(cache_key, config, root)
    config$meta$cached <- TRUE
  }

  # Block 11: Return config
  config
}

#' Print qretools Configuration
#'
#' Print a brief summary of a qretools configuration object.
#'
#' @param x A qt_config object
#' @param ... Additional arguments (ignored)
#'
#' @return Invisibly returns the original object
#'
#' @export
print.qt_config <- function(x, ...) {
  cat("qretools Configuration\n")
  cat(strrep("=", 50), "\n\n", sep = "")

  # Project title
  if (!is.null(x$project$title)) {
    cat("Project:", x$project$title, "\n")
  }

  # Project root
  cat("Root:   ", x$meta$project_root, "\n")

  # Cache status
  if (x$meta$cached) {
    cat("Cache:  ", x$meta$cache_type,
        " (", format(x$meta$cache_time, "%Y-%m-%d %H:%M:%S"), ")\n", sep = "")
  } else {
    cat("Cache:   none\n")
  }

  cat("\n")
  invisible(x)
}

#' Summarize qretools Configuration
#'
#' Print a detailed summary of a qretools configuration object.
#'
#' @param object A qt_config object
#' @param ... Additional arguments (ignored)
#'
#' @return Invisibly returns the original object
#'
#' @export
summary.qt_config <- function(object, ...) {
  cat("qretools Configuration Summary\n")
  cat(strrep("=", 50), "\n\n")

  # Project Information
  cat("Project Information:\n")
  cat("  Title:       ", object$project$title, "\n", sep = "")
  cat("  Organization:", object$project$organization, "\n", sep = "")

  # Principal Investigator(s)
  pi <- object$project$principal_investigator
  if (is.list(pi) && !is.null(names(pi))) {
    # Single PI with structure
    cat("  PI:          ", pi$name, "\n", sep = "")
    if (!is.null(pi$affiliation)) {
      cat("               ", pi$affiliation, "\n", sep = "")
    }
  } else if (is.list(pi)) {
    # Multiple PIs
    cat("  PIs:         ", pi[[1]]$name, "\n", sep = "")
    if (length(pi) > 1) {
      for (i in 2:length(pi)) {
        cat("               ", pi[[i]]$name, "\n", sep = "")
      }
    }
  }

  cat("  Root:        ", object$meta$project_root, "\n\n", sep = "")

  # Configuration Files
  cat("Configuration Files:\n")

  if (!is.na(object$meta$qretools_config_path)) {
    cat("  _qretools.yml:      \u2713 ", object$meta$qretools_config_path, "\n", sep = "")
  } else {
    cat("  _qretools.yml:      (not found)\n")
  }

  cat("  project-config.yml: \u2713 ", object$meta$project_config_path, "\n\n", sep = "")

  # Key Paths
  cat("Key Paths:\n")
  cat("  Question Bank: ", object$paths$question_bank, "\n", sep = "")
  cat("  Value Labels:  ", object$paths$value_labels, "\n", sep = "")
  cat("  Surveys:       ", object$paths$surveys, "\n\n", sep = "")

  # Settings
  cat("Settings:\n")
  cat("  Validation: ", object$settings$validation_strictness, "\n\n", sep = "")

  # Cache Status
  cat("Cache Status:\n")
  cat("  Type:   ", object$meta$cache_type, "\n", sep = "")
  cat("  Cached: ", object$meta$cached, "\n", sep = "")
  if (object$meta$cached) {
    cat("  Time:   ", format(object$meta$cache_time, "%Y-%m-%d %H:%M:%S"), "\n", sep = "")
  }
  cat("\n")

  # Overrides (if any)
  if (!is.null(object$meta$overrides)) {
    cat("Overrides:\n")
    overrides_str <- utils::capture.output(utils::str(object$meta$overrides, give.attr = FALSE))
    cat(paste0("  ", overrides_str[-1]), sep = "\n")  # Skip first line (List of...)
    cat("\n")
  }

  invisible(object)
}


#' Clear Cached qretools Configurations
#'
#' Remove cached configuration objects from memory (session cache) and/or
#' disk (persistent cache). Useful when configuration files have been modified
#' and you want to force a fresh read.
#'
#' @param persistent Logical. If TRUE, also clear persistent disk cache.
#'   If FALSE (default), only clear session cache.
#'
#' @return Invisibly returns TRUE if cache was cleared, FALSE if no cache existed.
#'
#' @details
#' Session cache is stored in memory and cleared automatically when R restarts.
#' Persistent cache is stored in the user's cache directory and survives R restarts.
#'
#' Cache is automatically invalidated when configuration files are modified,
#' so manual clearing is rarely needed. However, this function is useful if:
#' \itemize{
#'   \item{You want to force a fresh read for debugging}
#'   \item{You've made changes that don't update file modification times}
#'   \item{You want to reclaim disk space from persistent cache}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Clear session cache only
#' qt_clear_cache()
#'
#' # Clear both session and persistent cache
#' qt_clear_cache(persistent = TRUE)
#' }
qt_clear_cache <- function(persistent = FALSE) {

  session_cleared <- FALSE
  persistent_cleared <- FALSE

  # Clear session cache
  .session_cache$clear()
  session_cleared <- TRUE
  message("Session cache cleared")

  # Clear persistent cache if requested
  if (persistent) {
    .persistent_cache$clear()
    persistent_cleared <- TRUE

    cache_dir <- tools::R_user_dir("qretools", "cache")
    message("Persistent cache cleared from: ", cache_dir)
  }

  invisible(session_cleared || persistent_cleared)
}
