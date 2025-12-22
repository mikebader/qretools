# Survey Configuration Reading Functions
#
# Functions for reading and processing survey configuration files

#' Read Survey Configuration
#'
#' Read and parse a survey configuration YAML file, resolving all references
#' to questions, parameters, and modules, and returning a complete questionnaire
#' configuration object.
#'
#' @param survey_config_file Character string. Path to survey config YAML file.
#'   Example: "surveys/bas-2025/survey-config.yml"
#' @param config A qt_config object. If NULL (default), calls qt_config() to
#'   get/create configuration.
#'
#' @return S3 object of class \code{qt_qreconfig} containing:
#'   \describe{
#'     \item{meta}{Survey metadata from YAML}
#'     \item{items}{Ordered list of survey items (S3 objects)}
#'     \item{indices}{Lists and maps for quick lookups}
#'     \item{source_map}{Data frame tracking item provenance}
#'     \item{read_meta}{Metadata about the read operation}
#'   }
#'
#' @details
#' This function:
#' \itemize{
#'   \item Loads all necessary banks (questions, candidates, controls, modules)
#'   \item Processes sections and items recursively
#'   \item Expands modules and loops inline
#'   \item Creates typed S3 objects for each item
#'   \item Builds indices for efficient access
#'   \item Tracks provenance of all items
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Read survey config
#' qre <- qt_read_survey_config("surveys/bas-2025/survey-config.yml")
#'
#' # Access items
#' qre$items
#'
#' # Get all questions
#' qre$indices$questions
#'
#' # Questions by section
#' qre$indices$section_map
#' }
qt_read_survey_config <- function(survey_config_file, config = NULL) {

  # Step 1: Validate inputs
  if (!file.exists(survey_config_file)) {
    stop("Survey config file not found: ", survey_config_file, call. = FALSE)
  }

  if (is.null(config)) {
    config <- qt_config()
  }

  # Step 2: Load banks
  # TODO: Implement qt_qbank(), qt_ctrlbank(), qt_read_candidates(), qt_modbank()
  qbank <- list(variables = list())  # Placeholder
  ctrlbank <- list(variables = list())  # Placeholder

  # Resolve candidates path
  survey_yaml_temp <- .read_yaml_safe(survey_config_file, "survey config")
  candidates_path <- survey_yaml_temp$meta$candidates_path %||% "candidates"
  candidates_full_path <- file.path(dirname(survey_config_file), candidates_path)

  # TODO: Implement qt_read_candidates() - thin wrapper around qt_qbank()
  candidates <- list(variables = list())  # Placeholder

  # TODO: Implement qt_modbank()
  modbank <- list(modules = list())  # Placeholder

  # Step 3: Read and validate survey config YAML
  survey_yaml <- .read_yaml_safe(survey_config_file, "survey config")

  # Validate required top-level fields
  required_fields <- c("meta", "sections")
  missing <- setdiff(required_fields, names(survey_yaml))
  if (length(missing) > 0) {
    stop("Survey config missing required fields: ",
         paste(missing, collapse = ", "), call. = FALSE)
  }

  # Validate meta fields
  required_meta <- c("id", "title", "status")
  missing_meta <- setdiff(required_meta, names(survey_yaml$meta))
  if (length(missing_meta) > 0) {
    stop("Survey meta missing required fields: ",
         paste(missing_meta, collapse = ", "), call. = FALSE)
  }

  # Step 4: Process sections and items
  all_items <- list()

  for (section in survey_yaml$sections) {
    section_id <- section$id

    # Process items (which may be containers)
    section_items <- .process_items_recursive(
      section$items,
      section_id = section_id,
      qbank = qbank,
      candidates = candidates,
      config = config,
      container_context = NULL,
      modbank = modbank
    )

    all_items <- c(all_items, section_items)
  }

  # Step 5: Build indices and references
  varnames <- character()
  section_map <- list()
  control_params <- character()
  modules_used <- character()
  fills_found <- character()
  module_map <- list()
  loop_map <- list()
  logic_map <- list()
  split_map <- list()

  for (item in all_items) {
    if (item$item_type == "question") {
      varnames <- c(varnames, item$variable_id)

      # Add to section map
      if (is.null(section_map[[item$section_id]])) {
        section_map[[item$section_id]] <- character()
      }
      section_map[[item$section_id]] <- c(section_map[[item$section_id]], item$variable_id)

      # Extract fills from question text
      fills <- .extract_fills(item$definition$question_text)
      fills_found <- c(fills_found, fills)

      # Track module usage
      if (!is.null(item$module_id)) {
        modules_used <- c(modules_used, item$module_id)
        module_map[[item$module_id]] <- c(module_map[[item$module_id]], item$variable_id)
      }

      # Track loop usage
      if (!is.null(item$loop_id)) {
        loop_key <- paste0(item$loop_id, "_iter", item$loop_iteration)
        loop_map[[loop_key]] <- c(loop_map[[loop_key]], item$variable_id)
      }

      # Track logic usage
      if (!is.null(item$logic_condition)) {
        logic_key <- paste0(item$logic_condition, "_", item$logic_branch)
        logic_map[[logic_key]] <- c(logic_map[[logic_key]], item$variable_id)
      }

      # Track split usage
      if (!is.null(item$split_id)) {
        split_key <- paste0(item$split_id, "_", item$split_path)
        split_map[[split_key]] <- c(split_map[[split_key]], item$variable_id)
      }
    }

    # Extract fills from other item types
    if (item$item_type == "statement") {
      fills <- .extract_fills(item$text)
      fills_found <- c(fills_found, fills)
    }
  }

  # TODO: Extract control_params from fills_found and validate against ctrlbank

  # Step 6: Build source map
  source_map <- data.frame(
    item_id = character(),
    item_type = character(),
    source = character(),
    source_file = character(),
    section_id = character(),
    stringsAsFactors = FALSE
  )

  for (item in all_items) {
    source_file <- if (item$item_type == "question") {
      item$definition$source_file
    } else {
      NA
    }

    source_map <- rbind(source_map, data.frame(
      item_id = item$id,
      item_type = item$item_type,
      source = item$source %||% NA,
      source_file = source_file %||% NA,
      section_id = item$section_id,
      stringsAsFactors = FALSE
    ))
  }

  # Step 7: Build final structure
  result <- structure(
    list(
      meta = survey_yaml$meta,
      items = all_items,
      indices = list(
        questions = unique(varnames),
        controls = unique(control_params),
        modules = unique(modules_used),
        fills = unique(fills_found),
        items = seq_along(all_items),
        section_map = section_map,
        module_map = module_map,
        loop_map = loop_map,
        logic_map = logic_map,
        split_map = split_map
      ),
      source_map = source_map,
      read_meta = list(
        source_file = survey_config_file,
        read_time = Sys.time(),
        config_used = config$meta$project_root
      )
    ),
    class = "qt_qreconfig"
  )

  return(result)
}

# Internal helper functions -----------------------------------------------

#' Process Items Recursively
#'
#' Main dispatcher that processes survey items, handling both simple items
#' and containers (modules, loops, etc.)
#'
#' @keywords internal
#' @noRd
.process_items_recursive <- function(items, section_id, qbank, candidates, config,
                                     container_context = NULL, modbank) {
  result <- list()

  for (item in items) {
    item_type <- item$item_type

    processed <- switch(item_type,
                        # Simple items - return single item in list
                        "statement" = list(.make_statement_item(item, section_id, container_context)),
                        "question" = list(.make_question_item(item, section_id, qbank, candidates, container_context)),
                        "compute" = list(.make_compute_item(item, section_id, container_context)),

                        # Containers - return multiple items
                        "module" = .expand_module(item, section_id, qbank, candidates, config, container_context, modbank),
                        "loop" = .expand_loop(item, section_id, qbank, candidates, config, container_context, modbank),
                        "logic" = .process_logic_item(item, section_id, qbank, candidates, config, container_context, modbank),
                        "split" = .process_split(item, section_id, qbank, candidates, config, container_context, modbank),

                        # Simple containers - just tag items
                        "randomize" = .process_items_recursive(
                          item$items, section_id, qbank, candidates, config,
                          c(container_context, list(randomize_id = item$id)),
                          modbank
                        ),
                        "display_together" = .process_items_recursive(
                          item$items, section_id, qbank, candidates, config,
                          c(container_context, list(display_together_id = item$id)),
                          modbank
                        ),

                        stop("Unknown item_type: ", item_type, call. = FALSE)
    )

    result <- c(result, processed)
  }

  result
}

#' Create Question Item
#'
#' @keywords internal
#' @noRd
.make_question_item <- function(item, section_id, qbank, candidates, container_context) {
  varname <- item$variable_id
  source <- item$source %||% "qbank"

  # Resolve question definition
  q_def <- if (source == "qbank") {
    qbank$variables[[varname]]
  } else if (source == "candbank") {
    candidates$variables[[varname]]
  } else {
    stop("Unknown source: ", source, call. = FALSE)
  }

  if (is.null(q_def)) {
    stop("Question '", varname, "' not found in ", source, call. = FALSE)
  }

  # Create item
  structure(
    list(
      id = item$variable_id,
      item_type = "question",
      variable_id = varname,
      source = source,
      section_id = section_id,

      # Container membership
      module_id = container_context$module_id,
      loop_id = container_context$loop_id,
      loop_iteration = container_context$loop_iteration,
      randomize_id = container_context$randomize_id,
      split_id = container_context$split_id,
      split_path = container_context$split_path,
      display_together_id = container_context$display_together_id,
      logic_condition = container_context$logic_condition,
      logic_branch = container_context$logic_branch,

      # Item-specific overrides
      hide_question = item$hide_question %||% FALSE,
      if_condition = item$if_condition,
      programmer_note = item$programmer_note,

      # Embedded definition (includes source_file)
      definition = q_def
    ),
    class = c("qt_qitem", "qt_item")
  )
}

#' Create Statement Item
#'
#' @keywords internal
#' @noRd
.make_statement_item <- function(item, section_id, container_context) {
  structure(
    list(
      id = item$id,
      item_type = "statement",
      text = item$text,
      section_id = section_id,

      # Container membership
      module_id = container_context$module_id,
      loop_id = container_context$loop_id,
      loop_iteration = container_context$loop_iteration,
      randomize_id = container_context$randomize_id,
      split_id = container_context$split_id,
      split_path = container_context$split_path,
      display_together_id = container_context$display_together_id,
      logic_condition = container_context$logic_condition,
      logic_branch = container_context$logic_branch,

      # Statement-specific fields
      keep_with_next = item$keep_with_next %||% FALSE,
      if_condition = item$if_condition,
      display_class = item$display_class,
      programmer_note = item$programmer_note,
      note = item$note
    ),
    class = c("qt_stmtitem", "qt_item")
  )
}

#' Create Compute Item
#'
#' @keywords internal
#' @noRd
.make_compute_item <- function(item, section_id, container_context) {
  structure(
    list(
      id = item$id,
      item_type = "compute",
      description = item$description,
      command = item$command,
      section_id = section_id,

      # Container membership
      module_id = container_context$module_id,
      loop_id = container_context$loop_id,
      loop_iteration = container_context$loop_iteration,
      randomize_id = container_context$randomize_id,
      split_id = container_context$split_id,
      split_path = container_context$split_path,
      display_together_id = container_context$display_together_id,
      logic_condition = container_context$logic_condition,
      logic_branch = container_context$logic_branch,

      # Compute-specific
      requires = item$requires,
      provides = item$provides,
      programmer_note = item$programmer_note,
      note = item$note
    ),
    class = c("qt_compitem", "qt_item")
  )
}

#' Expand Module
#'
#' Read module definition and expand its items inline
#'
#' @keywords internal
#' @noRd
.expand_module <- function(item, section_id, qbank, candidates, config, container_context, modbank) {
  module_id <- item$module_id

  # Get module from bank
  module_def <- modbank$modules[[module_id]]

  if (is.null(module_def)) {
    stop("Module '", module_id, "' not found in module bank", call. = FALSE)
  }

  # Update container context
  module_context <- container_context
  module_context$module_id <- module_id

  # Process module's items
  module_items <- .process_items_recursive(
    module_def$items,
    section_id = section_id,
    qbank = qbank,
    candidates = candidates,
    config = config,
    container_context = module_context,
    modbank = modbank
  )

  # Apply module-level if_condition to all items if specified
  if (!is.null(item$if_condition)) {
    module_items <- lapply(module_items, function(mi) {
      mi$if_condition <- item$if_condition
      mi
    })
  }

  module_items
}

#' Expand Loop
#'
#' Create multiple copies of loop items, substituting {i} with fill values
#'
#' @keywords internal
#' @noRd
.expand_loop <- function(item, section_id, qbank, candidates, config, container_context, modbank) {
  loop_id <- item$id
  id_fills <- item$id_fills
  title_fills <- item$title_fills %||% id_fills

  if (length(id_fills) != length(title_fills)) {
    stop("Loop '", loop_id, "': id_fills and title_fills must have same length", call. = FALSE)
  }

  result <- list()

  # Expand loop for each iteration
  for (i in seq_along(id_fills)) {
    # Update container context with loop info
    loop_context <- container_context
    loop_context$loop_id <- loop_id
    loop_context$loop_iteration <- i
    loop_context$loop_id_fill <- id_fills[i]
    loop_context$loop_title_fill <- title_fills[i]

    # Process items for this iteration
    iteration_items <- .process_items_recursive(
      item$items,
      section_id = section_id,
      qbank = qbank,
      candidates = candidates,
      config = config,
      container_context = loop_context,
      modbank = modbank
    )

    # Substitute {i} in appropriate fields for each item type
    iteration_items <- lapply(iteration_items, function(it) {
      if (it$item_type == "question") {
        it$variable_id <- gsub("\\{i\\}", id_fills[i], it$variable_id)
        it$id <- it$variable_id  # Keep id in sync
        it$definition$title <- gsub("\\{i\\}", title_fills[i], it$definition$title)
      } else if (it$item_type == "statement") {
        it$text <- gsub("\\{i\\}", title_fills[i], it$text)
      } else if (it$item_type == "compute") {
        it$command <- gsub("\\{i\\}", id_fills[i], it$command)
        it$description <- gsub("\\{i\\}", title_fills[i], it$description)
      }
      it
    })

    result <- c(result, iteration_items)
  }

  result
}

#' Process Logic Block
#'
#' Process conditional logic, tagging items with condition and branch
#'
#' @keywords internal
#' @noRd
.process_logic_item <- function(item, section_id, qbank, candidates, config, container_context, modbank) {
  # Process 'then' branch
  then_context <- container_context
  then_context$logic_condition <- item$condition
  then_context$logic_branch <- "then"
  then_items <- .process_items_recursive(
    item$then,
    section_id = section_id,
    qbank = qbank,
    candidates = candidates,
    config = config,
    container_context = then_context,
    modbank = modbank
  )

  # Process 'else' branch if present
  else_items <- if (!is.null(item$else)) {
    else_context <- container_context
    else_context$logic_condition <- item$condition
    else_context$logic_branch <- "else"
    .process_items_recursive(
      item$else,
      section_id = section_id,
      qbank = qbank,
      candidates = candidates,
      config = config,
      container_context = else_context,
      modbank = modbank
    )
  } else {
    list()
  }

  c(then_items, else_items)
}

#' Process Split Block
#'
#' Process split paths, tagging items with split_id and path
#'
#' @keywords internal
#' @noRd
.process_split <- function(item, section_id, qbank, candidates, config, container_context, modbank) {
  split_id <- item$id

  result <- list()

  # Process each path
  for (path in item$paths) {
    path_context <- container_context
    path_context$split_id <- split_id
    path_context$split_path <- path$id

    path_items <- .process_items_recursive(
      path$items,
      section_id = section_id,
      qbank = qbank,
      candidates = candidates,
      config = config,
      container_context = path_context,
      modbank = modbank
    )

    result <- c(result, path_items)
  }

  result
}

#' Extract Fill References
#'
#' Extract all {{...}} patterns from text
#'
#' @keywords internal
#' @noRd
.extract_fills <- function(text) {
  if (is.null(text) || length(text) == 0 || is.na(text)) {
    return(character())
  }

  # Find all {{...}} patterns
  matches <- gregexpr("\\{\\{([^}]+)\\}\\}", text, perl = TRUE)

  if (matches[[1]][1] == -1) {
    return(character())
  }

  fills <- character()
  for (match_pos in matches[[1]]) {
    match_len <- attr(matches[[1]], "match.length")[which(matches[[1]] == match_pos)]
    # Extract content between {{ and }}
    fill <- substr(text, match_pos + 2, match_pos + match_len - 3)
    fills <- c(fills, trimws(fill))
  }

  unique(fills)
}

# S3 Methods --------------------------------------------------------------

#' Print Questionnaire Configuration
#'
#' @param x A qt_qreconfig object
#' @param ... Additional arguments (ignored)
#'
#' @return Invisibly returns the original object
#'
#' @export
print.qt_qreconfig <- function(x, ...) {
  cat("Questionnaire Configuration\n")
  cat(strrep("=", 50), "\n\n", sep = "")

  cat("Survey:   ", x$meta$title, "\n")
  cat("Year:     ", x$meta$year %||% "(not specified)", "\n")
  cat("Status:   ", x$meta$status, "\n")
  cat("Items:    ", length(x$items), "\n")
  cat("Questions:", length(x$indices$questions), "\n\n")

  invisible(x)
}

#' Summarize Questionnaire Configuration
#'
#' @param object A qt_qreconfig object
#' @param ... Additional arguments (ignored)
#'
#' @return Invisibly returns the original object
#'
#' @export
summary.qt_qreconfig <- function(object, ...) {
  cat("Questionnaire Configuration Summary\n")
  cat(strrep("=", 50), "\n\n")

  cat("Survey Information:\n")
  cat("  Title:    ", object$meta$title, "\n")
  cat("  ID:       ", object$meta$id, "\n")
  cat("  Status:   ", object$meta$status, "\n\n")

  cat("Content:\n")
  cat("  Total items:     ", length(object$items), "\n")
  cat("  Questions:       ", length(object$indices$questions), "\n")
  cat("  Sections:        ", length(object$indices$section_map), "\n")
  cat("  Modules used:    ", length(object$indices$modules), "\n")
  cat("  Controls needed: ", length(object$indices$controls), "\n\n")

  cat("Source:\n")
  cat("  Config file: ", object$read_meta$source_file, "\n")
  cat("  Read at:     ", format(object$read_meta$read_time), "\n\n")

  invisible(object)
}
