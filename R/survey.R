# Survey Config Reading Functions
#
# Read and parse survey configuration YAML files, resolving all references
# to questions, parameters, and modules.

# Internal helper: Extract {{...}} fill patterns from text
#
# @param text Character string to scan
# @return Character vector of unique fill variable names
.extract_fills <- function(text) {
  if (is.null(text) || length(text) == 0 || !is.character(text)) {
    return(character())
  }

  # Find all {{...}} patterns
  matches <- gregexpr("\\{\\{([^}]+)\\}\\}", text, perl = TRUE)
  fills <- character()

  if (matches[[1]][1] == -1) {
    return(character())
  }

  # Extract content between {{ and }}
  match_starts <- matches[[1]]
  match_lengths <- attr(matches[[1]], "match.length")

  for (i in seq_along(match_starts)) {
    # Extract the full match including {{ }}
    full_match <- substr(text, match_starts[i], match_starts[i] + match_lengths[i] - 1)
    # Remove {{ and }} to get just the variable name
    fill_content <- substr(full_match, 3, nchar(full_match) - 2)
    fills <- c(fills, trimws(fill_content))
  }

  unique(fills)
}

#' Process a single question item
#'
#' Internal helper to resolve a question reference and create a qt_qitem object
#'
#' @param item List containing question reference (varname, source, etc.)
#' @param section_name Character string, name of the section containing this question
#' @param all_vars List of all variable banks
#' @param survey_config_file Character string, path to survey config file (for error messages)
#' @param module_id Character string or NULL, module_id if this question comes from a module
#'
#' @return List with three elements:
#'   \item{q_item}{The constructed qt_qitem object}
#'   \item{varname}{The question varname}
#'   \item{source_info}{List with source and source_file for tracking}
#'   \item{fills}{Character vector of fills extracted from question_text}
#'
#' @keywords internal
.process_question <- function(item, section_name, all_vars, survey_config_file, module_id = NULL) {

  varname <- item$varname
  if (is.null(varname)) {
    stop("Question item in section '", section_name,
         "' missing 'varname' field",
         "\nFile: ", survey_config_file, call. = FALSE)
  }

  source <- item$source
  if (is.null(source)) source <- "question_bank"  # Default

  # Look up question definition
  if (source == "question_bank") {
    if (!varname %in% names(all_vars$question_bank)) {
      stop("Question '", varname, "' not found in question bank",
           "\nSection: ", section_name,
           "\nFile: ", survey_config_file, call. = FALSE)
    }
    q_def <- all_vars$question_bank[[varname]]
    source_file <- attr(q_def, "source_file") %||% q_def$source_file %||% "unknown"

  } else if (source == "survey") {
    # Look in generated bank (candidates)
    if (!varname %in% names(all_vars$generated)) {
      stop("Question '", varname, "' not found in survey candidates",
           "\nSection: ", section_name,
           "\nFile: ", survey_config_file, call. = FALSE)
    }
    q_def <- all_vars$generated[[varname]]
    source_file <- attr(q_def, "source_file") %||% q_def$source_file %||% "unknown"

  } else {
    stop("Unknown source '", source, "' for question '", varname, "'",
         "\nSection: ", section_name,
         "\nFile: ", survey_config_file, call. = FALSE)
  }

  # Create question item with embedded definition
  q_item <- structure(
    list(
      type = "question",
      varname = varname,
      source = source,
      position = item$position,
      section = section_name,
      module = module_id,  # NULL for direct questions, module_id for module questions
      action_before = item$action_before,
      action_after = item$action_after,
      definition = q_def  # EMBED the full qt_qvar object
    ),
    class = c("qt_qitem", "qt_item")
  )

  # Extract fills from question_text
  fills <- character(0)
  if (!is.null(q_def$question_text)) {
    fills <- .extract_fills(q_def$question_text)
  }

  # Return everything needed for tracking
  list(
    q_item = q_item,
    varname = varname,
    source_info = list(
      source = source,
      file = source_file
    ),
    fills = fills
  )
}

#' Read and expand a module file
#'
#' Internal function to read a module YAML file and return its contents
#' for splicing into survey configuration.
#'
#' @param module_ref List with 'name' element (module_id) from survey config
#' @param banks List of question/variable banks (unused, for signature consistency)
#' @param proj_root Character string, project root directory path
#'
#' @return List with three elements:
#'   \item{questions}{List of question references from module}
#'   \item{generated_variables}{List of generated variable references (may be empty)}
#'   \item{control_variables}{List of control variable references (may be empty)}
#'
#' @keywords internal
.read_and_expand_module <- function(module_ref, banks, config) {
  module_name <- module_ref$name

  # Resolve modules path from config
  modules_path <- .qt_resolve_path("modules", path = NULL, config)

  # Try modules.yml first, then modules/{module_name}.yml
  modules_single <- paste0(modules_path, ".yml")
  modules_dir <- file.path(modules_path, paste0(module_name, ".yml"))

  module_path <- NULL
  if (file.exists(modules_single)) {
    module_path <- modules_single
  } else if (file.exists(modules_dir)) {
    module_path <- modules_dir
  } else {
    stop("Module '", module_name, "' not found. Looked in:\n  ",
         modules_single, "\n  ", modules_dir,
         call. = FALSE)
  }

  # Read and parse YAML
  # Read and parse YAML
  module_yaml <- tryCatch(
    yaml::read_yaml(module_path),
    error = function(e) {
      stop("Failed to parse module file '", module_path, "':\n  ",
           e$message, call. = FALSE)
    }
  )

  # Should be a list of modules
  if (!is.list(module_yaml) || is.null(module_yaml[[1]]$module_id)) {
    stop("Module file must contain a list of modules. Each module should start with '-'",
         "\nFile: ", module_path, call. = FALSE)
  }

  modules_list <- module_yaml

  # Find the requested module
  module_data <- NULL
  for (mod in modules_list) {
    if (!is.null(mod$module_id) && mod$module_id == module_name) {
      module_data <- mod
      break
    }
  }

  if (is.null(module_data)) {
    stop("Module '", module_name, "' not found in ", module_path, call. = FALSE)
  }

  # Validate required fields
  if (is.null(module_data$module_id)) {
    stop("Module in '", module_path, "' missing required field: module_id",
         call. = FALSE)
  }

  if (is.null(module_data$questions) || length(module_data$questions) == 0) {
    stop("Module '", module_name, "' must contain at least one question",
         call. = FALSE)
  }

  # Return the three sections
  list(
    questions = module_data$questions,
    generated_variables = if (is.null(module_data$generated_variables)) list() else module_data$generated_variables,
    control_variables = if (is.null(module_data$control_variables)) list() else module_data$control_variables
  )
}

#' Read Survey Configuration
#'
#' Read and parse a survey configuration YAML file, resolving all references
#' to questions, parameters, and modules. Returns a complete, self-contained
#' survey configuration object.
#'
#' @param survey_config_file Character string. Path to survey config YAML file.
#'   Must be explicit path (e.g., "surveys/bas-2025/survey-2025-FINAL.yml").
#' @param config qt_config object. If NULL, calls qt_config() to get/create
#'   configuration. Used to resolve paths to banks and parameters.
#'
#' @return S3 object of class \code{qt_survey_config} containing:
#'   \describe{
#'     \item{survey}{Survey metadata (year, name, short_name, status, date_finalized)}
#'     \item{parameters}{Parameter configuration (project_load, survey_file)}
#'     \item{items}{Ordered list of survey items (typed objects: qt_qitem, qt_stmtitem, qt_ctrlitem)}
#'     \item{references}{Lists of all referenced questions, parameters, modules, rotation sets, fills}
#'     \item{source_map}{Data frames tracking sources of questions, parameters, modules}
#'     \item{source_file}{Path to survey config file}
#'     \item{read_time}{Timestamp of when config was read}
#'   }
#'
#' @details
#' This function performs comprehensive processing:
#' \itemize{
#'   \item{Reads survey config YAML and validates structure}
#'   \item{Loads all necessary variable banks (questions, control, generated)}
#'   \item{Processes each item, creating typed objects}
#'   \item{Embeds full variable definitions in question items}
#'   \item{Expands modules inline (when implemented)}
#'   \item{Extracts all {{...}} fill references from text}
#'   \item{Builds source maps for tracking}
#'   \item{Returns completely self-contained object ready for check() and make()}
#' }
#'
#' **Item Types:**
#' \itemize{
#'   \item{\code{statement}: Creates qt_stmtitem with text and formatting}
#'   \item{\code{question}: Creates qt_qitem with embedded qt_qvar definition}
#'   \item{\code{control_assignment}: Creates qt_ctrlitem with logic}
#'   \item{\code{module}: Expands into individual items (when implemented)}
#' }
#'
#' **Question Resolution:**
#' Questions can come from:
#' \itemize{
#'   \item{\code{source: question_bank} - From main question bank}
#'   \item{\code{source: survey} - From survey-specific candidates}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Read survey config
#' config <- qt_config(project_root = "inst/examples")
#' survey <- qt_read_survey_config(
#'   "inst/examples/surveys/bas-2025/survey-2025-FINAL.yml",
#'   config = config
#' )
#'
#' # Access items
#' survey$items[[1]]  # First item
#'
#' # Check what questions are used
#' survey$references$questions
#'
#' # See where questions come from
#' survey$source_map$questions
#' }
qt_read_survey_config <- function(survey_config_file, config = NULL) {

  # Step 1: Read and validate survey config YAML ----
  survey_yaml <- .read_yaml_safe(survey_config_file, "survey config")
  proj_root <- config$meta$project_root

  # Validate required top-level fields
  required_fields <- c("survey", "sections")
  missing <- setdiff(required_fields, names(survey_yaml))
  if (length(missing) > 0) {
    stop("Survey config missing required fields: ",
         paste(missing, collapse = ", "),
         "\nFile: ", survey_config_file, call. = FALSE)
  }

  # Extract and validate survey metadata
  survey_meta <- survey_yaml$survey
  required_survey_fields <- c("year", "name", "status")
  missing <- setdiff(required_survey_fields, names(survey_meta))
  if (length(missing) > 0) {
    stop("Survey metadata missing required fields: ",
         paste(missing, collapse = ", "),
         "\nFile: ", survey_config_file, call. = FALSE)
  }

  # Step 2: Load all necessary banks ----
  if (is.null(config)) {
    config <- qt_config()
  }

  qbank <- qt_qbank(config = config)
  ctrlbank <- qt_ctrlbank(config = config)
  genbank <- qt_genbank(config = config)

  # Combine into lookup structure
  all_vars <- list(
    question_bank = qbank$variables,
    control = ctrlbank$variables,
    generated = genbank$variables
  )

  # Step 3: Process parameters section ----
  params_config <- survey_yaml$parameters

  if (!is.null(params_config)) {
    project_load <- params_config$project_load
    if (is.null(project_load)) project_load <- character()

    survey_file <- params_config$survey_file
    if (is.null(survey_file)) survey_file <- "parameters.yml"
  } else {
    project_load <- character()
    survey_file <- "parameters.yml"
  }

  parameters <- list(
    project_load = project_load,
    survey_file = survey_file
  )

  # Step 4: Process sections and items ----

  # Initialize tracking
  all_items <- list()
  questions_referenced <- character()
  parameters_referenced <- character()
  modules_referenced <- character()
  fills_found <- character()

  question_source_map <- data.frame(
    varname = character(),
    source = character(),
    file = character(),
    stringsAsFactors = FALSE
  )

  parameter_source_map <- data.frame(
    parameter = character(),
    source = character(),
    file = character(),
    stringsAsFactors = FALSE
  )

  module_source_map <- data.frame(
    module_id = character(),
    file = character(),
    items = I(list()),
    stringsAsFactors = FALSE
  )

  # Process each section
  for (section in survey_yaml$sections) {
    section_name <- section$name

    if (is.null(section$items) || length(section$items) == 0) {
      warning("Section '", section_name, "' has no items", call. = FALSE)
      next
    }

    for (item in section$items) {
      item_type <- item$type

      if (is.null(item_type)) {
        stop("Item in section '", section_name, "' missing 'type' field",
             "\nFile: ", survey_config_file, call. = FALSE)
      }

      # Process based on item type
      if (item_type == "statement") {
        # Create statement item
        stmt_item <- structure(
          list(
            type = "statement",
            statement_type = item$statement_type,
            text = item$text,
            with_next = item$with_next,
            position = item$position,
            section = section_name
          ),
          class = c("qt_stmtitem", "qt_item")
        )

        # Extract fills from text
        if (!is.null(item$text)) {
          fills <- .extract_fills(item$text)
          fills_found <- c(fills_found, fills)
        }

        all_items <- c(all_items, list(stmt_item))

        # In qt_read_survey_config(), replace the question processing with:

      } else if (item_type == "question") {
        # Process question using helper
        result <- .process_question(item, section_name, all_vars, survey_config_file)

        # Track
        questions_referenced <- c(questions_referenced, result$varname)
        question_source_map <- rbind(
          question_source_map,
          data.frame(
            varname = result$varname,
            source = result$source_info$source,
            file = result$source_info$file,
            stringsAsFactors = FALSE
          )
        )
        fills_found <- c(fills_found, result$fills)
        all_items <- c(all_items, list(result$q_item))

      } else if (item_type == "module") {
        # Expand module and process each question
        module_name <- item$name
        if (is.null(module_name)) {
          stop("Module item in section '", section_name,
               "' missing 'name' field",
               "\nFile: ", survey_config_file, call. = FALSE)
        }

        # Read and expand the module
        expanded <- .read_and_expand_module(item, banks = NULL, config)

        # Process each question in the module
        for (mod_question in expanded$questions) {
          result <- .process_question(mod_question, section_name, all_vars,
                                      survey_config_file, module_id = module_name)

          # Track same as regular questions
          questions_referenced <- c(questions_referenced, result$varname)
          question_source_map <- rbind(
            question_source_map,
            data.frame(
              varname = result$varname,
              source = result$source_info$source,
              file = result$source_info$file,
              stringsAsFactors = FALSE
            )
          )
          fills_found <- c(fills_found, result$fills)
          all_items <- c(all_items, list(result$q_item))
        }

        # TODO: Handle expanded$generated_variables and expanded$control_variables

      } else if (item_type == "control_assignment") {
        # Create control assignment item
        param_name <- item$parameter
        if (is.null(param_name)) {
          stop("Control assignment in section '", section_name,
               "' missing 'parameter' field",
               "\nFile: ", survey_config_file, call. = FALSE)
        }

        ctrl_item <- structure(
          list(
            type = "control_assignment",
            parameter = param_name,
            logic = item$logic,
            position = item$position,
            section = section_name,
            module = NULL
          ),
          class = c("qt_ctrlitem", "qt_item")
        )

        # Track parameter
        parameters_referenced <- c(parameters_referenced, param_name)
        parameter_source_map <- rbind(
          parameter_source_map,
          data.frame(
            parameter = param_name,
            source = "survey_assignment",
            file = survey_config_file,
            stringsAsFactors = FALSE
          )
        )

        all_items <- c(all_items, list(ctrl_item))

      } else if (item_type == "module") {
        # Read and expand module
        module_id <- item$module_id
        if (is.null(module_id)) {
          stop("Module item in section '", section_name,
               "' missing 'module_id' field",
               "\nFile: ", survey_config_file, call. = FALSE)
        }

        module_position <- item$position

        # Expand module (placeholder - not yet implemented)
        module_items <- .read_and_expand_module(
          module_id, section_name, module_position, config, all_vars
        )

        # Track
        modules_referenced <- c(modules_referenced, module_id)

        # Add expanded items
        all_items <- c(all_items, module_items)

      } else {
        stop("Unknown item type '", item_type, "' in section '", section_name, "'",
             "\nFile: ", survey_config_file, call. = FALSE)
      }
    }
  }

  # Step 5: Build references ----
  references <- list(
    questions = unique(questions_referenced),
    parameters = unique(c(project_load, parameters_referenced)),
    modules = unique(modules_referenced),
    rotation_sets = survey_yaml$uses_rotation_sets %||% character(),
    fills_found = unique(fills_found)
  )

  # Step 6: Build source maps ----
  source_map <- list(
    questions = question_source_map,
    parameters = parameter_source_map,
    modules = module_source_map
  )

  # Step 7: Return structured object ----
  structure(
    list(
      survey = survey_meta,
      parameters = parameters,
      items = all_items,
      references = references,
      source_map = source_map,
      source_file = survey_config_file,
      read_time = Sys.time()
    ),
    class = "qt_survey_config"
  )
}
