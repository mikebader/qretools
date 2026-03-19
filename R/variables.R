# Variable Object Constructors
#
# These functions create typed variable objects from YAML data.
# All constructors are strict - they fail on missing required fields.

# Internal helper: Map response_type to storage_type
#
# @param response_type Character string - response type from schema
# @return Character string - corresponding storage type
# @keywords internal
# @noRd
.qt_response_to_storage <- function(response_type) {
  mapping <- c(
    factor        = "factor",
    character     = "character",
    integer       = "integer",
    number        = "numeric",
    ranking       = "integer",
    select_all    = "multiple_response",
    loop          = NA_character_
  )
  if (!response_type %in% names(mapping)) {
    stop(sprintf(
      "Unknown response_type '%s'. Must be one of: %s",
      response_type, paste(names(mapping), collapse = ", ")
    ), call. = FALSE)
  }
  result <- mapping[[response_type]]
  if (is.na(result)) {
    stop("response_type='loop' requires an explicit storage_type to be specified", call. = FALSE)
  }
  result
}

#' Create Variable Objects
#'
#' Constructor functions for creating typed variable objects from YAML data.
#' These functions are strict and will fail if required fields are missing.
#'
#' @name variable-constructors
#' @param var_data List containing variable definition from YAML
#'
#' @return A variable object with appropriate class:
#'   - `qt_make_qvar()`: Returns object with class `c("qt_qvar", "qt_variable")`
#'   - `qt_make_ctrlvar()`: Returns object with class `c("qt_ctrlvar", "qt_variable")`
#'   - `qt_make_genvar()`: Returns object with class `c("qt_genvar", "qt_variable")`
#'
#' @section Required Fields:
#'
#' **All variable types require:**
#' - `variable_id`: Variable identifier (string)
#' - `title`: Short descriptive title (string)
#' - `storage_type`: Data type (factor, integer, numeric, character, composite, multiple_response)
#' - `vargroup`: Topic grouping (string)
#' - `question_text`: Full question text as asked (string)
#' - `surveys_used`: Survey instances where fielded (string vector, e.g., c("bas-2023", "bas-2024"))
#'
#' **Additional requirements by type:**
#' - Factor variables: `value_label_id` (reference to value label set)
#' - Control variables: `variable_role` must be "parameter"
#' - Generated variables: `variable_role` must be "generated", requires `derivation_method`
#' - Restricted variables: `restriction_reason` required if `restricted_access = TRUE`
#' - Multi-part questions: `creates_variables` and `variable_parts` required if storage_type is "composite" or "multiple_response"
#'
#' @examples
#' \dontrun{
#' # Create a question variable
#' q_data <- list(
#'   variable_id = "nhd_sat",
#'   title = "Neighborhood Satisfaction",
#'   storage_type = "factor",
#'   vargroup = "nhd",
#'   question_text = "How satisfied are you with your neighborhood?",
#'   value_label_id = "satisfied5",
#'   surveys_used = c("bas-2023", "bas-2024", "bas-2025")
#' )
#' q_var <- qt_make_qvar(q_data)
#'
#' # Create a control parameter
#' ctrl_data <- list(
#'   variable_id = "JURISDICTION",
#'   title = "Survey Jurisdiction",
#'   storage_type = "character",
#'   vargroup = "svy",
#'   question_text = "Baltimore City or Baltimore County",
#'   variable_role = "parameter",
#'   surveys_used = c("bas-2020", "bas-2021", "bas-2022", "bas-2023", "bas-2024", "bas-2025")
#' )
#' ctrl_var <- qt_make_ctrlvar(ctrl_data)
#'
#' # Create a generated variable
#' gen_data <- list(
#'   variable_id = "dem_income",
#'   title = "Household Income",
#'   storage_type = "factor",
#'   vargroup = "dem",
#'   question_text = "Household income (combined)",
#'   value_label_id = "comb_income",
#'   variable_role = "generated",
#'   derived_from = c("dem_income_lo", "dem_income_hi"),
#'   derivation_method = "Combines low/high income based on 70k filter",
#'   surveys_used = c("bas-2023", "bas-2024", "bas-2025")
#' )
#' gen_var <- qt_make_genvar(gen_data)
#' }
NULL

#' @rdname variable-constructors
#' @export
qt_make_qvar <- function(var_data) {
  # Handle response_type → storage_type mapping
  if (!is.null(var_data$response_type) && is.null(var_data$storage_type)) {
    var_data$storage_type <- .qt_response_to_storage(var_data$response_type)
  }

  # Validate required fields
  required_fields <- c("variable_id", "title", "storage_type", "vargroup", "question_text", "surveys_used")
  missing <- setdiff(required_fields, names(var_data))

  if (length(missing) > 0) {
    stop(sprintf(
      "Missing required fields for question variable '%s': %s",
      var_data$variable_id %||% "[unknown]",
      paste(missing, collapse = ", ")
    ), call. = FALSE)
  }

  # Validate type
  valid_types <- c("integer", "numeric", "factor", "character", "composite",
                   "multiple_response", "logical", "boolean")
  if (!var_data$storage_type %in% valid_types) {
    stop(sprintf(
      "Invalid storage_type '%s' for variable '%s'. Must be one of: %s",
      var_data$storage_type, var_data$variable_id, paste(valid_types, collapse = ", ")
    ), call. = FALSE)
  }

  # Conditional validation: factor variables must have value_label_id or value_labels_name (legacy)
  is_factor <- var_data$storage_type == "factor"
  has_value_labels <- !is.null(var_data$value_label_id) || !is.null(var_data$value_labels_name)
  if (is_factor && !has_value_labels) {
    stop(sprintf(
      "Factor variable '%s' must specify 'value_label_id'",
      var_data$variable_id
    ), call. = FALSE)
  }
  # Normalize value_labels_name (legacy) → value_label_id for internal consistency
  if (is.null(var_data$value_label_id) && !is.null(var_data$value_labels_name)) {
    var_data$value_label_id <- var_data$value_labels_name
  }

  # Conditional validation: restricted access requires restriction_reason
  if (isTRUE(var_data$restricted_access) && is.null(var_data$restriction_reason)) {
    stop(sprintf(
      "Variable '%s' has restricted_access=TRUE but missing 'restriction_reason'",
      var_data$variable_id
    ), call. = FALSE)
  }

  # Conditional validation: multi-part questions
  # creates_variables may be an inline named mapping (compact format) or a
  # character vector paired with a separate variable_parts mapping.
  if (var_data$storage_type %in% c("composite", "multiple_response")) {
    if (is.null(var_data$creates_variables)) {
      stop(sprintf(
        "Multi-part variable '%s' (storage_type='%s') must specify 'creates_variables'",
        var_data$variable_id, var_data$storage_type
      ), call. = FALSE)
    }
    cv <- var_data$creates_variables
    is_inline <- is.list(cv) && !is.null(names(cv)) && length(cv) > 0
    if (!is_inline && is.null(var_data$variable_parts)) {
      stop(sprintf(
        "Multi-part variable '%s' must specify 'variable_parts' (or use the inline creates_variables mapping format)",
        var_data$variable_id
      ), call. = FALSE)
    }
  }

  # Validate surveys_used is character vector
  if (!is.character(var_data$surveys_used)) {
    stop(sprintf(
      "Field 'surveys_used' for variable '%s' must be a character vector",
      var_data$variable_id
    ), call. = FALSE)
  }

  # Set default for variable_role if not specified
  if (is.null(var_data$variable_role)) {
    var_data$variable_role <- "substantive"
  }

  # Create and return object
  structure(
    var_data,
    class = c("qt_qvar", "qt_variable")
  )
}

#' @rdname variable-constructors
#' @export
qt_make_ctrlvar <- function(var_data) {
  # Handle variable_type → storage_type alias (per schema for control/generated banks)
  if (!is.null(var_data$variable_type) && is.null(var_data$storage_type)) {
    var_data$storage_type <- var_data$variable_type
  }

  # Handle response_type → storage_type mapping
  if (!is.null(var_data$response_type) && is.null(var_data$storage_type)) {
    var_data$storage_type <- .qt_response_to_storage(var_data$response_type)
  }

  # Handle description → question_text alias (per schema for control/generated banks)
  if (!is.null(var_data$description) && is.null(var_data$question_text)) {
    var_data$question_text <- var_data$description
  }

  # Validate required fields
  required_fields <- c("variable_id", "title", "storage_type", "surveys_used")
  missing <- setdiff(required_fields, names(var_data))
  if (is.null(var_data$question_text)) {
    missing <- c(missing, "description (or question_text)")
  }

  if (length(missing) > 0) {
    stop(sprintf(
      "Missing required fields for control parameter '%s': %s",
      var_data$variable_id %||% "[unknown]",
      paste(missing, collapse = ", ")
    ), call. = FALSE)
  }

  # Validate type
  valid_types <- c("integer", "numeric", "factor", "character", "composite",
                   "multiple_response", "logical", "boolean")
  if (!var_data$storage_type %in% valid_types) {
    stop(sprintf(
      "Invalid storage_type '%s' for parameter '%s'. Must be one of: %s",
      var_data$storage_type, var_data$variable_id, paste(valid_types, collapse = ", ")
    ), call. = FALSE)
  }

  # Set default variable_role if not specified
  if (is.null(var_data$variable_role)) {
    var_data$variable_role <- "parameter"
  }

  # Conditional validation: factor variables must have value_label_id or value_labels_name (legacy)
  is_factor <- var_data$storage_type == "factor"
  has_value_labels <- !is.null(var_data$value_label_id) || !is.null(var_data$value_labels_name)
  if (is_factor && !has_value_labels) {
    stop(sprintf(
      "Factor parameter '%s' must specify 'value_label_id'",
      var_data$variable_id
    ), call. = FALSE)
  }
  # Normalize value_labels_name (legacy) → value_label_id for internal consistency
  if (is.null(var_data$value_label_id) && !is.null(var_data$value_labels_name)) {
    var_data$value_label_id <- var_data$value_labels_name
  }

  # Conditional validation: restricted access requires restriction_reason
  if (isTRUE(var_data$restricted_access) && is.null(var_data$restriction_reason)) {
    stop(sprintf(
      "Parameter '%s' has restricted_access=TRUE but missing 'restriction_reason'",
      var_data$variable_id
    ), call. = FALSE)
  }

  # Validate surveys_used is character vector
  if (!is.character(var_data$surveys_used)) {
    stop(sprintf(
      "Field 'surveys_used' for parameter '%s' must be a character vector",
      var_data$variable_id
    ), call. = FALSE)
  }

  # Create and return object
  structure(
    var_data,
    class = c("qt_ctrlvar", "qt_variable")
  )
}

#' @rdname variable-constructors
#' @export
qt_make_genvar <- function(var_data) {
  # Handle variable_type → storage_type alias (per schema for control/generated banks)
  if (!is.null(var_data$variable_type) && is.null(var_data$storage_type)) {
    var_data$storage_type <- var_data$variable_type
  }

  # Handle response_type → storage_type mapping
  if (!is.null(var_data$response_type) && is.null(var_data$storage_type)) {
    var_data$storage_type <- .qt_response_to_storage(var_data$response_type)
  }

  # Handle description → question_text alias (per schema for control/generated banks)
  if (!is.null(var_data$description) && is.null(var_data$question_text)) {
    var_data$question_text <- var_data$description
  }

  # Validate required fields
  required_fields <- c("variable_id", "title", "storage_type", "surveys_used")
  missing <- setdiff(required_fields, names(var_data))
  if (is.null(var_data$question_text)) {
    missing <- c(missing, "description (or question_text)")
  }

  if (length(missing) > 0) {
    stop(sprintf(
      "Missing required fields for generated variable '%s': %s",
      var_data$variable_id %||% "[unknown]",
      paste(missing, collapse = ", ")
    ), call. = FALSE)
  }

  # Validate type
  valid_types <- c("integer", "numeric", "factor", "character", "composite",
                   "multiple_response", "logical", "boolean")
  if (!var_data$storage_type %in% valid_types) {
    stop(sprintf(
      "Invalid storage_type '%s' for generated variable '%s'. Must be one of: %s",
      var_data$storage_type, var_data$variable_id, paste(valid_types, collapse = ", ")
    ), call. = FALSE)
  }

  # Set default variable_role if not specified
  if (is.null(var_data$variable_role)) {
    var_data$variable_role <- "generated"
  }

  # Conditional validation: factor variables must have value_label_id or value_labels_name (legacy)
  is_factor <- var_data$storage_type == "factor"
  has_value_labels <- !is.null(var_data$value_label_id) || !is.null(var_data$value_labels_name)
  if (is_factor && !has_value_labels) {
    stop(sprintf(
      "Factor variable '%s' must specify 'value_label_id'",
      var_data$variable_id
    ), call. = FALSE)
  }
  # Normalize value_labels_name (legacy) → value_label_id for internal consistency
  if (is.null(var_data$value_label_id) && !is.null(var_data$value_labels_name)) {
    var_data$value_label_id <- var_data$value_labels_name
  }

  # Conditional validation: restricted access requires restriction_reason
  if (isTRUE(var_data$restricted_access) && is.null(var_data$restriction_reason)) {
    stop(sprintf(
      "Generated variable '%s' has restricted_access=TRUE but missing 'restriction_reason'",
      var_data$variable_id
    ), call. = FALSE)
  }

  # Validate surveys_used is character vector
  if (!is.character(var_data$surveys_used)) {
    stop(sprintf(
      "Field 'surveys_used' for generated variable '%s' must be a character vector",
      var_data$variable_id
    ), call. = FALSE)
  }

  # Create and return object
  structure(
    var_data,
    class = c("qt_genvar", "qt_variable")
  )
}
