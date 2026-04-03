# Survey Questionnaire Builder
#
# Functions for programmatically constructing survey questionnaire
# configurations. All builder functions follow the qt_add_* naming convention
# and support two calling modes:
#
#   Pipe mode:  qre |> qt_add_question("dem_age")
#               First argument is a qt_qre object; returns modified qt_qre.
#
#   Spec mode:  qt_add_question("dem_age")
#               First argument is the item identifier; returns a plain item
#               spec list for use as a nested argument inside a container
#               function (e.g., inside qt_add_section()).
#
# Functions that only appear at the top level of a questionnaire
# (qt_add_section, qt_add_preload) operate in pipe mode only.

# Constructor ----------------------------------------------------------------

#' Create a Survey Questionnaire Object
#'
#' Constructs the root object for programmatically building a survey
#' questionnaire. The resulting `qt_qre` object can be built up using
#' `qt_add_section()`, `qt_add_preload()`, and other `qt_add_*` functions,
#' then processed with `qt_process()` or written to YAML with
#' `qt_write_yaml()`.
#'
#' @param id Character string. Survey identifier. Should match the survey's
#'   directory name (e.g., `"bas-2026"`).
#' @param title Character string. Full survey title.
#' @param status Character string. One of `"draft"` or `"final"`.
#' @param short_title Character string or NULL. Abbreviated title.
#' @param version Character string or NULL. Version identifier.
#' @param organization Character string or NULL. Name of fielding organization.
#' @param author Character string or NULL. Creator name.
#' @param description Character string or NULL. Survey description.
#' @param controls_required Character vector or NULL. Control parameter IDs
#'   that must be loaded for the survey (e.g., `c("YEAR", "JURISDICTION")`).
#' @param programmer_note Character string or NULL. Note to survey programmer.
#' @param note Character string or NULL. Implementation notes.
#'
#' @return An S3 object of class `qt_qre`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' qre <- qt_qre("bas-2026", "Baltimore Area Survey 2026", "draft",
#'               organization = "Johns Hopkins 21st Century Cities Initiative",
#'               controls_required = c("YEAR", "JURISDICTION"))
#' }
qt_qre <- function(id, title, status = c("draft", "final"),
                   short_title = NULL, version = NULL, organization = NULL,
                   author = NULL, description = NULL,
                   candidates_path = NULL,
                   controls_required = NULL,
                   programmer_note = NULL, note = NULL) {

  status <- match.arg(status)

  if (missing(id) || !nzchar(as.character(id)))
    stop("'id' is required", call. = FALSE)
  if (missing(title) || !nzchar(as.character(title)))
    stop("'title' is required", call. = FALSE)

  meta <- list(id = id, title = title, status = status)
  if (!is.null(short_title))       meta$short_title        <- short_title
  if (!is.null(version))           meta$version            <- version
  if (!is.null(organization))      meta$organization       <- organization
  if (!is.null(author))            meta$author             <- author
  if (!is.null(description))       meta$description        <- description
  if (!is.null(controls_required)) meta$controls_required  <- as.list(controls_required)
  if (!is.null(programmer_note))   meta$programmer_note    <- programmer_note
  if (!is.null(note))              meta$note               <- note

  structure(
    list(
      meta          = meta,
      questionnaire = list(
        preload = list(),
        items   = list()
      )
    ),
    class = "qt_qre"
  )
}


# Top-level adders (pipe mode only) ------------------------------------------

#' Add Preload Compute Items to a Survey
#'
#' Adds one or more compute items to the `preload` section of a survey
#' questionnaire. Only compute items (created with `qt_add_compute()` in
#' spec mode) are permitted in preload.
#'
#' @param qre A `qt_qre` object.
#' @param ... Compute item specs. Each must be created by `qt_add_compute()`
#'   in spec mode (i.e., without a `qt_qre` first argument).
#'
#' @return Invisibly returns the modified `qt_qre` object.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' qre <- qt_qre("bas-2026", "BAS 2026", "draft") |>
#'   qt_add_preload(
#'     qt_add_compute(
#'       id          = "assign_condition",
#'       description = "Assign A/B condition",
#'       command     = "condition <- sample(c('A', 'B'), 1)",
#'       provides    = "condition"
#'     )
#'   )
#' }
qt_add_preload <- function(qre, ...) {
  if (!inherits(qre, "qt_qre"))
    stop("First argument must be a qt_qre object", call. = FALSE)

  items <- list(...)
  for (i in seq_along(items)) {
    if (!identical(items[[i]]$item_type, "compute"))
      stop("preload only accepts compute items; item ", i,
           " has item_type '", items[[i]]$item_type, "'",
           call. = FALSE)
  }

  qre$questionnaire$preload <- c(qre$questionnaire$preload, items)
  invisible(qre)
}


#' Add a Section to a Survey
#'
#' Adds a section container to a survey questionnaire. Sections are the
#' primary top-level organizer. All item types can appear inside a section
#' as `...` arguments in spec mode.
#'
#' @param qre A `qt_qre` object.
#' @param id Character string. Unique section identifier.
#' @param title Character string. Section title.
#' @param ... Item specs for the section's contents. Each must be a spec-mode
#'   call to a `qt_add_*` function (e.g., `qt_add_question("dem_age")`).
#' @param description Character string or NULL. Section description.
#' @param note Character string or NULL. Implementation notes.
#'
#' @return Invisibly returns the modified `qt_qre` object.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' qre <- qt_qre("bas-2026", "BAS 2026", "draft") |>
#'   qt_add_section("demographics", "About You",
#'     qt_add_question("dem_age"),
#'     qt_add_question("dem_race")
#'   )
#' }
qt_add_section <- function(qre, id, title, ...,
                           description = NULL, note = NULL) {
  if (!inherits(qre, "qt_qre"))
    stop("First argument must be a qt_qre object", call. = FALSE)
  if (missing(id) || !nzchar(id))
    stop("'id' is required", call. = FALSE)
  if (missing(title) || !nzchar(title))
    stop("'title' is required", call. = FALSE)

  spec <- list(item_type = "section", id = id, title = title,
               items = list(...))
  if (!is.null(description)) spec$description <- description
  if (!is.null(note))        spec$note        <- note

  qre$questionnaire$items <- c(qre$questionnaire$items, list(spec))
  invisible(qre)
}


# Dual-mode item constructors ------------------------------------------------
#
# All functions below dispatch on whether .x is a qt_qre (pipe mode) or not
# (spec mode). Internal *_spec helpers build the plain list in both cases.

#' Add a Question Item
#'
#' In pipe mode (`qre |> qt_add_question("dem_age")`), appends a question to
#' the top-level `questionnaire$items`. In spec mode
#' (`qt_add_question("dem_age")`), returns a plain item spec list for use
#' inside a container such as `qt_add_section()`.
#'
#' @param .x A `qt_qre` object (pipe mode) or a variable ID string (spec mode).
#' @param variable_id Character string. Variable key in the question bank.
#'   Required in pipe mode; supply as `.x` in spec mode.
#' @param source Character string. Bank to use: `"qbank"` (default) or
#'   `"candbank"`.
#' @param hide_question Logical. Hide question text? Useful inside
#'   `qt_add_display_together()`. Default `FALSE`.
#' @param if_condition Character string or NULL. Survey-specific display logic
#'   (R expression, e.g., `"dem_child == 1"`).
#' @param programmer_note Character string or NULL. Note to survey programmer.
#'
#' @return Pipe mode: invisibly returns the modified `qt_qre`. Spec mode:
#'   returns an item spec list.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Pipe mode
#' qre |> qt_add_question("dem_age")
#'
#' # Spec mode (inside a section)
#' qt_add_section("demographics", "About You",
#'   qt_add_question("dem_age"),
#'   qt_add_question("dem_race", if_condition = "dem_age >= 18")
#' )
#' }
qt_add_question <- function(.x, variable_id = NULL, source = "qbank",
                            hide_question = FALSE, if_condition = NULL,
                            programmer_note = NULL) {
  if (inherits(.x, "qt_qre")) {
    if (is.null(variable_id) || !nzchar(variable_id))
      stop("'variable_id' is required", call. = FALSE)
    spec <- .qt_question_spec(variable_id, source, hide_question,
                               if_condition, programmer_note)
    .x$questionnaire$items <- c(.x$questionnaire$items, list(spec))
    return(invisible(.x))
  }
  .qt_question_spec(.x, source, hide_question, if_condition, programmer_note)
}


#' Add a Module
#'
#' Inserts a predefined module (a reusable group of questions) into the
#' questionnaire. Modules are defined in the module bank and can be reused
#' across surveys.
#'
#' @param .x A `qt_qre` object (pipe mode) or a module ID string (spec mode).
#' @param id Character string. The module identifier from the module bank.
#' @param intro_text Character string or NULL. Override the module's default
#'   intro text. Use `NA` to suppress the intro text entirely.
#' @param note Character string or NULL. Implementation notes for the module.
#' @param programmer_note Character string or NULL. Note to survey programmer.
#'
#' @return Pipe mode: invisibly returns modified `qt_qre`. Spec mode: item
#'   spec list.
#'
#' @examples
#' \dontrun{
#' # Use module with default intro from module bank
#' qre |>
#'   qt_add_module("transportation_mode")
#'
#' # Override intro text for this survey
#' qre |>
#'   qt_add_module("transportation_mode",
#'                 intro_text = "Let's talk about how you travel around Baltimore.")
#'
#' # Suppress intro text entirely
#' qre |>
#'   qt_add_module("transportation_mode", intro_text = NA)
#' }
#' @export
qt_add_module <- function(.x = NULL, id = NULL,
                          intro_text = NULL,
                          note = NULL,
                          programmer_note = NULL) {
  if (inherits(.x, "qt_qre")) {
    # Pipe mode
    if (is.null(id) || !nzchar(id))
      stop("'id' is required", call. = FALSE)
    spec <- .qt_module_spec(id, intro_text, note, programmer_note)
    .x$questionnaire$items <- c(.x$questionnaire$items, list(spec))
    return(invisible(.x))
  }

  # Spec mode
  effective_id <- if (!is.null(.x) && is.character(.x) && length(.x) == 1) {
    .x
  } else {
    id
  }

  .qt_module_spec(effective_id, intro_text, note, programmer_note)
}


#' Add a Statement Item
#'
#' A statement is a block of text displayed to the respondent (e.g., an
#' introduction, transition, or closing message). In pipe mode, appends to
#' the top-level questionnaire items. In spec mode, returns an item spec list.
#'
#' @param .x A `qt_qre` object (pipe mode), a statement ID string (spec mode),
#'   or omitted entirely when calling in spec mode with named arguments.
#' @param id Character string or NULL. Unique statement identifier. If omitted
#'   in both pipe and spec mode, an ID is auto-generated from a hash of the
#'   text content.
#' @param text Character string. Statement text to display.
#' @param keep_with_next Logical. Display on same screen as next item?
#'   Default `FALSE`.
#' @param if_condition Character string or NULL. Display logic (R expression).
#' @param display_class Character string or NULL. Display styling class.
#' @param programmer_note Character string or NULL. Note to survey programmer.
#' @param note Character string or NULL. Implementation notes.
#'
#' @return Pipe mode: invisibly returns modified `qt_qre`. Spec mode: item
#'   spec list.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Spec mode inside a logic block
#' qt_add_logic("nhd_sat <= 2",
#'   id = "nhd_problems_logic",
#'   condition = "nhd_sat <= 2",
#'   then = list(qt_add_question("nhd_prob_main")),
#'   otherwise = list(
#'     qt_add_statement("nhd_positive_affirm",
#'                      text = "Thank you for sharing that.")
#'   )
#' )
#' }
qt_add_statement <- function(.x = NULL, id = NULL, text = NULL,
                             keep_with_next = FALSE, if_condition = NULL,
                             display_class = NULL, programmer_note = NULL,
                             note = NULL) {
  if (inherits(.x, "qt_qre")) {
    if (is.null(text) || !nzchar(text))
      stop("'text' is required", call. = FALSE)
    if (is.null(id) || !nzchar(id))
      id <- paste0("stmt_", substr(digest::digest(text), 1, 8))
    spec <- .qt_statement_spec(id, text, keep_with_next, if_condition,
                                display_class, programmer_note, note)
    .x$questionnaire$items <- c(.x$questionnaire$items, list(spec))
    return(invisible(.x))
  }
  # Spec mode: .x is the statement id; id param holds text if supplied
  # positionally, otherwise text param holds it.
  effective_id   <- if (!is.null(.x) && is.character(.x)) .x else id
  effective_text <- if (!is.null(.x) && is.character(.x)) id  else text
  if (is.null(effective_text) || !nzchar(effective_text))
    stop("'text' is required", call. = FALSE)
  # Auto-generate an id from the text content when none is provided
  if (is.null(effective_id) || !nzchar(effective_id))
    effective_id <- paste0("stmt_", substr(digest::digest(effective_text), 1, 8))
  .qt_statement_spec(effective_id, effective_text, keep_with_next,
                     if_condition, display_class, programmer_note, note)
}


#' Add a Compute Item
#'
#' A compute item executes an R expression during survey flow. In the
#' `preload` section, compute items run before any questions are presented.
#' In spec mode (for use in `qt_add_preload()`), all arguments must be named.
#'
#' @param .x A `qt_qre` object (pipe mode) or NULL (spec mode). In spec mode,
#'   omit `.x` and supply all arguments by name.
#' @param id Character string. Unique identifier.
#' @param description Character string. Description of what is computed.
#' @param command Character string. R expression to execute.
#' @param requires Character vector or NULL. Parameter names the command
#'   depends on.
#' @param provides Character vector or NULL. Parameter names the command
#'   creates or modifies (required if those parameters are used later in the
#'   survey flow).
#' @param programmer_note Character string or NULL. Note to survey programmer.
#' @param note Character string or NULL. Implementation notes.
#'
#' @return Pipe mode: invisibly returns modified `qt_qre`. Spec mode: item
#'   spec list.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' qre <- qt_qre("bas-2026", "BAS 2026", "draft") |>
#'   qt_add_preload(
#'     qt_add_compute(
#'       id          = "assign_condition",
#'       description = "Assign A/B experimental condition",
#'       command     = "condition <- sample(c('A', 'B'), 1)",
#'       provides    = "condition"
#'     )
#'   )
#' }
qt_add_compute <- function(.x = NULL, id = NULL, description = NULL,
                           command = NULL, requires = NULL, provides = NULL,
                           programmer_note = NULL, note = NULL) {
  if (inherits(.x, "qt_qre")) {
    spec <- .qt_compute_spec(id, description, command, requires, provides,
                              programmer_note, note)
    .x$questionnaire$items <- c(.x$questionnaire$items, list(spec))
    return(invisible(.x))
  }
  .qt_compute_spec(id, description, command, requires, provides,
                   programmer_note, note)
}


#' Add a Logic (Conditional) Block
#'
#' Routes survey flow based on an R expression. Items in `then` are shown
#' when the condition is true; items in `otherwise` are shown when false.
#' The `otherwise` argument serializes to `else:` in YAML, which is what the
#' reader and DDI schema expect.
#'
#' @param .x A `qt_qre` object (pipe mode) or a logic block ID string (spec
#'   mode). In spec mode, supply the ID as `.x` and all other arguments by
#'   name.
#' @param id Character string. Unique identifier for the logic block.
#' @param condition Character string. R expression to evaluate
#'   (e.g., `"dem_child == 1"`).
#' @param then List of item specs shown when `condition` is `TRUE`.
#' @param otherwise List of item specs shown when `condition` is `FALSE`.
#'   Serializes as `else:` in YAML.
#' @param note Character string or NULL. Implementation notes.
#' @param programmer_note Character string or NULL. Note to survey programmer.
#'
#' @return Pipe mode: invisibly returns modified `qt_qre`. Spec mode: item
#'   spec list.
#'
#' @export
qt_add_logic <- function(.x = NULL, id = NULL, condition = NULL,
                         then = list(), otherwise = list(),
                         note = NULL, programmer_note = NULL) {
  if (inherits(.x, "qt_qre")) {
    # Pipe mode: .x is qre, id must be provided
    if (is.null(id) || !nzchar(id))
      stop("'id' is required", call. = FALSE)
    spec <- .qt_logic_spec(id, condition, then, otherwise,
                           note, programmer_note)
    .x$questionnaire$items <- c(.x$questionnaire$items, list(spec))
    return(invisible(.x))
  }

  # Spec mode: .x is id (if provided positionally), otherwise use id parameter
  effective_id <- if (!is.null(.x) && is.character(.x) && length(.x) == 1) {
    .x
  } else {
    id
  }

  .qt_logic_spec(effective_id, condition, then, otherwise,
                 note, programmer_note)
}


#' Add a Loop Block
#'
#' Repeats a set of items for each element in a vector variable. Commonly used
#' for household rosters or repeated measurements.
#'
#' @param .x A `qt_qre` object (pipe mode) or a loop block ID string (spec mode).
#' @param id Character string. Unique identifier for the loop block.
#' @param over Character string. Name of the variable to loop over.
#' @param ... Item specs to repeat in each iteration.
#' @param note Character string or NULL. Implementation notes.
#' @param programmer_note Character string or NULL. Note to survey programmer.
#'
#' @return Pipe mode: invisibly returns modified `qt_qre`. Spec mode: item
#'   spec list.
#'
#' @export
qt_add_loop <- function(.x = NULL, id = NULL, over = NULL, ...,
                        note = NULL, programmer_note = NULL) {
  items <- list(...)

  if (inherits(.x, "qt_qre")) {
    # Pipe mode: .x is qre, id must be provided
    if (is.null(id) || !nzchar(id))
      stop("'id' is required", call. = FALSE)
    spec <- .qt_loop_spec(id, over, items, note, programmer_note)
    .x$questionnaire$items <- c(.x$questionnaire$items, list(spec))
    return(invisible(.x))
  }

  # Spec mode: determine effective_id
  effective_id <- if (!is.null(.x) && is.character(.x) && length(.x) == 1) {
    .x
  } else {
    # If .x is not a character id, it might be the first item
    if (!is.null(.x) && !is.character(.x)) {
      items <- c(list(.x), items)
    }
    id
  }

  .qt_loop_spec(effective_id, over, items, note, programmer_note)
}


#' Add a Randomize Block
#'
#' Randomizes the order of items or groups of items. Used for experimental
#' designs or to reduce order effects in questions.
#'
#' @param .x A `qt_qre` object (pipe mode) or a randomize block ID string (spec mode).
#' @param id Character string. Unique identifier for the randomize block.
#' @param ... Item specs to randomize.
#' @param note Character string or NULL. Implementation notes.
#' @param programmer_note Character string or NULL. Note to survey programmer.
#'
#' @return Pipe mode: invisibly returns modified `qt_qre`. Spec mode: item
#'   spec list.
#'
#' @export
qt_add_randomize <- function(.x = NULL, id = NULL, ...,
                             note = NULL, programmer_note = NULL) {
  items <- list(...)

  if (inherits(.x, "qt_qre")) {
    # Pipe mode: .x is qre, id must be provided
    if (is.null(id) || !nzchar(id))
      stop("'id' is required", call. = FALSE)
    spec <- .qt_randomize_spec(id, items, note, programmer_note)
    .x$questionnaire$items <- c(.x$questionnaire$items, list(spec))
    return(invisible(.x))
  }

  # Spec mode: determine effective_id
  effective_id <- if (!is.null(.x) && is.character(.x) && length(.x) == 1) {
    .x
  } else {
    # If .x is not a character id, it might be the first item
    if (!is.null(.x) && !is.character(.x)) {
      items <- c(list(.x), items)
    }
    id
  }

  .qt_randomize_spec(effective_id, items, note, programmer_note)
}


#' Add a Display-Together Block
#'
#' Groups items that should be displayed on the same screen or as a single
#' response block (e.g., a grid of related questions).
#'
#' @param .x A `qt_qre` object (pipe mode) or a block ID string (spec mode).
#' @param id Character string. Unique block identifier.
#' @param ... Item specs to display together.
#' @param note Character string or NULL. Implementation notes.
#' @param programmer_note Character string or NULL. Note to survey programmer.
#'
#' @return Pipe mode: invisibly returns modified `qt_qre`. Spec mode: item
#'   spec list.
#'
#' @export
qt_add_display_together <- function(.x = NULL, id = NULL, ...,
                                    note = NULL, programmer_note = NULL) {
  items <- list(...)

  if (inherits(.x, "qt_qre")) {
    # Pipe mode: .x is the qre object, id must be provided
    if (is.null(id) || !nzchar(id))
      stop("'id' is required", call. = FALSE)
    spec <- .qt_display_together_spec(id, items, note, programmer_note)
    .x$questionnaire$items <- c(.x$questionnaire$items, list(spec))
    return(invisible(.x))
  }

  # Spec mode: determine effective_id
  # If .x is a character string, use it as the id
  # Otherwise use the id parameter (for when id is passed as named argument)
  effective_id <- if (!is.null(.x) && is.character(.x) && length(.x) == 1) {
    .x
  } else {
    # If .x is not a character id, it might be the first item passed positionally
    # In that case, prepend it to items
    if (!is.null(.x) && !is.character(.x)) {
      items <- c(list(.x), items)
    }
    id
  }

  .qt_display_together_spec(effective_id, items, note, programmer_note)
}


#' Add a Split (Experimental Assignment) Block
#'
#' Randomly assigns respondents to experimental conditions with different
#' question paths. Each path is shown to a subset of respondents.
#'
#' @param .x A `qt_qre` object (pipe mode) or a split block ID string (spec mode).
#' @param id Character string. Unique identifier for the split block.
#' @param on Character string. Variable name to create for tracking assignment.
#' @param paths Named list. Each element is a list of item specs for that path.
#'   Names become the path identifiers.
#' @param note Character string or NULL. Implementation notes.
#' @param programmer_note Character string or NULL. Note to survey programmer.
#'
#' @return Pipe mode: invisibly returns modified `qt_qre`. Spec mode: item
#'   spec list.
#'
#' @export
qt_add_split <- function(.x = NULL, id = NULL, on = NULL, paths = list(),
                         note = NULL, programmer_note = NULL) {
  if (inherits(.x, "qt_qre")) {
    # Pipe mode: .x is qre, id must be provided
    if (is.null(id) || !nzchar(id))
      stop("'id' is required", call. = FALSE)
    spec <- .qt_split_spec(id, on, paths, note, programmer_note)
    .x$questionnaire$items <- c(.x$questionnaire$items, list(spec))
    return(invisible(.x))
  }

  # Spec mode: .x is id (if provided positionally), otherwise use id parameter
  effective_id <- if (!is.null(.x) && is.character(.x) && length(.x) == 1) {
    .x
  } else {
    id
  }

  .qt_split_spec(effective_id, on, paths, note, programmer_note)
}


#' Build a Split Path Spec
#'
#' Constructs a single path for use inside `qt_add_split()`. Not a dual-mode
#' function; always returns a plain list (spec mode only).
#'
#' @param id Character string. Unique path identifier.
#' @param control_value Character string. The value of the split's
#'   `control_id` that routes respondents to this path.
#' @param ... Item specs for this path.
#'
#' @return A plain list representing the path spec.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' qt_add_path("wording_a", control_value = "A",
#'   qt_add_question("com_attach_a"))
#' }
qt_add_path <- function(id, control_value, ...) {
  if (missing(id) || !nzchar(id))
    stop("'id' is required", call. = FALSE)
  if (missing(control_value))
    stop("'control_value' is required", call. = FALSE)
  list(id = id, control_value = control_value, items = list(...))
}


# Internal spec builders -----------------------------------------------------

#' @keywords internal
#' @noRd
.qt_question_spec <- function(variable_id, source = "qbank",
                               hide_question = FALSE, if_condition = NULL,
                               programmer_note = NULL) {
  if (is.null(variable_id) || !nzchar(as.character(variable_id)))
    stop("'variable_id' must be a non-empty string", call. = FALSE)

  spec <- list(item_type = "question", variable_id = variable_id)
  if (!identical(source, "qbank"))    spec$source          <- source
  if (isTRUE(hide_question))          spec$hide_question   <- TRUE
  if (!is.null(if_condition))         spec$if_condition    <- if_condition
  if (!is.null(programmer_note))      spec$programmer_note <- programmer_note
  spec
}

#' Create Module Spec (Internal)
#' @keywords internal
#' @noRd
.qt_module_spec <- function(id, intro_text = NULL, note = NULL,
                            programmer_note = NULL) {
  if (is.null(id) || !nzchar(as.character(id)))
    stop("Module 'id' is required", call. = FALSE)

  spec <- list(
    item_type = "module",
    module_id = id
  )

  # Store intro text override if provided
  if (!is.null(intro_text)) spec$intro_text <- intro_text
  if (!is.null(note)) spec$note <- note
  if (!is.null(programmer_note)) spec$programmer_note <- programmer_note

  spec
}

#' @keywords internal
#' @noRd
.qt_statement_spec <- function(id, text, keep_with_next = FALSE,
                                if_condition = NULL, display_class = NULL,
                                programmer_note = NULL, note = NULL) {
  if (is.null(id) || !nzchar(as.character(id)))
    stop("'id' must be a non-empty string", call. = FALSE)
  if (is.null(text) || !nzchar(as.character(text)))
    stop("'text' must be a non-empty string", call. = FALSE)

  spec <- list(item_type = "statement", id = id, text = text)
  if (isTRUE(keep_with_next))    spec$keep_with_next  <- TRUE
  if (!is.null(if_condition))    spec$if_condition    <- if_condition
  if (!is.null(display_class))   spec$display_class   <- display_class
  if (!is.null(programmer_note)) spec$programmer_note <- programmer_note
  if (!is.null(note))            spec$note            <- note
  spec
}

#' @keywords internal
#' @noRd
.qt_compute_spec <- function(id, description, command,
                              requires = NULL, provides = NULL,
                              programmer_note = NULL, note = NULL) {
  if (is.null(id) || !nzchar(as.character(id)))
    stop("'id' must be a non-empty string", call. = FALSE)
  if (is.null(description) || !nzchar(as.character(description)))
    stop("'description' is required", call. = FALSE)
  if (is.null(command) || !nzchar(as.character(command)))
    stop("'command' is required", call. = FALSE)

  spec <- list(item_type = "compute", id = id,
               description = description, command = command)
  if (!is.null(requires))        spec$requires        <- as.list(requires)
  if (!is.null(provides))        spec$provides        <- as.list(provides)
  if (!is.null(programmer_note)) spec$programmer_note <- programmer_note
  if (!is.null(note))            spec$note            <- note
  spec
}

#' @keywords internal
#' @noRd
.qt_logic_spec <- function(id, condition, then = list(),
                            otherwise = list(),
                            note = NULL, programmer_note = NULL) {
  if (is.null(id) || !nzchar(as.character(id)))
    stop("'id' is required for a logic block", call. = FALSE)
  if (is.null(condition) || !nzchar(as.character(condition)))
    stop("'condition' is required for a logic block", call. = FALSE)

  spec <- list(item_type = "logic", id = id, condition = condition,
               then = then)
  # Store 'otherwise' under the key "else" so that:
  #   (a) qt_write_yaml() serializes it as `else:` without any renaming, and
  #   (b) qt_process() passes it directly to the reader, which also accesses
  #       item[["else"]].
  if (length(otherwise) > 0) spec[["else"]] <- otherwise
  if (!is.null(note))            spec$note            <- note
  if (!is.null(programmer_note)) spec$programmer_note <- programmer_note
  spec
}

#' @keywords internal
#' @noRd
.qt_loop_spec <- function(id, id_fills, title_fills = NULL, items = list(),
                           note = NULL, programmer_note = NULL) {
  if (is.null(id) || !nzchar(as.character(id)))
    stop("'id' is required for a loop", call. = FALSE)
  if (is.null(id_fills) || length(id_fills) == 0)
    stop("'id_fills' is required for a loop", call. = FALSE)

  spec <- list(item_type = "loop", id = id,
               id_fills = as.list(id_fills), items = items)
  if (!is.null(title_fills)) spec$title_fills <- as.list(title_fills)
  if (!is.null(note))            spec$note            <- note
  if (!is.null(programmer_note)) spec$programmer_note <- programmer_note
  spec
}

#' @keywords internal
#' @noRd
.qt_randomize_spec <- function(id, items = list(), description = NULL,
                                note = NULL, programmer_note = NULL) {
  if (is.null(id) || !nzchar(as.character(id)))
    stop("'id' is required for a randomize block", call. = FALSE)

  spec <- list(item_type = "randomize", id = id, items = items)
  if (!is.null(description))     spec$description     <- description
  if (!is.null(note))            spec$note            <- note
  if (!is.null(programmer_note)) spec$programmer_note <- programmer_note
  spec
}

#' @keywords internal
#' @noRd
.qt_display_together_spec <- function(id, items = list(),
                                       note = NULL, programmer_note = NULL) {
  if (is.null(id) || !nzchar(as.character(id)))
    stop("'id' is required for a display_together block", call. = FALSE)

  spec <- list(item_type = "display_together", id = id, items = items)
  if (!is.null(note))            spec$note            <- note
  if (!is.null(programmer_note)) spec$programmer_note <- programmer_note
  spec
}

#' @keywords internal
#' @noRd
.qt_split_spec <- function(id, description, control_id, paths = list(),
                            note = NULL, programmer_note = NULL) {
  if (is.null(id) || !nzchar(as.character(id)))
    stop("'id' is required for a split", call. = FALSE)
  if (is.null(description) || !nzchar(as.character(description)))
    stop("'description' is required for a split", call. = FALSE)
  if (is.null(control_id) || !nzchar(as.character(control_id)))
    stop("'control_id' is required for a split", call. = FALSE)
  if (length(paths) < 2)
    stop("A split requires at least 2 paths", call. = FALSE)

  spec <- list(item_type = "split", id = id, description = description,
               control_id = control_id, paths = paths)
  if (!is.null(note))            spec$note            <- note
  if (!is.null(programmer_note)) spec$programmer_note <- programmer_note
  spec
}


# YAML serialization ---------------------------------------------------------

#' Write a Survey Questionnaire to YAML
#'
#' Serializes a `qt_qre` object to a YAML file that conforms to the qretools
#' survey configuration schema. The `otherwise` branch of logic blocks is
#' written as `else:` in the output, matching the schema and the reader.
#'
#' Directories are created automatically if they do not exist.
#'
#' @param qre A `qt_qre` object.
#' @param path Character string. File path to write. The `.yml` extension is
#'   conventional.
#' @param ... Additional arguments passed to [yaml::write_yaml()].
#'
#' @return Invisibly returns `path`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' qt_write_yaml(qre, "surveys/bas-2026/design/survey-bas-2026.yml")
#' }
qt_write_yaml <- function(qre, path, ...) {
  if (!inherits(qre, "qt_qre"))
    stop("'qre' must be a qt_qre object", call. = FALSE)
  if (missing(path) || !nzchar(path))
    stop("'path' is required", call. = FALSE)

  dir_path <- dirname(path)
  if (!dir.exists(dir_path))
    dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)

  out <- .qt_qre_to_list(qre)
  yaml::write_yaml(out, path, ...)
  invisible(path)
}


#' Convert a qt_qre Object to a Plain Serializable List
#'
#' Recursively converts the `qt_qre` structure to a plain list matching the
#' YAML survey config schema. Because logic specs already store their else
#' branch under the key `"else"` (see `.qt_logic_spec()`), no renaming is
#' needed here — the list is already in the right shape for serialization
#' and for the reader.
#'
#' @param qre A `qt_qre` object.
#' @return A plain list.
#'
#' @keywords internal
#' @noRd
.qt_qre_to_list <- function(qre) {
  out <- list(meta = qre$meta, questionnaire = list())

  if (length(qre$questionnaire$preload) > 0)
    out$questionnaire$preload <- qre$questionnaire$preload

  out$questionnaire$items <-
    lapply(qre$questionnaire$items, .qt_item_to_list)

  out
}

#' Recursively Prepare an Item Spec for Serialization
#'
#' Handles nested containers by recursively processing their `items` (and,
#' for splits, their `paths`). Simple items (question, module, statement,
#' compute) are returned as-is since they contain no nested item specs.
#'
#' @keywords internal
#' @noRd
.qt_item_to_list <- function(item) {
  switch(item$item_type,
    section = {
      item$items <- lapply(item$items, .qt_item_to_list)
      item
    },
    logic = {
      item$then <- lapply(item$then, .qt_item_to_list)
      if (!is.null(item[["else"]]))
        item[["else"]] <- lapply(item[["else"]], .qt_item_to_list)
      item
    },
    loop = {
      item$items <- lapply(item$items, .qt_item_to_list)
      item
    },
    randomize = {
      item$items <- lapply(item$items, .qt_item_to_list)
      item
    },
    display_together = {
      item$items <- lapply(item$items, .qt_item_to_list)
      item
    },
    split = {
      item$paths <- lapply(item$paths, function(p) {
        p$items <- lapply(p$items, .qt_item_to_list)
        p
      })
      item
    },
    # Simple items: question, module, statement, compute
    item
  )
}


# Processing -----------------------------------------------------------------

#' Process a Survey Questionnaire
#'
#' Expands a survey questionnaire into a flat `qt_qreconfig` object suitable
#' for analysis, codebook generation, and export. Accepts either a `qt_qre`
#' object (built programmatically) or a file path to a survey config YAML.
#'
#' When called on a `qt_qre` object, the YAML is written to `yaml_path`
#' before processing. This keeps a documented YAML artifact on disk (for
#' version control and DDI export) while allowing the `qt_qre` object to be
#' passed directly without a separate write step.
#'
#' @param x A `qt_qre` object or a character string file path.
#' @param ... Additional arguments (see methods).
#'
#' @return A `qt_qreconfig` object (same as `qt_read_survey_config()`).
#'
#' @export
qt_process <- function(x, ...) UseMethod("qt_process")


#' @rdname qt_process
#'
#' @param yaml_path Character string or NULL. Path to write the YAML file
#'   before processing. Defaults to a project-conventional path derived from
#'   the survey ID and the `qt_config()` path settings:
#'   `<surveys>/<id>/design/survey-<id>.yml`. Pass `NULL` to suppress writing.
#' @param config A `qt_config` object or NULL. If NULL, calls `qt_config()`.
#'
#' @export
qt_process.qt_qre <- function(x, yaml_path = .qt_default_yaml_path(x),
                               config = NULL, ...) {
  if (!is.null(yaml_path)) {
    qt_write_yaml(x, yaml_path)
    message("Survey YAML written to: ", yaml_path)
  }

  if (is.null(config)) config <- qt_config()

  survey_yaml <- .qt_qre_to_list(x)

  # Load value labels once; pass to all bank readers to avoid redundant I/O.
  vlabs    <- qt_read_value_labels(config)
  qbank    <- qt_read_question_bank(config, value_labels = vlabs)
  ctrlbank <- qt_read_control_parameters(config, value_labels = vlabs)
  modbank  <- qt_read_module_bank(config)

  # Candidates: look in the survey's design directory when yaml_path is known.
  candidates_rel  <- x$meta$candidates_path %||% "candidates"
  candidates_path <- if (!is.null(yaml_path)) {
    file.path(dirname(yaml_path), candidates_rel)
  } else {
    NULL
  }
  candidates <- .qt_read_candidates_safe(candidates_path, config,
                                         value_labels = vlabs)

  .qt_build_qreconfig(survey_yaml,
                       source_file = yaml_path,
                       config      = config,
                       qbank       = qbank,
                       candidates  = candidates,
                       ctrlbank    = ctrlbank,
                       modbank     = modbank)
}


#' @rdname qt_process
#'
#' @param config A `qt_config` object or NULL. If NULL, calls `qt_config()`.
#'
#' @export
qt_process.character <- function(x, config = NULL, ...) {
  qt_read_survey_config(x, config = config)
}


#' Derive the Default YAML Output Path for a qt_qre Object
#'
#' Constructs the conventional path
#' `<project_root>/<surveys>/<id>/design/survey-<id>.yml` from the survey ID
#' and the active `qt_config()`. Returns NULL if `qt_config()` is unavailable
#' (e.g., outside a qretools project).
#'
#' @param qre A `qt_qre` object.
#' @param config A `qt_config` object or NULL.
#' @return Character string path or NULL.
#'
#' @keywords internal
#' @noRd
.qt_default_yaml_path <- function(qre, config = NULL) {
  if (is.null(config)) {
    config <- tryCatch(qt_config(), error = function(e) NULL)
  }
  if (is.null(config)) return(NULL)

  file.path(
    config$meta$project_root,
    config$paths$surveys,
    qre$meta$id,
    config$dirnames$design,
    paste0("survey-", qre$meta$id, ".yml")
  )
}


# S3 methods for qt_qre ------------------------------------------------------

#' Print a Survey Questionnaire Object
#'
#' @param x A `qt_qre` object.
#' @param ... Additional arguments (ignored).
#' @return Invisibly returns `x`.
#' @export
print.qt_qre <- function(x, ...) {
  cat("Survey Questionnaire\n")
  cat(strrep("=", 50), "\n\n", sep = "")
  cat("ID:     ", x$meta$id,     "\n")
  cat("Title:  ", x$meta$title,  "\n")
  cat("Status: ", x$meta$status, "\n")

  n_preload  <- length(x$questionnaire$preload)
  top_items  <- x$questionnaire$items
  n_sections <- sum(vapply(top_items,
                           function(i) identical(i$item_type, "section"),
                           logical(1)))
  n_top      <- length(top_items)

  cat("\n")
  if (n_preload > 0)
    cat("Preload:", n_preload, "compute item(s)\n")

  if (n_sections > 0) {
    cat("Sections:", n_sections, "\n")
    for (s in top_items[vapply(top_items,
                               function(i) identical(i$item_type, "section"),
                               logical(1))]) {
      cat("  -", s$id, ":", s$title, "\n")
    }
  } else {
    cat("Top-level items:", n_top, "\n")
  }

  invisible(x)
}


#' Summarize a Survey Questionnaire Object
#'
#' @param object A `qt_qre` object.
#' @param ... Additional arguments (ignored).
#' @return Invisibly returns `object`.
#' @export
summary.qt_qre <- function(object, ...) {
  cat("Survey Questionnaire Summary\n")
  cat(strrep("=", 50), "\n\n", sep = "")

  cat("Survey Information:\n")
  cat("  ID:     ", object$meta$id,     "\n")
  cat("  Title:  ", object$meta$title,  "\n")
  cat("  Status: ", object$meta$status, "\n")
  if (!is.null(object$meta$organization))
    cat("  Org:    ", object$meta$organization, "\n")
  if (!is.null(object$meta$version))
    cat("  Version:", object$meta$version, "\n")
  cat("\n")

  cat("Content:\n")
  n_preload <- length(object$questionnaire$preload)
  if (n_preload > 0)
    cat("  Preload items:", n_preload, "\n")

  top_items <- object$questionnaire$items
  cat("  Top-level items:", length(top_items), "\n")

  sections <- top_items[vapply(top_items,
                               function(i) identical(i$item_type, "section"),
                               logical(1))]
  if (length(sections) > 0) {
    cat("\n  Sections:\n")
    for (s in sections) {
      n_items <- length(s$items)
      cat(sprintf("    %-20s %s (%d item%s)\n",
                  paste0(s$id, ":"), s$title,
                  n_items, if (n_items == 1) "" else "s"))
    }
  }

  if (!is.null(object$meta$controls_required)) {
    cat("\n  Controls required:",
        paste(unlist(object$meta$controls_required), collapse = ", "), "\n")
  }

  cat("\n")
  invisible(object)
}
