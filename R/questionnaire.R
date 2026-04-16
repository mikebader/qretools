# Questionnaire Rendering Functions
#
# Produces a human-readable Markdown questionnaire document from a survey
# config YAML file, preserving the full hierarchical structure (sections,
# logic blocks, splits, loops, modules, etc.).

# S3 Generic ------------------------------------------------------------------

#' Render a Human-Readable Questionnaire
#'
#' Produce a human-readable Markdown questionnaire document from a survey
#' config YAML file. The document shows the full survey flow including
#' sections, skip logic, splits, loops, response codes, and variable names.
#'
#' @param x Character string (path to survey config YAML) or a
#'   \code{qt_qreconfig} object.
#' @param survey_id Character string. Required. Survey identifier used to
#'   select version-specific question text and value labels. When no version
#'   matches \code{survey_id}, the last entry in the \code{versions} list
#'   is used (most recent).
#' @param config A \code{qt_config} object or NULL. If NULL, calls
#'   \code{qt_config()}.
#' @param mode Character string. Output mode: \code{"clean"} (default) for
#'   plain Markdown; \code{"full"} for Markdown with pandoc fenced divs
#'   using \code{.qre-*} and \code{.programming} CSS class markers.
#' @param output Character string. Output destination: \code{"print"}
#'   (default) sends to console; \code{"file"} writes to \code{output_file};
#'   \code{"return"} returns the character vector invisibly.
#' @param output_file Character string or NULL. Required when
#'   \code{output = "file"}.
#' @param indent_char Character string. Indentation unit. Default: two spaces.
#' @param ... Ignored.
#'
#' @return For \code{output = "return"}: invisibly returns a character vector
#'   of Markdown lines. For \code{output = "print"}: invisibly returns the
#'   character vector after printing. For \code{output = "file"}: invisibly
#'   returns the output file path.
#'
#' @details
#' The rendered questionnaire includes preload compute items, section
#' headers, questions with variable names and response options, skip
#' patterns (IF/ELSE), experimental splits, loop iterations with fill
#' substitution, randomized blocks with programmer begin/end markers,
#' modules expanded inline with membership notes, and select-all options
#' with generated variable names.
#'
#' Candidate questions from a \code{candidates/} directory next to the
#' survey config file are loaded automatically as a fallback. Candidate
#' modules, control parameters, and generated variables are not yet
#' supported; they must reside in the main banks.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' config      <- qt_config(project_root = "inst/examples")
#' survey_file <- "inst/examples/surveys/bas-example/design/survey-bas-example.yml"
#'
#' # Print to console
#' qt_render_questionnaire(survey_file, survey_id = "bas-example",
#'                          config = config)
#'
#' # Save to file
#' qt_render_questionnaire(survey_file, survey_id = "bas-example",
#'                          config = config, output = "file",
#'                          output_file = "questionnaire.md")
#'
#' # Return character vector
#' lines <- qt_render_questionnaire(survey_file, survey_id = "bas-example",
#'                                   config = config, output = "return")
#'
#' # Full mode with CSS class markers
#' qt_render_questionnaire(survey_file, survey_id = "bas-example",
#'                          config = config, mode = "full")
#'
#' # From an already-loaded qt_qreconfig object
#' qre <- qt_read_survey_config(survey_file, config = config)
#' qt_render_questionnaire(qre, survey_id = "bas-example", config = config)
#' }
qt_render_questionnaire <- function(x, ...) UseMethod("qt_render_questionnaire")


# Internal: Bank loading and output -------------------------------------------

# Load all banks needed for rendering. Mirrors the bank-loading block in
# qt_process.qt_qre() (R/builder.R). Returns a named list with survey_yaml,
# qbank, candidates, vlabs, and modbank.
#
# @keywords internal
# @noRd
.qt_render_load_banks <- function(survey_config_file, config) {
  survey_yaml <- .read_yaml_safe(survey_config_file, "survey config")
  if (is.null(survey_yaml)) {
    stop("Could not read survey config: ", survey_config_file, call. = FALSE)
  }

  vlabs    <- qt_read_value_labels(config)
  qbank    <- qt_read_question_bank(config, value_labels = vlabs)
  ctrlbank <- qt_read_control_parameters(config, value_labels = vlabs)
  modbank  <- qt_read_module_bank(config)

  candidates_rel  <- survey_yaml$meta$candidates_path %||% "candidates"
  candidates_path <- file.path(dirname(survey_config_file), candidates_rel)
  candidates      <- .qt_read_candidates_safe(candidates_path, config,
                                              value_labels = vlabs)

  list(survey_yaml = survey_yaml,
       qbank       = qbank,
       candidates  = candidates,
       vlabs       = vlabs,
       modbank     = modbank)
}


# Route rendered lines to console, file, or return value.
#
# @keywords internal
# @noRd
.qt_render_output <- function(lines, output, output_file) {
  if (output == "print") {
    cat(paste(lines, collapse = "\n"), "\n")
    invisible(lines)
  } else if (output == "file") {
    writeLines(lines, output_file)
    invisible(output_file)
  } else {
    invisible(lines)
  }
}


# Methods ---------------------------------------------------------------------

#' @rdname qt_render_questionnaire
#' @export
qt_render_questionnaire.character <- function(
    x,
    survey_id,
    config      = NULL,
    mode        = c("clean", "full"),
    output      = c("print", "file", "return"),
    output_file = NULL,
    indent_char = "  ",
    ...
) {
  mode   <- match.arg(mode)
  output <- match.arg(output)

  if (missing(survey_id) || !nzchar(as.character(survey_id))) {
    stop("'survey_id' is required", call. = FALSE)
  }
  if (output == "file" && (is.null(output_file) || !nzchar(output_file))) {
    stop("'output_file' is required when output = 'file'", call. = FALSE)
  }
  if (!file.exists(x)) {
    stop("Survey config file not found: ", x, call. = FALSE)
  }

  if (is.null(config)) config <- qt_config()

  banks <- .qt_render_load_banks(x, config)

  lines <- .qt_render_questionnaire_lines(
    survey_yaml = banks$survey_yaml,
    qbank       = banks$qbank,
    candidates  = banks$candidates,
    vlabs       = banks$vlabs,
    modbank     = banks$modbank,
    survey_id   = survey_id,
    mode        = mode,
    indent_char = indent_char
  )

  .qt_render_output(lines, output, output_file)
}


# Internal: Main engine -------------------------------------------------------

# Build the full character vector of Markdown lines for a questionnaire.
#
# @keywords internal
# @noRd
.qt_render_questionnaire_lines <- function(survey_yaml, qbank, candidates,
                                           vlabs, modbank, survey_id, mode,
                                           indent_char) {
  meta <- survey_yaml$meta
  qre  <- survey_yaml$questionnaire

  lines <- c(
    paste0("# ", meta$title %||% meta$id),
    "",
    paste0("**Status:** ", meta$status %||% "unknown"),
    ""
  )

  if (!is.null(meta$controls_required) && length(meta$controls_required) > 0) {
    ctrl_note <- paste0("[Controls required: ",
                        paste(meta$controls_required, collapse = ", "), "]")
    lines <- c(lines, .qt_fenced_div(ctrl_note, "programming", mode), "")
  }

  if (!is.null(meta$programmer_note) && nzchar(meta$programmer_note)) {
    lines <- c(lines,
               .qt_fenced_div(paste0("[", meta$programmer_note, "]"),
                              "programming", mode),
               "")
  }

  # Preload computes
  if (length(qre$preload) > 0) {
    lines <- c(lines, .qt_render_preload(qre$preload, mode), "")
  }

  # Questionnaire items
  if (length(qre$items) > 0) {
    lines <- c(lines,
               .qt_render_items(qre$items, depth = 0,
                                qbank = qbank, candidates = candidates,
                                vlabs = vlabs, modbank = modbank,
                                survey_id = survey_id, mode = mode,
                                indent_char = indent_char))
  }

  lines
}


# Internal: Version resolution ------------------------------------------------

# Resolve the correct version of a question's text/labels for survey_id.
# Only fields present in a version entry override the base value.
# Falls back to the last version if survey_id matches none.
#
# @keywords internal
# @noRd
.qt_render_resolve_version <- function(qvar, survey_id, vlabs) {
  title          <- qvar$title
  question_text  <- qvar$question_text
  value_label_id <- qvar$value_label_id
  instruction    <- qvar$instruction
  note           <- qvar$note
  source         <- qvar$source %||% qvar$cite

  versions <- qvar$versions
  if (!is.null(versions) && length(versions) > 0) {
    matched <- NULL
    for (v in versions) {
      if (survey_id %in% (v$surveys_used %||% character())) {
        matched <- v
        break
      }
    }
    apply_ver <- matched %||% versions[[length(versions)]]
    if (!is.null(apply_ver$title))          title          <- apply_ver$title
    if (!is.null(apply_ver$question_text))  question_text  <- apply_ver$question_text
    if (!is.null(apply_ver$value_label_id)) value_label_id <- apply_ver$value_label_id
    if (!is.null(apply_ver$instruction))    instruction    <- apply_ver$instruction
  }

  resolved_labels <- NULL
  if (!is.null(value_label_id) && !is.null(vlabs)) {
    resolved_labels <- vlabs$labels[[value_label_id]]
  }

  list(title          = title,
       question_text  = question_text,
       value_label_id = value_label_id,
       resolved_labels = resolved_labels,
       instruction    = instruction,
       note           = note,
       source         = source)
}


# Internal: Item dispatcher ---------------------------------------------------

# Iterate over a list of YAML items and dispatch to the correct renderer.
#
# @keywords internal
# @noRd
.qt_render_items <- function(items, depth, qbank, candidates, vlabs, modbank,
                              survey_id, mode, indent_char) {
  lines <- character()
  for (item in items) {
    item_lines <- switch(
      item$item_type,
      "section"   = .qt_render_section(item, depth, qbank, candidates, vlabs,
                                        modbank, survey_id, mode, indent_char),
      "question"  = .qt_render_question_item(item, depth, qbank, candidates,
                                              vlabs, survey_id, mode,
                                              indent_char,
                                              module_id = NULL,
                                              if_condition_override = NULL),
      "statement" = .qt_render_statement(item, depth, mode, indent_char),
      "compute"   = .qt_render_compute(item, depth, mode, indent_char),
      "logic"     = .qt_render_logic(item, depth, qbank, candidates, vlabs,
                                      modbank, survey_id, mode, indent_char),
      "split"     = .qt_render_split(item, depth, qbank, candidates, vlabs,
                                      modbank, survey_id, mode, indent_char),
      "loop"      = .qt_render_loop(item, depth, qbank, candidates, vlabs,
                                     modbank, survey_id, mode, indent_char),
      "randomize" = .qt_render_randomize(item, depth, qbank, candidates, vlabs,
                                          modbank, survey_id, mode, indent_char),
      "display_together" = .qt_render_display_together(item, depth, qbank,
                                                         candidates, vlabs,
                                                         modbank, survey_id,
                                                         mode, indent_char),
      "module"    = .qt_render_module(item, depth, qbank, candidates, vlabs,
                                       modbank, survey_id, mode, indent_char),
      {
        warning("Unknown item_type '", item$item_type, "' — skipping",
                call. = FALSE)
        character()
      }
    )
    lines <- c(lines, item_lines)
  }
  lines
}


# Internal: Section renderer --------------------------------------------------

# @keywords internal
# @noRd
.qt_render_section <- function(item, depth, qbank, candidates, vlabs, modbank,
                                survey_id, mode, indent_char) {
  id    <- item$id    %||% ""
  title <- item$title %||% id
  hdr   <- paste0("## Section: ", title,
                  if (nzchar(id)) paste0(" {#", id, "}") else "")

  inner <- .qt_render_items(item$items %||% list(), depth = depth,
                             qbank = qbank, candidates = candidates,
                             vlabs = vlabs, modbank = modbank,
                             survey_id = survey_id, mode = mode,
                             indent_char = indent_char)

  .qt_fenced_div(c(hdr, "", inner), "qre-section", mode)
}


# Internal: Preload renderer --------------------------------------------------

# @keywords internal
# @noRd
.qt_render_preload <- function(preload_items, mode) {
  header <- "### Preload"
  lines  <- .qt_fenced_div(header, "qre-preload", mode)

  for (item in preload_items) {
    block <- c(paste0("**", item$id, "**"))
    if (!is.null(item$description) && nzchar(item$description))
      block <- c(block, paste0("> ", item$description))
    if (!is.null(item$command) && nzchar(item$command))
      block <- c(block, paste0("> Command: `", item$command, "`"))
    if (length(item$requires) > 0)
      block <- c(block, paste0("> Requires: ", paste(item$requires, collapse = ", ")))
    if (length(item$provides) > 0)
      block <- c(block, paste0("> Provides: ", paste(item$provides, collapse = ", ")))
    block <- c(block, "")
    lines <- c(lines, .qt_fenced_div(block, "programming", mode))
  }

  lines
}


# Internal: Statement renderer ------------------------------------------------

# @keywords internal
# @noRd
.qt_render_statement <- function(item, depth, mode, indent_char) {
  pfx  <- strrep(indent_char, depth)
  text <- item$text %||% ""
  c(paste0(pfx, text), "")
}


# Internal: Inline compute renderer ------------------------------------------

# @keywords internal
# @noRd
.qt_render_compute <- function(item, depth, mode, indent_char) {
  pfx   <- strrep(indent_char, depth)
  block <- c(paste0(pfx, "[COMPUTE: ", item$id %||% "", "]"))
  if (!is.null(item$description) && nzchar(item$description))
    block <- c(block, paste0(pfx, "[", item$description, "]"))
  if (!is.null(item$command) && nzchar(item$command))
    block <- c(block, paste0(pfx, "[Command: `", item$command, "`]"))
  if (length(item$provides) > 0)
    block <- c(block, paste0(pfx, "[Provides: ",
                              paste(item$provides, collapse = ", "), "]"))
  .qt_fenced_div(c(block, ""), "programming", mode)
}


#' @rdname qt_render_questionnaire
#' @export
qt_render_questionnaire.qt_qreconfig <- function(
    x,
    survey_id,
    config      = NULL,
    mode        = c("clean", "full"),
    output      = c("print", "file", "return"),
    output_file = NULL,
    indent_char = "  ",
    ...
) {
  source_file <- x$read_meta$source_file
  if (is.null(source_file)) {
    stop(
      "Cannot render from a qt_qreconfig with no source file. ",
      "Pass the survey config YAML path directly to qt_render_questionnaire().",
      call. = FALSE
    )
  }
  qt_render_questionnaire.character(
    source_file,
    survey_id   = survey_id,
    config      = config,
    mode        = mode,
    output      = output,
    output_file = output_file,
    indent_char = indent_char
  )
}


# Internal: Question renderers ------------------------------------------------

# Look up a variable from qbank/candidates and render it as a question block.
# Called from the item dispatcher and from module/loop renderers.
#
# @param module_id Character or NULL — adds "[Part of module: X]" note.
# @param if_condition_override Character or NULL — overrides the per-item and
#   bank-level if_condition (used by module question specs).
#
# @keywords internal
# @noRd
.qt_render_question_item <- function(item, depth, qbank, candidates, vlabs,
                                      survey_id, mode, indent_char,
                                      module_id = NULL,
                                      if_condition_override = NULL) {
  varname <- item$variable_id
  source  <- item$source %||% "qbank"

  qvar <- if (source == "candbank") {
    candidates$variables[[varname]]
  } else {
    qbank$variables[[varname]]
  }
  if (is.null(qvar) && source == "qbank" && !is.null(candidates)) {
    qvar <- candidates$variables[[varname]]
  }
  if (is.null(qvar)) {
    warning("Variable '", varname, "' not found — skipping", call. = FALSE)
    return(character())
  }

  resolved    <- .qt_render_resolve_version(qvar, survey_id, vlabs)
  if_cond     <- if_condition_override %||% item$if_condition %||%
                   qvar$if_condition
  prog_note   <- item$programmer_note

  .qt_render_question(varname, resolved, qvar, depth, mode, indent_char,
                      if_condition = if_cond, module_id = module_id,
                      programmer_note = prog_note)
}


# Render one question block: varname, question text, responses, source, notes.
#
# @keywords internal
# @noRd
.qt_render_question <- function(variable_id, resolved, qvar, depth, mode,
                                 indent_char, if_condition = NULL,
                                 module_id = NULL, programmer_note = NULL) {
  pfx   <- strrep(indent_char, depth)
  lines <- character()

  # Variable name + title
  varname_line <- paste0("**", variable_id, "**",
                         if (!is.null(resolved$title) && nzchar(resolved$title))
                           paste0(" \u2014 ", resolved$title)
                         else "")
  lines <- c(lines, .qt_fenced_div(paste0(pfx, varname_line),
                                    "qre-varname", mode))

  # Question text
  qt <- trimws(resolved$question_text %||% "")
  if (nzchar(qt)) {
    lines <- c(lines, .qt_fenced_div(paste0(pfx, qt),
                                      "qre-question-text", mode))
  }

  # Instruction (e.g. "Select all that apply.")
  instr <- trimws(resolved$instruction %||% "")
  if (nzchar(instr)) {
    lines <- c(lines, paste0(pfx, "*", instr, "*"))
  }

  # Response options
  if (qvar$storage_type == "multiple_response") {
    parts <- .qt_render_select_all_parts(qvar, mode)
    lines <- c(lines, .qt_indent(parts, depth + 1, indent_char))
  } else if (!is.null(resolved$resolved_labels)) {
    vl <- .qt_render_value_labels_block(resolved$resolved_labels, mode)
    lines <- c(lines, .qt_indent(vl, depth + 1, indent_char))
  } else if (qvar$storage_type == "character") {
    clen <- qvar$character_length
    tag  <- if (!is.null(clen)) paste0("[open text, max ", clen, " chars]")
            else "[open text]"
    lines <- c(lines, paste0(pfx, indent_char, tag))
  } else if (qvar$storage_type == "integer") {
    lines <- c(lines, paste0(pfx, indent_char, "[integer]"))
  } else if (qvar$storage_type == "numeric") {
    lines <- c(lines, paste0(pfx, indent_char, "[numeric]"))
  }

  # Source / citation
  src <- trimws(resolved$source %||% "")
  if (nzchar(src)) {
    lines <- c(lines,
               .qt_fenced_div(paste0(pfx, "[Source: ", src, "]"),
                              "qre-source", mode))
  }

  # Programmer-facing annotations
  prog <- character()
  if (!is.null(if_condition) && nzchar(if_condition))
    prog <- c(prog, paste0(pfx, "[Show if: ", if_condition, "]"))
  if (!is.null(module_id))
    prog <- c(prog, paste0(pfx, "[Part of module: ", module_id, "]"))
  if (!is.null(programmer_note) && nzchar(programmer_note))
    prog <- c(prog, paste0(pfx, "[", trimws(programmer_note), "]"))
  note <- trimws(resolved$note %||% "")
  if (nzchar(note))
    prog <- c(prog, paste0(pfx, "[", note, "]"))

  if (length(prog) > 0)
    lines <- c(lines, .qt_fenced_div(prog, "programming", mode))

  c(lines, "")
}


# Internal: Logic renderer ----------------------------------------------------

# @keywords internal
# @noRd
.qt_render_logic <- function(item, depth, qbank, candidates, vlabs, modbank,
                              survey_id, mode, indent_char) {
  pfx       <- strrep(indent_char, depth)
  condition <- item$condition %||% ""
  lines     <- character()

  lines <- c(lines,
             .qt_fenced_div(paste0(pfx, "[IF ", condition, "]"),
                            "programming", mode))

  if (length(item$then) > 0) {
    then_inner <- .qt_render_items(item$then, depth = depth + 1,
                                   qbank = qbank, candidates = candidates,
                                   vlabs = vlabs, modbank = modbank,
                                   survey_id = survey_id, mode = mode,
                                   indent_char = indent_char)
    lines <- c(lines, .qt_fenced_div(then_inner, "qre-logic-then", mode))
  }

  else_items <- item[["else"]]
  if (!is.null(else_items) && length(else_items) > 0) {
    lines <- c(lines,
               .qt_fenced_div(paste0(pfx, "[ELSE]"), "programming", mode))
    else_inner <- .qt_render_items(else_items, depth = depth + 1,
                                   qbank = qbank, candidates = candidates,
                                   vlabs = vlabs, modbank = modbank,
                                   survey_id = survey_id, mode = mode,
                                   indent_char = indent_char)
    lines <- c(lines, .qt_fenced_div(else_inner, "qre-logic-else", mode))
  }

  c(lines, "")
}


# Internal: Split renderer ----------------------------------------------------

# @keywords internal
# @noRd
.qt_render_split <- function(item, depth, qbank, candidates, vlabs, modbank,
                              survey_id, mode, indent_char) {
  pfx     <- strrep(indent_char, depth)
  ctrl_id <- item$control_id %||% ""
  lines   <- c("",
               .qt_fenced_div(paste0(pfx, "[SPLIT on ", ctrl_id, "]"),
                              "programming", mode))

  for (path in (item$paths %||% list())) {
    path_id  <- path$id            %||% ""
    ctrl_val <- path$control_value %||% ""
    path_note <- paste0(pfx, "[PATH ", path_id, ": ",
                        ctrl_id, " == \"", ctrl_val, "\"]")
    lines <- c(lines, "",
               .qt_fenced_div(path_note, "programming", mode))

    path_inner <- .qt_render_items(path$items %||% list(),
                                   depth = depth + 1,
                                   qbank = qbank, candidates = candidates,
                                   vlabs = vlabs, modbank = modbank,
                                   survey_id = survey_id, mode = mode,
                                   indent_char = indent_char)
    lines <- c(lines, .qt_fenced_div(path_inner, "qre-split-path", mode))
  }

  c(lines, "")
}


# Internal: Loop renderer -----------------------------------------------------

# Expands all iterations, substituting {i} in variable_id, title, and
# question_text. The bank is first queried with the template key (e.g.
# "feat_rating_{i}"); the rendered question shows the substituted ID.
#
# @keywords internal
# @noRd
.qt_render_loop <- function(item, depth, qbank, candidates, vlabs, modbank,
                             survey_id, mode, indent_char) {
  pfx         <- strrep(indent_char, depth)
  id_fills    <- item$id_fills    %||% character()
  title_fills <- item$title_fills %||% id_fills
  n           <- length(id_fills)

  loop_hdr <- paste0(pfx, "[LOOP ", item$id %||% "",
                     " \u2014 ", n, " iteration", if (n != 1) "s" else "",
                     ": ", paste(title_fills, collapse = ", "), "]")
  lines <- c("", .qt_fenced_div(loop_hdr, "programming", mode))

  for (i in seq_along(id_fills)) {
    id_fill    <- id_fills[i]
    title_fill <- title_fills[i]

    iter_note <- paste0(pfx, "[--- Iteration ", i, ": ", title_fill, " ---]")
    lines <- c(lines, "",
               .qt_fenced_div(iter_note, "programming", mode))

    for (tmpl_item in (item$items %||% list())) {
      if (identical(tmpl_item$item_type, "question")) {
        tmpl_id  <- tmpl_item$variable_id %||% ""
        rend_id  <- gsub("\\{i\\}", id_fill, tmpl_id)

        # Look up variable: try template key first, then substituted key
        qvar <- qbank$variables[[tmpl_id]] %||%
                qbank$variables[[rend_id]]
        if (is.null(qvar) && !is.null(candidates)) {
          qvar <- candidates$variables[[tmpl_id]] %||%
                  candidates$variables[[rend_id]]
        }
        if (is.null(qvar)) {
          warning("Variable '", tmpl_id, "' not found — skipping",
                  call. = FALSE)
          next
        }

        resolved <- .qt_render_resolve_version(qvar, survey_id, vlabs)
        resolved$title        <- gsub("\\{i\\}", title_fill,
                                       resolved$title        %||% "")
        resolved$question_text <- gsub("\\{i\\}", title_fill,
                                        resolved$question_text %||% "")

        lines <- c(lines,
                   .qt_render_question(rend_id, resolved, qvar, depth + 1,
                                       mode, indent_char,
                                       if_condition = tmpl_item$if_condition %||%
                                         qvar$if_condition,
                                       programmer_note = tmpl_item$programmer_note))
      } else {
        item_lines <- switch(
          tmpl_item$item_type,
          "statement" = .qt_render_statement(tmpl_item, depth + 1,
                                              mode, indent_char),
          "compute"   = .qt_render_compute(tmpl_item, depth + 1,
                                            mode, indent_char),
          character()
        )
        lines <- c(lines, item_lines)
      }
    }
  }

  c(lines, "")
}


# Internal: Randomize renderer ------------------------------------------------

# Emits BEGIN/END programmer notes around the randomized items, listing the
# variable IDs so programmers know which questions are in the block.
#
# @keywords internal
# @noRd
.qt_render_randomize <- function(item, depth, qbank, candidates, vlabs,
                                  modbank, survey_id, mode, indent_char) {
  pfx     <- strrep(indent_char, depth)
  blk_id  <- item$id %||% ""

  var_ids <- vapply(item$items %||% list(), function(it) {
    if (identical(it$item_type, "question")) it$variable_id %||% "" else ""
  }, character(1))
  var_ids <- var_ids[nzchar(var_ids)]

  begin <- paste0(pfx, "[BEGIN RANDOMIZED BLOCK: ", blk_id,
                  if (length(var_ids) > 0)
                    paste0(" \u2014 variables: ", paste(var_ids, collapse = ", "))
                  else "",
                  "]")
  end   <- paste0(pfx, "[END RANDOMIZED BLOCK: ", blk_id, "]")

  inner <- .qt_render_items(item$items %||% list(), depth = depth,
                             qbank = qbank, candidates = candidates,
                             vlabs = vlabs, modbank = modbank,
                             survey_id = survey_id, mode = mode,
                             indent_char = indent_char)

  c(.qt_fenced_div(begin, "programming", mode),
    "",
    inner,
    .qt_fenced_div(end, "programming", mode),
    "")
}


# Internal: Module renderer ---------------------------------------------------

# Expands module questions inline. Each question gets a "[Part of module: X]"
# programmer note. The module intro_text (if any) is shown first.
#
# @keywords internal
# @noRd
.qt_render_module <- function(item, depth, qbank, candidates, vlabs, modbank,
                               survey_id, mode, indent_char) {
  pfx       <- strrep(indent_char, depth)
  module_id <- item$module_id
  lines     <- character()

  if (is.null(modbank) || is.null(modbank$modules[[module_id]])) {
    warning("Module '", module_id, "' not found in module bank — skipping",
            call. = FALSE)
    return(character())
  }
  mod_def <- modbank$modules[[module_id]]

  # Intro text (survey-level override of module default)
  intro_text <- if (!is.null(item$intro_text)) {
    if (is.na(item$intro_text)) NULL else item$intro_text
  } else {
    mod_def$intro_text
  }
  if (!is.null(intro_text) && nzchar(trimws(intro_text))) {
    lines <- c(lines,
               .qt_fenced_div(paste0(pfx, "[Module intro: ",
                                     trimws(intro_text), "]"),
                              "programming", mode),
               "")
  }

  for (q_spec in (mod_def$questions %||% list())) {
    q_item <- list(item_type   = "question",
                   variable_id = q_spec$variable_id,
                   source      = q_spec$source %||% "qbank",
                   if_condition = NULL,
                   programmer_note = NULL)
    lines <- c(lines,
               .qt_render_question_item(q_item, depth, qbank, candidates,
                                        vlabs, survey_id, mode, indent_char,
                                        module_id = module_id,
                                        if_condition_override = q_spec$if_condition))
  }

  lines
}


# Internal: Display-together renderer -----------------------------------------

# @keywords internal
# @noRd
.qt_render_display_together <- function(item, depth, qbank, candidates, vlabs,
                                         modbank, survey_id, mode, indent_char) {
  pfx   <- strrep(indent_char, depth)
  hdr   <- paste0(pfx, "**DISPLAY TOGETHER: ", item$id %||% "", "**")
  inner <- .qt_render_items(item$items %||% list(), depth = depth + 1,
                             qbank = qbank, candidates = candidates,
                             vlabs = vlabs, modbank = modbank,
                             survey_id = survey_id, mode = mode,
                             indent_char = indent_char)
  c(.qt_fenced_div(c(hdr, "", inner), "qre-display-together", mode), "")
}


# Internal: Utility renderers -------------------------------------------------

# Render a factor's value labels as "[code] label" lines.
#
# @keywords internal
# @noRd
.qt_render_value_labels_block <- function(resolved_labels, mode) {
  if (is.null(resolved_labels) || length(resolved_labels$values) == 0)
    return(character())

  mapply(function(code, label) {
    if (mode == "full") {
      paste0("[", .qt_span(code, "factor-code"), "] ",
             .qt_span(label, "factor-label"))
    } else {
      paste0("[", code, "] ", label)
    }
  }, resolved_labels$values, resolved_labels$labels,
  SIMPLIFY = TRUE, USE.NAMES = FALSE)
}


# Render the variable_parts of a select_all (multiple_response) question.
#
# @keywords internal
# @noRd
.qt_render_select_all_parts <- function(qvar, mode) {
  # Accept both variable_parts and inline creates_variables named list
  parts_list <- qvar$variable_parts
  if (is.null(parts_list)) {
    cv <- qvar$creates_variables
    if (is.list(cv) && !is.null(names(cv)) && length(cv) > 0)
      parts_list <- cv
  }
  if (is.null(parts_list) || length(parts_list) == 0)
    return(character())

  lines <- character()
  for (part_id in names(parts_list)) {
    part <- parts_list[[part_id]]
    option_text <- if (is.character(part)) part
                   else part$option_text %||% part$option_title %||% part_id

    if (mode == "full") {
      line <- paste0("* ", .qt_span(option_text, "option-text"),
                     " [", .qt_span(part_id, "option-variable"), "]")
    } else {
      line <- paste0("* ", option_text, " [", part_id, "]")
    }
    lines <- c(lines, line)

    # other_specify sub-entry
    if (!is.character(part) && !is.null(part$other_specify)) {
      os     <- part$other_specify
      os_id  <- os$id %||% paste0(part_id, "_text")
      os_len <- os$character_length
      os_detail <- paste0("[character",
                          if (!is.null(os_len)) paste0(", max length ", os_len)
                          else "", "]")
      if (mode == "full") {
        other_line <- paste0("  -> ________ ", os_detail,
                             " [", .qt_span(os_id, "option-variable"), "]")
      } else {
        other_line <- paste0("  -> ________ ", os_detail, " [", os_id, "]")
      }
      lines <- c(lines, other_line)
    }
  }
  lines
}


# Wrap lines in a pandoc fenced div; pass through unchanged in clean mode.
#
# @keywords internal
# @noRd
.qt_fenced_div <- function(lines, class, mode) {
  if (mode != "full") return(lines)
  c(paste0("::: {.", class, "}"), lines, ":::")
}


# Wrap inline text in a pandoc span; pass through in clean mode.
#
# @keywords internal
# @noRd
.qt_span <- function(text, class) {
  if (is.null(text)) return("")
  paste0("[", text, "]{.", class, "}")
}


# Prepend indentation to non-empty lines.
#
# @keywords internal
# @noRd
.qt_indent <- function(lines, depth, indent_char) {
  pfx <- strrep(indent_char, depth)
  ifelse(nzchar(lines), paste0(pfx, lines), lines)
}
