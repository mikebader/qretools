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
    "---",
    paste0("title: ", meta$title %||% meta$id),
    paste0("status: ", meta$status %||% "unknown"),
    "---",
    ""
  )

  if (!is.null(meta$controls_required) && length(meta$controls_required) > 0) {
    ctrl_note <- paste0("[Controls required: ",
                        paste(meta$controls_required, collapse = ", "), "]")
    lines <- c(lines, .qt_fenced_div(ctrl_note, "qre-programming", mode), "")
  }

  if (!is.null(meta$programmer_note) && nzchar(meta$programmer_note)) {
    lines <- c(lines,
               .qt_fenced_div(paste0("[", meta$programmer_note, "]"),
                              "qre-programming", mode),
               "")
  }

  # Preload items
  if (length(qre$preload) > 0) {
    lines <- c(lines,
               .qt_render_preload(qre$preload, mode,
                                  qbank = qbank, candidates = candidates,
                                  vlabs = vlabs, survey_id = survey_id,
                                  indent_char = indent_char),
               "")
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

# Resolve the correct version of a question entry for rendering.
#
# Determines the effective version_id — from the explicit spec-stored value
# when provided, or by matching survey_id against each version's surveys_used
# (falling back to the last version when nothing matches) — then delegates all
# field merging to .qt_resolve_version() for cumulative traversal. Attaches
# resolved_labels as the only computed field not present in the YAML entry.
# All other fields flow through under their original YAML names with no
# hardcoded extraction.
#
# @keywords internal
# @noRd
.qt_render_resolve_version <- function(qvar, survey_id, vlabs,
                                        version_id = NULL) {
  effective_vid <- if (!is.null(version_id)) {
    version_id
  } else {
    .qt_effective_version_id(qvar, survey_id)
  }

  resolved <- .qt_resolve_version(qvar, effective_vid)

  resolved$resolved_labels <- if (!is.null(resolved$value_label_id) &&
                                   !is.null(vlabs)) {
    vlabs$labels[[resolved$value_label_id]]
  } else {
    NULL
  }

  resolved
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
.qt_render_preload <- function(preload_items, mode, qbank, candidates, vlabs,
                                survey_id, indent_char) {
  header <- "### Preload"
  lines  <- .qt_fenced_div(header, "qre-preload", mode)

  for (item in preload_items) {
    item_type  <- item$item_type %||% "compute"
    item_lines <- switch(
      item_type,

      "compute" = .qt_render_compute(item, depth = 0, mode, indent_char),

      "fill" = {
        id   <- item$id %||% ""
        desc <- trimws(item$description %||% "")
        fill_text <- if (mode == "full") {
          .qt_span(paste0("{{", id, "}}"), "qt-survey-control")
        } else {
          paste0("{{", id, "}}")
        }
        body <- if (nzchar(desc)) paste0(fill_text, " — ", desc) else fill_text
        .qt_fenced_div(body, "qre-programming", mode)
      },

      "question" = ,
      "factor"   = .qt_render_question_item(item, depth = 0, qbank, candidates,
                                             vlabs, survey_id, mode, indent_char),

      "statement" = .qt_render_statement(item, depth = 0, mode, indent_char),

      "character" = ,
      "integer"   = ,
      "numeric"   = {
        id   <- item$id %||% ""
        desc <- trimws(item$description %||% "")
        tag  <- .qt_storage_tag(item_type, item$validation,
                                 item$character_length)
        body <- paste0("**", id, "** ", tag,
                       if (nzchar(desc)) paste0(" — ", desc) else "")
        .qt_fenced_div(body, "qre-programming", mode)
      },

      {
        warning("Unknown preload item_type '", item_type, "' — skipping",
                call. = FALSE)
        character()
      }
    )
    lines <- c(lines, item_lines, "")
  }

  lines
}


# Internal: Statement renderer ------------------------------------------------

# @keywords internal
# @noRd
.qt_render_statement <- function(item, depth, mode, indent_char) {
  pfx  <- if (mode == "full") "" else strrep(indent_char, depth)
  text <- .qt_wrap_fills(item$text %||% "", mode)
  c(.qt_fenced_div(paste0(pfx, text), "qre-statement", mode), "")
}


# Internal: Inline compute renderer ------------------------------------------

# @keywords internal
# @noRd
.qt_render_compute <- function(item, depth, mode, indent_char) {
  pfx   <- if (mode == "full") "" else strrep(indent_char, depth)
  block <- paste0(pfx, "[COMPUTE: ", item$provides %||% "")
  if (!is.null(item$command) && nzchar(item$command))
    block <- c(block, "```", item$command, "```")
  block <- c(block, "]")
  .qt_fenced_div(c(block, ""), "qre-survey-control", mode)
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

  resolved    <- .qt_render_resolve_version(qvar, survey_id, vlabs,
                                             version_id = item$version)
  if_cond     <- if_condition_override %||% item$if_condition %||%
                   qvar$if_condition
  notes       <- trimws(c(resolved$programmer_note %||% character(),
                          item$programmer_note     %||% character()))
  prog_note   <- notes[nzchar(notes)]
  if (length(prog_note) == 0) prog_note <- NULL

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
  pfx   <- if (mode == "full") "" else strrep(indent_char, depth)
  lines <- character()

  # Variable name + title
  # varname_line <- paste0("**", variable_id, "**",
  #                        if (!is.null(resolved$title) && nzchar(resolved$title))
  #                          paste0(" \u2014 ", resolved$title)
  #                        else "")
  varname_line <- paste0("**", variable_id, "**")
  lines <- c(lines, .qt_fenced_div(paste0(pfx, varname_line),
                                    "qre-varname", mode))

  # Question text


  # qt <- trimws(resolved$question_text %||% "")
  qb <- character() # initialize question block
  if (!is.null(if_condition) && nzchar(if_condition)) {
    qb <- c(qb, paste0(pfx, .qt_span(
      paste0("[DISPLAY IF: ", if_condition, "]"), "qre-survey-control"
    )))
  }
  qt <- gsub("\n\n", "\\\\\n\\\\\n", resolved$question_text) %||% "" # Preserve internal line breaks without creating <p> elements
  qt <- .qt_wrap_fills(qt, mode)

  if (nzchar(qt)) {
    qb <- c(qb, .qt_span(paste0(pfx, qt), "qre-question-text"))
    lines <- c(lines, .qt_fenced_div(qb, "qre-question-block", mode))
  }

  # Instruction (e.g. "Select all that apply.")
  instr <- trimws(resolved$instruction %||% "")
  if (nzchar(instr)) {
    lines <- c(lines, paste0(pfx, "*", instr, "*"))
  }

  # Response options
  if (qvar$storage_type == "multiple_response") {
    parts <- .qt_render_select_all_parts(qvar, mode)
    lines <- c(lines, .qt_indent(parts, 0, indent_char))
  } else if (!is.null(resolved$resolved_labels)) {
    vl <- .qt_render_value_labels_block(resolved$resolved_labels, mode)
    lines <- c(lines, .qt_indent(vl, 0, indent_char))
  } else if (qvar$storage_type == "character") {
    tag <- .qt_storage_tag("character", qvar$validation, qvar$character_length,
                            qvar$string_label)
    lines <- c(lines, paste0(pfx, indent_char, strrep("_", 10), tag))
  } else if (qvar$storage_type == "integer") {
    tag <- .qt_storage_tag("integer", qvar$validation)
    lines <- c(lines, paste0(pfx, indent_char, strrep("_", 5), " ", tag))
  } else if (qvar$storage_type == "numeric") {
    tag <- .qt_storage_tag("numeric", qvar$validation)
    lines <- c(lines, paste0(pfx, indent_char, tag))
  }
  lines <- c(lines, "")

  # Source / citation
  src <- trimws(resolved$cite %||% "")
  if (nzchar(src)) {
    lines <- c(lines,
               .qt_fenced_div(paste0(pfx, "[Source: ", src, "]"),
                              "qre-cite", mode))
  }

  # Programmer-facing annotations
  prog <- character()
  for (note in (programmer_note %||% character())) {
    if (nzchar(note))
      prog <- c(prog, paste0(pfx, "[", note, "]"))
  }

  if (length(prog) > 0)
    lines <- c(lines, .qt_fenced_div(prog, "qre-programming", mode))

  if (mode == "full") {
    c("<div class=\"qre-question-unit\">", lines, "</div>", "")
  } else {
    c(lines, "")
  }
}


# Internal: Logic renderer ----------------------------------------------------

# @keywords internal
# @noRd
.qt_render_logic <- function(item, depth, qbank, candidates, vlabs, modbank,
                              survey_id, mode, indent_char) {
  pfx       <- if (mode == "full") "" else strrep(indent_char, depth)
  condition <- item$condition %||% ""
  lines     <- character()

  lines <- c(lines,
             .qt_span(paste0(pfx, "[IF ", condition, "] "),
                            "qre-survey-control"))

  if (length(item$then) > 0) {
    then_inner <- .qt_render_items(item$then,
                                   depth = depth + 0,
                                   # original (maybe not necessary):
                                   # depth = depth + 1,
                                   qbank = qbank, candidates = candidates,
                                   vlabs = vlabs, modbank = modbank,
                                   survey_id = survey_id, mode = mode,
                                   indent_char = indent_char)
    if(mode == "full") {
      lines <- c(
        lines,
        "<div class='qre-logic-then'>",
        then_inner,
        "</div>"
      )
    }
  }

  else_items <- item[["else"]]
  if (!is.null(else_items) && length(else_items) > 0) {
    lines <- c(lines,
               .qt_span(paste0(pfx, "[ELSE] "), "qre-survey-control"))
    else_inner <- .qt_render_items(else_items,
                                   depth = depth + 0,
                                   # original (maybe not necessary):
                                   # depth = depth + 1,
                                   qbank = qbank, candidates = candidates,
                                   vlabs = vlabs, modbank = modbank,
                                   survey_id = survey_id, mode = mode,
                                   indent_char = indent_char)
    if(mode == "full") {
      lines <- c(
        lines,
        "<div class='qre-logic-else'>",
        else_inner,
        "</div>"
      )
    }
  }

  if(mode == "full") {
    lines <- c(
      "<div class='qre-logic-block'>",
      lines,
      "[ENDIF]",
      "</div>"
    )
  }
}


# Internal: Split renderer ----------------------------------------------------

# @keywords internal
# @noRd
.qt_render_split <- function(item, depth, qbank, candidates, vlabs, modbank,
                              survey_id, mode, indent_char) {
  pfx     <- if (mode == "full") "" else strrep(indent_char, depth)
  ctrl_id <- item$control_id %||% ""
  lines   <- c("",
               .qt_fenced_div(paste0(pfx, "[SPLIT on ", ctrl_id, "]"),
                              "qre-programming", mode))

  for (path in (item$paths %||% list())) {
    path_id  <- path$id            %||% ""
    ctrl_val <- path$control_value %||% ""
    path_note <- paste0(pfx, "[PATH ", path_id, ": ",
                        ctrl_id, " == \"", ctrl_val, "\"]")
    lines <- c(lines, "",
               .qt_fenced_div(path_note, "qre-programming", mode))

    path_inner <- .qt_render_items(path$items %||% list(),
                                   depth = depth + 0,
                                   # original (maybe not necessary):
                                   # depth = depth + 1,
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
  pfx         <- if (mode == "full") "" else strrep(indent_char, depth)
  id_fills    <- item$id_fills    %||% character()
  title_fills <- item$title_fills %||% id_fills
  n           <- length(id_fills)

  loop_hdr <- paste0(pfx, "[LOOP ", item$id %||% "",
                     " \u2014 ", n, " iteration", if (n != 1) "s" else "",
                     ": ", paste(title_fills, collapse = ", "), "]")
  lines <- c("", .qt_fenced_div(loop_hdr, "qre-programming", mode))

  for (i in seq_along(id_fills)) {
    id_fill    <- id_fills[i]
    title_fill <- title_fills[i]

    iter_note <- paste0(pfx, "[--- Iteration ", i, ": ", title_fill, " ---]")
    lines <- c(lines, "",
               .qt_fenced_div(iter_note, "qre-programming", mode))

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

        loop_notes <- trimws(c(resolved$programmer_note %||% character(),
                                tmpl_item$programmer_note %||% character()))
        loop_notes <- loop_notes[nzchar(loop_notes)]
        if (length(loop_notes) == 0) loop_notes <- NULL

        lines <- c(lines,
                   .qt_render_question(rend_id, resolved, qvar,
                                       depth = depth + 0,
                                       # original (maybe not necessary):
                                       # depth = depth + 1,
                                       mode, indent_char,
                                       if_condition = tmpl_item$if_condition %||%
                                         qvar$if_condition,
                                       programmer_note = loop_notes))
      } else {
        item_lines <- switch(
          tmpl_item$item_type,
          "statement" = .qt_render_statement(tmpl_item,
                                             depth = depth + 0,
                                             # original (maybe not necessary):
                                             # depth = depth + 1,
                                              mode, indent_char),
          "compute"   = .qt_render_compute(tmpl_item,
                                           depth = depth + 0,
                                           # original (maybe not necessary):
                                           # depth = depth + 1,
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
  pfx     <- if (mode == "full") "" else strrep(indent_char, depth)
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

  inner <- .qt_render_items(item$items %||% list(),
                            depth = depth + 0,
                            # original (maybe not necessary):
                            # depth = depth + 1,
                             qbank = qbank, candidates = candidates,
                             vlabs = vlabs, modbank = modbank,
                             survey_id = survey_id, mode = mode,
                             indent_char = indent_char)

  c(.qt_fenced_div(begin, "qre-programming", mode),
    "",
    inner,
    .qt_fenced_div(end, "qre-programming", mode),
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
  pfx       <- if (mode == "full") "" else strrep(indent_char, depth)
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
    intro_item <- list(item_type = "statement", text = trimws(intro_text))
    lines <- c(lines, .qt_render_statement(intro_item, depth, mode, indent_char))
  }

  for (q_spec in (mod_def$questions %||% list())) {
    q_item <- list(item_type   = "question",
                   variable_id = q_spec$variable_id,
                   source      = q_spec$cite %||% "qbank",
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
  inner <- .qt_render_items(item$items %||% list(),
                            depth = depth + 0,
                            # original (maybe not necessary):
                            # depth = depth + 1,
                             qbank = qbank, candidates = candidates,
                             vlabs = vlabs, modbank = modbank,
                             survey_id = survey_id, mode = mode,
                             indent_char = indent_char)
  c(.qt_fenced_div(inner, "qre-display-together", mode), "")
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
    label <- .qt_wrap_fills(label, mode)
    if (mode == "full") {
      paste0(
        "* ",
        .qt_span(paste0("[", code, "]"), "qre-factor-code"),
        " ",
        .qt_span(label, "qre-factor-label")
      )
    } else {
      paste0("* [", code, "] ", label)
    }
  }, resolved_labels$values, resolved_labels$labels,
  SIMPLIFY = FALSE, USE.NAMES = FALSE)
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
    option_text <- part$option_text %||% part$option_title %||% part_id
    if(grepl("^hlt_ediag", option_text)) print(part$option_text)
    option_text <- .qt_wrap_fills(option_text, mode)

    if (mode == "full") {
      line <- paste0("* ", .qt_span(option_text, "option-text"),
                     " ", .qt_span(paste0("[", part_id, "]"), "option-variable"))
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
  if (mode == "full" && length(lines) > 0) {
    return(c("<div class=\"qre-response-options\">", lines, "</div>"))
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


# Build a [type; validation] survey-control tag for scalar storage types.
#
# @keywords internal
# @noRd
.qt_storage_tag <- function(storage_type, validation = NULL,
                             character_length = NULL, string_label = NULL) {
  notes <- character()
  if (!is.null(character_length))
    notes <- c(notes, paste0("maximum ", character_length, " characters"))
  if (!is.null(validation) && nzchar(validation))
    notes <- c(notes, validation)
  note_str <- if (length(notes) > 0) paste0("; ", paste(notes, collapse = "; ")) else ""
  tag <- .qt_span(paste0("[", storage_type, note_str, "]"), "qre-survey-control")
  if (!is.null(string_label) && nzchar(string_label))
    tag <- paste(paste0("(", string_label, ")"), tag)
  tag
}


# Wrap inline text in a pandoc span; pass through in clean mode.
#
# @keywords internal
# @noRd
.qt_span <- function(text, class) {
  if (is.null(text)) return("")
  paste0("[", text, "]{.", class, "}")
}


# Wrap every {{fill}} pattern in a qt-survey-control span in full mode.
#
# @keywords internal
# @noRd
.qt_wrap_fills <- function(text, mode) {
  if (mode != "full" || is.null(text) || !nzchar(text)) return(text)
  gsub("(\\{\\{[^}]+\\}\\})", "[\\1]{.qt-survey-control}", text, perl = TRUE)
}


# Prepend indentation to non-empty lines.
#
# @keywords internal
# @noRd
.qt_indent <- function(lines, depth, indent_char) {
  pfx <- strrep(indent_char, depth)
  ifelse(nzchar(lines), paste0(pfx, lines), lines)
}


# Questionnaire Compilation ---------------------------------------------------

#' Compile a Questionnaire Markdown File to Styled HTML
#'
#' Converts a questionnaire Markdown file (produced by
#' \code{\link{qt_render_questionnaire}}) to a styled, self-contained HTML
#' document suitable for on-screen review and printing to PDF.
#'
#' @param input Character string. Path to the input Markdown file. If
#'   \code{NULL}, \code{qt_render_questionnaire()} is called first using
#'   \code{survey_config} and related arguments to produce a temporary file.
#' @param output_file Character string. Path for the output HTML file.
#'   Defaults to replacing the \code{.md} extension of \code{input} with
#'   \code{.html}, or \code{"questionnaire.html"} when \code{input} is
#'   \code{NULL}.
#' @param pandoc_path Character string or NULL. Path to the pandoc executable.
#'   When NULL, uses \code{rmarkdown::find_pandoc()}.
#' @param filter_path Character string or NULL. Path to the Lua filter file
#'   (\code{qre-questionnaire.lua}). When NULL, looks in
#'   \code{system.file("pandoc", package = "qretools")}.
#' @param template_path Character string or NULL. Path to the HTML template
#'   file (\code{qre-questionnaire-template.html}). When NULL, looks in
#'   \code{system.file("pandoc", package = "qretools")}.
#' @param survey_config Character string or \code{qt_qreconfig} object.
#'   Passed to \code{qt_render_questionnaire()} when \code{input} is
#'   \code{NULL}. Ignored otherwise.
#' @param survey_id Character string. Passed to
#'   \code{qt_render_questionnaire()} when \code{input} is \code{NULL}.
#'   Ignored otherwise.
#' @param config A \code{qt_config} object or NULL. Passed to
#'   \code{qt_render_questionnaire()} when \code{input} is \code{NULL}.
#' @param ... Additional arguments passed to \code{qt_render_questionnaire()}
#'   when \code{input} is \code{NULL}.
#'
#' @return Invisibly returns \code{output_file}. Called for its side effect of
#'   writing the HTML file.
#'
#' @details
#' Requires pandoc (>= 2.11) to be installed and findable. The function calls
#' pandoc with \code{--lua-filter} and \code{--template}, so both the filter
#' and template must be present. When using the package-installed copies
#' (the default), they are located in \code{inst/pandoc/} and found via
#' \code{system.file()}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Compile an existing markdown file
#' qt_compile_questionnaire(
#'   input       = "questionnaire-bas-2026.md",
#'   output_file = "questionnaire-bas-2026.html"
#' )
#'
#' # Render and compile in one step
#' qt_compile_questionnaire(
#'   survey_config = "surveys/bas-2026/design/survey-bas-2026.yml",
#'   survey_id     = "bas-2026",
#'   output_file   = "questionnaire-bas-2026.html"
#' )
#' }
qt_compile_questionnaire <- function(
    input         = NULL,
    output_file   = NULL,
    pandoc_path   = NULL,
    filter_path   = NULL,
    template_path = NULL,
    survey_config = NULL,
    survey_id     = NULL,
    config        = NULL,
    ...
) {
  # --- Locate pandoc ----------------------------------------------------------
  if (is.null(pandoc_path)) {
    pandoc_info <- rmarkdown::find_pandoc()
    pandoc_path <- file.path(pandoc_info$dir, "pandoc")
    if (!nzchar(pandoc_info$dir) || !file.exists(pandoc_path)) {
      stop(
        "pandoc not found. Install pandoc or supply its path via 'pandoc_path'.",
        call. = FALSE
      )
    }
  }

  # --- Locate filter and template ---------------------------------------------
  pandoc_dir <- system.file("pandoc", package = "qretools")

  if (is.null(filter_path)) {
    filter_path <- file.path(pandoc_dir, "qre-questionnaire.lua")
  }
  if (!file.exists(filter_path)) {
    stop("Lua filter not found: ", filter_path, call. = FALSE)
  }

  if (is.null(template_path)) {
    template_path <- file.path(pandoc_dir, "qre-questionnaire-template.html")
  }
  if (!file.exists(template_path)) {
    stop("HTML template not found: ", template_path, call. = FALSE)
  }

  # --- Resolve input file -----------------------------------------------------
  tmp_md <- NULL
  if (is.null(input)) {
    if (is.null(survey_config) || is.null(survey_id)) {
      stop(
        "Provide either 'input' (a Markdown path) or both 'survey_config' ",
        "and 'survey_id' to render first.",
        call. = FALSE
      )
    }
    tmp_md <- tempfile(fileext = ".md")
    qt_render_questionnaire(
      survey_config,
      survey_id   = survey_id,
      config      = config,
      mode        = "full",
      output      = "file",
      output_file = tmp_md,
      ...
    )
    input <- tmp_md
    on.exit(unlink(tmp_md), add = TRUE)
  }

  if (!file.exists(input)) {
    stop("Input file not found: ", input, call. = FALSE)
  }

  # --- Resolve output file ----------------------------------------------------
  if (is.null(output_file)) {
    output_file <- sub("\\.md$", ".html", input, ignore.case = TRUE)
    if (identical(output_file, input)) output_file <- "questionnaire.html"
  }

  # --- Call pandoc ------------------------------------------------------------
  args <- c(
    input,
    "--lua-filter", filter_path,
    "--template",   template_path,
    "--output",     output_file,
    "--standalone"
  )

  result <- system2(pandoc_path, args = args, stdout = TRUE, stderr = TRUE)
  status <- attr(result, "status") %||% 0L

  if (status != 0L) {
    stop(
      "pandoc failed with status ", status, ":\n",
      paste(result, collapse = "\n"),
      call. = FALSE
    )
  }

  message("Questionnaire compiled to: ", output_file)
  invisible(output_file)
}
