# R/promote.R
#
# Candidate promotion functions.
#
# Integrates candidate questions into the main question bank and updates
# `surveys_used` for all questions and modules referenced by a survey.


#' Promote Candidate Questions to the Question Bank
#'
#' After a survey is finalized, integrates candidate questions (those
#' referenced with `source: candbank` in the survey config) into the main
#' question bank, and updates `surveys_used` for all questions and modules
#' already in the bank.
#'
#' Two operations are performed for every question and module referenced by
#' the survey:
#' \enumerate{
#'   \item{**New candidates** (not yet in the bank) are appended to the
#'         matching bank YAML file. The target file mirrors the candidate
#'         file name (e.g., `candidates/neighborhoods.yml` maps to
#'         `banks/questions/neighborhoods.yml`). If the bank is a single
#'         file, new questions are appended there.}
#'   \item{**Existing questions and modules** (in bank or newly promoted)
#'         have the survey ID appended to their `surveys_used` list if it
#'         is not already present.}
#' }
#'
#' After promotion, the candidate YAML files containing promoted variables
#' are moved to `candidates/promoted/` and a `promoted.yml` tracking file
#' is written there.
#'
#' @note
#' Bank YAML files modified during promotion are re-serialised with
#' [yaml::as.yaml()]. This preserves all field values but may reformat
#' whitespace and field ordering compared to hand-edited files.
#'
#' @param survey_config_file Character string. Path to the survey
#'   configuration YAML file (e.g.,
#'   `"surveys/bas-2025/design/survey-bas-2025.yml"`).
#' @param config A `qt_config` object. Defaults to `qt_config()`.
#' @param dry_run Logical. If `TRUE`, report the planned changes without
#'   writing any files. Default `FALSE`.
#'
#' @return Invisibly returns a named list summarising the changes made (or
#'   planned in dry-run mode):
#'   \describe{
#'     \item{`survey_id`}{The survey ID from the config.}
#'     \item{`added`}{Character vector of variable IDs added to the bank.}
#'     \item{`update_qbank`}{Character vector of question IDs whose
#'       `surveys_used` was updated.}
#'     \item{`update_mbank`}{Character vector of module IDs whose
#'       `surveys_used` was updated.}
#'   }
#'
#' @seealso [qt_read_question_bank()], [qt_read_survey_config()]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Promote candidates and update surveys_used after finalizing bas-2025
#' qt_promote_candidates(
#'   "surveys/bas-2025/design/survey-bas-2025.yml"
#' )
#'
#' # Preview changes without writing any files
#' qt_promote_candidates(
#'   "surveys/bas-2025/design/survey-bas-2025.yml",
#'   dry_run = TRUE
#' )
#' }
qt_promote_candidates <- function(survey_config_file,
                                  config = qt_config(),
                                  dry_run = FALSE) {

  # --- 1. Load survey YAML ---
  if (!file.exists(survey_config_file))
    stop("Survey config not found: ", survey_config_file, call. = FALSE)

  survey_yaml <- .read_yaml_safe(survey_config_file, "survey config")

  survey_id <- survey_yaml$meta$id
  if (is.null(survey_id) || !nzchar(as.character(survey_id)))
    stop("Survey config missing required field 'meta.id'", call. = FALSE)

  # --- 2. Locate candidates directory ---
  cands_rel  <- survey_yaml$meta$candidates_path %||% "candidates"
  cands_path <- file.path(dirname(survey_config_file), cands_rel)

  # --- 3. Collect all item references from the survey questionnaire ---
  all_items <- .qt_collect_survey_items(survey_yaml$questionnaire$items)

  candbank_ids <- unique(Filter(Negate(is.null), lapply(all_items, function(x) {
    if (isTRUE(x$item_type == "question") &&
        isTRUE((x$source %||% "qbank") == "candbank"))
      x$variable_id
  })))

  qbank_ids <- unique(Filter(Negate(is.null), lapply(all_items, function(x) {
    if (isTRUE(x$item_type == "question") &&
        !isTRUE((x$source %||% "qbank") == "candbank"))
      x$variable_id
  })))

  module_ids <- unique(Filter(Negate(is.null), lapply(all_items, function(x) {
    if (isTRUE(x$item_type == "module")) x$module_id
  })))

  # --- 4. Read raw candidate YAML (no validation; preserves source_file) ---
  cand_vars <- .qt_read_raw_yaml_bank(cands_path)

  missing_cands <- setdiff(candbank_ids, names(cand_vars))
  if (length(missing_cands) > 0)
    stop("Candidate variable(s) not found in candidates directory ",
         sQuote(cands_path), ":\n  ",
         paste(missing_cands, collapse = ", "), call. = FALSE)

  # --- 5. Resolve bank paths and read existing IDs ---
  qbank_path <- .qt_resolve_path("question_bank", NULL, config)
  mbank_path <- .qt_resolve_path("module_bank",   NULL, config)

  existing_q_ids <- .qt_read_bank_ids(qbank_path)
  existing_m_ids <- .qt_read_bank_ids(mbank_path)

  # --- 6. Build plan ---
  new_ids      <- candbank_ids[!candbank_ids %in% existing_q_ids]
  cand_in_bank <- candbank_ids[ candbank_ids %in% existing_q_ids]

  # All questions to receive surveys_used update (candbank + qbank already in bank)
  update_qbank <- unique(c(
    cand_in_bank,
    qbank_ids[qbank_ids %in% existing_q_ids]
  ))
  update_mbank <- module_ids[module_ids %in% existing_m_ids]

  # Warn about references not found in bank (neither new nor existing)
  missing_q <- qbank_ids[!qbank_ids %in% existing_q_ids]
  if (length(missing_q) > 0)
    warning("Variable(s) referenced in survey but not found in question bank ",
            "(skipping surveys_used update):\n  ",
            paste(missing_q, collapse = ", "), call. = FALSE)

  missing_m <- module_ids[!module_ids %in% existing_m_ids]
  if (length(missing_m) > 0)
    warning("Module(s) referenced in survey but not found in module bank ",
            "(skipping surveys_used update):\n  ",
            paste(missing_m, collapse = ", "), call. = FALSE)

  plan <- list(
    survey_id    = survey_id,
    added        = new_ids,
    update_qbank = update_qbank,
    update_mbank = update_mbank
  )

  # --- 7. Report ---
  .qt_report_promotion_plan(plan, dry_run)

  if (dry_run)
    return(invisible(plan))

  # --- 8. Add new candidate variables to question bank ---
  for (vid in new_ids) {
    target_file <- .qt_candidate_to_bank_file(
      cand_vars[[vid]]$source_file, qbank_path
    )
    .qt_append_variable_to_bank(vid, cand_vars[[vid]], survey_id, target_file)
  }

  # --- 9. Update surveys_used in question bank ---
  for (vid in update_qbank) {
    .qt_add_survey_to_bank_entry(vid, survey_id, qbank_path)
  }

  # --- 10. Update surveys_used in module bank ---
  for (mid in update_mbank) {
    .qt_add_survey_to_bank_entry(mid, survey_id, mbank_path)
  }

  # --- 11. Move promoted candidate files and write tracking record ---
  if (length(candbank_ids) > 0) {
    promoted_dir <- file.path(cands_path, "promoted")
    .qt_move_promoted_candidates(cand_vars, candbank_ids, promoted_dir)
    .qt_write_promoted_yml(plan, promoted_dir)
  }

  invisible(plan)
}


# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

# Recursively collect all items from a survey's questionnaire items list.
# Traverses sections, logic blocks, splits, loops, randomize containers,
# and display_together blocks.
#
# @param items List of survey item nodes (from parsed survey YAML).
# @return Flat list of all item nodes found at any nesting depth.
# @keywords internal
# @noRd
.qt_collect_survey_items <- function(items) {
  if (is.null(items) || length(items) == 0) return(list())

  result <- list()
  for (item in items) {
    result <- c(result, list(item))

    # Standard nested items (section, loop, randomize, display_together)
    if (!is.null(item$items))
      result <- c(result, .qt_collect_survey_items(item$items))

    # Logic then/else branches
    if (!is.null(item$then))
      result <- c(result, .qt_collect_survey_items(item$then))
    if (!is.null(item[["else"]]))
      result <- c(result, .qt_collect_survey_items(item[["else"]]))

    # Split paths
    if (!is.null(item$paths)) {
      for (path_entry in item$paths)
        result <- c(result, .qt_collect_survey_items(path_entry$items))
    }
  }
  result
}


# Read all YAML files in a bank directory (or a single .yml file) without
# validation. Attaches `source_file` to each entry for later file-mapping.
#
# @param bank_path Character string. Path to a .yml file (without extension)
#   or a directory.
# @return Named list: variable_id -> raw list (with source_file field).
#   Empty list when path does not exist.
# @keywords internal
# @noRd
.qt_read_raw_yaml_bank <- function(bank_path) {
  all_vars <- list()

  if (file.exists(paste0(bank_path, ".yml"))) {
    content <- .read_yaml_safe(paste0(bank_path, ".yml"))
    if (!is.null(content)) {
      for (vid in names(content)) {
        content[[vid]]$source_file <- paste0(bank_path, ".yml")
        all_vars[[vid]] <- content[[vid]]
      }
    }

  } else if (dir.exists(bank_path)) {
    yml_files <- sort(list.files(bank_path, pattern = "\\.yml$",
                                 full.names = TRUE, recursive = FALSE))
    for (f in yml_files) {
      content <- .read_yaml_safe(f)
      if (!is.null(content)) {
        for (vid in names(content)) {
          content[[vid]]$source_file <- f
          all_vars[[vid]] <- content[[vid]]
        }
      }
    }
  }

  all_vars
}


# Return only the top-level keys (IDs) present in a bank path.
#
# @param bank_path Character string. Path to .yml file (no ext) or directory.
# @return Character vector of IDs.
# @keywords internal
# @noRd
.qt_read_bank_ids <- function(bank_path) {
  names(.qt_read_raw_yaml_bank(bank_path))
}


# Determine the target bank file for a new candidate variable.
#
# If the question bank is a single .yml file all new candidates are written
# there. If the bank is a directory the target file mirrors the candidate
# file name (e.g. candidates/neighborhoods.yml →
# banks/questions/neighborhoods.yml). The file need not exist yet; it will
# be created on write.
#
# @param source_file Character string. Full path to the candidate's YAML file.
# @param qbank_path Character string. Path to question bank (no .yml ext).
# @return Character string. Full path to the target bank file (.yml ext).
# @keywords internal
# @noRd
.qt_candidate_to_bank_file <- function(source_file, qbank_path) {
  if (file.exists(paste0(qbank_path, ".yml")))
    return(paste0(qbank_path, ".yml"))

  file.path(qbank_path, basename(source_file))
}


# Remove internal metadata fields added by the YAML reader before writing.
#
# @param var_data Named list. Raw variable data from .qt_read_raw_yaml_bank.
# @return The list without source_file, file_position, or variable_id.
# @keywords internal
# @noRd
.qt_strip_internal_fields <- function(var_data) {
  var_data[setdiff(names(var_data),
                   c("source_file", "file_position", "variable_id"))]
}


# Append a new variable definition to a bank YAML file.
#
# Strips internal metadata, ensures survey_id is in surveys_used, reads
# the existing file (creating it if absent), appends the new entry, then
# writes back.
#
# @param variable_id Character string. The variable ID (YAML dict key).
# @param var_data Named list. Raw variable data (may include internal fields).
# @param survey_id Character string. Survey ID to add to surveys_used.
# @param bank_file Character string. Full path to the target .yml file.
# @return Invisibly returns bank_file.
# @keywords internal
# @noRd
.qt_append_variable_to_bank <- function(variable_id, var_data,
                                        survey_id, bank_file) {
  clean <- .qt_strip_internal_fields(var_data)

  existing_surveys <- clean$surveys_used %||% character()
  if (!survey_id %in% existing_surveys)
    clean$surveys_used <- c(existing_surveys, survey_id)

  bank_content <- list()
  if (file.exists(bank_file))
    bank_content <- .read_yaml_safe(bank_file) %||% list()

  if (variable_id %in% names(bank_content))
    stop("Variable '", variable_id, "' already exists in '", bank_file, "'",
         call. = FALSE)

  bank_content[[variable_id]] <- clean

  dir.create(dirname(bank_file), showWarnings = FALSE, recursive = TRUE)
  .qt_write_yaml_bank(bank_content, bank_file)

  invisible(bank_file)
}


# Find the YAML file within a bank that contains a given entry ID.
#
# @param id Character string. The variable or module ID to find.
# @param bank_path Character string. Path to the bank file or directory.
# @return Character string (full path) or NULL if not found.
# @keywords internal
# @noRd
.qt_find_bank_file <- function(id, bank_path) {
  if (file.exists(paste0(bank_path, ".yml"))) {
    content <- .read_yaml_safe(paste0(bank_path, ".yml"))
    if (!is.null(content) && id %in% names(content))
      return(paste0(bank_path, ".yml"))
    return(NULL)
  }

  if (dir.exists(bank_path)) {
    for (f in sort(list.files(bank_path, pattern = "\\.yml$",
                               full.names = TRUE))) {
      content <- .read_yaml_safe(f)
      if (!is.null(content) && id %in% names(content))
        return(f)
    }
  }

  NULL
}


# Append survey_id to the surveys_used list of an entry in a bank YAML file.
# No-op if survey_id is already present.
#
# @param id Character string. Variable or module ID.
# @param survey_id Character string. Survey ID to add.
# @param bank_path Character string. Path to the bank file or directory.
# @return Invisibly returns the path to the modified file, or NULL if the
#   entry was not found or the survey was already listed.
# @keywords internal
# @noRd
.qt_add_survey_to_bank_entry <- function(id, survey_id, bank_path) {
  bank_file <- .qt_find_bank_file(id, bank_path)
  if (is.null(bank_file)) {
    warning("'", id, "' not found in bank '", bank_path,
            "' — skipping surveys_used update", call. = FALSE)
    return(invisible(NULL))
  }

  content <- .read_yaml_safe(bank_file)
  if (is.null(content) || !id %in% names(content))
    return(invisible(NULL))

  existing_surveys <- content[[id]]$surveys_used %||% character()
  if (survey_id %in% existing_surveys)
    return(invisible(NULL))

  content[[id]]$surveys_used <- c(existing_surveys, survey_id)
  .qt_write_yaml_bank(content, bank_file)

  invisible(bank_file)
}


# Write a named list as a YAML bank file with standard qretools formatting.
#
# @param content Named list. The complete bank content to serialise.
# @param file Character string. Full path to the output .yml file.
# @return Invisibly returns file.
# @keywords internal
# @noRd
.qt_write_yaml_bank <- function(content, file) {
  yaml_text <- yaml::as.yaml(content, indent = 2,
                              indent.mapping.sequence = FALSE)
  writeLines(yaml_text, file)
  invisible(file)
}


# Move candidate YAML files for promoted variables to a promoted/ directory.
#
# If a source file contains both promoted and non-promoted variables, the
# promoted entries are written to promoted/ and the original file is updated
# in-place to retain only the remaining entries. If all entries are promoted
# the original file is removed.
#
# @param cand_vars Named list. Raw candidate variables with source_file field.
# @param promoted_ids Character vector. Variable IDs being promoted.
# @param promoted_dir Character string. Destination directory (created if
#   needed).
# @return Invisibly NULL.
# @keywords internal
# @noRd
.qt_move_promoted_candidates <- function(cand_vars, promoted_ids,
                                         promoted_dir) {
  if (length(promoted_ids) == 0) return(invisible(NULL))

  dir.create(promoted_dir, showWarnings = FALSE, recursive = TRUE)

  # Group promoted IDs by their source file
  source_files <- unique(vapply(
    promoted_ids,
    function(vid) cand_vars[[vid]]$source_file,
    character(1)
  ))

  for (src_file in source_files) {
    if (!file.exists(src_file)) next

    file_content  <- .read_yaml_safe(src_file) %||% list()
    file_ids      <- names(file_content)
    promoted_here <- intersect(file_ids, promoted_ids)
    remaining     <- setdiff(file_ids, promoted_ids)

    dest_file <- file.path(promoted_dir, basename(src_file))
    .qt_write_yaml_bank(file_content[promoted_here], dest_file)

    if (length(remaining) == 0) {
      file.remove(src_file)
    } else {
      .qt_write_yaml_bank(file_content[remaining], src_file)
    }
  }

  invisible(NULL)
}


# Write a promoted.yml tracking file recording what was promoted and when.
#
# @param plan List. The promotion plan returned by qt_promote_candidates.
# @param promoted_dir Character string. Directory to write promoted.yml into.
# @return Invisibly returns the path to the written file.
# @keywords internal
# @noRd
.qt_write_promoted_yml <- function(plan, promoted_dir) {
  tracking <- list(
    survey_id      = plan$survey_id,
    promotion_date = format(Sys.Date(), "%Y-%m-%d"),
    added_to_bank  = as.list(plan$added),
    updated_qbank  = as.list(plan$update_qbank),
    updated_mbank  = as.list(plan$update_mbank)
  )

  out_file <- file.path(promoted_dir, "promoted.yml")
  .qt_write_yaml_bank(tracking, out_file)
  invisible(out_file)
}


# Report the promotion plan to the user via cli.
#
# @param plan List. The promotion plan.
# @param dry_run Logical. If TRUE, prefix messages with "(dry run)".
# @return Invisibly NULL.
# @keywords internal
# @noRd
.qt_report_promotion_plan <- function(plan, dry_run) {
  mode_label <- if (dry_run) " (dry run)" else ""
  cli::cli_h1("Candidate promotion: {plan$survey_id}{mode_label}")

  if (length(plan$added) > 0) {
    cli::cli_h2(
      "New variables to add to question bank ({length(plan$added)})"
    )
    for (vid in plan$added)
      cli::cli_li("{.val {vid}}")
  } else {
    cli::cli_alert_info("No new candidate variables to add to bank.")
  }

  if (length(plan$update_qbank) > 0) {
    cli::cli_h2(
      "Question bank: add {.val {plan$survey_id}} to surveys_used \\
       ({length(plan$update_qbank)})"
    )
    for (vid in plan$update_qbank)
      cli::cli_li("{.val {vid}}")
  }

  if (length(plan$update_mbank) > 0) {
    cli::cli_h2(
      "Module bank: add {.val {plan$survey_id}} to surveys_used \\
       ({length(plan$update_mbank)})"
    )
    for (mid in plan$update_mbank)
      cli::cli_li("{.val {mid}}")
  }

  invisible(NULL)
}
