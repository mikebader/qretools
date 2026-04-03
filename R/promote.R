# R/promote.R
#
# Candidate promotion and survey finalization.
#
# The workflow is intentionally two-step:
#
#   1. qt_promote_candidates() -- sort candidates into promoted/ and unused/
#      without touching the main bank.  Lets the researcher review the plan.
#
#   2. qt_finalize_survey()    -- read promoted/ and write to the main bank;
#      update surveys_used for every item referenced by the survey.
#      Calls qt_promote_candidates() automatically when promoted/ is absent.
#
# Candidate directory layout (relative to the survey config file):
#
#   candidates/               question candidates (flat .yml files)
#   candidates/control/       control-parameter candidates
#   candidates/generated/     generated-variable candidates
#   candidates/promoted/      written by step 1 (cleared by step 2)
#   candidates/promoted/questions/
#   candidates/promoted/control/
#   candidates/promoted/generated/
#   candidates/unused/        question candidates not referenced in survey
#   candidates/unused/questions/
#
# The root candidates/ path can be overridden with meta.candidates_path in
# the survey config YAML.


# ---------------------------------------------------------------------------
# Step 1: qt_promote_candidates
# ---------------------------------------------------------------------------

#' Sort Candidate Variables into Promoted and Unused Directories
#'
#' The first step of the two-step survey finalization workflow. Reads
#' candidate YAML files and sorts them into `candidates/promoted/` and
#' `candidates/unused/` based on whether they are referenced in the survey.
#' Does **not** modify the main question, control, or generated variable banks.
#'
#' **Question candidates** (flat `.yml` files in `candidates/`) are promoted
#' when they appear in the survey config with `source: candbank`.  Unreferenced
#' question candidates are relegated to `candidates/unused/`.
#'
#' **Control-parameter and generated-variable candidates** (in
#' `candidates/control/` and `candidates/generated/`) are all promoted; there
#' is no unused bucket for these types.
#'
#' When a promoted question candidate's `variable_id` already exists in the
#' main question bank the promoted entry is tagged `_bank_action: version_update`
#' (only version-relevant fields will be appended as a new `versions` entry by
#' [qt_finalize_survey()]).  Genuinely new questions are tagged
#' `_bank_action: add`.
#'
#' @param survey_config_file Character string. Path to the survey
#'   configuration YAML file.
#' @param config A `qt_config` object. Defaults to `qt_config()`.
#' @param dry_run Logical. If `TRUE`, report planned changes without writing
#'   any files. Default `FALSE`.
#'
#' @return Invisibly returns a named list summarising what was sorted:
#'   \describe{
#'     \item{`survey_id`}{Survey ID from the config.}
#'     \item{`questions`}{List with `$promoted` and `$relegated` ID vectors.}
#'     \item{`control`}{List with `$promoted` ID vector.}
#'     \item{`generated`}{List with `$promoted` ID vector.}
#'   }
#'
#' @seealso [qt_finalize_survey()]
#' @export
qt_promote_candidates <- function(survey_config_file,
                                  config = qt_config(),
                                  dry_run = FALSE) {

  survey_yaml <- .qt_load_survey_yaml(survey_config_file)
  survey_id   <- .qt_get_survey_id(survey_yaml)
  cands_base  <- .qt_candidates_base(survey_config_file, survey_yaml)

  promoted_base <- file.path(cands_base, "promoted")
  unused_base   <- file.path(cands_base, "unused")

  # Candidate references from survey questionnaire
  refs <- .qt_collect_cand_refs(survey_yaml$questionnaire$items)

  # Resolve bank paths to detect existing IDs (for version vs add decision)
  q_bank_ids <- .qt_read_bank_ids(
    .qt_resolve_path("question_bank", NULL, config))

  # --- Question candidates -------------------------------------------------
  q_result <- .qt_promote_bank_candidates(
    cand_path       = cands_base,
    used_ids        = refs$candbank,
    bank_ids        = q_bank_ids,
    survey_id       = survey_id,
    promoted_dir    = file.path(promoted_base, "questions"),
    unused_dir      = file.path(unused_base, "questions"),
    relegate_unused = TRUE,
    dry_run         = dry_run
  )

  # --- Control-parameter candidates ----------------------------------------
  ctrl_cand_path <- file.path(cands_base, "control")
  ctrl_bank_ids  <- .qt_read_bank_ids(
    .qt_resolve_path("control_bank", NULL, config))

  ctrl_result <- .qt_promote_bank_candidates(
    cand_path       = ctrl_cand_path,
    used_ids        = NULL,
    bank_ids        = ctrl_bank_ids,
    survey_id       = survey_id,
    promoted_dir    = file.path(promoted_base, "control"),
    unused_dir      = NULL,
    relegate_unused = FALSE,
    dry_run         = dry_run
  )

  # --- Generated-variable candidates ---------------------------------------
  gen_cand_path <- file.path(cands_base, "generated")
  gen_bank_ids  <- .qt_read_bank_ids(
    .qt_resolve_path("generated_bank", NULL, config))

  gen_result <- .qt_promote_bank_candidates(
    cand_path       = gen_cand_path,
    used_ids        = NULL,
    bank_ids        = gen_bank_ids,
    survey_id       = survey_id,
    promoted_dir    = file.path(promoted_base, "generated"),
    unused_dir      = NULL,
    relegate_unused = FALSE,
    dry_run         = dry_run
  )

  plan <- list(
    survey_id = survey_id,
    questions = q_result,
    control   = ctrl_result,
    generated = gen_result
  )

  .qt_report_promotion_plan(plan, dry_run)

  if (!dry_run)
    .qt_write_promoted_yml(plan, promoted_base)

  invisible(plan)
}


# ---------------------------------------------------------------------------
# Step 2: qt_finalize_survey
# ---------------------------------------------------------------------------

#' Finalize a Survey and Integrate Candidates into the Main Banks
#'
#' The second step of the two-step survey finalization workflow.  Reads files
#' written by [qt_promote_candidates()] from `candidates/promoted/` and
#' integrates them into the main question, control, and generated variable
#' banks.  Also appends the survey ID to `surveys_used` for every question,
#' module, control parameter, and generated variable referenced by the survey.
#'
#' [qt_promote_candidates()] is called automatically if `candidates/promoted/`
#' does not yet contain any `.yml` files.  Use `force_promote = TRUE` to
#' re-run it even when promoted files are already present.
#'
#' **Bank integration rules:**
#' \itemize{
#'   \item{Entries tagged `_bank_action: add` are appended as new variables.}
#'   \item{Entries tagged `_bank_action: version_update` are appended as a new
#'     `versions` entry on the existing bank question (version-specific fields
#'     only; base fields such as `storage_type` and `vargroup` are excluded).}
#' }
#'
#' @param survey_config_file Character string. Path to the survey config YAML.
#' @param config A `qt_config` object. Defaults to `qt_config()`.
#' @param force_promote Logical. If `TRUE`, always re-run
#'   [qt_promote_candidates()] even when `candidates/promoted/` already
#'   contains files. Default `FALSE`.
#' @param dry_run Logical. If `TRUE`, run [qt_promote_candidates()] in
#'   dry-run mode and skip bank integration. Default `FALSE`.
#'
#' @return Invisibly returns a named list summarising what was integrated:
#'   \describe{
#'     \item{`survey_id`}{Survey ID from the config.}
#'     \item{`questions`}{List with `$added` and `$versioned` ID vectors.}
#'     \item{`control`}{List with `$added` ID vector.}
#'     \item{`generated`}{List with `$added` ID vector.}
#'     \item{`surveys_used_updated`}{Character vector of all IDs whose
#'       `surveys_used` was updated.}
#'   }
#'
#' @seealso [qt_promote_candidates()]
#' @export
qt_finalize_survey <- function(survey_config_file,
                               config = qt_config(),
                               force_promote = FALSE,
                               dry_run = FALSE) {

  survey_yaml <- .qt_load_survey_yaml(survey_config_file)
  survey_id   <- .qt_get_survey_id(survey_yaml)
  cands_base  <- .qt_candidates_base(survey_config_file, survey_yaml)

  promoted_base <- file.path(cands_base, "promoted")

  # Run promotion step unless already done
  already_promoted <- dir.exists(promoted_base) &&
    length(list.files(promoted_base, pattern = "\\.yml$",
                      recursive = TRUE)) > 0

  if (!already_promoted || force_promote)
    qt_promote_candidates(survey_config_file, config, dry_run = dry_run)

  if (dry_run) {
    cli::cli_alert_info("Dry run: skipping bank integration.")
    return(invisible(NULL))
  }

  # Resolve bank paths
  qbank_path <- .qt_resolve_path("question_bank", NULL, config)
  ctrl_path  <- .qt_resolve_path("control_bank",  NULL, config)
  gen_path   <- .qt_resolve_path("generated_bank", NULL, config)
  mbank_path <- .qt_resolve_path("module_bank",   NULL, config)

  # Integrate promoted candidates into banks
  q_fin    <- .qt_finalize_bank(
    file.path(promoted_base, "questions"), qbank_path, survey_id)
  ctrl_fin <- .qt_finalize_bank(
    file.path(promoted_base, "control"),   ctrl_path,  survey_id)
  gen_fin  <- .qt_finalize_bank(
    file.path(promoted_base, "generated"), gen_path,   survey_id)

  # Update surveys_used for all items referenced by the survey
  all_items <- .qt_collect_survey_items(survey_yaml$questionnaire$items)

  q_ids <- unique(Filter(Negate(is.null), lapply(all_items, function(x) {
    if (isTRUE(x$item_type == "question")) x$variable_id
  })))
  mod_ids <- unique(Filter(Negate(is.null), lapply(all_items, function(x) {
    if (isTRUE(x$item_type == "module")) x$module_id
  })))

  existing_m_ids <- .qt_read_bank_ids(mbank_path)
  updated <- character()

  for (vid in q_ids) {
    result <- .qt_add_survey_to_bank_entry(vid, survey_id, qbank_path)
    if (!is.null(result)) updated <- c(updated, vid)
  }
  for (mid in mod_ids[mod_ids %in% existing_m_ids]) {
    result <- .qt_add_survey_to_bank_entry(mid, survey_id, mbank_path)
    if (!is.null(result)) updated <- c(updated, mid)
  }

  plan <- list(
    survey_id           = survey_id,
    questions           = q_fin,
    control             = ctrl_fin,
    generated           = gen_fin,
    surveys_used_updated = updated
  )

  .qt_report_finalization_plan(plan)
  invisible(plan)
}


# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

# Load and basic-validate a survey YAML file.
# @keywords internal
# @noRd
.qt_load_survey_yaml <- function(path) {
  if (!file.exists(path))
    stop("Survey config not found: ", path, call. = FALSE)
  .read_yaml_safe(path, "survey config")
}

# Extract and validate survey ID from parsed survey YAML.
# @keywords internal
# @noRd
.qt_get_survey_id <- function(survey_yaml) {
  id <- survey_yaml$meta$id
  if (is.null(id) || !nzchar(as.character(id)))
    stop("Survey config missing required field 'meta.id'", call. = FALSE)
  as.character(id)
}

# Resolve the candidates base directory from the survey config file path.
# @keywords internal
# @noRd
.qt_candidates_base <- function(survey_config_file, survey_yaml) {
  rel <- survey_yaml$meta$candidates_path %||% "candidates"
  file.path(dirname(survey_config_file), rel)
}

# Collect candidate-bank references from a survey questionnaire's item list.
# Returns a list with element $candbank (question IDs sourced from candbank).
# @keywords internal
# @noRd
.qt_collect_cand_refs <- function(items) {
  flat <- .qt_collect_survey_items(items)

  candbank <- unique(Filter(Negate(is.null), lapply(flat, function(x) {
    if (isTRUE(x$item_type == "question") &&
        isTRUE((x$source %||% "qbank") == "candbank"))
      x$variable_id
  })))

  list(candbank = candbank)
}

# Recursively collect all item nodes from a survey questionnaire items list.
# Traverses sections, logic, splits, loops, randomize, and display_together.
# @keywords internal
# @noRd
.qt_collect_survey_items <- function(items) {
  if (is.null(items) || length(items) == 0) return(list())

  result <- list()
  for (item in items) {
    result <- c(result, list(item))

    if (!is.null(item$items))
      result <- c(result, .qt_collect_survey_items(item$items))
    if (!is.null(item$then))
      result <- c(result, .qt_collect_survey_items(item$then))
    if (!is.null(item[["else"]]))
      result <- c(result, .qt_collect_survey_items(item[["else"]]))
    if (!is.null(item$paths)) {
      for (path_entry in item$paths)
        result <- c(result, .qt_collect_survey_items(path_entry$items))
    }
  }
  result
}

# Sort candidates from one bank type into promoted/ and unused/ directories.
#
# @param cand_path    Path to flat candidate directory (or single .yml).
# @param used_ids     Character vector of IDs referenced in survey, or NULL
#                     to promote everything.
# @param bank_ids     Character vector of IDs already in the main bank.
# @param survey_id    Survey ID string (used as fallback version_id).
# @param promoted_dir Directory to write promoted entries.
# @param unused_dir   Directory to write unused entries (or NULL).
# @param relegate_unused  Logical. If TRUE, write unused entries to unused_dir.
# @param dry_run      Logical.
# @return List with $promoted and $relegated character vectors.
# @keywords internal
# @noRd
.qt_promote_bank_candidates <- function(cand_path, used_ids, bank_ids,
                                        survey_id, promoted_dir, unused_dir,
                                        relegate_unused = TRUE,
                                        dry_run = FALSE) {

  cand_vars <- .qt_read_raw_yaml_bank(cand_path)

  if (length(cand_vars) == 0)
    return(list(promoted = character(), relegated = character()))

  all_ids <- names(cand_vars)

  # Determine which IDs are promoted vs relegated
  if (is.null(used_ids)) {
    promote_ids <- all_ids
    unused_ids  <- character()
  } else {
    missing_refs <- setdiff(used_ids, all_ids)
    if (length(missing_refs) > 0)
      stop("Candidate variable(s) referenced in survey but not found in ",
           sQuote(cand_path), ":\n  ",
           paste(missing_refs, collapse = ", "), call. = FALSE)

    promote_ids <- intersect(all_ids, used_ids)
    unused_ids  <- setdiff(all_ids, used_ids)
  }

  if (dry_run)
    return(list(promoted = promote_ids, relegated = unused_ids))

  # Write promoted entries, grouped by source file
  if (length(promote_ids) > 0) {
    dir.create(promoted_dir, showWarnings = FALSE, recursive = TRUE)

    by_file <- split(promote_ids, vapply(
      promote_ids,
      function(id) cand_vars[[id]]$source_file,
      character(1)
    ))

    for (src_file in names(by_file)) {
      promo_content <- list()
      for (id in by_file[[src_file]]) {
        entry <- .qt_strip_internal_fields(cand_vars[[id]])
        entry[["_bank_action"]] <- if (id %in% bank_ids)
          "version_update" else "add"
        promo_content[[id]] <- entry
      }
      dest_file <- file.path(promoted_dir, basename(src_file))
      .qt_write_yaml_bank(promo_content, dest_file)
    }
  }

  # Write unused entries (questions only)
  if (relegate_unused && length(unused_ids) > 0 && !is.null(unused_dir)) {
    dir.create(unused_dir, showWarnings = FALSE, recursive = TRUE)

    by_file <- split(unused_ids, vapply(
      unused_ids,
      function(id) cand_vars[[id]]$source_file,
      character(1)
    ))

    for (src_file in names(by_file)) {
      unused_content <- lapply(
        setNames(by_file[[src_file]], by_file[[src_file]]),
        function(id) .qt_strip_internal_fields(cand_vars[[id]])
      )
      dest_file <- file.path(unused_dir, basename(src_file))
      .qt_write_yaml_bank(unused_content, dest_file)
    }
  }

  # Remove original candidate files that have been fully sorted
  src_files <- unique(vapply(
    names(cand_vars),
    function(id) cand_vars[[id]]$source_file,
    character(1)
  ))
  accounted <- union(promote_ids, unused_ids)
  for (src_file in src_files) {
    if (file.exists(src_file)) {
      file_ids <- names(.read_yaml_safe(src_file) %||% list())
      if (all(file_ids %in% accounted))
        file.remove(src_file)
    }
  }

  list(promoted = promote_ids, relegated = unused_ids)
}

# Read promoted/ files for one bank type and integrate into the main bank.
#
# Entries with _bank_action: add are appended as new variables.
# Entries with _bank_action: version_update are appended as new version
# entries on the existing bank variable.
#
# @param promoted_dir  Directory containing promoted .yml files for this type.
# @param bank_path     Main bank path (file without .yml ext, or directory).
# @param survey_id     Survey ID (used for surveys_used and version_id).
# @return List with $added and $versioned character vectors.
# @keywords internal
# @noRd
.qt_finalize_bank <- function(promoted_dir, bank_path, survey_id) {
  added     <- character()
  versioned <- character()

  if (!dir.exists(promoted_dir))
    return(list(added = added, versioned = versioned))

  promoted_files <- sort(list.files(
    promoted_dir, pattern = "\\.yml$", full.names = TRUE))
  # Skip the tracking file
  promoted_files <- promoted_files[basename(promoted_files) != "promoted.yml"]

  for (f in promoted_files) {
    content <- .read_yaml_safe(f) %||% list()

    for (id in names(content)) {
      entry       <- content[[id]]
      bank_action <- entry[["_bank_action"]] %||% "add"
      clean       <- entry[setdiff(names(entry), "_bank_action")]

      if (bank_action == "add") {
        target_file <- .qt_candidate_to_bank_file(f, bank_path)
        .qt_append_variable_to_bank(id, clean, survey_id, target_file)
        added <- c(added, id)

      } else if (bank_action == "version_update") {
        .qt_append_version_to_bank(id, clean, survey_id, bank_path)
        versioned <- c(versioned, id)
      }
    }
  }

  list(added = added, versioned = versioned)
}

# Append a version entry to an existing bank question.
#
# Extracts version-relevant fields from version_data (dropping base-question
# fields like storage_type and vargroup that should not vary per version).
# Adds version_id and surveys_used if absent, then appends to the existing
# question's versions list and updates the base surveys_used.
#
# @param variable_id  ID of the existing bank variable.
# @param version_data Named list of version fields from the promoted entry.
# @param survey_id    Survey ID used as fallback version_id.
# @param bank_path    Main bank path (file without .yml ext, or directory).
# @keywords internal
# @noRd
.qt_append_version_to_bank <- function(variable_id, version_data,
                                       survey_id, bank_path) {
  bank_file <- .qt_find_bank_file(variable_id, bank_path)
  if (is.null(bank_file))
    stop("Cannot add version: '", variable_id, "' not found in bank '",
         bank_path, "'", call. = FALSE)

  content <- .read_yaml_safe(bank_file)
  if (is.null(content) || !variable_id %in% names(content))
    stop("Cannot add version: '", variable_id, "' not found in '",
         bank_file, "'", call. = FALSE)

  # Fields that belong only on the base question, not in a version entry
  base_only <- c("storage_type", "vargroup", "source", "description",
                 "variable_id", "source_file", "file_position")
  version_entry <- version_data[setdiff(names(version_data), base_only)]

  if (is.null(version_entry$version_id))
    version_entry$version_id <- survey_id

  if (is.null(version_entry$surveys_used))
    version_entry$surveys_used <- list(survey_id)

  # Append version
  existing_versions <- content[[variable_id]]$versions %||% list()
  content[[variable_id]]$versions <- c(existing_versions, list(version_entry))

  # Also update base surveys_used
  base_surveys <- content[[variable_id]]$surveys_used %||% character()
  if (!survey_id %in% base_surveys)
    content[[variable_id]]$surveys_used <- c(base_surveys, survey_id)

  .qt_write_yaml_bank(content, bank_file)
  invisible(bank_file)
}

# Read all YAML files in a bank directory (or a single .yml file) without
# validation. Attaches source_file to each entry for file mapping.
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

# Return just the top-level keys (IDs) present in a bank path.
# @keywords internal
# @noRd
.qt_read_bank_ids <- function(bank_path) {
  names(.qt_read_raw_yaml_bank(bank_path))
}

# Determine the target bank file for a new candidate variable.
# Single-file bank: write there. Directory bank: mirror candidate basename.
# @keywords internal
# @noRd
.qt_candidate_to_bank_file <- function(source_file, qbank_path) {
  if (file.exists(paste0(qbank_path, ".yml")))
    return(paste0(qbank_path, ".yml"))
  file.path(qbank_path, basename(source_file))
}

# Strip internal metadata fields added by the reader before writing.
# @keywords internal
# @noRd
.qt_strip_internal_fields <- function(var_data) {
  var_data[setdiff(names(var_data),
                   c("source_file", "file_position", "variable_id"))]
}

# Append a new variable definition to a bank YAML file.
# Ensures survey_id is in surveys_used; creates the file if absent.
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

# Find the YAML file within a bank that contains a given ID.
# Returns NULL when not found.
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
# Returns the modified file path, or NULL if already listed or not found.
# @keywords internal
# @noRd
.qt_add_survey_to_bank_entry <- function(id, survey_id, bank_path) {
  bank_file <- .qt_find_bank_file(id, bank_path)
  if (is.null(bank_file)) return(invisible(NULL))

  content <- .read_yaml_safe(bank_file)
  if (is.null(content) || !id %in% names(content)) return(invisible(NULL))

  existing_surveys <- content[[id]]$surveys_used %||% character()
  if (survey_id %in% existing_surveys) return(invisible(NULL))

  content[[id]]$surveys_used <- c(existing_surveys, survey_id)
  .qt_write_yaml_bank(content, bank_file)
  invisible(bank_file)
}

# Write a named list as a YAML bank file with standard qretools formatting.
# @keywords internal
# @noRd
.qt_write_yaml_bank <- function(content, file) {
  yaml_text <- yaml::as.yaml(content, indent = 2,
                              indent.mapping.sequence = FALSE)
  writeLines(yaml_text, file)
  invisible(file)
}

# Write a promoted.yml tracking file in the promoted/ directory.
# @keywords internal
# @noRd
.qt_write_promoted_yml <- function(plan, promoted_base) {
  dir.create(promoted_base, showWarnings = FALSE, recursive = TRUE)

  tracking <- list(
    survey_id      = plan$survey_id,
    promotion_date = format(Sys.Date(), "%Y-%m-%d"),
    questions = list(
      promoted  = as.list(plan$questions$promoted),
      relegated = as.list(plan$questions$relegated)
    ),
    control   = list(promoted = as.list(plan$control$promoted)),
    generated = list(promoted = as.list(plan$generated$promoted))
  )

  out_file <- file.path(promoted_base, "promoted.yml")
  .qt_write_yaml_bank(tracking, out_file)
  invisible(out_file)
}

# Report the promotion plan via cli.
# @keywords internal
# @noRd
.qt_report_promotion_plan <- function(plan, dry_run) {
  mode <- if (dry_run) " (dry run)" else ""
  cli::cli_h1("Candidate promotion: {plan$survey_id}{mode}")

  .qt_report_bank_result("Questions", plan$questions,
                         show_relegated = TRUE)
  .qt_report_bank_result("Control parameters", plan$control,
                         show_relegated = FALSE)
  .qt_report_bank_result("Generated variables", plan$generated,
                         show_relegated = FALSE)

  invisible(NULL)
}

# Report the finalization plan via cli.
# @keywords internal
# @noRd
.qt_report_finalization_plan <- function(plan) {
  cli::cli_h1("Survey finalization: {plan$survey_id}")

  if (length(plan$questions$added) > 0) {
    cli::cli_h2("Questions added to bank ({length(plan$questions$added)})")
    for (vid in plan$questions$added) cli::cli_li("{.val {vid}}")
  }
  if (length(plan$questions$versioned) > 0) {
    cli::cli_h2(
      "Questions with new version entry ({length(plan$questions$versioned)})")
    for (vid in plan$questions$versioned) cli::cli_li("{.val {vid}}")
  }
  if (length(plan$control$added) > 0) {
    cli::cli_h2(
      "Control parameters added ({length(plan$control$added)})")
    for (vid in plan$control$added) cli::cli_li("{.val {vid}}")
  }
  if (length(plan$generated$added) > 0) {
    cli::cli_h2(
      "Generated variables added ({length(plan$generated$added)})")
    for (vid in plan$generated$added) cli::cli_li("{.val {vid}}")
  }
  if (length(plan$surveys_used_updated) > 0) {
    n <- length(plan$surveys_used_updated)
    cli::cli_alert_success(
      "surveys_used updated for {n} question{?s}/module{?s}.")
  }
  invisible(NULL)
}

# Helper: report one bank type's promotion result.
# @keywords internal
# @noRd
.qt_report_bank_result <- function(label, result, show_relegated) {
  n_promo <- length(result$promoted)
  n_rel   <- if (show_relegated) length(result$relegated) else 0L

  if (n_promo == 0 && n_rel == 0) {
    cli::cli_alert_info("No {label} candidates found.")
    return(invisible(NULL))
  }

  if (n_promo > 0) {
    cli::cli_h2("{label}: {n_promo} promoted")
    for (id in result$promoted) cli::cli_li("{.val {id}}")
  }
  if (show_relegated && n_rel > 0) {
    cli::cli_h2("{label}: {n_rel} relegated to unused/")
    for (id in result$relegated) cli::cli_li("{.val {id}}")
  }
  invisible(NULL)
}
