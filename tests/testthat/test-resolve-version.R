# Tests for .qt_resolve_version() and the version parameter on qt_add_question()

# A minimal bank entry with no versions
base_only <- list(
  variable_id   = "q1",
  title         = "Question 1",
  question_text = "Original text",
  value_label_id = "orig_labels",
  surveys_used  = list("s1"),
  source_file   = "/fake/path.yml",
  file_position = 1L
)

# Entry with two versions:
#   v2 changes value_label_id
#   v3 changes question_text
versioned <- list(
  variable_id   = "q2",
  title         = "Question 2",
  question_text = "Original text",
  value_label_id = "orig_labels",
  surveys_used  = list("s1"),
  source_file   = "/fake/path.yml",
  file_position = 2L,
  versions = list(
    list(
      version_id    = "v2",
      surveys_used  = list("s2"),
      value_label_id = "v2_labels"
    ),
    list(
      version_id    = "v3",
      surveys_used  = list("s3"),
      question_text = "v3 text"
    )
  )
)

# --- Base-only entry -----------------------------------------------------------

test_that("base-only entry: NULL version returns all base fields minus internals", {
  out <- qretools:::.qt_resolve_version(base_only)
  expect_equal(out$question_text,  "Original text")
  expect_equal(out$value_label_id, "orig_labels")
  expect_null(out$versions)
  expect_null(out$source_file)
  expect_null(out$file_position)
})

test_that("base-only entry: 'base' version behaves identically to NULL", {
  expect_equal(
    qretools:::.qt_resolve_version(base_only, "base"),
    qretools:::.qt_resolve_version(base_only)
  )
})

# --- Versioned entry: NULL (default = last) ------------------------------------

test_that("NULL version returns last version (v3)", {
  out <- qretools:::.qt_resolve_version(versioned)
  # question_text from v3
  expect_equal(out$question_text, "v3 text")
  # value_label_id from v2 (v3 did not change it)
  expect_equal(out$value_label_id, "v2_labels")
  # title unchanged from base
  expect_equal(out$title, "Question 2")
})

test_that("NULL version: internal fields are excluded", {
  out <- qretools:::.qt_resolve_version(versioned)
  expect_null(out$versions)
  expect_null(out$source_file)
  expect_null(out$file_position)
})

test_that("NULL version: version metadata fields are not merged in", {
  out <- qretools:::.qt_resolve_version(versioned)
  expect_null(out$version_id)
  # surveys_used in the result should be the base value, not a version's value
  expect_equal(out$surveys_used, list("s1"))
})

# --- Versioned entry: explicit 'base' -----------------------------------------

test_that("'base' version returns base fields, ignoring all versions", {
  out <- qretools:::.qt_resolve_version(versioned, "base")
  expect_equal(out$question_text,  "Original text")
  expect_equal(out$value_label_id, "orig_labels")
  expect_null(out$versions)
})

# --- Versioned entry: pin to v2 -----------------------------------------------

test_that("version 'v2' applies only v2, not v3", {
  out <- qretools:::.qt_resolve_version(versioned, "v2")
  # value_label_id updated by v2
  expect_equal(out$value_label_id, "v2_labels")
  # question_text unchanged (v3 not applied)
  expect_equal(out$question_text, "Original text")
})

# --- Versioned entry: pin to v3 -----------------------------------------------

test_that("version 'v3' applies v2 then v3 cumulatively", {
  out <- qretools:::.qt_resolve_version(versioned, "v3")
  expect_equal(out$value_label_id, "v2_labels")
  expect_equal(out$question_text,  "v3 text")
})

# --- Unknown version_id --------------------------------------------------------

test_that("unknown version_id raises an error", {
  expect_error(
    qretools:::.qt_resolve_version(versioned, "v99"),
    "version_id 'v99' not found"
  )
})

# --- qt_add_question() spec stores version ------------------------------------

test_that("qt_add_question() spec mode stores version in spec", {
  spec <- qt_add_question("dem_age", version = "v2")
  expect_equal(spec$version, "v2")
  expect_equal(spec$variable_id, "dem_age")
  expect_equal(spec$item_type, "question")
})

test_that("qt_add_question() spec mode: NULL version is absent from spec", {
  spec <- qt_add_question("dem_age")
  expect_null(spec$version)
})

test_that("qt_add_question() spec mode: 'base' version is stored", {
  spec <- qt_add_question("dem_age", version = "base")
  expect_equal(spec$version, "base")
})

test_that("qt_add_question() pipe mode stores version in questionnaire item", {
  qre  <- qt_qre("test-2026", "Test Survey", "draft")
  qre  <- qt_add_question(qre, "dem_age", version = "v3")
  item <- qre$questionnaire$items[[1]]
  expect_equal(item$version, "v3")
  expect_equal(item$variable_id, "dem_age")
})


# --- .qt_render_resolve_version(): explicit version_id path ------------------
#
# These tests exercise the rendering resolver directly, without needing a
# full bank. A minimal vlabs stub is used so value-label lookup can be
# exercised independently of file I/O.

# Stub vlabs object matching the label ids used in `versioned`
stub_vlabs <- list(
  labels = list(
    orig_labels = list(values = c(1L, 2L), labels = c("Yes", "No")),
    v2_labels   = list(values = c(1L, 2L, 3L),
                       labels = c("Yes", "No", "Maybe"))
  )
)

test_that("render resolver: NULL version_id uses survey_id matching", {
  # "s2" matches v2, which changed value_label_id
  out <- qretools:::.qt_render_resolve_version(versioned, survey_id = "s2",
                                               vlabs = stub_vlabs)
  expect_equal(out$value_label_id, "v2_labels")
  # question_text was not changed in v2, so still base
  expect_equal(out$question_text, "Original text")
  # resolved_labels from v2_labels
  expect_equal(out$resolved_labels$labels, c("Yes", "No", "Maybe"))
})

test_that("render resolver: NULL version_id falls back to last and applies cumulatively", {
  # No survey match → falls back to last (v3). Cumulative traversal means v2's
  # value_label_id change is also applied, not just v3's question_text change.
  out <- qretools:::.qt_render_resolve_version(versioned, survey_id = "unknown",
                                               vlabs = stub_vlabs)
  expect_equal(out$question_text,  "v3 text")
  expect_equal(out$value_label_id, "v2_labels")
})

test_that("render resolver: survey_id match uses cumulative traversal up to matched version", {
  # "s3" matches v3; cumulative means v2's value_label_id change is also applied
  out <- qretools:::.qt_render_resolve_version(versioned, survey_id = "s3",
                                               vlabs = stub_vlabs)
  expect_equal(out$question_text,  "v3 text")
  expect_equal(out$value_label_id, "v2_labels")
})

test_that("render resolver: explicit version_id uses cumulative traversal", {
  # version_id = "v3" should apply v2 (value_label_id) then v3 (question_text)
  out <- qretools:::.qt_render_resolve_version(versioned, survey_id = "ignored",
                                               vlabs = stub_vlabs,
                                               version_id = "v3")
  expect_equal(out$question_text,  "v3 text")
  expect_equal(out$value_label_id, "v2_labels")
  expect_equal(out$resolved_labels$labels, c("Yes", "No", "Maybe"))
})

test_that("render resolver: explicit version_id='v2' stops before v3", {
  out <- qretools:::.qt_render_resolve_version(versioned, survey_id = "ignored",
                                               vlabs = stub_vlabs,
                                               version_id = "v2")
  expect_equal(out$value_label_id, "v2_labels")
  expect_equal(out$question_text,  "Original text")
})

test_that("render resolver: explicit version_id='base' returns base fields", {
  out <- qretools:::.qt_render_resolve_version(versioned, survey_id = "ignored",
                                               vlabs = stub_vlabs,
                                               version_id = "base")
  expect_equal(out$question_text,  "Original text")
  expect_equal(out$value_label_id, "orig_labels")
  expect_equal(out$resolved_labels$labels, c("Yes", "No"))
})

test_that("render resolver: value labels resolved from correct label set", {
  out_base <- qretools:::.qt_render_resolve_version(versioned, survey_id = "x",
                                                    vlabs = stub_vlabs,
                                                    version_id = "base")
  out_v2   <- qretools:::.qt_render_resolve_version(versioned, survey_id = "x",
                                                    vlabs = stub_vlabs,
                                                    version_id = "v2")
  expect_length(out_base$resolved_labels$labels, 2L)
  expect_length(out_v2$resolved_labels$labels,   3L)
})

test_that("render resolver: no vlabs returns NULL resolved_labels", {
  out <- qretools:::.qt_render_resolve_version(versioned, survey_id = "x",
                                               vlabs = NULL,
                                               version_id = "v3")
  expect_null(out$resolved_labels)
  expect_equal(out$value_label_id, "v2_labels")
})


# --- value_labels_name alias in version entries --------------------------------
#
# The bank normalizer converts value_labels_name → value_label_id for the base
# entry, but version entries are stored raw and may use the legacy field name.
# .qt_resolve_version() must keep value_label_id in sync when a version uses
# value_labels_name without an explicit value_label_id.

# Mirrors the nhd_sat real-world case: base normalized to have value_label_id,
# version uses legacy value_labels_name field name.
legacy_alias_entry <- list(
  variable_id    = "nhd_sat",
  title          = "Neighborhood satisfaction",
  storage_type   = "factor",
  question_text  = "How satisfied are you?",
  value_label_id = "satisfied4",   # bank-normalized from value_labels_name
  value_labels_name = "satisfied4", # original field still present after normalization
  surveys_used   = list("bas-2023"),
  source_file    = "/fake/path.yml",
  file_position  = 1L,
  versions = list(
    list(
      version_id        = "v2",
      surveys_used      = list("bas-2024", "bas-2025"),
      value_labels_name = "satisfied5"   # legacy field name, no value_label_id
    )
  )
)

test_that("version using value_labels_name updates value_label_id", {
  out <- qretools:::.qt_resolve_version(legacy_alias_entry, "v2")
  expect_equal(out$value_label_id, "satisfied5")
  expect_equal(out$value_labels_name, "satisfied5")
})

test_that("base version (no versions applied) keeps original value_label_id", {
  out <- qretools:::.qt_resolve_version(legacy_alias_entry, "base")
  expect_equal(out$value_label_id, "satisfied4")
})

test_that("render resolver: version with value_labels_name resolves correct labels", {
  stub_satisfy <- list(
    labels = list(
      satisfied4 = list(values = 1:4,
                        labels = c("Very satisfied", "Somewhat satisfied",
                                   "Somewhat dissatisfied", "Very dissatisfied")),
      satisfied5 = list(values = 1:5,
                        labels = c("Very satisfied", "Somewhat satisfied",
                                   "Neither", "Somewhat dissatisfied",
                                   "Very dissatisfied"))
    )
  )
  out <- qretools:::.qt_render_resolve_version(legacy_alias_entry,
                                               survey_id = "bas-2026",
                                               vlabs = stub_satisfy,
                                               version_id = "v2")
  expect_equal(out$value_label_id, "satisfied5")
  expect_length(out$resolved_labels$labels, 5L)
  expect_equal(out$resolved_labels$labels[[3]], "Neither")
})
