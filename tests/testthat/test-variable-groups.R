# Tests for the variable-groups reader and topical ordering of codebook entries

good <- list(
  variable_groups = list(
    list(id = "nhd", label = "Neighborhoods"),
    list(id = "fin", label = "Finances and Employment", description = "Income"),
    list(id = "dem", label = "Demographic Information")
  ),
  unclassified = list(label = "Other")
)

test_that("a valid file keeps order, descriptions and the unclassified label", {
  vg <- .qt_parse_variable_groups(good)
  expect_s3_class(vg, "qt_variable_groups")
  expect_equal(vg$id, c("nhd", "fin", "dem"))
  expect_equal(vg$description, c(NA, "Income", NA))
  expect_equal(attr(vg, "unclassified"), "Other")
})

test_that("unclassified defaults to 'Not classified'", {
  vg <- .qt_parse_variable_groups(good["variable_groups"])
  expect_equal(attr(vg, "unclassified"), "Not classified")
})

test_that("malformed files are rejected", {
  expect_error(.qt_parse_variable_groups(list()), "variable_groups")
  expect_error(.qt_parse_variable_groups(list(variable_groups = list(nhd = "x"))),
               "variable_groups")
  expect_error(.qt_parse_variable_groups(
    list(variable_groups = list(list(id = "a", label = "A"),
                                list(id = "a", label = "B")))),
    "duplicate ids")
  expect_error(.qt_parse_variable_groups(
    list(variable_groups = list(list(id = "a", label = "A"),
                                list(id = "b", label = "A")))),
    "duplicate labels")
  expect_error(.qt_parse_variable_groups(
    list(variable_groups = list(list(id = "a")))), "label")
  expect_error(.qt_parse_variable_groups(
    list(variable_groups = list(list(id = "a", label = "A")),
         unclassified = list(label = "A"))), "unclassified")
})

test_that("reader returns NULL without a default file and errors on an explicit one", {
  cfg <- list(meta = list(project_root = tempfile()), paths = list())
  expect_null(qt_read_variable_groups(cfg))
  cfg$paths$variable_groups <- "nope.yml"
  expect_error(qt_read_variable_groups(cfg), "not found")
})

test_that("reader reads the file at the configured path", {
  dir <- tempfile(); dir.create(dir)
  yaml::write_yaml(good, file.path(dir, "_variable_groups.yml"))
  cfg <- list(meta = list(project_root = dir), paths = list())
  vg  <- qt_read_variable_groups(cfg)
  expect_equal(vg$label[1], "Neighborhoods")
})

mk <- function(id, g = NULL, parent = NULL)
  list(variable_id = id, vargroup = g, parent_id = parent)

test_that("entries are grouped in file order, stable within groups", {
  vg <- .qt_parse_variable_groups(good)
  entries <- list(mk("a", "dem"), mk("b", "nhd"), mk("c", "fin"),
                  mk("d", "nhd"), mk("e", "dem"))
  out <- .qt_group_entries(entries, vg)
  expect_equal(vapply(out, function(e) e$variable_id, ""),
               c("b", "d", "c", "a", "e"))
  expect_equal(vapply(out, function(e) e$group_label, ""),
               c("Neighborhoods", "Neighborhoods", "Finances and Employment",
                 "Demographic Information", "Demographic Information"))
})

test_that("ungrouped and unlisted groups go last, unlisted ones with a warning", {
  vg <- .qt_parse_variable_groups(good)
  entries <- list(mk("a"), mk("b", "nhd"), mk("c", "nhdd"))
  expect_warning(out <- .qt_group_entries(entries, vg), "nhdd")
  expect_equal(vapply(out, function(e) e$variable_id, ""), c("b", "a", "c"))
  expect_equal(out[[2]]$group_label, "Other")
  expect_equal(out[[3]]$group_label, "Other")
})

test_that("child variables take their parent's group", {
  vg <- .qt_parse_variable_groups(good)
  entries <- list(mk("p", "fin"), mk("x", "nhd"),
                  mk("p_1", parent = "p"), mk("p_2", parent = "p"))
  out <- .qt_group_entries(entries, vg)
  expect_equal(vapply(out, function(e) e$variable_id, ""),
               c("x", "p", "p_1", "p_2"))
  expect_equal(out[[3]]$group_label, "Finances and Employment")
})

test_that("without a file, sections use raw codes in order of first appearance", {
  vg <- .qt_resolve_variable_groups(TRUE, config = list(
    meta = list(project_root = tempfile()), paths = list()))
  expect_true(isTRUE(attr(vg, "fallback")))
  entries <- list(mk("a", "dem"), mk("b", "nhd"), mk("c", "dem"), mk("d"))
  expect_message(out <- .qt_group_entries(entries, vg), "No variable-groups file")
  expect_equal(vapply(out, function(e) e$group_label, ""),
               c("dem", "dem", "nhd", "Not classified"))
})

test_that("variable_groups argument is validated", {
  expect_null(.qt_resolve_variable_groups(FALSE))
  expect_error(.qt_resolve_variable_groups("vargroup"), "TRUE, FALSE")
})
