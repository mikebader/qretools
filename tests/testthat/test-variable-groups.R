# Tests for the variable-groups reader and topical grouping of codebook entries

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

# A project directory whose config has (or lacks) a variable-groups file
mk_cfg <- function(with_file = TRUE, groups = good) {
  dir <- tempfile(); dir.create(dir)
  if (with_file) yaml::write_yaml(groups, file.path(dir, "_variable_groups.yml"))
  list(meta = list(project_root = dir), paths = list())
}

mk <- function(id, g = NULL)
  list(variable_id = id, vargroup = g)

ids    <- function(x) vapply(x, function(e) e$variable_id, "")
gnames <- function(grps) vapply(grps, function(g) g$name, "")

test_that("labelled entries group in file order, stable within groups", {
  entries <- list(mk("a", "dem"), mk("b", "nhd"), mk("c", "fin"),
                  mk("d", "nhd"), mk("e", "dem"))
  out <- .qt_label_entry_groups(entries, "vargroup", mk_cfg())
  grps <- .qt_group_entries(out, "vargroup")
  expect_equal(gnames(grps),
               c("Neighborhoods", "Finances and Employment",
                 "Demographic Information"))
  expect_equal(ids(grps[[1]]$entries), c("b", "d"))
  expect_equal(ids(grps[[3]]$entries), c("a", "e"))
})

test_that("ungrouped and unlisted groups go last; unlisted ones warn", {
  entries <- list(mk("a"), mk("b", "nhd"), mk("c", "nhdd"))
  expect_warning(out <- .qt_label_entry_groups(entries, "vargroup", mk_cfg()),
                 "nhdd")
  grps <- .qt_group_entries(out, "vargroup")
  expect_equal(gnames(grps), c("Neighborhoods", "Other"))
  expect_equal(ids(grps[[2]]$entries), c("a", "c"))
})

test_that("a group with no entries gets no section", {
  entries <- list(mk("a", "dem"))
  grps <- .qt_group_entries(
    .qt_label_entry_groups(entries, "vargroup", mk_cfg()), "vargroup")
  expect_equal(gnames(grps), "Demographic Information")
})

test_that("without a file, grouping is by raw code (existing behavior)", {
  entries <- list(mk("a", "dem"), mk("b", "nhd"), mk("c", "dem"), mk("d"))
  out  <- .qt_label_entry_groups(entries, "vargroup", mk_cfg(with_file = FALSE))
  expect_null(out[[1]]$group_rank)
  grps <- .qt_group_entries(out, "vargroup")
  expect_equal(gnames(grps), c("dem", "nhd", "Other variables"))
})

test_that("group_by = 'none' leaves entries alone", {
  entries <- list(mk("a", "dem"))
  expect_identical(.qt_label_entry_groups(entries, "none", mk_cfg()), entries)
  expect_null(.qt_group_entries(entries, "none")[[1]]$name)
})
