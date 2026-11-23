# Tests for the helpers behind qt_apply_codings()

mc <- data.frame(
  type  = c("skip", "item_nonresponse"),
  value = c("-7", "-8"),
  label = c("Missing: Skip pattern", "Missing: Item nonresponse"),
  stringsAsFactors = FALSE
)
set <- list(values = c(1, 2), labels = c("Yes", "No"))

test_that("integers keep missing codes as values and record them", {
  res <- .qt_ac_numeric(c("1", "5", "-8", NA), "integer", mc)
  expect_type(res$x, "integer")
  expect_equal(as.integer(res$x), c(1L, 5L, -8L, NA))
  expect_setequal(attr(res$x, "qt_missing"), c(-7L, -8L))
  expect_equal(res$n_failed, 0)
  expect_length(res$undeclared, 0)
})

test_that("negative values not in the codes are registered and reported", {
  res <- .qt_ac_numeric(c(1, -3), "integer", mc)
  expect_equal(res$undeclared, -3)
  expect_true(-3L %in% attr(res$x, "qt_missing"))
})

test_that("non-numeric values are counted; non-whole integers stay numeric", {
  expect_equal(.qt_ac_numeric(c("a", "2"), "integer", NULL)$n_failed, 1)
  res <- .qt_ac_numeric(c(1.5, 2), "integer", NULL)
  expect_equal(res$n_fractional, 1)
  expect_type(res$x, "double")
})

test_that("no missing codes and no negatives returns an untagged vector", {
  res <- .qt_ac_numeric(c(1, 2), "integer", NULL)
  expect_null(attr(res$x, "qt_missing"))
})

test_that("factors keep missing codes as levels with full labels", {
  res <- .qt_ac_factor(c(1, 2, -8, NA), set, mc)
  expect_equal(levels(res$x),
               c("Yes", "No", "Missing: Skip pattern", "Missing: Item nonresponse"))
  expect_equal(as.character(res$x),
               c("Yes", "No", "Missing: Item nonresponse", NA))
  expect_equal(unname(attr(res$x, "qt_codes")), c("1", "2", "-7", "-8"))
  expect_length(res$unlabeled, 0)
})

test_that("unlabeled factor values are kept as levels and reported", {
  res <- .qt_ac_factor(c(1, 9), set, NULL)
  expect_equal(res$unlabeled, "9")
  expect_equal(as.character(res$x), c("Yes", "9"))
})

test_that("zapping a factor drops missing levels and sets those values to NA", {
  x   <- .qt_ac_factor(c(1, 2, -8, -7), set, mc)$x
  out <- .qt_zap_factor(x)
  expect_equal(levels(out), c("Yes", "No"))
  expect_equal(as.character(out), c("Yes", "No", NA, NA))
  expect_equal(unname(attr(out, "qt_codes")), c("1", "2"))
})

test_that("qt_zap_missing() handles factors, numbers and strings", {
  df <- data.frame(s = c("a", "-8", "b"), stringsAsFactors = FALSE)
  df$f <- .qt_ac_factor(c(1, -8, 2), set, mc)$x
  df$n <- .qt_ac_numeric(c(1, -8, 3), "integer", mc)$x
  out <- qt_zap_missing(df, missing_codes = mc)
  expect_equal(out$s, c("a", NA, "b"))
  expect_equal(as.character(out$f), c("Yes", NA, "No"))
  expect_equal(as.integer(out$n), c(1L, NA, 3L))
})
