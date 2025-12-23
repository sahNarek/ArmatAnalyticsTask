library(testthat)

# Note: Files are sourced from tests/testthat.R

test_that("S4: buildReport returns correct class", {
  df <- make_visits(100)
  report <- buildReport(df)
  
  expect_s4_class(report, "VisitReportS4")
})

test_that("S4: validity check rejects invalid objects", {
  # Create an invalid object with missing columns
  expect_error(
    new("VisitReportS4",
        overview = list(nrows = 100),  # missing required fields
        patient_tbl = data.frame(),
        rules_tbl = data.frame(),
        examples_tbl = data.frame(),
        meta = list()),
    "missing"
  )
})

test_that("S4: summary returns all required components", {
  df <- make_visits(100)
  report <- buildReport(df)
  result <- summary(report)
  
  # Check all four required outputs exist
  expect_true("overview" %in% names(result))
  expect_true("patient_summary" %in% names(result))
  expect_true("rule_results" %in% names(result))
  expect_true("examples" %in% names(result))
  
  # Check slot access works
  expect_equal(report@overview$nrows, nrow(df))
})

test_that("S4: show method works without error", {
  df <- make_visits(100)
  report <- buildReport(df)
  
  expect_output(show(report), "Visit Audit Report")
})
