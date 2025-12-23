library(testthat)

# Note: Files are sourced from tests/testthat.R

test_that("R6: VisitEngine initializes with default rules", {
  engine <- VisitEngine$new()
  
  expect_true(engine$n_rules > 0)
})

test_that("R6: get_report fails before run", {
  engine <- VisitEngine$new()
  
  expect_error(engine$get_report(), "No report available")
})

test_that("R6: run caches report correctly", {
  df <- make_visits(100)
  engine <- VisitEngine$new()
  engine$run(df)
  
  report <- engine$get_report()
  
  # Check all required components exist
  expect_true("overview" %in% names(report))
  expect_true("patient_tbl" %in% names(report))
  expect_true("rules_tbl" %in% names(report))
  expect_true("examples_tbl" %in% names(report))
})

test_that("R6: add_rule increases rule count and invalidates cache", {
  df <- make_visits(100)
  engine <- VisitEngine$new()
  initial_count <- engine$n_rules
  
  # Run first to populate cache
  engine$run(df)
  expect_false(is.null(engine$get_report()))
  
  # Add a custom rule
  custom_rule <- list(
    id = "R_custom",
    desc = "Custom test rule",
    check = function(df) rep(TRUE, nrow(df))
  )
  engine$add_rule(custom_rule)
  
  # Check rule count increased
  expect_equal(engine$n_rules, initial_count + 1)
  
  # Check cache was invalidated
  expect_error(engine$get_report(), "No report available")
})

test_that("R6: summary returns correct structure after run", {
  df <- make_visits(100)
  engine <- VisitEngine$new()
  engine$run(df)
  
  result <- engine$summary()
  
  # Check all four required outputs exist
  expect_true("overview" %in% names(result))
  expect_true("patient_summary" %in% names(result))
  expect_true("rule_results" %in% names(result))
  expect_true("examples" %in% names(result))
  
  # Check patient_summary has n_flagged column
  expect_true("n_flagged" %in% colnames(result$patient_summary))
})

test_that("R6: print method works without error", {
  df <- make_visits(100)
  engine <- VisitEngine$new()
  engine$run(df)
  
  expect_output(engine$print(), "Visit Engine")
})
