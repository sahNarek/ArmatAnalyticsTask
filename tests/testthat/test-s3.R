library(testthat)

# Note: Files are sourced from tests/testthat.R

test_that("S3: build_report_s3 returns correct class", {
  df <- make_visits(100)
  report <- build_report_s3(df)
  
  expect_s3_class(report, "visit_report_s3")
})

test_that("S3: summary returns all required components", {
  df <- make_visits(100)
  report <- build_report_s3(df)
  result <- summary(report)
  
  # Check all four required outputs exist
  expect_true("overview" %in% names(result))
  expect_true("patient_summary" %in% names(result))
  expect_true("rule_results" %in% names(result))
  expect_true("examples" %in% names(result))
  
  # Check overview has required fields
  expect_true(all(c("nrows", "npatients", "nclinics", "datemin", "datemax") %in% 
                    names(result$overview)))
  
  # Check patient_summary has required columns
  expect_true(all(c("patient_id", "n_visits", "first_visit", "last_visit", 
                    "pct_missing_vitals", "n_flagged") %in% 
                    colnames(result$patient_summary)))
  
  # Check rule_results has required columns
  expect_true(all(c("rule_id", "description", "n_flagged", "pct_flagged") %in% 
                    colnames(result$rule_results)))
  
  # Check examples has required columns
  expect_true(all(c("rule_id", "row_id", "patient_id", "visit_time") %in% 
                    colnames(result$examples)))
})

test_that("S3: print method works without error", {
  df <- make_visits(100)
  report <- build_report_s3(df)
  
  expect_output(print(report), "Visit Audit Report")
})
