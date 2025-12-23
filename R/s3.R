# TODO: Create a script for installing all packages required
library(dplyr)
source("R/utils.recursive.R")

new_report_s3 <- function(default_df) {
  df <- default_df
  df$row_id <- c(1:nrow(df))
  
  overview_df <- create_overview_list(df)
  patient_overview_df <- create_patient_overview(df)
  rules_list <- create_rules_overview(df)
  
  res_list <- list(
    mapped_df = rules_list$original_df,
    filtered_df = rules_list$filtered_df,
    overview = overview_df,
    patient_overview = patient_overview_df,
    rules_overview = rules_list$rules_overview,
    rules_example = rules_list$examples
  )
  structure(res_list, class = "visit_report_s3")
}

# TODO: print class should print both overview and patient overview
# TODO: The class should apply the rules and keep original_df, applied df

print.visit_report_s3 <- function(x, ...) {
  cat("=== Visit Audit Report (S3) ===\n")
  cat(sprintf("Total Records:    %d\n", x$overview$nrows))
  cat(sprintf("Unique Patients:  %d\n", x$overview$npatients))
  cat(sprintf("Date Range:       %s to %s\n", 
              format(x$overview$datemin, "%Y-%m-%d"), 
              format(x$overview$datemax, "%Y-%m-%d")))
  cat("-------------------------------\n")
  top_rules <- x$rules_overview %>%
    arrange(desc(n_failed)) %>%
    head(2)
  
  cat("Top 2 Issues Found:\n")
  if (nrow(top_rules) > 0) {
    for(i in 1:nrow(top_rules)) {
      cat(sprintf("  %d. [%s] %s: %d flags\n", 
                  i, top_rules$rule_id[i], 
                  top_rules$description[i], 
                  top_rules$n_failed[i]))
    }
  } else {
    cat("  No issues found!\n")
  }
  cat("===============================\n")
  invisible(x)
}

summary.visit_report_s3 <- function(object, ...) {
  return(list(
    overview = object$overview,
    patient_summary = object$patient_overview,
    rule_results = object$rules_overview,
    examples = object$rules_example
  ))
}

