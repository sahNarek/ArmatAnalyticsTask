library(dplyr)
source("R/utils.recursive.R")

new_report_s3 <- function(overview, patient_tbl, rules_tbl, examples_tbl, meta) {
  structure(
    list(
      overview = overview,
      patient_tbl = patient_tbl,
      rules_tbl = rules_tbl,
      examples_tbl = examples_tbl,
      meta = meta
    ),
    class = "visit_report_s3"
  )
}

build_report_s3 <- function(data, rules = default_rules()) {
  df <- data
  df$row_id <- seq_len(nrow(df))
  
  overview <- create_overview_list(df)
  patient_overview <- create_patient_overview(df)
  rules_list <- create_rules_overview(df, rules)
  
  patient_flags <- rules_list$original_df %>%
    group_by(patient_id) %>%
    summarise(n_flagged = sum(flags > 0))
  
  patient_tbl <- patient_overview %>%
    left_join(patient_flags, by = "patient_id") %>%
    mutate(n_flagged = ifelse(is.na(n_flagged), 0, n_flagged))
  
  rules_tbl <- rules_list$rules_overview %>%
    select(rule_id, description, n_flagged = n_failed, pct_flagged = pct_failed)
  
  new_report_s3(
    overview = overview,
    patient_tbl = as.data.frame(patient_tbl),
    rules_tbl = as.data.frame(rules_tbl),
    examples_tbl = as.data.frame(rules_list$examples),
    meta = list(
      created_at = Sys.time(),
      n_rules = length(rules),
      original_df = rules_list$original_df,
      filtered_df = rules_list$filtered_df
    )
  )
}

print.visit_report_s3 <- function(x, ...) {
  cat("=== Visit Audit Report (S3) ===\n")
  cat(sprintf("Total Records:    %d\n", x$overview$nrows))
  cat(sprintf("Unique Patients:  %d\n", x$overview$npatients))
  cat(sprintf("Clinics:          %d\n", x$overview$nclinics))
  cat(sprintf("Date Range:       %s to %s\n", 
              format(x$overview$datemin, "%Y-%m-%d"), 
              format(x$overview$datemax, "%Y-%m-%d")))
  cat("-------------------------------\n")
  
  top_rules <- x$rules_tbl %>%
    arrange(desc(n_flagged)) %>%
    head(2)
  
  cat("Top 2 Issues Found:\n")
  if (nrow(top_rules) > 0) {
    for(i in seq_len(nrow(top_rules))) {
      cat(sprintf("  %d. [%s] %s: %d flags\n", 
                  i, top_rules$rule_id[i], 
                  top_rules$description[i], 
                  top_rules$n_flagged[i]))
    }
  } else {
    cat("  No issues found!\n")
  }
  cat("===============================\n")
  invisible(x)
}

summary.visit_report_s3 <- function(object, ...) {
  list(
    overview = object$overview,
    patient_summary = object$patient_tbl,
    rule_results = object$rules_tbl,
    examples = object$examples_tbl
  )
}
