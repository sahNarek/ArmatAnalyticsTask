library(methods)
library(dplyr)
source("R/utils.recursive.R")

setClass(
 "VisitReportS4",
 slots = c(
   overview = "list",
   patient_tbl = "data.frame",
   rules_tbl = "data.frame",
   examples_tbl = "data.frame",
   meta = "list"
 ),
 prototype = list(
   overview = list(),
   patient_tbl = data.frame(),
   rules_tbl = data.frame(),
   examples_tbl = data.frame(),
   meta = list()
 )
)

setValidity("VisitReportS4", function(object) {
 errors <- character()
 
 patient_cols <- c("patient_id", "n_visits", "first_visit", "last_visit")
 if (nrow(object@patient_tbl) > 0) {
   missing_patient <- setdiff(patient_cols, colnames(object@patient_tbl))
   if (length(missing_patient) > 0) {
     errors <- c(errors, paste("patient_tbl missing columns:", 
                               paste(missing_patient, collapse = ", ")))
   }
 }
 
 rules_cols <- c("rule_id", "description", "n_flagged", "pct_flagged")
 if (nrow(object@rules_tbl) > 0) {
   missing_rules <- setdiff(rules_cols, colnames(object@rules_tbl))
   if (length(missing_rules) > 0) {
     errors <- c(errors, paste("rules_tbl missing columns:", 
                               paste(missing_rules, collapse = ", ")))
   }
 }
 
 examples_cols <- c("rule_id", "row_id", "patient_id", "visit_time")
 if (nrow(object@examples_tbl) > 0) {
   missing_examples <- setdiff(examples_cols, colnames(object@examples_tbl))
   if (length(missing_examples) > 0) {
     errors <- c(errors, paste("examples_tbl missing columns:", 
                               paste(missing_examples, collapse = ", ")))
   }
 }
 
 overview_fields <- c("nrows", "npatients", "nclinics", "datemin", "datemax")
 if (length(object@overview) > 0) {
   missing_overview <- setdiff(overview_fields, names(object@overview))
   if (length(missing_overview) > 0) {
     errors <- c(errors, paste("overview missing fields:", 
                               paste(missing_overview, collapse = ", ")))
   }
 }
 
 if (length(errors) == 0) TRUE else errors
})

setGeneric("buildReport", function(data, rules = default_rules()) {
 standardGeneric("buildReport")
})

setMethod("buildReport", "data.frame", function(data, rules = default_rules()) {
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
 
 new("VisitReportS4",
     overview = overview,
     patient_tbl = as.data.frame(patient_tbl),
     rules_tbl = as.data.frame(rules_tbl),
     examples_tbl = as.data.frame(rules_list$examples),
     meta = list(
       created_at = Sys.time(),
       n_rules = length(rules),
       original_df = rules_list$original_df,
       filtered_df = rules_list$filtered_df
     ))
})

setMethod("show", "VisitReportS4", function(object) {
 cat("=== Visit Audit Report (S4) ===\n")
 cat(sprintf("Total Records:    %d\n", object@overview$nrows))
 cat(sprintf("Unique Patients:  %d\n", object@overview$npatients))
 cat(sprintf("Clinics:          %d\n", object@overview$nclinics))
 cat(sprintf("Date Range:       %s to %s\n", 
             format(object@overview$datemin, "%Y-%m-%d"), 
             format(object@overview$datemax, "%Y-%m-%d")))
 cat("-------------------------------\n")
 
 top_rules <- object@rules_tbl %>%
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
})

setMethod("summary", "VisitReportS4", function(object, ...) {
 list(
   overview = object@overview,
   patient_summary = object@patient_tbl,
   rule_results = object@rules_tbl,
   examples = object@examples_tbl
 )
})

