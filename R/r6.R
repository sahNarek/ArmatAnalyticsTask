library(R6)
library(dplyr)
source("R/utils.recursive.R")

VisitEngine <- R6Class(
  "VisitEngine",
  
  private = list(
    rules = NULL,
    report_cache = NULL
  ),
  
  active = list(
    n_rules = function() {
      length(private$rules)
    }
  ),
  
  public = list(
    initialize = function(rules = default_rules()) {
      private$rules <- rules
      private$report_cache <- NULL
      invisible(self)
    },
    
    run = function(data) {
      df <- data
      df$row_id <- seq_len(nrow(df))
      
      overview <- create_overview_list(df)
      patient_overview <- create_patient_overview(df)
      rules_list <- create_rules_overview(df, private$rules)
      
      patient_flags <- rules_list$original_df %>%
        group_by(patient_id) %>%
        summarise(n_flagged = sum(flags > 0))
      
      patient_tbl <- patient_overview %>%
        left_join(patient_flags, by = "patient_id") %>%
        mutate(n_flagged = ifelse(is.na(n_flagged), 0, n_flagged))
      
      rules_tbl <- rules_list$rules_overview %>%
        select(rule_id, description, n_flagged = n_failed, pct_flagged = pct_failed)
      
      private$report_cache <- list(
        overview = overview,
        patient_tbl = as.data.frame(patient_tbl),
        rules_tbl = as.data.frame(rules_tbl),
        examples_tbl = as.data.frame(rules_list$examples),
        meta = list(
          created_at = Sys.time(),
          n_rules = length(private$rules),
          original_df = rules_list$original_df,
          filtered_df = rules_list$filtered_df
        )
      )
      
      invisible(self)
    },
    
    get_report = function() {
      if (is.null(private$report_cache)) {
        stop("No report available. Call $run(data) first.")
      }
      private$report_cache
    },
    
    add_rule = function(rule) {
      if (!is.list(rule) || is.null(rule$id) || is.null(rule$check)) {
        stop("Rule must be a list with 'id', 'desc', and 'check' elements")
      }
      private$rules <- c(private$rules, list(rule))
      private$report_cache <- NULL
      invisible(self)
    },
    
    print = function(...) {
      cat("=== Visit Engine (R6) ===\n")
      cat(sprintf("Rules loaded:     %d\n", self$n_rules))
      
      if (!is.null(private$report_cache)) {
        report <- private$report_cache
        cat(sprintf("Total Records:    %d\n", report$overview$nrows))
        cat(sprintf("Unique Patients:  %d\n", report$overview$npatients))
        cat(sprintf("Clinics:          %d\n", report$overview$nclinics))
        cat(sprintf("Date Range:       %s to %s\n", 
                    format(report$overview$datemin, "%Y-%m-%d"), 
                    format(report$overview$datemax, "%Y-%m-%d")))
        cat("-------------------------------\n")
        
        top_rules <- report$rules_tbl %>%
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
      } else {
        cat("Report:           Not generated yet\n")
        cat("(Call $run(data) to generate report)\n")
      }
      cat("=========================\n")
      invisible(self)
    },
    
    summary = function() {
      if (is.null(private$report_cache)) {
        stop("No report available. Call $run(data) first.")
      }
      list(
        overview = private$report_cache$overview,
        patient_summary = private$report_cache$patient_tbl,
        rule_results = private$report_cache$rules_tbl,
        examples = private$report_cache$examples_tbl
      )
    }
  )
)

