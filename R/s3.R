# TODO: Create a script for installing all packages required
library(dplyr)
source("R/utils.recursive.R")

new_report_s3 <- function(x) {
  structure(x, class = "visit_report_s3")
}




build_report_s3 <- function(data) {
  n_rows     <- nrow(data)
  n_patients <- length(unique(data$patient_id))
  n_clinics  <- length(unique(data$clinic_id))
  date_min   <- min(data$visit_time)
  date_max   <- max(data$visit_time)
  
  # TODO: Add row_id for each row
  
  
  
  vitals <- c("systolic", "diastolic", "heart_rate", "temp_c")
  miss_pct <- vapply(
    data[vitals],
    function(x){
      calculate_missing_percentage(x)
    },
    FUN.VALUE = numeric(1)
  )
  
  overview_df <- data.frame(
    n_rows = n_rows,
    n_patients = n_patients,
    n_clinics = n_clinics,
    date_min = date_min,
    date_max = date_max,
    as.list(miss_pct)
  )
  
  patient_overview_df <- data %>% 
    group_by(patient_id) %>% 
    summarise(
      n_visits = n(),
      first_visit = min(visit_time),
      last_visit = max(visit_time),
      # TODO: Find a dynamic solution to add a missing percentage for each vital
      systolic_miss_pct = calculate_missing_percentage(systolic),
      diastolic_miss_pct = calculate_missing_percentage(diastolic),
      heart_rate_miss_pct = calculate_missing_percentage(heart_rate),
      temp_c_miss_pct = calculate_missing_percentage(temp_c)
      # TODO: Implement missing rules
    )
  
  res_list <- list(
    overview = overview_df,
    patient_overview = patient_overview_df
  )
  print("Error is here")
  new_report_s3(res_list)
}

# TODO: print class should print both overview and patient overview
# TODO: The class should apply the rules and keep original_df, applied df

print.visit_report_s3 <- function(x, ...) {
  cat("=== Visit Audit Report (S3) ===\n")
  cat("Total Records:", x$overview$n_rows, "\n")
  cat("Unique Patients:", x$overview$n_patients, "\n")
}

summary.visit_report_s3 <- function(object, ...) {
  return(object)
}


