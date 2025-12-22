library(R6)

source("R/s3.R")

load("data/visits_df.rda")

#S3 implementation
report_s3 <- build_report_s3(visits_df)
print(report_s3$patient_overview)
summary(report_s3$overview)

visits_df$row_id <- c(1:nrow(visits_df))
applied_results <- lapply(default_rules(), function(rule){
  rule$check(visits_df)
})

total_flags_per_row <- Reduce(`+`, applied_results)


nested_visits <- Reduce(
  f = function(df, flag) {
    df[flag[as.numeric(rownames(df))], ]
  }, 
  x = applied_results, 
  init = visits_df
)

all_criteria_met <- Reduce(`&`, applied_results)

final_filtered_visits <- visits_df[all_criteria_met, ]
