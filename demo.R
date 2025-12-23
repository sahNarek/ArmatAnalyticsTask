source("R/s3.R")

df <- make_visits(2500)
report <- new_report_s3(df)
print(report)
summary(report)
