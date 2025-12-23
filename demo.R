source("R/s3.R")
source("R/s4.R")
source("R/r6.R")

cat("Generating visit data...\n\n")
df <- make_visits(2500)

print_section_header("         S3 IMPLEMENTATION")

report_s3 <- build_report_s3(df)
print(report_s3)

summary_s3 <- summary(report_s3)
print_summary_structure("S3", summary_s3)

print_section_header("         S4 IMPLEMENTATION", leading_newlines = 2)

report_s4 <- buildReport(df)
show(report_s4)

summary_s4 <- summary(report_s4)
print_summary_structure("S4", summary_s4)

print_section_header("         R6 IMPLEMENTATION", leading_newlines = 2)

engine <- VisitEngine$new()
cat(sprintf("Initial rules count: %d\n\n", engine$n_rules))

engine$run(df)
engine$print()

summary_r6 <- engine$summary()
print_summary_structure("R6", summary_r6)

print_section_header("    R6 PLUGIN DEMO: Add Custom Rule", leading_newlines = 2)

engine2 <- VisitEngine$new()
cat(sprintf("Rules before adding custom rule: %d\n", engine2$n_rules))

custom_rule <- list(
  id = "R5",
  desc = "High Fever (temp_c > 38.5)",
  check = function(df) {
    is.na(df$temp_c) | df$temp_c <= 38.5
  }
)
engine2$add_rule(custom_rule)
cat(sprintf("Rules after adding custom rule: %d\n\n", engine2$n_rules))

engine2$run(df)
engine2$print()
