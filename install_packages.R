
required_packages <- c(
  "dplyr",
  "purrr",  
  "dotenv", 
  "R6",  
  "methods",
  "testthat"
)

install_if_missing <- function(packages) {
  installed <- installed.packages()[, "Package"]
  missing <- packages[!(packages %in% installed)]
  
  if (length(missing) > 0) {
    cat("Installing missing packages:", paste(missing, collapse = ", "), "\n")
    install.packages(missing, repos = "https://cloud.r-project.org")
  } else {
    cat("All required packages are already installed.\n")
  }
  
  cat("\nVerifying package loading...\n")
  success <- TRUE
  for (pkg in packages) {
    if (requireNamespace(pkg, quietly = TRUE)) {
      cat(sprintf("  ✓ %s\n", pkg))
    } else {
      cat(sprintf("  ✗ %s (failed to load)\n", pkg))
      success <- FALSE
    }
  }
  
  if (success) {
    cat("\n✓ All packages installed and verified successfully!\n")
  } else {
    cat("\n✗ Some packages failed to load. Please check the errors above.\n")
  }
  
  invisible(success)
}

install_if_missing(required_packages)

