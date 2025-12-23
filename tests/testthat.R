library(testthat)

old_wd <- getwd()
if (basename(getwd()) == "tests") {
  setwd("..")
}

source("R/s3.R")
source("R/s4.R")
source("R/r6.R")

test_dir("tests/testthat")

setwd(old_wd)
