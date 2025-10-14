library("testthat")
library("sentopics")

# For solving 'Examples with CPU time > 2.5 times elapsed time'
Sys.setenv("OMP_THREAD_LIMIT" = 2)

if (Sys.getenv("R_COVR") != "true") {
  test_check("sentopics")
} else {
  test_check("sentopics", reporter = default_reporter())
  # test_check("sentopics", reporter = LocationReporter)
}

# setwd("tests")
# print(test_check("sentopics", reporter = ListReporter))
# getwd()
# e <- covr::package_coverage(quiet = FALSE, clean = FALSE)
# attr(e, "library")
