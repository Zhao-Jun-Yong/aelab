## usage:
## test_file("file.xlsx")
test_file <- function(fname) testthat::test_path("ref", fname)
