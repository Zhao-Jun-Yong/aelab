## usage:
## test_file("file.xlsx")
test_file <- function(fname) testthat::test_path("ref", fname)

# test_that() for tests whose synthetic readings are exact (noise-free) or constant: base R
# warns "essentially perfect fit" / "the standard deviation is zero" there, which is expected.
test_quiet <- function(desc, code) {
  code <- substitute(code)
  muffle <- function(w) if (grepl("essentially perfect fit|standard deviation is zero",
                                  conditionMessage(w))) invokeRestart("muffleWarning")
  eval(bquote(testthat::test_that(.(desc), withCallingHandlers(.(code), warning = .(muffle)))),
       parent.frame())
}
