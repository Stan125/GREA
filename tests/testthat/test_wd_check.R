# ---------- wd_check() Tests ---------- #

test_that("wd_check returns a string", {
  expect_true(is.character(wd_check("/Users/stani/GitHub/GREA/testdata/spss_SAQ.sav")))
})

test_that("wd_check can deal with windows strings", {
  path <- "D:\\Users\\stani\\GitHub\\GREA\\testdata\\spss_SAQ.sav"
  expect_true(grepl("/", wd_check(path)))
  rm(path)
})

test_that("wd_check knows when a file lies in a working directory", {
  # Preliminaries
  path <- file.path(getwd(), "data/file.csv")
  temppath <- file.path(tempdir(), "data/file.csv")

  # Tests
  expect_equal(wd_check(path), "data/file.csv")
  expect_equal(wd_check(temppath), temppath)

  # Cleaning up
  rm(list = c("path", "temppath"))
})

