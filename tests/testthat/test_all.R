# ---------- wd_check() ---------- #

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

# ---------- GREA_read() ---------- #

test_that("GREA_read attaches code as attribute to the read-in df", {
  write.table(iris, "iris.csv", sep = ";", dec = ".")
  df <- GREA_read(file.path(getwd(), "iris.csv"), header = TRUE, sep = ";", dec = ".")
  expect_true(!is.null(attributes(df)$GREAcommand))

  # Cleaning up
  rm(df)
  file.remove("iris.csv")
})

test_that("GREA_read returns NULL when no filelocation is specified", {
  expect_null(GREA_read(filelocation = NULL))
})
