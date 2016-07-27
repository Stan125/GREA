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

test_that("GREA_read can read files with encoding", {
  levels(iris$Species)[4] <- "Österreich"
  iris$Species[1] <- "Österreich"
  write.table(iris, "iris_with_encoding.csv", fileEncoding = "latin1")
  df <- GREA_read("iris_with_encoding.csv", encoding = "latin1", sep = " ", header = TRUE)
  expect_true(any(class(df) == "data.frame"))

  # Cleaning up
  rm(df)
  file.remove("iris_with_encoding.csv")
})

test_that("GREA_read can read while skipping rows", {
  write.table(iris, "iris.csv")
  df <- GREA_read("iris.csv", skip = 1, header = FALSE, sep = " ")
  expect_true(df[1, 2] == 5.1)

  # Cleaning up
  file.remove("iris.csv")
  rm(df)
})
