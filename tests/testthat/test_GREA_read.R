# ---------- GREA_read() Tests ---------- #



test_that("GREA_read attaches code as attribute to the read-in df", {
  write.table(iris, "iris.csv", sep = ";", dec = ".")
  df <- GREA_read(file.path(getwd(), "iris.csv"), header = TRUE, sep = ";", dec = ".")
  expect_true(!is.null(attributes(df)$GREAcommand))

  # Cleaning up
  rm(df)
  file.remove("iris.csv")
})

####### Read all filetypes #######

test_that("GREA_read can read .csv", {
  path <- system.file("extdata", "csv_food_supply.csv", package = "GREA")
  df <- GREA_read(path,
                  sep = ";", dec = ",", header = TRUE)
  expect_true(class(df) == "data.frame")
  expect_true(ncol(df) > 1)

  # Cleaning up
  rm(df)
  rm(path)
})

test_that("GREA_read can read .xlsx", {
  path <- system.file("extdata", "excel_aachen.xlsx", package = "GREA")
  df <- GREA_read(path, sheetIndex = 1)
  expect_true(class(df) == "data.frame")
  expect_true(ncol(df) > 1)

  # Cleaning up
  rm(df)
  rm(path)
})

test_that("GREA_read can read .dat", {
  path <- system.file("extdata", "soep.dat", package = "GREA")
  df <- GREA_read(path, sep = " ", dec = ".", header = TRUE)
  expect_true(class(df) == "data.frame")
  expect_true(ncol(df) > 1)

  # Cleaning up
  rm(df)
  rm(path)
})

test_that("GREA_read can read .sav", {
  path <- system.file("extdata", "spss_SAQ.sav", package = "GREA")
  df <- GREA_read(path)
  expect_true(class(df) == "data.frame")
  expect_true(ncol(df) > 1)

  # Cleaning up
  rm(df)
  rm(path)
})

test_that("GREA_read can read .dta", {
  path <- system.file("extdata", "stata_africa.dta", package = "GREA")
  df <- GREA_read(path)
  expect_true(class(df) == "data.frame")
  expect_true(ncol(df) > 1)

  # Cleaning up
  rm(df)
  rm(path)
})

test_that("GREA_read can read .mat", {
  path <- system.file("extdata", "time_series.mat", package = "GREA")
  df <- GREA_read(path)
  expect_true(class(df) == "list")
  expect_true(!is.null(df[[1]]))

  # Cleaning up
  rm(df)
  rm(path)
})

####### Read specific files #######

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

test_that("GREA_read can read custom NA's in .csv", {
  path <- system.file("extdata", "iris_with_NA.csv", package = "GREA")
  df <- GREA_read(path, header = TRUE, na.values = "MV", sep = " ")
  expect_true(is.na(df[1, 4]))

  # Cleaning up
  rm(df)
  rm(path)
})

test_that("GREA_read can read custom NA's in .xlsx", {
  path <- system.file("extdata", "iris_with_NA.xlsx", package = "GREA")
  df <- GREA_read(path, na.values = "MV")
  expect_true(is.na(df[1, 5]))

  # Cleaning up
  rm(df)
  rm(path)
})

