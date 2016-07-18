test_that("wd_check returns a string", {
  expect_true(is.character(wd_check("/Users/stani/GitHub/GREA/testdata/spss_SAQ.sav")))
})

test_that("wd_check can deal with windows strings", {
  path <- "D:\\Users\\stani\\GitHub\\GREA\\testdata\\spss_SAQ.sav"
  expect_true(grepl("/", wd_check(path)))
  rm(path)
})
