library(miscR)
context("Returned sample size")


test_that("simple sample returns correct percent", {
  expect_equal(nrow(sample.df(workhrs, 0.13)),  793)
  expect_equal(nrow(sample.df(workhrs, 0.71)), 4330)
  expect_equal(nrow(sample.df(workhrs, 0.99)), 6037)
})

test_that("simple sample returns correct absolute", {
  expect_equal(nrow(sample.df(workhrs,  1)),  1)
  expect_equal(nrow(sample.df(workhrs, 10)), 10)
})

test_that("stratified sample returns correct percent", {
  expect_equal(nrow(sample.df(workhrs, 0.13, "sex")),         792)
  expect_equal(nrow(sample.df(workhrs, 0.99, "education")),  6037)
  expect_equal(nrow(sample.df(workhrs, 0.71, "work.class")), 4329)

  expect_equal(nrow(sample.df(workhrs, 0.71, c("race", "sex"))),           4329)
  expect_equal(nrow(sample.df(workhrs, 0.99, c("sex", "education"))),      6039)
  expect_equal(nrow(sample.df(workhrs, 0.13, c("race", "marital.status"))), 793)
})

test_that("stratified sample returns correct absolute", {
  expect_equal(nrow(sample.df(workhrs,  1, "sex")),         2)
  expect_equal(nrow(sample.df(workhrs, 33, "race")),       99)
  expect_equal(nrow(sample.df(workhrs, 12, "work.class")), 60)
})

test_that("size too great warning", {
  expect_warning(sample.df(workhrs, 160, "race"), "Strata cell contains fewer records than specified sample size. No observations selected.")
  expect_warning(sample.df(workhrs, 100, c("race", "sex")), "Strata cell contains fewer records than specified sample size. No observations selected.")
})

# Need to test if sample is correct, even if warning is given
options(warn = -1)
test.1 <- nrow(sample.df(workhrs, 160, "race"))
test.2 <- nrow(sample.df(workhrs, 225, "marital.status"))
test.3 <- nrow(sample.df(workhrs, 100, c("race", "sex")))
options(warn = 0)

test_that("small cells skipped", {
  expect_equal(test.1, 320)
  expect_equal(test.2, 675)
  expect_equal(test.3, 400)
})

rm(test.1, test.2, test.3)




