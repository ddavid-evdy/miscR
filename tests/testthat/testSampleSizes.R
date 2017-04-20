library(miscR)
context("Returned sample size")


test_that("simple sample returns correct percent", {
  expect_equal(nrow(sample.df(mtcars, 0.13)), floor(nrow(mtcars)*0.13))
  expect_equal(nrow(sample.df(mtcars, 0.71)), floor(nrow(mtcars)*0.71))
  expect_equal(nrow(sample.df(mtcars, 0.99)), floor(nrow(mtcars)*0.99))
})

test_that("simple sample returns correct absolute", {
  expect_equal(nrow(sample.df(mtcars, 1)),  1)
  expect_equal(nrow(sample.df(mtcars, 10)), 10)
})

# test_that("sample.df, stratified sample returns correct %", {
#   expect_equal(nrow(sample.df(mtcars, 1)),    1)
#   expect_equal(nrow(sample.df(mtcars, 10)),   10)
# })
