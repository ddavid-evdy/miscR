library(miscR)
context("time.log")


test_that("logs time", {
  time.log()
  expect_equal(nrow(time.log(action="return")), 1)

  time.log()
  expect_equal(nrow(time.log(action="return")), 2)
})
time.log(action="clear")


test_that("can initiate with units", {
  time.log(unit="secs")
  expect_equal(nrow(time.log(action="return")), 1)
  time.log(action="clear")

  time.log(unit="mins")
  expect_equal(nrow(time.log(action="return")), 1)
  time.log(action="clear")

  time.log(unit="hours")
  expect_equal(nrow(time.log(action="return")), 1)
  time.log(action="clear")
})

test_that("persistent time unit", {
  time.log(unit="secs")
  expect_equal(nrow(time.log(action="return")), 1)
  time.log()
  expect_equal(nrow(time.log(action="return")), 2)
  time.log(action="clear")

  time.log(unit="mins")
  expect_equal(nrow(time.log(action="return")), 1)
  time.log()
  expect_equal(nrow(time.log(action="return")), 2)
  time.log(action="clear")

  time.log(unit="hours")
  expect_equal(nrow(time.log(action="return")), 1)
  time.log()
  expect_equal(nrow(time.log(action="return")), 2)
  time.log(action="clear")
})



test_that("can't change unit afterward", {
  time.log()
  expect_error(time.log(action="log", unit="secs"),  "Cannot alter unit after log is created")
  expect_equal(nrow(time.log(action="return")), 1)

  expect_error(time.log(action="log", unit="hours"), "Cannot alter unit after log is created")
  expect_equal(nrow(time.log(action="return")), 1)
  time.log(action="clear")

  time.log(unit="secs")
  expect_error(time.log(action="log", unit="mins"),  "Cannot alter unit after log is created")
  expect_equal(nrow(time.log(action="return")), 1)

  expect_error(time.log(action="log", unit="hours"), "Cannot alter unit after log is created")
  expect_equal(nrow(time.log(action="return")), 1)
  time.log(action="clear")

})


# devtools::test()

