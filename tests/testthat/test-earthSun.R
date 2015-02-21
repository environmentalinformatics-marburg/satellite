context("earthSun")

test_that("earthSun works as expected", {
  expect_equal(earthSun(date = "2015-01-01", formula = "Spencer"), 1.03505)
})
