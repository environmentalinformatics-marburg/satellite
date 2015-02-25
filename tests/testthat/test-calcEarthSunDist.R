context("calcEartSunDist")

test_that("calcEartSunDist works as expected", {
  expect_equal(calcEartSunDist(date = "2015-01-01", formula = "Spencer"), 1.03505)
})
