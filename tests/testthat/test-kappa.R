context("calcKappa")

test_that("calcKappa works as expected", {
  df <-  data.frame(REFERENCE = c(rep(seq(5), 5)),
                    PREDICTION = c(rep(seq(5), 4), seq(5)))
  ctable <- table(df)
  calcKappa(ctable)
  })
