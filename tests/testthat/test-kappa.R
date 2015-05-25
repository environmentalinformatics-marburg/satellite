context("calcKappa")

test_that("calcKappa works as expected", {
  df <-  data.frame(REFERENCE = c(rep(seq(5), 6)),
                    PREDICTION = c(rep(seq(5), 6)))
  df$REFERENCE <-  df$REFERENCE - c(1,1,0)
  df$PREDICTION <-  df$PREDICTION - c(1,0,1)
  ctable <- table(df)
  calcKappa(ctable)
  })

