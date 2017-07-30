context("fitMC function")

source("minimalEx.R")

ald1 <- alienData(df_nd0, df_int0, dfSites=df_sit0, dfOcc = df_occ0, verbose=F)
# the line below take a while so I commented for now (I'll change mxt and
# implement few tets soon).
# fitMC(ald1, d=1, 10)


test_that("check overfit", {
  expect_error(fitMC(ald1, d=7, 10), 'npar < prod(dim(netObs)) is not TRUE', fixed = TRUE)
})
