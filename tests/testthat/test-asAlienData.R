context("as.alienData functions")
source("minimalEx.R")
ald1 <- alienData(df_nd0, df_int0, dfSites=df_sit0, dfOcc = df_occ0, verbose=F)

##
ald2 <- as.alienData(ald1)
##
mat1 <- matrix(c(0,1,1,0), 2, 2)
res1 <- as.alienData(mat1, verbose = F)
dat1 <- data.frame(
    idNodes = paste0("node_", 1:4),
    var1 = stats::runif(4)
    )
res3 <- as.alienData(mat1, dfNodes = dat1,  trait = 2, verbose = F)



##--
test_that("check method", {
  expect_true(identical(ald1, ald2))
})

##--
test_that("check conversion from matrix", {
  expect_equal(res1$nbNodes, 4)
  expect_equal(res1$nbEdges, 2)
  expect_is(res3, "alienData")
  expect_equal(res3$nmTrait, "var1")
})
