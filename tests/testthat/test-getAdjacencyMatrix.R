context("getAdjacencyMatrix function")

source("minimalEx.R")

ald1 <- alienData(df_nd0, df_int0, dfSites=df_sit0, dfOcc = df_occ0, verbose=F)
res1 <- getAdjacencyMatrix(ald1)
res2 <- getAdjacencyMatrix(ald1, bipartite = TRUE)
#
ald1$dfEdges$value <- ald1$dfEdges$value*.5
res3 <- getAdjacencyMatrix(ald1)
res4 <- getAdjacencyMatrix(ald1, binary = TRUE)
#


##
 test_that("check error", {
   expect_error(getAdjacencyMatrix(1), 'class(object) == "alienData" is not TRUE', fixed = TRUE)
 })

##
test_that("check dimensions", {
  expect_equal(dim(res1), c(ald1$nbNodes,ald1$nbNodes))
  expect_equal(dim(res2), c(length(unique(ald1$dfEdges$idFrom)),length(unique(ald1$dfEdges$idTo))))
})

##
test_that("check value of interaction", {
  expect_equal(sum(res3), .5*ald1$nbEdges)
  expect_equal(sum(res4), nrow(unique(ald1$dfEdges)))
})
