context("getAdjacencyMatrix and getEdgesList functions")

# source("minimalEx.R")

# ald1 <- alienData(df_nd0, df_int0, dfSites=df_sit0, dfOcc = df_occ0, verbose=FALSE)
# res1 <- getAdjacencyMatrix(ald1)
# res2 <- getAdjacencyMatrix(ald1, bipartite = TRUE)
# #
# ald1$dfEdges$value <- ald1$dfEdges$value*.5
# res3 <- getAdjacencyMatrix(ald1)
# res4 <- getAdjacencyMatrix(ald1, binary = TRUE)
# ##--
# res5 <- getEdgesList(res4)
# identical(sort(ald1$dfEdges$idFrom), sort(res5$idFrom))
# identical(sort(ald1$dfEdges$idTo), sort(res5$idTo))
# ##--
# res6 <- getEdgesList(matrix(1,2,2))
# res7 <- getEdgesList(matrix(1,2,2), bipartite = FALSE)
#
#
# ##--
#  test_that("check getAdjacencyMatrix error", {
#    expect_error(getAdjacencyMatrix(1), 'class(object) == "alienData" is not TRUE', fixed = TRUE)
#  })
#
# ##--
# test_that("check getAdjacencyMatrix dimensions", {
#   expect_equal(dim(res1), rep(ald1$nbNodes,2L))
#   expect_equal(dim(res2), c(length(unique(ald1$dfEdges$idTo)), length(unique(ald1$dfEdges$idFrom))))
# })
#
# ##--
# test_that("check interaction values", {
#   expect_equal(sum(res3), .5*ald1$nbEdges)
#   expect_equal(sum(res4), nrow(unique(ald1$dfEdges)))
# })
#
#
# ##--
# test_that("check inverse transformation", {
#   expect_true(identical(sort(ald1$dfEdges$idFrom), sort(res5$idFrom)))
#   expect_true(identical(sort(ald1$dfEdges$idTo), sort(res5$idTo)))
# })
#
# ##--
# test_that("check getEdgesList", {
#   expect_true(all(res6$value == 1))
#   expect_true(all(res7$value == 1))
#   ##
#   expect_true(all(res6$idFrom == paste0("node_", rep(1:2, 2))))
#   expect_true(all(res6$idTo == paste0("node_", rep(3:4, each = 2))))
#   ##
#   expect_true(all(res7$idFrom == paste0("node_", rep(1:2, 2))))
#   expect_true(all(res7$idTo == paste0("node_", rep(1:2, each = 2))))
# })
