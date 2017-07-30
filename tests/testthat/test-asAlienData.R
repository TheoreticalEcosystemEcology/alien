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

##-- to test igraph conversion
links1 <- data.frame(from=c("sp_1", "sp_2"), to=c("sp_3", "sp_4"))
links2 <- data.frame(from=c("sp_1", "sp_2"), to=c("sp_3", "sp_4"), weight=c(1,2))
nodes1 <- data.frame(id = c("sp_1", "sp_2", "sp_3", "sp_4"))
nodes2 <- data.frame(id = c("sp_1", "sp_2", "sp_3", "sp_4"), var1 = runif(4))
##--
net1 <- igraph::graph.data.frame(links1, nodes1, directed=T)
net2 <- igraph::graph.data.frame(links2, nodes1, directed=T)
net3 <- igraph::graph.data.frame(links1, nodes2, directed=T)
##--
res4 <- as.alienData(net1, verbose = F)
res5 <- as.alienData(net2, verbose = F)
res6 <- as.alienData(net3, verbose = F)


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


##--
test_that("check conversion from igraph", {
  expect_true(all(res4$dfEdges$value == c(1,1)))
  expect_true(all(res5$dfEdges$value == c(1,2)))
  expect_true(all(res6$dfEdges$value == c(1,1)))
  ##--
  expect_true(all(res4$dfNodes$idNodes == paste0("sp_",1:4)))
  expect_true(all(res5$dfNodes$idNodes == paste0("sp_",1:4)))
  expect_true(all(res6$dfNodes$idNodes == paste0("sp_",1:4)))
  expect_true("var1"%in%names(res6$dfNodes))
  ##--

})
