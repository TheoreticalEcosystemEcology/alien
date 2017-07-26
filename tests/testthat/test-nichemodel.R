context("Niche model")
#
set.seed(1789)
nsp <- 10L
connec <- .1

# res0 <- webFromNicheModel(nsp, connec, FALSE)
# # # When connec_all is TRUE then at east n-1 species must have a prey
# # tmp1 <- webFromNicheModel(nsp, connec, TRUE)
# res1 <- sum(rowSums(tmp1)>0) >= (nsp-1)
# # Average values of the connectance
# tmp2 <- tmp3 <- tmp4 <-integer(10000)
# for (i in 1:length(tmp2)) {
#   tmp2[i] <- sum(webFromNicheModel(nsp, connec, FALSE))
#   tmp3[i] <- sum(webFromNicheModel(nsp, connec, FALSE, niche = runif(nsp)))
#   tmp4[i] <- sum(webFromNicheModel(nsp, connec, FALSE, TRUE, niche = runif(nsp)))
# }
# res2 <- round(mean(tmp2)/100, 2)
# res3 <- round(mean(tmp3)/100, 2)
# res4 <- round(mean(tmp4)/100, 2)
# res34 <- mean(tmp3)/100 < mean(tmp4)/10
#
#
# test_that("Output format", {
#   test_dim <- all(dim(res0) == c(nsp, nsp))
#   expect_equal(test_dim, TRUE)
#   expect_equal(class(res0), "matrix")
#   expect_equal(class(res0[1L, 1L]), "logical")
# })
#
#
# test_that("Output", {
#   expect_equal(res1, TRUE)
#   expect_equal(res2, connec)
#   expect_equal(res3, connec)
#   expect_equal(res4, connec)
#   expect_equal(res34, TRUE)
# })
#
#
# test_that("webFromNicheModel errors", {
#   expect_error(webFromNicheModel(nsp, -1, TRUE))
#   expect_error(webFromNicheModel(nsp, .6, TRUE))
#   expect_error(webFromNicheModel(nsp, 0, TRUE))
#   expect_error(webFromNicheModel(nsp, 0, TRUE,  niche = c(.1, .3)))
#   expect_error(webFromNicheModel(nsp, 0, TRUE, niche = rep(-1, nsp)))
# })
