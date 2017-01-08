context("Co-occurrence simulation")

set.seed(1789)
nsp <- 20
ntr <- 4
nsite <- 100
res1 <- simPhyloTraits(nsp, ntr, 'lambda', 0)

env <- seq(-1,1,.1)
tr1 <- runif(nsp)
tmp1 <- webFromNicheModel(nsp, .1)*1.0

res5 <- traitsBasedJointCooc(env, tmp1, 1, trait1 = tr1)
res6 <- traitsBasedJointCooc(env, tmp1, 1, trait1 = tr1, trait2 = rep(1.0, nsp))
res7 <- traitsBasedJointCooc(env, tmp1, 1, trait1 = tr1, trait2 = rep(1.0, nsp), rep(1.0, nsp))


test_that("webFromNicheModel errors", {
  expect_error(traitsBasedJointCooc(env, tmp1, -1, tr1))
  expect_error(traitsBasedJointCooc(env, tmp1, 1, tr1=c(2,2)))
  expect_error(traitsBasedJointCooc(env, tmp1, 1, trait1=tr1, trait2=-tr1))
  expect_error(traitsBasedJointCooc(env, tmp1, 1, trait1=tr1, trait3=1+tr1))
})


test_that("Output format", {
  test_dim <- all(dim(res1) == c(nsp, ntr))
  test_dim2 <- all(dim(res5) == c(nsp, length(env)))
  expect_equal(test_dim, TRUE)
  expect_equal(test_dim2, TRUE)
  expect_equal(class(res5), "matrix")
  expect_equal(class(res5[1L, 1L]), "numeric")
  expect_equal(all(res5 == res6), TRUE)
  expect_equal(all(res5 == res7), TRUE)
})

test_that("simPhyloTraits errors", {
  expect_error(simPhyloTraits(nsp, ntr, 'zzz', 0))
})
