context("Co-occurrence simulation")

nsp <- 20
ntr <- 4
nsite <- 100
res1 <- simPhyloTraits(nsp, ntr, 'lambda', 0)


test_that("simPhyloTraits output format", {
  test_dim <- all(dim(res1) == c(nsp, ntr))
  expect_equal(test_dim, TRUE)
  expect_equal(class(res1), "matrix")
  expect_equal(class(res1[1L, 1L]), "numeric")
})

test_that("simPhyloTraits errors", {
  expect_error(simPhyloTraits(nsp, ntr, 'zzz', 0))
})
