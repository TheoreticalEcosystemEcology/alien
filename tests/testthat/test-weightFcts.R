context("Useful functions")

test_that("weigthed mean works", {
  out <- weightedMean(c(2,3,4), c(1,1,2))
  expect_equal(out, 3.25)
})

test_that("weigthed sd works", {
  out <- weightedSd(c(2,3,4), c(2,1,2))
  expect_equal(out, 1.118034)
})
