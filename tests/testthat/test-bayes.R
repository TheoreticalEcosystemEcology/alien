context("Bayesian models")

fl1 <- paste0(tempdir(), "/binjag.jags")
fl2 <- paste0(tempdir(), "/intjag.jags")
fl3 <- paste0(tempdir(), "/muljag.jags")
fl4 <- paste0(tempdir(), "/poijag.jags")
fl5 <- paste0(tempdir(), "/occjag.jags")
binomialToJags(fl1)
interceptToJags(fl2)
multinomialToJags(fl3)
poissonToJags(fl4)
OccupancyToJags(fl5)


test_that("generating models as jags files", {
  expect_equal(TRUE, file.exists(fl1))
  expect_equal(TRUE, file.exists(fl2))
  expect_equal(TRUE, file.exists(fl3))
  expect_equal(TRUE, file.exists(fl4))
  expect_equal(TRUE, file.exists(fl5))
})
