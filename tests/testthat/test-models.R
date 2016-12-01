context("Models")

test_that("integrated model works", {
    out <- integrated_model(pars = c(1,1,1,1), 
                            Tlevel1 = rnorm(10), 
                            Tlevel2 = rnorm(10), 
                            mean_Tlevel1 = 0, 
                            sd_Tlevel1 = 1)
  expect_equal(TRUE, is.numeric(out))
})

test_that("niche model works", {
  out <- niche_model(pars = c(1,1,1,1), 
                          Tlevel1 = rnorm(10), 
                          Tlevel2 = rnorm(10), 
                          mean_Tlevel1 = 0, 
                          sd_Tlevel1 = 1)
  expect_equal(TRUE, is.numeric(out))
})

test_that("neutral model works", {
  out <- neutral_model(pars = NULL, 
                          Tlevel1 = rnorm(10), 
                          Tlevel2 = rnorm(10), 
                          mean_Tlevel1 = 0, 
                          sd_Tlevel1 = 1)
  expect_equal(TRUE, is.numeric(out))
})
