context("Fit")

test_that("fit it works", {
  out <- fit_it(integrated_model, 
                pars = c(1,1,1,1), 
                Tlevel1 = rnorm(10), 
                Tlevel2 = rnorm(10), 
                mean_Tlevel1 = 0, 
                sd_Tlevel1 = 1,
                max.time = 5)
  expect_equal(4, length(out))
})

