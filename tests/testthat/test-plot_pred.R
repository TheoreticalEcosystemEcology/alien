context("Plot")

test_that("plot it works", {
  out <- plot_pred( 
                pars = c(1,1,1,1), 
                Tlevel1 = rnorm(10), 
                Tlevel2 = rnorm(10)) 
  expect_equal(TRUE, out$xpd)
})
