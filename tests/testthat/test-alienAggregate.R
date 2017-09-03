context("alienAggregate function")

plantsPolSp <- alienAggregate(plantsPol, 'idSp', 'type')
plantsPol2 <- plantsPol
plantsPol2$dfNodes$var1 <- runif(nrow(plantsPol2$dfNodes))


data(plantsPol)

test_that("check errors", {
  expect_error(alienAggregate(1,NULL))
  expect_error(alienAggregate(1,"wrong"))
  expect_error(alienAggregate(1, c("idSp", "wrong")))
  expect_error(alienAggregate(plantsPol2, 'idSp', 'var1'))
})

test_that("check aggregation", {
  expect_equal(nrow(plantsPolSp$dfNodes), length(unique(plantsPol$dfNodes$idSp)))
})
