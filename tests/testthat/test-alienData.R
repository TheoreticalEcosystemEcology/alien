context("alienData")

node1 <- data.frame(cbind(idInd = c(1,2,3,4), idSp = c(2,1,3,3)))
edge1 <- data.frame(cbind(from = c(1,2), to = c(3,4)))
edge2 <- data.frame(cbind(from = c(1,2), to = c(3,4), value = .5))

al1 <- alienData(
    node = node1,
    edge = edge1
)

al2 <- alienData(
    node = node1,
    edge = edge2
)

test_that("Expect output", {
  expect_identical(node1, al1$node)
  expect_identical(cbind(edge1, value = 1), al1$edge)
  expect_identical(edge2, al2$edge)
})


test_that("test Adjancy matrix", {
  expect_equal(sum(getAdjacencyMatrix(al2)), 1)
  expect_true(all(getAdjacencyMatrix(al2, threshold = .5, binary = TRUE) == 0)) 
  expect_equal(sum(getAdjacencyMatrix(al2, threshold = .49, binary = TRUE)), 2) 
})