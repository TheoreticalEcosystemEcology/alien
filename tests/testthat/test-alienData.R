context("alienData function")

load('./argsAlienData.RData')
out <- as.alienData(idObs=idObs,interactPair=interactPair,traitSp=traitSp,traitInd=traitInd,verbose=FALSE)

test_that("check data structure", {
  expect_is(out, "alienData")
})

test_that("check data integrity", {
})
