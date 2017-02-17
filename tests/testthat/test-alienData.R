context("alienData function")

load('./argsAlienData.RData')
out <- as.alienData(idObs=idObs,interactPair=interactPair,traitSp=traitSp,traitInd=traitInd,verbose=FALSE)

test_that("check data structure", {
  expect_is(out, "alienData")
  # Even if items from the list are NULL, all items have to be returned
  expect_equal(names(out),c("idObs","interactSp","interactInd","coOcc","coAbund","siteEnv","traitSp","traitInd","phylo"))
})

test_that("check data integrity", {
  # create fake data
  idObs <- data.frame(idSite=rep(3,c("")), idTime=NULL)

})
