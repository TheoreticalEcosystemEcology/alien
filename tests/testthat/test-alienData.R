context("alienData function")

load('./argsAlienData.RData')
out <- as.alienData(idObs=idObs,interactPair=interactPair,traitSp=traitSp,traitInd=traitInd,verbose=FALSE)

test_that("check data structure", {
  expect_is(out, "alienData")
  # Even if items from the list are NULL, all items have to be returned
  expect_equal(names(out),c("idObs","interactSp","interactInd","coOcc","coAbund","siteEnv","traitSp","traitInd","phylo"))
})

test_that("check data integrity", {

  # Generate fake data for idObs
  len <- 3
  sites <- sample(paste("site",letters[1:3],sep="_"),len,replace=TRUE) # 3 sites
  sp <- paste0("sp",sample(1:len,len,replace=TRUE)) # 3 species
  ind <- 1:len # 3 individus
  idObs <- data.frame(idSite=sites,idTime=rep(NA,len),idSp=sp, idInd=ind,stringsAsFactors=FALSE)

  interactPair <-  data.frame(idTo=c("1","sp1"),idFrom=c("1","2"),strength=c(NA,NA),verbose=FALSE,stringsAsFactors=FALSE)
  expect_error(as.alienData(idObs=idObs, interactPair=interactPair,verbose=FALSE),"'idTo' values belongs to 'idSp' and 'idInd' in 'idObs'. Interaction can't be at the species AND individual levels")

  interactPair <-  data.frame(idTo=c("1","2"),idFrom=c("sp1","2"),strength=c(NA,NA),verbose=FALSE,stringsAsFactors=FALSE)
  expect_error(as.alienData(idObs=idObs, interactPair=interactPair,verbose=FALSE),"'idFrom' values belongs to 'idSp' and 'idInd' in 'idObs'. Interaction can't be at the species AND individual levels")

  interactPair <- data.frame(idTo="sp4",idFrom="sp1",strength=NA,stringsAsFactors=FALSE)
  expect_error(as.alienData(idObs=idObs, interactPair=interactPair,verbose=FALSE),"Some species ids in 'idTo' are not in 'idObs'")

  interactPair <-  data.frame(idTo="4",idFrom="1",strength=NA,stringsAsFactors=FALSE)
  expect_error(as.alienData(idObs=idObs, interactPair=interactPair,verbose=FALSE),"Some individus ids in 'idTo' are not in 'idObs'")

  interactPair <- data.frame(idTo="sp1",idFrom="sp4",strength=NA,stringsAsFactors=FALSE)
  expect_error(as.alienData(idObs=idObs, interactPair=interactPair,verbose=FALSE),"Some species ids in 'idFrom' are not in 'idObs'")

  interactPair <-  data.frame(idTo="1",idFrom="4",strength=NA,stringsAsFactors=FALSE)
  expect_error(as.alienData(idObs=idObs, interactPair=interactPair,verbose=FALSE),"Some individus ids in 'idFrom' are not in 'idObs'")

  interactPair <-  data.frame(idTo=c("2","2","3"),idFrom=c("2","2","2"),strength=c(NA,NA,NA),verbose=FALSE,stringsAsFactors=FALSE)
  expect_error(as.alienData(idObs=idObs, interactPair=interactPair,verbose=FALSE),"Some 'idFrom' and 'idTo' are duplicated")

})
