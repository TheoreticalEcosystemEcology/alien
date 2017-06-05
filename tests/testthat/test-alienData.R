context("alienData function")

####
nind <- 10
df_ex0 <- data.frame(
  idInd = paste0("ind", sprintf("%02d", 1:10)),
  idSp = paste0("sp", sprintf("%02d", 1:10)),
  stringsAsFactors=FALSE)
df_ex3 <- df_ex2 <- df_ex1 <- df_ex0
df_ex1$idInd[nind] <- df_ex1$idInd[1L]
df_ex2$idSp[nind] <- df_ex1$idSp[1L]
df_ex3$idInd[nind] <- df_ex1$idSp[1L]
df_ex4 <- cbind(df_ex1, val1 = runif(nind))

##
sp1 <- sample(1:10, 8, replace=T)
sp2 <- sample(1:10, 8, replace=T)
##
df_int0 <- cbind(idFrom = paste0("sp", sprintf("%02d", sp1)), idTo = paste0("sp", sprintf("%02d", sp2)))
df_int3 <- df_int2 <- df_int1 <- df_ex0

##
res0 <- as.alienData(df_ex0, df_int0, verbose=F)

test_that("check dfSpecies", {
  expect_error(as.alienData(df_ex1, df_int0), "!any(table(dfSpecies$idInd) > 1) is not TRUE", fixed=TRUE)
  expect_error(as.alienData(df_ex2[,-1], df_int0), '"idSp" %in% names(dfSpecies) is not TRUE', fixed=TRUE)
  expect_error(as.alienData(df_ex3, df_int0), "!any(dfSpecies$idInd %in% dfSpecies$idSp) is not TRUE", fixed=TRUE)
  expect_error(as.alienData(df_ex4, df_int0, trait=4))
  expect_error(as.alienData(df_ex4, df_int0, phylo="val2"))
  expect_error(as.alienData(df_ex4, df_int0, taxo=4))
})

# test_that("check dfInteract", {
#   expect_error(as.alienData(df_ex0, df_int0))
# })


test_that("check values", {
  expect_is(res0, "alienData")
  expect_equal(res0$nbSpecies, 10)
  expect_equal(res0$nbInteractions, 8)
})





#
#   load('test_idObs.rda')
#   out <- as.alienData(idObs=idObs, interactPair=interactPair, verbose=FALSE)
#
#   expect_is(out, "alienData")
#   # Even if items from the list are NULL, all items have to be returned
#   expect_equal(names(out), c("idObs", "interactPair", "interactSp",
#     "interactInd", "coOcc", "coAbund", "siteEnv", "traitSp", "traitInd", "phy"))
# })
#
# test_that("check data integrity", {
#
#   # load fake data
#   load('test_idObs.rda')
#
#   interactPair <-  data.frame(idTo=c("1","sp1"),idFrom=c("1","2"),strength=c(NA,NA), stringsAsFactors=FALSE)
#   expect_error(as.alienData(idObs=idObs, interactPair=interactPair,verbose=FALSE),"'idTo' values belongs to 'idSp' and 'idInd' in 'idObs'. Interaction can't be at the species AND individual levels")
#
#   interactPair <-  data.frame(idTo=c("1","2"),idFrom=c("sp1","2"),strength=c(NA,NA), stringsAsFactors=FALSE)
#   expect_error(as.alienData(idObs=idObs, interactPair=interactPair,verbose=FALSE),"'idFrom' values belongs to 'idSp' and 'idInd' in 'idObs'. Interaction can't be at the species AND individual levels")
#
#   interactPair <- data.frame(idTo="sp4",idFrom="sp1",strength=NA,stringsAsFactors=FALSE)
#   expect_error(as.alienData(idObs=idObs, interactPair=interactPair,verbose=FALSE),"Some species ids in 'idTo' are not in 'idObs'")
#
#   interactPair <-  data.frame(idTo="4",idFrom="1",strength=NA,stringsAsFactors=FALSE)
#   expect_error(as.alienData(idObs=idObs, interactPair=interactPair,verbose=FALSE),"Some individus ids in 'idTo' are not in 'idObs'")
#
#   interactPair <- data.frame(idTo="sp1",idFrom="sp4",strength=NA,stringsAsFactors=FALSE)
#   expect_error(as.alienData(idObs=idObs, interactPair=interactPair,verbose=FALSE),"Some species ids in 'idFrom' are not in 'idObs'")
#
#   interactPair <-  data.frame(idTo="1",idFrom="4",strength=NA,stringsAsFactors=FALSE)
#   expect_error(as.alienData(idObs=idObs, interactPair=interactPair,verbose=FALSE),"Some individus ids in 'idFrom' are not in 'idObs'")
#
#   interactPair <-  data.frame(idTo=c("2","2","3"),idFrom=c("2","2","2"),strength=c(NA,NA,NA), stringsAsFactors=FALSE)
#   expect_error(as.alienData(idObs=idObs, interactPair=interactPair,verbose=FALSE),"Some 'idFrom' and 'idTo' are duplicated")
#
# })
