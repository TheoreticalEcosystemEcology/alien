context("alienData function")

####
nind <- 10
nsit <- 5
df_ex0 <- data.frame(
  idInd = paste0("ind", sprintf("%02d", 1:10)),
  idSp = paste0("sp", sprintf("%02d", 1:10)),
  stringsAsFactors=FALSE)
##
df_exa <- df_ex3 <- df_ex2 <- df_ex1 <- df_ex0
names(df_exa)[2] <- "val1"
df_ex1$idInd[nind] <- df_ex1$idInd[1L]
df_ex2$idSp[nind] <- df_ex1$idSp[1L]
df_ex3$idInd[nind] <- df_ex1$idSp[1L]
df_ex4 <- cbind(df_ex1, val1 = runif(nind))

####
sp1 <- sample(1:nind, 8, replace=T)
sp2 <- sample(1:nind, 8, replace=T)
sit <- sample(1:nsit, 8, replace=T)
##
df_int0 <- data.frame(
  idFrom = paste0("sp", sprintf("%02d", sp1)),
  idTo = paste0("sp", sprintf("%02d", sp2)),
  stringsAsFactors = FALSE)
df_inta <- df_intb <- df_int2 <- df_int1 <- df_int0
names(df_inta)[1L] <- "new"
names(df_intb)[2L] <- "new"
df_int1[1L,1L] <- "new"
df_int2[1L,2L] <- "new"

###
df_sit0 <-  data.frame(
  idSite = paste0("site", sprintf("%02d", 1:5)),
  stringsAsFactors = FALSE)
df_sit2 <- df_sit1 <- df_sit0
names(df_sit1)[1L] <- "new"
df_sit2$idSite[5L] <- df_sit2$idSite[1L]
##
df_int4 <- cbind(df_int0, idSite = paste0("site", sprintf("%02d", sit)))
df_int6 <- df_int5 <- df_int4

###
df_occ0 <-  data.frame(
  idSite = paste0("site", sprintf("%02d", 1:5)),
  idSp = paste0("sp", sprintf("%02d", 1:5)),
  stringsAsFactors = FALSE)
df_occ4 <-df_occ3 <- df_occ2 <- df_occ1 <- df_occ0
names(df_occ1)[1L] <- "new"
names(df_occ2)[2L] <- "new"
df_occ3[1L,1L] <- "new"
df_occ4[1L,2L] <- "new"

####
res0 <- as.alienData(df_ex0, df_int0, dfSite=df_sit0, verbose=F)



test_that("check dfSpecies", {
  expect_error(as.alienData(df_exa, df_int0), '"idSp" %in% names(dfSpecies) is not TRUE', fixed = TRUE)
  expect_error(as.alienData(df_ex1, df_int0), "!any(table(dfSpecies$idInd) > 1) is not TRUE", fixed = TRUE)
  expect_error(as.alienData(df_ex2[,-1], df_int0), '"idSp" %in% names(dfSpecies) is not TRUE', fixed = TRUE)
  expect_error(as.alienData(df_ex3, df_int0), "!any(dfSpecies$idInd %in% dfSpecies$idSp) is not TRUE", fixed = TRUE)
  expect_error(as.alienData(df_ex4, df_int0, trait=4))
  expect_error(as.alienData(df_ex4, df_int0, phylo="val2"))
  expect_error(as.alienData(df_ex4, df_int0, taxo=4))
})


test_that("check dfInteract", {
  expect_error(as.alienData(df_ex0, df_inta), '"idFrom" %in% names(dfInteract) is not TRUE', fixed = TRUE)
  expect_error(as.alienData(df_ex0, df_intb), '"idTo" %in% names(dfInteract) is not TRUE', fixed = TRUE)
  expect_error(as.alienData(df_ex0, df_int1), "all(dfInteract$idFrom %in% dfSpecies$idSp) is not TRUE", fixed=TRUE)
  expect_error(as.alienData(df_ex0, df_int2), "all(dfInteract$idTo %in% dfSpecies$idSp) is not TRUE", fixed=TRUE)
})

test_that("check dfSite", {
  expect_error(as.alienData(df_ex0, df_int0, dfSite = df_sit1), '"idSite" %in% names(dfSite) is not TRUE', fixed = TRUE)
  expect_error(as.alienData(df_ex0, df_int0, dfSite = df_sit2), "all(table(dfSite$idSite) == 1) is not TRUE", fixed = TRUE)
})

test_that("check dfOcc", {
  expect_error(as.alienData(df_ex0, df_int0, dfSite = df_sit0, dfOcc = df_occ1), '"idSite" %in% names(dfOcc) is not TRUE', fixed = TRUE)
  expect_error(as.alienData(df_ex0, df_int0, dfSite = df_sit0, dfOcc = df_occ2), '"idSp" %in% names(dfOcc) is not TRUE', fixed = TRUE)
  expect_error(as.alienData(df_ex0, df_int0, dfSite = df_sit0, dfOcc = df_occ3), "all(dfOcc$idSite %in% dfSite$idSite) is not TRUE", fixed = TRUE)
  expect_error(as.alienData(df_ex0, df_int0, dfSite = df_sit0, dfOcc = df_occ4), "all(dfOcc$idSp %in% dfSpecies$idSp) is not TRUE", fixed = TRUE)
})



test_that("check output values", {
  expect_warning(as.alienData(df_ex0, df_int0, dfSite=df_sit0, verbose=F), "Site information provided without any occurrence")
  expect_is(res0, "alienData")
  expect_equal(res0$nbSpecies, 10)
  expect_equal(res0$nbInteractions, 8)
})

# tester le level des inetractions
# binary
# site in inetarctions dgf
# idInd dans dfOcc
