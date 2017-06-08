context("alienData function")

##----
nbnod <- 20
nsit <- 10
nint <- 15
ind1 <- sample(1:nbnod, nint, replace=TRUE)
ind2 <- sample(1:nbnod, nint, replace=TRUE)
sit <- sample(1:nsit, nint, replace=TRUE)
idsit <- paste0("site", sprintf("%02d", sit))
##----
df_nd0 <- data.frame(
  idNodes = paste0("id", sprintf("%02d", 1:nbnod)),
  stringsAsFactors=FALSE)
df_nd3 <- df_nd2 <- df_nd1 <- df_nd0
##
names(df_nd1) <- "val1"
df_nd2$idNodes[nbnod] <- df_nd0$idNodes[1L]
df_nd3$var1 <- runif(nbnod)
df_nd3$var2 <- runif(nbnod)

##----
df_int0 <- data.frame(
  idFrom = paste0("id", sprintf("%02d", ind1)),
  idTo = paste0("id", sprintf("%02d", ind2)),
  stringsAsFactors = FALSE)
df_int5 <- df_int4 <- df_int3 <- df_int2 <- df_int1 <- df_int0
names(df_int1)[1L] <- "new"
names(df_int2)[2L] <- "new"
df_int3[1L,1L] <- "new"
df_int4[1L,2L] <- "new"
df_int5$idSite <- idsit
df_int7 <- df_int6 <- df_int5
df_int6$idSite[1L] <- "new"
df_int7$value <- runif(nint)

##----
df_sit0 <-  data.frame(
  idSite = paste0("site", sprintf("%02d", 1:nsit)),
  stringsAsFactors = FALSE)
df_sit2 <- df_sit1 <- df_sit0
names(df_sit1)[1L] <- "new"
df_sit2$idSite[5L] <- df_sit2$idSite[1L]

##----
 df_occ0 <-  data.frame(
   idSite = paste0("site", sprintf( "%02d", sample(1:nsit, 2*nbnod, replace=TRUE))),
   idNodes = rep(df_nd0$idNodes, 2),
   stringsAsFactors = FALSE)
 df_occ4 <-df_occ3 <- df_occ2 <- df_occ1 <- df_occ0
 names(df_occ1)[1L] <- "new"
 names(df_occ2)[2L] <- "new"
 df_occ3[1L,1L] <- "new"
 df_occ4[1L,2L] <- "new"

# ####
res0 <- as.alienData(df_nd0, df_int0, dfSites=df_sit0, verbose=F)
res1 <- as.alienData(df_nd0, df_int0, dfSites=df_sit0, dfOcc = df_occ0, verbose=F)
res2 <- as.alienData(df_nd3, df_int0, dfSites=df_sit0, dfOcc = df_occ0, trait=2, phylo=2, taxo=3, verbose=F)
res3 <- as.alienData(df_nd3, df_int7, verbose=F)


##
test_that("check dfSpecies", {
  expect_error(as.alienData(df_nd1, df_int0), '"idNodes" %in% names(dfNodes) is not TRUE', fixed = TRUE)
  expect_error(as.alienData(df_nd2, df_int0), "!any(table(dfNodes$idNodes) > 1) is not TRUE", fixed = TRUE)
})

##
test_that("check dfEdges", {
  expect_error(as.alienData(df_nd0, df_int1), '"idFrom" %in% names(dfEdges) is not TRUE', fixed = TRUE)
  expect_error(as.alienData(df_nd0, df_int2), '"idTo" %in% names(dfEdges) is not TRUE', fixed = TRUE)
  expect_error(as.alienData(df_nd0, df_int3), "all(dfEdges$idFrom %in% dfNodes$idNodes) is not TRUE", fixed=TRUE)
  expect_error(as.alienData(df_nd0, df_int4), "all(dfEdges$idTo %in% dfNodes$idNodes) is not TRUE", fixed=TRUE)
})

##
test_that("check dfSites", {
  expect_error(as.alienData(df_nd0, df_int0, dfSites = df_sit1), '"idSite" %in% names(dfSites) is not TRUE', fixed = TRUE)
  expect_error(as.alienData(df_nd0, df_int0, dfSites = df_sit2), "all(table(dfSites$idSite) == 1) is not TRUE", fixed = TRUE)
  expect_error(as.alienData(df_nd0, df_int6, dfSites = df_sit0), "all(dfSites$idSite %in% dfEdges$idSite) is not TRUE", fixed = TRUE)
})

##
test_that("check dfOcc", {
  expect_error(as.alienData(df_nd0, df_int0, dfSites = df_sit0, dfOcc = df_occ1), '"idSite" %in% names(dfOcc) is not TRUE', fixed = TRUE)
  expect_error(as.alienData(df_nd0, df_int0, dfSites = df_sit0, dfOcc = df_occ2), '"idNodes" %in% names(dfOcc) is not TRUE', fixed = TRUE)
  expect_error(as.alienData(df_nd0, df_int0, dfSites = df_sit0, dfOcc = df_occ3), "all(dfOcc$idSite %in% dfSites$idSite) is not TRUE", fixed = TRUE)
  expect_error(as.alienData(df_nd0, df_int0, dfSites = df_sit0, dfOcc = df_occ4), "all(dfOcc$idNodes %in% dfNodes$idNodes) is not TRUE", fixed = TRUE)
})

##
test_that("check output values", {
  expect_warning(as.alienData(df_nd0, df_int0, dfSites=df_sit0, verbose=F), "Site information provided without any occurrence")
  expect_is(res1, "alienData")
  expect_equal(res1$nbNodes, nbnod)
  expect_equal(res1$nbInteractions, nint)
  expect_equal(res1$nbSite, nsit)
  expect_equal(res1$availableMeths$available, c(TRUE, FALSE, FALSE))
  expect_true(all(res1$dfEdges$value==1))
  expect_equal(res2$nmTrait, "var1")
  expect_equal(res2$nmPhylo, "var1")
  expect_equal(res2$nmTaxo, "var2")
  expect_true(all(res2$availableMeths$available))
  expect_equal(res3$nbSites, length(unique(idsit)))
  expect_true(!all(res3$dfEdges$value==1))
})
