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
##----
df_int0 <- data.frame(
  idFrom = paste0("id", sprintf("%02d", ind1)),
  idTo = paste0("id", sprintf("%02d", ind2)),
  stringsAsFactors = FALSE)
##----
df_sit0 <-  data.frame(
  idSite = paste0("site", sprintf("%02d", 1:nsit)),
  stringsAsFactors = FALSE)
##----
 df_occ0 <-  data.frame(
   idSite = paste0("site", sprintf( "%02d", sample(1:nsit, 2*nbnod, replace=TRUE))),
   idNodes = rep(df_nd0$idNodes, 2),
   stringsAsFactors = FALSE)
# ####
