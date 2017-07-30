set.seed(1871)
##---- Simulated data
nbnod <- 20
nint <- 40
##---- Sample in a bipartite way
ind1 <- sample(1:12, nint, replace=TRUE)
ind2 <- sample(13:nbnod, nint, replace=TRUE)
##---- Nodes
df_nd0 <- data.frame(
  idNodes = paste0("id", sprintf("%02d", 1:nbnod)),
  stringsAsFactors=FALSE)
##---- Edges
df_int0 <- data.frame(
  idFrom = paste0("id", sprintf("%02d", ind1)),
  idTo = paste0("id", sprintf("%02d", ind2)),
  stringsAsFactors = FALSE) %>% unique
##---- Sites
nsit <- 10
sit <- sample(1:nsit, nrow(df_int0), replace=TRUE)
idsit <- paste0("site", sprintf("%02d", sit))
df_sit0 <-  data.frame(
  idSite = paste0("site", sprintf("%02d", 1:nsit)),
  stringsAsFactors = FALSE) %>% unique
##----
 df_occ0 <-  data.frame(
   idSite = paste0("site", sprintf( "%02d", sample(1:nsit, 2*nbnod, replace=TRUE))),
   idNodes = rep(df_nd0$idNodes, 2),
   stringsAsFactors = FALSE) %>% unique
