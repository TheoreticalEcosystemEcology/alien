# Set seed
set.seed(43)

#######################
# Generate bogus adjMat
#######################
bipart <- matrix(rbinom(n = 20, prob = 0.6, size = 1),
                 nrow = 5, ncol = 4)

# Add row an column names
rownames(bipart) <- letters[1:5]
colnames(bipart) <- LETTERS[1:4]

##########################
# Generate bogus traitFrom
##########################
# Convert TraitF to data.frame 
TraitFDF <- data.frame(tr1 = rnorm(5),
                       tr2 = rnorm(5),
                       tr3 = as.factor(c("red", "red",
                                         "blue", "green",
                                         "green")))

rownames(TraitFDF) <- letters[1:5]

########################
# Generate bogus traitTo
########################
# Convert TraitT to data.frame 
TraitTDF <- data.frame(Tr1 = rnorm(4),
                       Tr2 = as.factor(c("red", "red",
                                         "blue","blue")),
                       Tr3 = rnorm(4))

rownames(TraitTDF) <- LETTERS[1:4]

##########################
# Generate bogus phyloFrom
##########################
phyloF <- ape::rtree(n = 5, tip.label = letters[1:5])

##########################
# Generate bogus phyloFrom
##########################
phyloT <- ape::rtree(n = 4, tip.label = LETTERS[1:4])

##############################
# Generate bogus traitDistFrom
##############################
TraitFDist <- dist(rnorm(5))
attributes(TraitFDist)$Labels <- letters[1:5]

############################
# Generate bogus traitDistTo
############################
TraitTDist <- dist(rnorm(4))
attributes(TraitTDist)$Labels <- LETTERS[1:4]

##############################
# Generate bogus phyloDistFrom
##############################
phyloFDist <- dist(rnorm(5))
attributes(phyloFDist)$Labels <- letters[1:5]

############################
# Generate bogus phyloDistTo
############################
phyloTDist <- dist(rnorm(4))
attributes(phyloTDist)$Labels <- LETTERS[1:4]

#################
# Build alienData
#################
AllData <- alienData(adjMat = bipart,
                     traitFrom = TraitFDF,
                     traitTo = TraitTDF,
                     phyloFrom = phyloF,
                     phyloTo = phyloT,
                     traitDistFrom = TraitFDist,
                     traitDistTo = TraitTDist,
                     phyloDistFrom = phyloFDist, 
                     phyloDistTo = phyloTDist)

###################
# Test fitKNN trait
###################
# Use traits distance matrices in AllData
fitKNNDistRes <-fitKNN(AllData,
                       distFrom = "jaccard",
                       distTo = "bray",
                       distTraitFrom = NULL,
                       distTraitTo = NULL, 
                       weight = 0.5,
                       nNeig = 3, 
                       phylo = FALSE)

# Construct distance matrices from raw traits in AllData
fitKNNnoDistRes <-fitKNN(AllData,
                         distFrom = "jaccard",
                         distTo = "bray",
                         distTraitFrom = "manhattan",
                         distTraitTo = "euclidean", 
                         weight = 0.5,
                         nNeig = 3, 
                         phylo = FALSE)

# Build numeric trait matrix
traitFromNum <- model.matrix(~ -1 +.,
                             data = AllData$traitFrom)
traitToNum <- model.matrix(~ -1 +.,
                           data = AllData$traitTo)

# Construct distance matrix
traitDistFromNum <- vegan::vegdist(traitFromNum,
                                   method = "manhattan")
traitDistToNum <- vegan::vegdist(traitToNum,
                                 method = "euclidean")

#-----
# Test
#-----
test_that("fitKNN - trait expected output", {
# Check From traits distance
expect_equivalent(AllData$traitDistFrom,
                 attributes(fitKNNDistRes)$distTraitFrom)

expect_equivalent(traitDistFromNum,
                 attributes(fitKNNnoDistRes)$distTraitFrom)

# Check To traits distance
expect_equivalent(AllData$traitDistTo,
                 attributes(fitKNNDistRes)$distTraitTo)

expect_equivalent(traitDistToNum,
                 attributes(fitKNNnoDistRes)$distTraitTo)
})

###################
# Test fitKNN phylo
###################
# Use phylogenetic distance matrices
phyloDistData <- alienData(adjMat = bipart,
                           traitFrom = TraitFDF,
                           traitTo = TraitTDF,
                           phyloFrom = NULL,
                           phyloTo = NULL,
                           traitDistFrom = TraitFDist,
                           traitDistTo = TraitTDist,
                           phyloDistFrom = phyloFDist, 
                           phyloDistTo = phyloTDist)

# Use phylogenetic distance matrices in AllData
fitKNNDistPhyloRes <-fitKNN(phyloDistData,
                            distFrom = "jaccard",
                            distTo = "bray",
                            distTraitFrom = NULL,
                            distTraitTo = NULL, 
                            weight = 0.5,
                            nNeig = 3, 
                            phylo = TRUE)

# Use phylo object
phyloApeData <- alienData(adjMat = bipart,
                          traitFrom = TraitFDF,
                          traitTo = TraitTDF,
                          phyloFrom = phyloF,
                          phyloTo = phyloT,
                          traitDistFrom = TraitFDist,
                          traitDistTo = TraitTDist,
                          phyloDistFrom = NULL, 
                          phyloDistTo = NULL)

# Construct distance matrices from raw traits in AllData
fitKNNPhyloApeRes <-fitKNN(phyloApeData,
                           distFrom = "jaccard",
                           distTo = "bray",
                           distTraitFrom = NULL,
                           distTraitTo = NULL, 
                           weight = 0.5,
                           nNeig = 3, 
                           phylo = TRUE)

# Build cophenetic matrices
cophenFrom <- as.dist(ape::cophenetic.phylo(phyloApeData$phyloFrom))
cophenTo <- as.dist(ape::cophenetic.phylo(phyloApeData$phyloTo))

#-----
# Test
#-----
test_that("fitKNN - phylo expected output", {
  # Check From traits distance
  expect_equivalent(AllData$phyloDistFrom,
                    attributes(fitKNNDistPhyloRes)$distTraitFrom)
  
  expect_equivalent(cophenFrom,
                    attributes(fitKNNPhyloApeRes)$distTraitFrom)
  
  # Check To traits distance
  expect_equivalent(AllData$phyloDistTo,
                    attributes(fitKNNDistPhyloRes)$distTraitTo)
  
  expect_equivalent(cophenTo,
                    attributes(fitKNNPhyloApeRes)$distTraitTo)
})

################################
# Test KNN calculations - no NAs
################################
# Use traits distance matrices in AllData
fitKNNRes <-fitKNN(AllData,
                   distFrom = "jaccard",
                   distTo = "bray",
                   distTraitFrom = NULL,
                   distTraitTo = NULL, 
                   weight = 0.4,
                   nNeig = 3, 
                   phylo = FALSE)

#----------------------
# Calculate KNN by hand
#----------------------
# Distance matrix
fromDist <- as.matrix(vegan::vegdist(AllData$adjMat, method = "jaccard"))
toDist <- as.matrix(vegan::vegdist(t(AllData$adjMat), method = "bray"))

# Weight
weight <- 0.4

# Weighted distance matrix
fromDistw <- fromDist * 0.6 + 0.4 * as.matrix(AllData$traitDistFrom)
toDistw <- toDist * 0.6 + 0.4 * as.matrix(AllData$traitDistTo)

# Result object
KNNres <- matrix(NA, nrow = 5, ncol = 4)

# Estimation
for(i in 1:5){
  for(j in 1:4){
    ordFrom <- order(fromDistw[i,])[-1]
    ordTo <- order(toDistw[j,])[-1]
    
    dataSelFrom <- AllData$adjMat[i,ordTo][1:3]
    dataSelTo <- AllData$adjMat[ordFrom,j][1:3]
    
    KNNres[i,j] <- sum(c(dataSelFrom,dataSelTo)) / (3 * 2)
  }
}

# Trick to compare the result obtained from fitKNN
fitKNNResMat <- matrix(NA, nrow = 5, ncol = 4)
fitKNNResMat[,1:4] <- fitKNNRes[,1:4]

expect_equivalent(KNNres, fitKNNResMat)

#-----
# Test
#-----
test_that("fitKNN no NA expected output", 
          expect_equivalent(KNNres, fitKNNResMat))


##################################
# Test KNN calculations - with NAs
##################################
#################
# Build alienData
#################
# Add NA
bipartNA <- bipart
bipartNA[3,2]<-NA
bipartNA[2,1]<-NA

AllDataNA <- alienData(adjMat = bipartNA,
                     traitFrom = TraitFDF,
                     traitTo = TraitTDF,
                     phyloFrom = phyloF,
                     phyloTo = phyloT,
                     traitDistFrom = TraitFDist,
                     traitDistTo = TraitTDist,
                     phyloDistFrom = phyloFDist, 
                     phyloDistTo = phyloTDist)


# Use traits distance matrices in AllData
fitKNNNARes <-fitKNN(AllDataNA,
                     distFrom = "jaccard",
                     distTo = "bray",
                     distTraitFrom = NULL,
                     distTraitTo = NULL, 
                     weight = 0.4,
                     nNeig = 3, 
                     phylo = FALSE)

# *Warnings are OK

#----------------------
# Calculate KNN by hand
#----------------------
# Distance matrix
fromDistNA <- as.matrix(vegan::vegdist(AllDataNA$adjMat,
                                       method = "jaccard",
                                       na.rm = TRUE))
toDistNA <- as.matrix(vegan::vegdist(t(AllDataNA$adjMat),
                                     method = "bray",
                                     na.rm = TRUE))

# Weight
weight <- 0.4

# Weighted distance matrix
fromDistNAw <- fromDistNA * 0.6 + 0.4 * as.matrix(AllData$traitDistFrom)
toDistNAw <- toDistNA * 0.6 + 0.4 * as.matrix(AllData$traitDistTo)

# Result object
KNNNAres <- matrix(NA, nrow = 5, ncol = 4)

# Estimation
for(i in 1:5){
  for(j in 1:4){
    ordFrom <- order(fromDistNAw[i,])[-1]
    ordTo <- order(toDistNAw[j,])[-1]
    
    dataSelFrom <- AllDataNA$adjMat[i,ordTo][1:3]
    dataSelTo <- AllDataNA$adjMat[ordFrom,j][1:3]
    
    KNNNAres[i,j] <- sum(c(dataSelFrom,dataSelTo)) / (3 * 2)
  }
}

# Trick to compare the result obtained from fitKNN
fitKNNResMat <- matrix(NA, nrow = 5, ncol = 4)
fitKNNResMat[,1:4] <- fitKNNRes[,1:4]

expect_equal(KNNres, fitKNNResMat)

######
# Test
######
test_that("fitKNN NA expected output", 
          expect_equivalent(KNNres, fitKNNResMat))
