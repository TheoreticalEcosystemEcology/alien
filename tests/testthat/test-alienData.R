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
                     traitDistFrom = TraitFDist,
                     traitDistTo = TraitTDist,
                     phyloDistFrom = phyloFDist, 
                     phyloDistTo = phyloTDist)

######
# Test
######
test_that("alienData expected output", {
  expect_identical(bipart, AllData$adjMat)
  expect_identical(TraitFDF, AllData$traitFrom)
  expect_identical(TraitTDF, AllData$traitTo)
  expect_identical(TraitFDist, AllData$traitDistFrom)
  expect_identical(TraitTDist, AllData$traitDistTo)
  expect_identical(phyloFDist, AllData$phyloDistFrom)
  expect_identical(phyloTDist, AllData$phyloDistTo)
})
