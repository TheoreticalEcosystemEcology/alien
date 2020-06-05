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

#################
# Build alienData
#################
AllData <- alienData(adjMat = bipart,
                     traitFrom = TraitFDF,
                     traitTo = TraitTDF)

