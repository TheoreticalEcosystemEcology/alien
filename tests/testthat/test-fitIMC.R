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

#################
# Build alienData
#################
AllData <- alienData(adjMat = bipart)

data <- AllData
d = 1
verbose = TRUE
control = list()
