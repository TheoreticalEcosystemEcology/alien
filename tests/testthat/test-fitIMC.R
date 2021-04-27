# Set seed
set.seed(43)

#######################
# Generate bogus adjMat
#######################
#------
# Small
#------
bipart <- matrix(rbinom(n = 20, prob = 0.6, size = 1),
                 nrow = 5, ncol = 4)

# Add row an column names
rownames(bipart) <- letters[1:5]
colnames(bipart) <- LETTERS[1:4]

#-------
# Larger
#-------
bipartLarger <- matrix(rbinom(n = 50, prob = 0.6, size = 1),
                       nrow = 10, ncol = 5)

# Add row an column names
rownames(bipartLarger) <- letters[1:nrow(bipartLarger)]
colnames(bipartLarger) <- LETTERS[1:ncol(bipartLarger)]

#################
# Build alienData
#################
AllData <- alienData(adjMat = bipart)
AllDataLarger <- alienData(adjMat = bipartLarger)

#############################
# Test fitIMC - overfit check
#############################
test_that("fitIMC check overfit",
          expect_error(fitIMC(AllData, 
                              d = 2,
                              verbose = FALSE,
                              control = list(maxit = 1))))

#################
# Test fitIMC - d
#################
fitIMCBad <-  fitIMC(AllDataLarger, 
                     d = 1,
                     verbose = FALSE,
                     control = list(maxit = 100, seed = 42))

fitIMCBetter <-  fitIMC(AllDataLarger, 
                        d = 2,
                        verbose = FALSE,
                        control = list(maxit = 100, seed = 42))

# Calculate sums of squares
SSBad <- sum((bipartLarger - fitIMCBad)^2)
SSBetter <- sum((bipartLarger - fitIMCBetter)^2)

# Test
test_that("fitIMC d", 
          expect_gt(SSBad ,SSBetter))

#######################
# Test fitIMC - control
#######################
fitIMCBad <-  fitIMC(AllData, 
                     d = 1,
                     verbose = FALSE,
                     control = list(maxit = 1))

fitIMCBetter <-  fitIMC(AllData, 
                        d = 1,
                        verbose = FALSE,
                        control = list(maxit = 10))

# Calculate sums of squares
SSBad <- sum((bipart - fitIMCBad)^2)
SSBetter <- sum((bipart - fitIMCBetter)^2)

# Test
test_that("fitIMC control", 
          expect_gt(SSBad ,SSBetter))
