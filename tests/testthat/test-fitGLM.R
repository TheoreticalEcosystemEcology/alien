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

#############################
# Test overfitted model - glm
#############################
# Function
#options(warn = -1) # To remove warning
#fitGLMRes <- fitGLM(data = AllData, formula = ~ .*.,
#                    family = binomial(link = "logit"))
#options(warn = 0) # Back to default

# Trick to compare the result obtained from fit
#fitGLMResMat <- matrix(NA, nrow = 5, ncol = 4)
#fitGLMResMat[,1:4] <- fitGLMRes[,1:4]

# Test
#test_that("fitGLM overfitting expected output", 
#          expect_equivalent(AllData$adjMat, fitGLMResMat))

#######################
# Test if formula works
#######################
# Overly simple formula
fitGLMRes <- fitGLM(data = AllData,
                         formula = ~ -1 + tr1 + tr2,
                         family = binomial(link = "logit"))

# Trick to compare the result obtained from fit
fitGLMResMat <- matrix(NA, nrow = 5, ncol = 4)
fitGLMResMat[,1:4] <- fitGLMRes[,1:4]

# Overly simple formula
fitGLMBasicRes <- fitGLM(data = AllData,
                         formula = ~ -1 + tr1,
                         family = binomial(link = "logit"))

# Trick to compare the result obtained from fit
fitGLMBasicResMat <- matrix(NA, nrow = 5, ncol = 4)
fitGLMBasicResMat[,1:4] <- fitGLMBasicRes[,1:4]


# Test
test_that("fitGLM formula", 
          expect_false(isTRUE(all.equal(fitGLMBasicResMat,
                                        fitGLMResMat))))

# Overly simple formula with species as random effect 
fitGLMRandomRes <- fitGLM(data = AllData,
                          formula = ~ -1 + scale(tr1),
                          family = binomial(link = "logit"),
                          spRandom = TRUE)

fitGLMnoRandomRes <- fitGLM(data = AllData,
                            formula = ~ -1 + scale(tr1),
                            family = binomial(link = "logit"),
                            spRandom = FALSE)

# Trick to compare the result obtained from fit
fitGLMRandomResMat <- matrix(NA, nrow = 5, ncol = 4)
fitGLMRandomResMat[,1:4] <- fitGLMRandomRes[,1:4]

fitGLMnoRandomResMat <- matrix(NA, nrow = 5, ncol = 4)
fitGLMnoRandomResMat[,1:4] <- fitGLMnoRandomRes[,1:4]

# Test
test_that("fitGLM formula", 
          expect_false(isTRUE(all.equal(fitGLMRandomResMat,
                                        fitGLMnoRandomResMat))))

