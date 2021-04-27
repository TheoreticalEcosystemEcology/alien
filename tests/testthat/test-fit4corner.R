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

###################
# Test NULL formula
###################
# Model
cornerModel <- mvabund::traitglm(L = AllData$adjMat,
                                 R = AllData$traitFrom, 
                                 Q = AllData$traitTo,
                                 family = binomial(link = "logit"),
                                 formula = NULL)

# Prediction
cornerPred <- predict(cornerModel)

# Function
#fit4cornerRes <- fit4corner(data = AllData,
#                            formula = NULL,
#                            family =  binomial(link = "logit"))

# Trick to compare the result obtained from fitKNN
#fit4cornerResMat <- matrix(NA, nrow = 5, ncol = 4)
#fit4cornerResMat[,1:4] <- fit4cornerRes[,1:4]

#-----
# Test
#-----
#test_that("fit4corner NULL formula expected output", 
#          expect_equivalent(cornerPred, fit4cornerResMat))


#######################
# Test specific formula
#######################
# Model
cornerFormModel <- mvabund::traitglm(L = AllData$adjMat,
                                     R = AllData$traitFrom, 
                                     Q = AllData$traitTo,
                                     family = binomial(link = "logit"),
                                     formula = ~tr1:Tr3)

# Prediction
cornerFormPred <- predict(cornerFormModel)

# Function
fit4cornerFormRes <- fit4corner(data = AllData,
                            formula = ~tr1:Tr3,
                            family =  binomial(link = "logit"))

# Trick to compare the result obtained from fitKNN
fit4cornerFormResMat <- matrix(NA, nrow = 5, ncol = 4)
fit4cornerFormResMat[,1:4] <- fit4cornerFormRes[,1:4]

#-----
# Test
#-----
test_that("fit4corner formula expected output", 
          expect_equivalent(cornerFormPred, fit4cornerFormResMat))

