library(devtools)
install_github("TheoreticalEcosystemEcology/alienR", ref = "alienData")
library("alienR")
load("data/argsAlienData.RData") # How I created this file is explained in the explnation.Rmd. File is in attachment
dataBartomeus <- as.alienData(idObs=idObs,interactPair=interactPair,traitInd=traitInd,traitSp=traitSp)
str(dataBartomeus)

head(dataBartomeus$idObs)
head(dataBartomeus$interactSp) #being this a bipartite netw, you don't want to have a square matrix here
dim(dataBartomeus$interactSp)
head(dataBartomeus$interactInd)
dim(dataBartomeus$interactInd) #same here, this is quite a waste of space, because most indiv won't interact with others, and certainly not with themselves
head(dataBartomeus$coOcc) #this I think is not well calculated. SHould be from idObs
head(dataBartomeus$traitSp)
head(dataBartomeus$traitInd)
