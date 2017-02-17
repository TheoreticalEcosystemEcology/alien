library(devtools)
install_github("TheoreticalEcosystemEcology/alienR", ref = "alienData")
library("alienR")
load("data/argsAlienData.RData") # How I created this file is explained in the explnation.Rmd. File is in attachment
dataBartomeus <- as.alienData(idObs=idObs,interactPair=interactPair,traitInd=traitInd,traitSp=traitSp)
str(dataBartomeus)
