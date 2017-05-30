library(BoolNet)
setwd("~/System_Biology_Seminar/insulin")

network <- loadNetwork("insulin_model.boolnet")

res <- testNetworkProperties(GSD, numRandomNets=100, testFunction="testAttractorRobustness", 
                             testFunctionParams = list(copies=100, perturb="functions"))
  

