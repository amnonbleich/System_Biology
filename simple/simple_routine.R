library(BoolNet)
library(readr)
setwd("~/System_Biology_Seminar/simple")

network1 <- loadNetwork("simple_system_model_remodelled.boolnet")
start_state1 = c(0,0,0,0,1,0,1,0,1,1,1)
plotSequence(network1, startState = start_state1 )
pertubed_network1 <- perturbNetwork(network1, perturb = "functions", method="bitflip", maxNumBits = 10)
plotSequence(pertubed_network1, startState = start_state1 )
