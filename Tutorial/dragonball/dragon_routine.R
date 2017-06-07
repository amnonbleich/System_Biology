library(BoolNet)
library(readr)
setwd("~/System_Biology_Seminar/dragonball")

network1 <- loadNetwork("dragon-ball-simplified_model.boolnet")
start_state1 = c(0,0,0,0,0,0,0,1,0,1,0,1,0,1,1,0,1,1)
plotSequence(network1, startState = start_state1 )
