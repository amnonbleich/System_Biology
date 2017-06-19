library(BoolNet)
setwd("~/System_Biology/Final/models/mathias_v1")

extrinsic <- loadNetwork("extrinsic.boolnet")
intrinsic <- loadNetwork("intrinsic.boolnet")

start_state_ex = c(1,0,0,0,0)
start_state_in = c(1,1,0,0,1,1,1,0,0,0,0,0)
start_state_in = c(1,1,0,0,1,0,0,0,0,0,0,0)

plotSequence(extrinsic, startState = start_state_ex)
plotSequence(intrinsic, startState = start_state_in)

attractors_in=getAttractors(intrinsic, type="synchronous", method="exhaustive", returnTable=T)
plotAttractors(attractors_in, drawLegend=F)

attractors_in_chosen=getAttractors(intrinsic, type="synchronous", method='chosen', startStates = list(start_state_in), returnTable=T)
plotAttractors(attractors_in_chosen)
