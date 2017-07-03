library(BoolNet)
# setwd("~/System_Biology/Final/models/")
setwd("~/Workspace/uni/System_Biology/Final/models/")

extrinsic <- loadNetwork("extrinsic.boolnet")
intrinsic <- loadNetwork("intrinsic.boolnet")
intrinsic_feedback <- loadNetwork("intrinsic_feedback.boolnet")

plotNetworkWiring(extrinsic)
plotNetworkWiring(intrinsic)
plotNetworkWiring(intrinsic_feedback)

start_state_ex = c(1,0,0,0,0)
# start_state_in = c(1,1,0,0,1,1,1,0,0,0,0,0)
start_state_in = c(1,0,0,1,0,0,0,0,0,0,0,0)
start_state_in_IAP = c(1,0,0,1,0,0,0,0,0,1,0,0)
# start_state_in_feedback = c(1,0,0,1,0,0,0,0,0,0,0,0)

par(mfrow=c(1,2))
plotSequence(extrinsic, startState = start_state_ex)
plotSequence(intrinsic, startState = start_state_in, title = "no_feedback")
plotSequence(intrinsic_feedback, startState = start_state_in, title = "Casp3->Casp9 feedback")

plotSequence(intrinsic, startState = start_state_in, title = "no_feedback")
plotSequence(intrinsic_feedback, startState = start_state_in, title = "Casp3->Casp9 feedback")

plotSequence(intrinsic, startState = start_state_in_IAP, title = "no_feedback")
plotSequence(intrinsic_feedback, startState = start_state_in_IAP, title = "Casp3->Casp9 feedback")

attractors_in=getAttractors(intrinsic, type="synchronous", method="exhaustive", returnTable=T)
plotAttractors(attractors_in, drawLegend=F)

attractors_in_chosen=getAttractors(intrinsic, type="synchronous", method='chosen', startStates = list(start_state_in), returnTable=T)
plotAttractors(attractors_in_chosen)

attractors_in_feedback=getAttractors(intrinsic_feedback, type="asynchronous", method="chosen", startStates = list(start_state_in),  returnTable=T)
attractors_in_feedback=getAttractors(intrinsic_feedback, type="synchronous", method="chosen", startStates = list(start_state_in),  returnTable=T)
plotAttractors(attractors_in_feedback, drawLegend=T)
#### short remark: when IAP is present, there is no apoptosis. without is, bot apoptosis and no apoptosis are possible. in a synchronous network, there's always apoptosis