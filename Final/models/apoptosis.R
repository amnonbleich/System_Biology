library(BoolNet)
# setwd("~/System_Biology/Final/models/")
setwd("~/Workspace/System_Biology/System_Biology/Final/models/")

extrinsic <- loadNetwork("extrinsic.boolnet")
intrinsic <- loadNetwork("intrinsic.boolnet")
intrinsic_feedback <- loadNetwork("intrinsic_feedback.boolnet")

plotNetworkWiring(extrinsic)
# plotNetworkWiring(intrinsic)
plotNetworkWiring(intrinsic_feedback)

start_state_ex = c(1,0,0,0,0)
start_state_in = c(1,0,1,0,0,0,0,0,0,1,0,0)

par(mfrow=c(1,2))
# plotSequence(extrinsic, startState = start_state_ex)
plotSequence(intrinsic, startState = start_state_in, title = "no_feedback", drawLegend=F)
plotSequence(intrinsic_feedback, startState = start_state_in, title = "Casp3->Casp9 feedback", drawLegend=F)

middle_start_state_in = c(0,1,0,1,1,1,1,1,1,0,1,1)
## check lock effect
par(mfrow=c(1,2))
plotSequence(intrinsic, startState = middle_start_state_in ,title = "no_feedback", drawLegend=F)
plotSequence(intrinsic_feedback, startState = middle_start_state_in, title = "Casp3->Casp9 feedback", drawLegend=F)

## check lock effect with IAP on/off without stimulus after apoptosis (synchronous/asynchronous)
par(mfrow=c(2,2))
attractors_in=getAttractors(intrinsic, type="synchronous", method="chosen", startStates = list(middle_start_state_in), returnTable=T)
attractors_in_lock=getAttractors(intrinsic_feedback, type="synchronous", method="chosen", startStates = list(middle_start_state_in), returnTable=T)
no_iap_attractors_in=getAttractors(intrinsic, type="synchronous", method="chosen", startStates = list(middle_start_state_in), genesOFF=("IAP"), returnTable=T)
no_iap_attractors_in_lock=getAttractors(intrinsic_feedback, type="synchronous", method="chosen", startStates = list(middle_start_state_in), genesOFF=("IAP"), returnTable=T)

plotAttractors(attractors_in, drawLegend=F, title = "stimulus stops, no lock")
plotAttractors(attractors_in_lock, drawLegend=F, title = "lock - stimulus stops")
plotAttractors(no_iap_attractors_in, drawLegend=F, title = "stimulus stops, no lock, no IAP")
plotAttractors(no_iap_attractors_in_lock, drawLegend=F, title = "lock - stimulus stops, no IAP")

attractors_in=getAttractors(intrinsic, type="asynchronous", method="chosen", startStates = list(middle_start_state_in), returnTable=T)
attractors_in_lock=getAttractors(intrinsic_feedback, type="asynchronous", method="chosen", startStates = list(middle_start_state_in), returnTable=T)
no_iap_attractors_in=getAttractors(intrinsic, type="asynchronous", method="chosen", startStates = list(middle_start_state_in), genesOFF=("IAP"), returnTable=T)
no_iap_attractors_in_lock=getAttractors(intrinsic_feedback, type="asynchronous", method="chosen", startStates = list(middle_start_state_in), genesOFF=("IAP"), returnTable=T)

plotAttractors(attractors_in, drawLegend=F, title = "stimulus stops, no lock")
plotAttractors(attractors_in_lock, drawLegend=F, title = "lock - stimulus stops")
plotAttractors(no_iap_attractors_in, drawLegend=F, title = "stimulus stops, no lock, no IAP")
plotAttractors(no_iap_attractors_in_lock, drawLegend=F, title = "lock - stimulus stops, no IAP")
#### conclusion: IAP dominates the process (both in sync or async)

### BCL2 forced on, with and w/o feedback lock #########################
start_state_in_BCL = c(1,0,1,0,0,0,0,0,0,0,0,0)
middle_start_state_in_BCL = c(0,1,1,1,1,1,1,1,1,0,1,1)

par(mfrow=c(2,2))
bcl2_on_attractors_in=getAttractors(intrinsic, type="synchronous", method="chosen", startStates = list(start_state_in_BCL), genesON=c("BCL2"), genesOFF = c("IAP"), returnTable=T)
bcl2_on_attractors_in_lock=getAttractors(intrinsic_feedback, type="synchronous", method="chosen", startStates = list(start_state_in_BCL), genesON=c("BCL2"), genesOFF = c("IAP"), returnTable=T)
middle_bcl2_on_attractors_in=getAttractors(intrinsic, type="synchronous", method="chosen", startStates = list(middle_start_state_in_BCL), genesON=c("BCL2"), genesOFF = c("IAP"), returnTable=T)
middle_bcl2_on_attractors_in_lock=getAttractors(intrinsic_feedback, type="synchronous", method="chosen", startStates = list(middle_start_state_in_BCL), genesON=c("BCL2"), genesOFF = c("IAP"), returnTable=T)

plotAttractors(bcl2_on_attractors_in, drawLegend=F, title = "bcl2 on, no lock")
plotAttractors(bcl2_on_attractors_in_lock, drawLegend=F, title = "bcl2 on, lock")
plotAttractors(middle_bcl2_on_attractors_in, drawLegend=F, title = "stimulus stops, bcl2 on, no lock")
plotAttractors(middle_bcl2_on_attractors_in_lock, drawLegend=F, title = "stimulus stops, bcl2 on, lock")


### BCL2-IAP 00 01 10 11 stimulus as start state #########################
start_state_00 = c(1,0,0,0,0,0,0,0,0,0,0,0)
start_state_01 = c(1,0,0,0,0,0,0,0,0,1,0,0)
start_state_10 = c(1,0,1,0,0,0,0,0,0,0,0,0)
start_state_11 = c(1,0,1,0,0,0,0,0,0,1,0,0)

par(mfrow=c(2,2))
attractors_in_lock_00=getAttractors(intrinsic_feedback, type="synchronous", method="chosen", startStates = list(start_state_00), genesOFF = c("BCL2","IAP"), returnTable=T)
attractors_in_lock_01=getAttractors(intrinsic_feedback, type="synchronous", method="chosen", startStates = list(start_state_01), genesON=c("IAP"), genesOFF = c("BCL2"), returnTable=T)
attractors_in_lock_10=getAttractors(intrinsic_feedback, type="synchronous", method="chosen", startStates = list(start_state_10), genesON=c("BCL2"), genesOFF = c("IAP"), returnTable=T)
attractors_in_lock_11=getAttractors(intrinsic_feedback, type="synchronous", method="chosen", startStates = list(start_state_11), genesON=c("BCL2", "IAP"), returnTable=T)

plotAttractors(attractors_in_lock_00, drawLegend=F, title = "00")
plotAttractors(attractors_in_lock_01, drawLegend=F, title = "01")
plotAttractors(attractors_in_lock_10, drawLegend=F, title = "10")
plotAttractors(attractors_in_lock_11, drawLegend=F, title = "11")

### BCL2-IAP 00 01 10 11 stop stimulus after apoptosis #########################
middle_start_state_00 = c(0,1,0,1,1,1,1,1,1,0,1,1)
middle_start_state_01 = c(0,1,0,1,1,1,1,1,1,1,1,1)
middle_start_state_10 = c(0,1,1,1,1,1,1,1,1,0,1,1)
middle_start_state_11 = c(0,1,1,1,1,1,1,1,1,1,1,1)

par(mfrow=c(2,2))
attractors_in_middle_lock_00=getAttractors(intrinsic_feedback, type="synchronous", method="chosen", startStates = list(middle_start_state_00), genesOFF = c("BCL2","IAP"), returnTable=T)
attractors_in_middle_lock_01=getAttractors(intrinsic_feedback, type="synchronous", method="chosen", startStates = list(middle_start_state_01), genesON=c("IAP"), genesOFF = c("BCL2"), returnTable=T)
attractors_in_middle_lock_10=getAttractors(intrinsic_feedback, type="synchronous", method="chosen", startStates = list(middle_start_state_10), genesON=c("BCL2"), genesOFF = c("IAP"), returnTable=T)
attractors_in_middle_lock_11=getAttractors(intrinsic_feedback, type="synchronous", method="chosen", startStates = list(middle_start_state_11), genesON=c("BCL2", "IAP"), returnTable=T)

plotAttractors(attractors_in_middle_lock_00, drawLegend=F, title = "00")
plotAttractors(attractors_in_middle_lock_01, drawLegend=F, title = "01")
plotAttractors(attractors_in_middle_lock_10, drawLegend=F, title = "10")
plotAttractors(attractors_in_middle_lock_11, drawLegend=F, title = "11")
## the one thing that IAP gives in addition to BCL2 is breaking the lock