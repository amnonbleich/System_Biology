library(BoolNet)
# setwd("~/System_Biology/Final/models/")
setwd("~/Workspace/uni/System_Biology/Final/models/")

extrinsic <- loadNetwork("extrinsic.boolnet")
intrinsic <- loadNetwork("intrinsic.boolnet")
intrinsic_feedback <- loadNetwork("intrinsic_feedback.boolnet")

plotNetworkWiring(extrinsic)
# plotNetworkWiring(intrinsic)
plotNetworkWiring(intrinsic_feedback)

start_state_in = c(1,0,1,0,0,0,0,0,0,1,0,0)

## attractors wt
par(mfrow=c(1,1))
start_state_in = c(1,0,1,0,0,0,0,0,0,1,0,0)
att = getAttractors(intrinsic, method="exhaustive", startStates = list(middle_start_state_in), returnTable=T)
plotAttractors(att, drawLegend=F)

## trajectory wt
par(mfrow=c(1,2))
# plotSequence(extrinsic, startState = start_state_ex)
plotSequence(intrinsic, startState = start_state_in, title = "no_feedback", drawLegend=F)
plotSequence(intrinsic_feedback, startState = start_state_in, title = "Casp3->Casp9 feedback", drawLegend=F)

##Stimulus stops - no lock effect
middle_start_state_in = c(0,1,0,1,1,1,1,1,1,0,1,1)
par(mfrow=c(1,2))
par(mfrow=c(2,2))
plotSequence(intrinsic, startState = middle_start_state_in ,title = "no_feedback", drawLegend=F)
plotSequence(intrinsic_feedback, startState = middle_start_state_in, title = "Casp3->Casp9 feedback", drawLegend=F)
attractors=getAttractors(intrinsic, type="asynchronous", method="chosen", startStates = list(middle_start_state_in), returnTable=T)
attractors_lock=getAttractors(intrinsic_feedback, type="asynchronous", method="chosen", startStates = list(middle_start_state_in), returnTable=T)
plotAttractors(attractors, drawLegend=F, title = "Asynchronous, no lock")
plotAttractors(attractors_lock, drawLegend=F, title = "Asynchronous")

######### IAP Forced off ######### 
#### Stimulus stops
## Synchronous
par(mfrow=c(2,2))
no_iap_attractors=getAttractors(intrinsic, type="synchronous", method="chosen", startStates = list(middle_start_state_in), genesOFF=("IAP"), returnTable=T)
no_iap_attractors_lock=getAttractors(intrinsic_feedback, type="synchronous", method="chosen", startStates = list(middle_start_state_in), genesOFF=("IAP"), returnTable=T)
plotAttractors(no_iap_attractors, drawLegend=F, title = "No IAP - Stimulus stops, no lock")
plotAttractors(no_iap_attractors_lock, drawLegend=F, title = " No IAP- Stimulus stops, lock,")
## Asynchronious
no_iap_attractors=getAttractors(intrinsic, type="asynchronous", method="chosen", startStates = list(middle_start_state_in), genesOFF=("IAP"), returnTable=T)
no_iap_attractors_lock=getAttractors(intrinsic_feedback, type="asynchronous", method="chosen", startStates = list(middle_start_state_in), genesOFF=("IAP"), returnTable=T)
plotAttractors(no_iap_attractors, drawLegend=F, title = "No IAP - Stimulus stops, no lock")
plotAttractors(no_iap_attractors_lock, drawLegend=F, title = " No IAP- Stimulus stops, lock,")
#### conclusion: IAP dominates the process (both in sync or async)

######### BCL2 forced on ######### 
start_state_in_BCL = c(1,0,1,0,0,0,0,0,0,0,0,0)
middle_start_state_in_BCL = c(0,1,1,1,1,1,1,1,1,0,1,1)

par(mfrow=c(1,2))
bcl2_on_attractors=getAttractors(intrinsic, type="synchronous", method="chosen", startStates = list(start_state_in_BCL), genesON=c("BCL2"), genesOFF = c("IAP"), returnTable=T)
bcl2_on_attractors_lock=getAttractors(intrinsic_feedback, type="synchronous", method="chosen", startStates = list(start_state_in_BCL), genesON=c("BCL2"), genesOFF = c("IAP"), returnTable=T)
plotAttractors(bcl2_on_attractors, drawLegend=F, title = "bcl2 on, no lock")
plotAttractors(bcl2_on_attractors_lock, drawLegend=F, title = "bcl2 on, lock")
par(mfrow=c(1,2))
bcl_on_stim_stop_attractors=getAttractors(intrinsic, type="synchronous", method="chosen", startStates = list(middle_start_state_in_BCL), genesON=c("BCL2"), genesOFF = c("IAP"), returnTable=T)
bcl_on_stim_stop_attractors_lock=getAttractors(intrinsic_feedback, type="synchronous", method="chosen", startStates = list(middle_start_state_in_BCL), genesON=c("BCL2"), genesOFF = c("IAP"), returnTable=T)
plotAttractors(bcl_on_stim_stop_attractors, drawLegend=F, title = "stimulus stops, bcl2 on, no lock")
plotAttractors(bcl_on_stim_stop_attractors_lock, drawLegend=F, title = "stimulus stops, bcl2 on, lock")


######### BCL2-IAP 00 01 10 11######### 
start_state_00 = c(1,0,0,0,0,0,0,0,0,0,0,0)
start_state_01 = c(1,0,0,0,0,0,0,0,0,1,0,0)
start_state_10 = c(1,0,1,0,0,0,0,0,0,0,0,0)
start_state_11 = c(1,0,1,0,0,0,0,0,0,1,0,0)

par(mfrow=c(2,2))
attractors_lock_00=getAttractors(intrinsic_feedback, type="asynchronous", method="chosen", startStates = list(start_state_00), genesOFF = c("BCL2","IAP"), returnTable=T)
attractors_lock_01=getAttractors(intrinsic_feedback, type="asynchronous", method="chosen", startStates = list(start_state_01), genesON=c("IAP"), genesOFF = c("BCL2"), returnTable=T)
attractors_lock_10=getAttractors(intrinsic_feedback, type="asynchronous", method="chosen", startStates = list(start_state_10), genesON=c("BCL2"), genesOFF = c("IAP"), returnTable=T)
attractors_lock_11=getAttractors(intrinsic_feedback, type="asynchronous", method="chosen", startStates = list(start_state_11), genesON=c("BCL2", "IAP"), returnTable=T)

plotAttractors(attractors_lock_00, drawLegend=F, title = "00")
plotAttractors(attractors_lock_01, drawLegend=F, title = "01")
plotAttractors(attractors_lock_10, drawLegend=F, title = "10")
plotAttractors(attractors_lock_11, drawLegend=F, title = "11")

######### BCL2-IAP 00 01 10 11 stimulus stops ######### 
middle_start_state_00 = c(0,1,0,1,1,1,1,1,1,0,1,1)
middle_start_state_01 = c(0,1,0,1,1,1,1,1,1,1,1,1)
middle_start_state_10 = c(0,1,1,1,1,1,1,1,1,0,1,1)
middle_start_state_11 = c(0,1,1,1,1,1,1,1,1,1,1,1)

par(mfrow=c(2,2))
attractors_middle_lock_00=getAttractors(intrinsic_feedback, type="synchronous", method="chosen", startStates = list(middle_start_state_00), genesOFF = c("BCL2","IAP"), returnTable=T)
attractors_middle_lock_01=getAttractors(intrinsic_feedback, type="synchronous", method="chosen", startStates = list(middle_start_state_01), genesON=c("IAP"), genesOFF = c("BCL2"), returnTable=T)
attractors_middle_lock_10=getAttractors(intrinsic_feedback, type="synchronous", method="chosen", startStates = list(middle_start_state_10), genesON=c("BCL2"), genesOFF = c("IAP"), returnTable=T)
attractors_middle_lock_11=getAttractors(intrinsic_feedback, type="synchronous", method="chosen", startStates = list(middle_start_state_11), genesON=c("BCL2", "IAP"), returnTable=T)

plotAttractors(attractors_middle_lock_00, drawLegend=F, title = "00")
plotAttractors(attractors_middle_lock_01, drawLegend=F, title = "01")
plotAttractors(attractors_middle_lock_10, drawLegend=F, title = "10")
plotAttractors(attractors_middle_lock_11, drawLegend=F, title = "11")
## the one thing that IAP gives in addition to BCL2 is breaking the lock

## robustness analysis
hamming <- perturbTrajectories(intrinsic, measure="hamming", numSamples=1000)
print(hamming$value)
hamming <- perturbTrajectories(intrinsic_feedback, measure="hamming", numSamples=1000)
print(hamming$value)

robust_no_feedback = perturbTrajectories(intrinsic,
                                         measure = c("hamming", "sensitivity", "attractor"),
                                         numSamples = 1000,
                                         flipBits = 1,
                                         updateType = c("synchronous", "asynchronous", "probabilistic"))

robust_feedback = perturbTrajectories(intrinsic_feedback,
                                      measure = c("hamming", "sensitivity", "attractor"),
                                      numSamples = 1000,
                                      flipBits = 1,
                                      updateType = c("synchronous", "asynchronous", "probabilistic"))
## same robustness ~ 0.85
print(robust_no_feedback$value)
print(robust_feedback$value) # ~724
