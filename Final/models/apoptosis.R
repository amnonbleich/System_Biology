library(BoolNet)
setwd("~/System_Biology/Final/models/")
# setwd("~/Workspace/uni/System_Biology/Final/models/")

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
att = getAttractors(intrinsic, method="exhaustive", returnTable=T)
plotAttractors(att, drawLegend=F)

## trajectory wt
par(mfrow=c(1,2))
# plotSequence(extrinsic, startState = start_state_ex)
plotSequence(intrinsic, startState = start_state_in, title = "no_feedback", drawLegend=F)
plotSequence(intrinsic_feedback, startState = start_state_in, title = "Casp3->Casp9 feedback", drawLegend=F)

## get attractor sequence, defined start state
start_state_in = c(1,0,1,0,0,0,0,0,0,1,0,0)
att <- getAttractors(network=intrinsic_feedback, type="asynchronous", method="chosen", startStates=list(start_state_in))

##Stimulus stops - no lock effect
middle_start_state_in = c(0,1,0,1,1,1,1,1,1,0,1,1)
par(mfrow=c(1,2))
par(mfrow=c(2,2))

# original:
plotSequence(intrinsic, startState = middle_start_state_in ,title = "no_feedback", drawLegend=F) 
plotSequence(intrinsic_feedback, startState = middle_start_state_in, title = "Casp3->Casp9 feedback", drawLegend=F)

# new, alternative:
genes_to_plot = c("stimulus", "IAP","Casp9", "Apoptosis") # optional
genes_to_plot = c("IAP","Apoptosis","BCL2") # optional
cols=c("orange", "blue", "red") #optional
# par(mfrow=c(1,2))
source("plot_attractor_path.R")
plot_attractor_path(intrinsic, title = "no_feedback", startStates = start_state_in, 
                    second_round_states = middle_start_state_in, genes = genes_to_plot, colors = cols, save_png = T, draw_legend = F)
plot_attractor_path(intrinsic_feedback, title = "IAP - BCL2 relationship", startStates = start_state_in, 
                    second_round_states = middle_start_state_in, genes = genes_to_plot, colors = cols, draw_legend = F, save_png = T )

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

attractors_lock_00=getAttractors(intrinsic_feedback, type="asynchronous", method="chosen", startStates = list(start_state_00), genesOFF = c("BCL2","IAP"), returnTable=T)
attractors_lock_01=getAttractors(intrinsic_feedback, type="asynchronous", method="chosen", startStates = list(start_state_01), genesON=c("IAP"), genesOFF = c("BCL2"), returnTable=T)
attractors_lock_10=getAttractors(intrinsic_feedback, type="asynchronous", method="chosen", startStates = list(start_state_10), genesON=c("BCL2"), genesOFF = c("IAP"), returnTable=T)
attractors_lock_11=getAttractors(intrinsic_feedback, type="asynchronous", method="chosen", startStates = list(start_state_11), genesON=c("BCL2", "IAP"), returnTable=T)

# old:
par(mfrow=c(2,2))
plotAttractors(attractors_lock_00, drawLegend=F, title = "00")
plotAttractors(attractors_lock_01, drawLegend=F, title = "01")
plotAttractors(attractors_lock_10, drawLegend=F, title = "10")
plotAttractors(attractors_lock_11, drawLegend=F, title = "11")


# does not work to well with par(mfrow)
plot_attractor_path(network=intrinsic_feedback, startStates = start_state_00, genes = genes_to_plot, colors = cols)
plot_attractor_path(network=intrinsic_feedback, startStates = start_state_01, genes = genes_to_plot, colors = cols)
plot_attractor_path(network=intrinsic_feedback, startStates = start_state_10, genes = genes_to_plot, colors = cols)
plot_attractor_path(network=intrinsic_feedback, startStates = start_state_11, genes = genes_to_plot, colors = cols)

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

robust_no_feedback_att = perturbTrajectories(intrinsic,
                                             measure = "attractor",
                                             numSamples = 1000,
                                             flipBits = 1)

robust_no_feedback_ham = perturbTrajectories(intrinsic,
                                             measure = "hamming",
                                             numSamples = 1000,
                                             flipBits = 1,
                                             updateType ="asynchronous")

robust_feedback_att = perturbTrajectories(intrinsic_feedback,
                                             measure = "attractor",
                                             numSamples = 1000,
                                             flipBits = 1)

robust_feedback_ham = perturbTrajectories(intrinsic_feedback,
                                             measure = "hamming",
                                             numSamples = 1000,
                                             flipBits = 1,
                                             updateType ="asynchronous")


## comparable robustness 
print(robust_no_feedback_att$value)
print(robust_no_feedback_ham$value)
print(robust_feedback_att$value) # ~724
print(robust_feedback_ham$value) # ~724
