setwd('~/Workspace/System_Biology/seminar/')
install.packages("BoolNet");
library(BoolNet);

### Build network
GSD=loadNetwork('GSD.boolnet')

### Analyze ###

#all possible initial activation states
wt_attractors=getAttractors(GSD, type="synchronous", method="exhaustive", returnTable=T)
par(mfrow=c(1,length(wt_attractors$attractors)))
plotAttractors(wt_attractors)
plotAttractors(wt_attractors, mode="graph")
# get attractors with probabilities
print(markovSimulation(GSD, returnTable=FALSE))

## male pathway
#set UGR to ON
male_genesOn = c('UGR')
male_attractors=getAttractors(GSD, type="synchronous", genesON=male_genesOn, returnTable=T)
par(mfrow=c(2,length(male_attractors$attractors)))
plotAttractors(male_attractors)
plotAttractors(male_attractors, mode="graph")


## female pathway
# set UGR & WNT4 to ON
female_genesOn = c('UGR','WNT4')
female_attractors=getAttractors(GSD, type="synchronous", genesON=female_genesOn, returnTable=T)
par(mfrow=c(2,length(female_attractors$attractors)))
plotAttractors(female_attractors)
plotAttractors(female_attractors, mode="graph")

### compare to random network with same structure
comparisson<-testNetworkProperties(GSD, numRandomNets = 10)

### Additional Analyzations
# stateTransition(GSD, rep(1,19))
# trans_table<-getTransitionTable(wt_attractors)
# print(trans_table[2,])
# getStateSummary(wt_attractors, c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1))
# path<-getPathToAttractor(GSD,rep(1,19))
# plotSequence(sequence=path)


### Pertubation Analysis
# r <- perturbTrajectories(GSD, measure="hamming", numSamples=100, flipBits=1)
perturbedNet <- perturbNetwork(GDS, perturb="functions", method="bitflip")


res <- testNetworkProperties(GSD, numRandomNets=100, testFunction="testAttractorRobustness", testFunctionParams = list(copies=100, perturb="functions"))
testNetworkProperties(GSD, numRandomNets=100, testFunction="testTransitionRobustness", testFunctionParams=list(numSamples=100), alternative="less")
testNetworkProperties(GSD, numRandomNets=100, testFunction="testIndegree")