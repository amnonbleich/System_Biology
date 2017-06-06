setwd('~/Workspace/System_Biology/System_Biology_Seminar/')
# install.packages("BoolNet");
library(BoolNet);

### Build network
GSD=loadNetwork('GSD.boolnet')
plotNetworkWiring(GSD)

### Analyze ###

#all possible initial activation states
wt_attractors=getAttractors(GSD, type="synchronous", method="exhaustive", returnTable=T)
plotAttractors(wt_attractors, drawLegend=F)
plotAttractors(wt_attractors, mode="graph")

## male pathway
male_start=c(1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
female_start=c(1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0)

male_attractors_wt=getAttractors(GSD, type="synchronous", method='chosen', startStates = list(male_start), returnTable=T)
female_attractors_wt=getAttractors(GSD, type="synchronous", method='chosen', startStates = list(female_start), returnTable=T)

par(mfcol=c(3,length(female_attractors_wt$attractors)))
plotAttractors(wt_attractors, drawLegend = F, title="Exhaustive Attractors start")
plotAttractors(male_attractors_wt, drawLegend = F, title="Attractors start UGR (Male WT)")
plotAttractors(female_attractors_wt, drawLegend = F, title="Attractors start UGR+WNT4 (Female WT)")


## Perturbation - force genes on -> robustness
# page 9 last paragraph
female_genesOn = c('UGR','WNT4')
female_genesOff = c('SRY')
female_attractors_forced_on=getAttractors(GSD, type="synchronous", method='chosen', startStates = list(female_start),  genesON=female_genesOn, returnTable=T)
plotAttractors(female_attractors_forced_on, drawLegend = F, title="Attractors UGR+WNT4 ON (Female Perturbed)")
# 46,XX DSD - page 10 top
male_genesOn = c('SRY')
male_attractors_forced_on=getAttractors(GSD, type="synchronous",genesON=male_genesOn, returnTable=T)
plotAttractors(male_attractors_forced_on, drawLegend = F, title="Attractors SRY ON (Male Perturbed)")
male_genesOn = c('SOX9')
male_attractors_forced_on=getAttractors(GSD, type="synchronous",genesON=male_genesOn, returnTable=T)
plotAttractors(male_attractors_forced_on, drawLegend = F, title="Attractors SOX9 ON (Male Perturbed)")


# Trajuectory to Attractor
male_path<-getPathToAttractor(GSD,male_start)
par(mfcol=c(1,1))
plotSequence(sequence=male_path,title="Male path", drawLegend=F)
female_path<-getPathToAttractor(GSD,female_start)
plotSequence(sequence=female_path,title="Female path",drawLegend=F)


### Additional possible Analyzations

# transitions from all possible starts
trans_table<-getTransitionTable(wt_attractors)
print(trans_table[2,])
# get specific state
getStateSummary(wt_attractors, c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1))


### Pertubation Analysis
perturbTrajectories(GSD, measure="hamming", numSamples=100, flipBits=1)
perturbTrajectories(GSD, measure="sensitivity", numSamples=100, flipBits=1, gene = 'WNT4')
perturbTrajectories(GSD, measure="attractor", numSamples=100, flipBits=1)
perturbTrajectories(GSD, measure="attractor", numSamples=100, flipBits=10)
perturbedNet <- perturbNetwork(GSD, perturb=c("functions","transitions"), method = c("bitflip","shuffle"))


### other functionalities, network is too big to show here

small_network=loadNetwork('sys-bio-beispiel_model.boolnet')
plotNetworkWiring(small_network)


# Markov Chain
# par(mfcol=c(2,1))
sim<-markovSimulation(small_network, returnTable=T)
plotNetworkWiring(small_network)
plotPBNTransitions(sim)

# compare to random network with same structure
testNetworkProperties(small_network, numRandomNets=100, testFunction="testAttractorRobustness", testFunctionParams = list(copies=100, perturb="functions"))
