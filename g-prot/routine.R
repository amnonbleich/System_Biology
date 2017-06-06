library(BoolNet)
library(readr)
setwd("~/System_Biology_Seminar/g-prot")

network1 <- loadNetwork("g-protein_simplified_model.boolnet")
g_protein_symbols1 <- read_csv("~/System_Biology_Seminar/g-prot/g-protein_simplified_symbols.csv", col_names = FALSE)
g_protein_initial_vals1 <- read_csv("~/System_Biology_Seminar/g-prot/g-protein_simplified_initial_vals.csv", col_names = FALSE)
c(g_protein_initial_vals1[2])
start_state1 = c(0,0,0,0,0,1,0,1,0,1,1,0,1)
plotSequence(network1, startState = start_state1 )
# pertubed_network1 <- perturbNetwork(network1, perturb = "transitions", method="bitflip", numStates = 2)

network2 <- loadNetwork("g-protein_model.boolnet")

g_protein_symbols2 <- read_csv("~/System_Biology_Seminar/g-prot/g-protein_symbols.csv", col_names = FALSE)
g_protein_initial_vals2 <- read_csv("~/System_Biology_Seminar/g-prot/g-protein_initial_vals.csv", col_names = FALSE)
c(g_protein_initial_vals2[2])
start_state2 = c(0,0,0,0,0,0,0,1,0,1,0,1,1,0,1,1,0,1)
plotSequence(network2, startState = start_state2 )

pertubed_network1 <- perturbNetwork(network1, perturb = "functions", method="bitflip", maxNumBits = 10)
pertubed_network2 <- perturbNetwork(network2, perturb = "functions", method="bitflip", maxNumBits = 10)
plotSequence(pertubed_network1, startState = start_state1)
plotSequence(pertubed_network2, startState = start_state2)


# time_series <- generateTimeSeries(network, numSeries = 1, numMeasurements = 18)
attractors <- getAttractors(network, type="synchronous", method = "chosen", startStates = list(start_state))
plotAttractors(attractors)
table <- getTransitionTable(attractors)
table

path_attractor <- getPathToAttractor(network, state = start_state )
print(path_attractor)

