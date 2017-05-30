install.packages("BoolNet)
library(BoolNet)
cellcycle <- loadNetwork("cellcycle.txt")
data(cellcycle)
plotNetworkWiring(cellcycle)
attr <- getAttractors(cellcycle)
plotStateGraph(attr)
print(cellcycle)









