install.packages("BoolNet)
library(BoolNet)
data(cellcycle)
plotNetworkWiring(cellcycle)
attr <- getAttractors(cellcycle)
plotStateGraph(attr)
print(cellcycle)









