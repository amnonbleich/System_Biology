plot_attractor_path <- function(network, startStates, title="", genes="all", colors=c(), save_png=F){
  "Function that plots a line graph of components in an attractor path of a syncronous simulation.
   Inputs:
    network: boolean Network
    startStates: binary vector of start states for each gene
    title: optional, title of the plot
    genes: optional; either vector of gene names (strings) or indices. If not provided all are plotted.
    colors: optional, vector of colors
    save_png: optional Boolean; if TRUE then plots gets saved in current directory"
  library(BoolNet)
  attractor_info <- getAttractors(network = network, method = "chosen", startStates = list(startStates), 
                                  type="synchronous", returnTable = T)
  path_matrix <- getPathToAttractor(network=attractor_info, includeAttractorStates = "all", state=startStates)
  
  if ( (typeof(genes)=="character") && (genes == "all")){
    print("genes vector not defined. All genes of networks will be included in plot.")
    genes = colnames(path_matrix)
  }
  if (length(colors) < length(genes)){
    print("Color vector too short. Creating random color vector.")
    colors = sample(colours(), length(genes))
  }
  x = seq(1, length(path_matrix[,1]))
  plot(x=x, type='n', ylab = "activity", xlab= "time", 
       ylim = c(0,1), bty='L', yaxt="n", xaxt="n")
  if (title!=""){
    title(title)
  }
  for (l in (1:length(genes))){
    y = path_matrix[,genes[l]]
    lines(x=x, y= y, type='b', col = colors[l], lwd=2)
  }
  par(xpd=T, mar=c(5.1, 4.1, 4.1, 10))
  legend("right",inset=c(-0.4,0), legend = genes, fill = colors, title = "Genes", border="white",
         bty="n")
  if (save_png){
    # save file
    filename = paste(title, "png", sep=".")
    dev.copy(png,filename, width=800, height=400)
    # dev.copy(png,filename)
    dev.off()
    print(paste("Wrote file", filename, sep =" "))
  }
  
}

# Example call:

library(BoolNet)
setwd("~/System_Biology/Final/models")
intrinsic_feedback <- loadNetwork("intrinsic_feedback.boolnet", symbolic = F)
start_state_in = c(1,0,1,0,0,0,0,0,0,1,0,0)
genes_to_plot = c("BCL2", "Apaf1", "IAP", "Apoptosis")
cols=c("green", "orange", "blue", "red")

plot_attractor_path(network=intrinsic_feedback,startStates=start_state_in, genes=genes_to_plot, title = "Intrinsic Apoptosis Pathway", 
                    colors = cols, save_png = F)
