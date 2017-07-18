plot_attractor_path <- function(network, startStates, second_round_states = "" , title="", genes="all", 
                                colors=c(), save_png=F, draw_legend =T){
   # Function that plots a line graph of components in an attractor path of a synchronous simulation.
   # Inputs:
   #  network: BoolNet Network
   #  startStates: binary vector of start states for each gene
   #  second_round_states: optional, if provided a second simulation is done with this binary vector as
   #                       the start state. The resulting matrices get merged
   #  title: optional, title of the plot 
   #  genes: optional, either "all" or vector of gene names (strings) or indices. If not provided all are plotted.
   #  colors: optional, vector of colors
   #  save_png: optional Boolean, if TRUE then plots gets saved in current directory
   #  draw_legend: optional Boolean, if TRUE then legend is drawn
  
  library(BoolNet)
  
  # prepare data
  attractor_info <- getAttractors(network = network, method = "chosen", startStates = list(startStates),
                                  type="synchronous", returnTable = T)

  path_matrix <- getPathToAttractor(network=attractor_info, includeAttractorStates = "all", state=startStates)
  path_matrix <- rbind(rep(0, length(network$genes)), path_matrix) # add zero vectors in front of all values
  path_matrix <- rbind(rep(0, length(network$genes)), path_matrix) # to show signal coming in
  
  if (typeof(second_round_states)!="character"){
    # if a second state config is given
    attractor_info_2 <- getAttractors(network = network, method = "chosen", startStates = list(second_round_states),
                                    type="synchronous", returnTable = T)
    path_matrix_2 <- getPathToAttractor(network=attractor_info_2, includeAttractorStates = "all", state=second_round_states)
  }
  
  if ( (typeof(genes)=="character") && (genes == "all")){
    # default value for genes
    print("genes vector not defined. All genes of networks will be included in plot.")
    genes = colnames(path_matrix)
  }
  if (length(colors) < length(genes)){
    print("Color vector too short. Creating random color vector.")
    colors = sample(colours(), length(genes))
  }
  
  if (typeof(second_round_states)!="character"){
    nr_rows_path_matrix <- length(path_matrix[,1])
    # concatenations:
    path_matrix <- rbind(path_matrix, path_matrix[nr_rows_path_matrix,])
    path_matrix <- rbind(path_matrix, path_matrix_2) # path_matrix_2 created by above if statement
  }
  
  # Plotting:
  x = seq(1, length(path_matrix[,1]))
  plot(x=x, type='n', ylab = "activity", xlab= "time", 
       ylim = c(0,1), bty='L')
  if (title!=""){
    title(title)
  }
  for (l in (1:length(genes))){
    y = path_matrix[,genes[l]]
    lines(x=x, y= y, type='b', col = colors[l], lwd=2)
  }
  if (draw_legend){
    par(xpd=T, mar=c(5.1, 4.1, 4.1, 11))
    legend("right",inset=c(-0.8,0), legend = genes, fill = colors, title = "Genes", border="white",
         bty="n" )
  }
  if (save_png){
    # save file
    filename = paste(title, "png", sep=".")
    dev.copy(png,filename, width=800, height=400)
    dev.off()
    print(paste("Wrote file", filename, sep =" "))
  }
 
}

# Example call:
# 
# library(BoolNet)
# setwd("~/System_Biology/Final/models")
# intrinsic_feedback <- loadNetwork("intrinsic_feedback.boolnet", symbolic = F)
# start_state_in = c(1,0,1,0,0,0,0,0,0,1,0,0)
# genes_to_plot = c("BCL2", "Apaf1", "IAP", "Apoptosis")
# cols=c("green", "orange", "blue", "red")
# 
# 
# plot_attractor_path(network=intrinsic_feedback,startStates=start_state_in, genes=genes_to_plot, title = "Intrinsic Apoptosis Pathway",
#                     colors = cols, save_png = F)
