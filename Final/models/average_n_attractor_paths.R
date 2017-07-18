get_asynchronous_next_step <- function(network, start_state){
  # Applies asynchronous update rules until state of system changes
  # Inputs:
  #  network: BoolNet Network 
  #  start_state: state to compute transition from
  # Output:
  # next_state: state of the system after the step
  previous_state = start_state   # initialised with start state
  names(previous_state) = network$genes
  next_state <- stateTransition(network = network, state = previous_state, type = "asynchronous")
  j = 0
  while ((identical(previous_state, next_state)) && (j < length(network$genes)*500 ) ){ 
    # cap maximum operations to dirtily avoid endless loops
    # output from solvability algortihm would be better here
    previous_state <- next_state
    next_state <- stateTransition(network = network, state = previous_state, type = "asynchronous") # repeat if still identical
    j= j+1
  }
  return(next_state)
}

get_asynchronous_attractor <- function(network, start_state){
  # Applies asynchronous update rules until state of system does not change anymore
  # Inputs:
  #  network: BoolNet Network 
  #  start_state: state to compute transition from
  # Output:
  # steps_matrix: overview of all values of all genes at all timepoints
  previous_step <- start_state
  names(previous_step) = network$genes
  next_step <- get_asynchronous_next_step(network, previous_step)
  
  steps_matrix <- matrix(start_state, ncol=1)
  steps_matrix <- cbind(steps_matrix, next_step) #  concatenate matrices 
  while(!identical(previous_step, next_step)){
    # repeat as long as system still changes
    previous_step <- next_step
    next_step <- get_asynchronous_next_step(network, previous_step) # repeat until not identical any more
    steps_matrix <- cbind(steps_matrix, next_step)
  }
  next_step <- get_asynchronous_next_step(network, previous_step) # one last time in order emphazise that we hit a steady state attractor
  steps_matrix <- cbind(steps_matrix, next_step)
  colum_header <- sprintf("t%s", seq(1,length(steps_matrix[1,])))
  colnames(steps_matrix, colum_header)
  
  return(steps_matrix)
  
}

ensure_dimensions <- function(m1,m2){
  # Fixes dimensions of first input matrix to fit the dimensions of second input matrix
  cols_m1 = length(m1[1,])
  cols_m2 = length(m2[1,])
  rows_m1 = length(m1[,1])
  rows_m2 = length(m2[,1])
  if ( cols_m1 <  cols_m2){
    difference <- cols_m2 - cols_m1
    for (i in (seq(1, difference))){
      # add columns only consisting of zeros
      m1 <- cbind(m1, numeric(rows_m1))
      cols_m1 = length(m1[1,])
      print("Fixed matrix dimensions. Added column.")
    }
  }
  if ( rows_m1 <  rows_m2){
    difference <- rows_m2 - rows_m1
    for (i in (seq(1, difference))){
      m1 <- rbind(m1, numeric(cols_m1))
      rows_m2 = length(m2[,1])
      print("Fixed matrix dimensions. Added row")
    }
  }
  return(m1)
}

average_n_attractor_paths <- function(network, number_of_runs, start_state){
  # Computes n asynchronous attractors and their attractor paths in matrix. Gives averaged values.
  # Inputs:
  #  network: BoolNet Network 
  #  number_of_runs: number of iterations to compute average activity
  #  start_state: state to compute transition from
  # Output:
  # averaged_steps_matrix: overview of all values of all genes at all timepoints, averaged for all runs
  previous_state = start_state 
  names(start_state) = network$genes
  
  sum_holder <- get_asynchronous_attractor(network, start_state) # at leas once
  if (number_of_runs > 1){
    for (n in seq(2,number_of_runs)){
      next_matrix <- get_asynchronous_attractor(network, start_state)
      sum_holder <- ensure_dimensions(sum_holder, next_matrix) # fit dimensions by adding zero rows /columns to enable matrix sum
      sum_holder <- sum_holder + next_matrix
    }
  }
  averaged_steps_matrix <- sum_holder/number_of_runs
  return(averaged_steps_matrix)
  
}

## Examples: 
# asynchronous
# library(BoolNet)
# setwd("~/System_Biology/Final/models")
# intrinsic_feedback <- loadNetwork("intrinsic_feedback.boolnet")
# start_state_in = c(1,0,1,0,0,0,0,0,0,1,0,0)
# n_runs_matrix <- average_n_attractor_paths(network = intrinsic_feedback, number_of_runs = 1,
#                                            start_state = start_state_in)
# 
# write.csv(n_runs_matrix, "1_run_intrinsic_feedback_1.csv")


# source("plot_attractor_path.R")
# load("~/System_Biology/Final/models/.RData")
# 
# x = seq(1, length(n_runs_matrix_5[1,]))
# plot(x=x, type='n', ylab = "activity", xlab= "time", 
#      ylim = c(0,1), bty='L', xlim = c(3,9))
# title("average activity of 1000 runs")
# genes = rownames(n_runs_matrix_5)
# genes <- c("BAX", "BAK", "CytC","Apaf1","SMAC", "IAP")
# colors = sample(colours(), length(genes))
# colors <-c("blue", "darkblue", "orange", "black", "darkgrey", "green")
# for (l in (1:length(genes))){
#   y = n_runs_matrix_5[genes[l],]
#   lines(x=x, y= y, type='b', col = colors[l], lwd=2)
# }
