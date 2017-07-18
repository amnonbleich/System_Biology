get_asynchronous_next_step <- function(network, start_state){
  # eventuell auch alle gleichen zwischenschritte übergeben und auch in die matrix packen
  # dann sieht man wie lange ein signal brauch um durch zu kommen 
  previous_state = start_state   # initialised with start state
  names(previous_state) = network$genes
  next_state <- stateTransition(network = network, state = previous_state, type = "asynchronous")
  j = 0
  while ((identical(previous_state, next_state)) && (j < length(network$genes)*500 ) ){ 
    previous_state <- next_state
    next_state <- stateTransition(network = network, state = previous_state, type = "asynchronous")
    # print(c("step", j))
    j= j+1
  }
  return(next_state)
}

get_asynchronous_attractor <- function(network, start_state){
  previous_step <- start_state
  names(previous_step) = network$genes
  next_step <- get_asynchronous_next_step(network, previous_step)
  
  steps_matrix <- matrix(start_state, ncol=1)
  steps_matrix <- cbind(steps_matrix, next_step)
  i = 0
  while(!identical(previous_step, next_step)){
    # print(c("att",i))
    previous_step <- next_step
    next_step <- get_asynchronous_next_step(network, previous_step)
    steps_matrix <- cbind(steps_matrix, next_step)
    i = i +1
  }
  next_step <- get_asynchronous_next_step(network, previous_step) # one last time in order to show we hit a steady state attractor
  steps_matrix <- cbind(steps_matrix, next_step)
  colum_header <- sprintf("t%s", seq(1,length(steps_matrix[1,])))
  colnames(steps_matrix, colum_header)
  
  return(steps_matrix)
  
}

ensure_dimensions <- function(m1,m2){
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
  previous_state = start_state 
  names(start_state) = network$genes
  
  sum_holder <- get_asynchronous_attractor(network, start_state)
  if (number_of_runs > 1){
    for (n in seq(2,number_of_runs)){
      
      next_matrix <- get_asynchronous_attractor(network, start_state)
      sum_holder <- ensure_dimensions(sum_holder, next_matrix)
      sum_holder <- sum_holder + next_matrix
        
    }
  }
  return((sum_holder)/number_of_runs)
  
}

## Examples: 
## asynchronous
# library(BoolNet)
# setwd("~/System_Biology/Final/models")
# intrinsic_feedback <- loadNetwork("intrinsic_feedback.boolnet")
# start_state_in = c(1,0,1,0,0,0,0,0,0,1,0,0)
# n_runs_matrix_5 <- average_n_attractor_paths(network = intrinsic_feedback, number_of_runs = 4000,
#                                            start_state = start_state_in)
# write.csv(n_runs_matrix, "4000_runs_intrinsic_feedback.csv")

