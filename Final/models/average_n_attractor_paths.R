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

average_n_attractor_paths <- function(network, number_of_runs, start_state){
  previous_state = start_state   # initialised with start state
  names(previous_state) = network$genes
  previous_path_len = NA
  previous_path = NA
  
  transitions <- matrix(start_state, ncol=1)
  
  for (n in seq(1,number_of_runs)){
      
  
  #   # att <- getAttractors(network=network, type="asynchronous", method="chosen", startStates=list(start_state), returnTable = T)
  #   # path_to_attractor <- getPathToAttractor(network=att, state=start_state, includeAttractorStates = "all")
  #   # path_len = length(path_to_attractor[,1])
  #   next_state <- stateTransition(network = network, state = previous_state, type = "asynchronous")
  #   print(previous_state)
  #   print("####next:")
  #   print(next_state)
  #   i = 0 # dev variable
  #   while (!identical(next_state, previous_state)){
  #     print(i)
  #     transitions <- cbind(transitions, next_state)
  #     previous_state <- next_state
  #     next_state <- stateTransition(network = network, state = previous_state, type = "asynchronous")
  #     
  #     print(previous_state)
  #     print("####next:")
  #     print(next_state)
  #     
  #     i = i+1
  #     
  #   }
  #   # print(c("Attractor #:",n))
  #   # print(transitions)
      
  }
  
}

## Examples: 
## asynchronous
library(BoolNet)
setwd("~/System_Biology/Final/models")
intrinsic_feedback <- loadNetwork("intrinsic_feedback.boolnet")
start_state_in = c(1,0,1,0,0,0,0,0,0,1,0,0)
average_n_attractor_paths(network = intrinsic_feedback, number_of_runs = 1, start_state = start_state_in)


## dev stuff
next_step <- get_asynchronous_next_step(intrinsic_feedback, start_state_in)
step_matrix <- get_asynchronous_attractor(intrinsic_feedback, start_state_in)
# next_state <- stateTransition(network = intrinsic_feedback, state = start_state_in, type = "asynchronous")
# transitions <- matrix(start_state_in, ncol=1)
# length(transitions)
# print(next_state)
# cbind(transitions, next_state)
# print(transitions)
# View(transitions)

