average_n_attractor_paths <- function(network, number_of_runs, start_state){
  start_state = start_state
  previous_path_len = NA
  previous_path = NA
  for (n in seq(1,number_of_runs)){
    att <- getAttractors(network=network, type="asynchronous", method="chosen", startStates=list(start_state))
    path_to_attractor <- getPathToAttractor(network=network, state=start_state, includeAttractorStates = "all")
    path_len = length(path_to_attractor[,1])
    if (n!=1){
      # if (path_len != previous_path_len){
      if (!identical(path_to_attractor, previous_path)){
        print("YAY! We have differing paths!")
      }
      # else{
      #   print("nope")
      # }
    }
    previous_path_len <- length(path_to_attractor[,1])
    previous_path <- path_to_attractor
  }
  
}