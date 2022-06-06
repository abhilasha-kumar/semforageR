sim_drop_switch <- function(responses, sims) {
  # takes a list of fluency responses and their consecutive similarities as input and returns a vector of 0s and 1s corresponding to clusters and switches
  
  switchVector <- c(2) # first item designated with 2
  
  for (k in 2:length(responses)){
    # for all except last two items, check whether the current and previous item are in the same category
    if (k<length(responses)-1){
      if ((sims[k+1]>sims[k]) & (sims[k-1]>sims[k])){
        # if current item is more similar to previous than next item and similarity increases in next two items
        switchVector <- append(switchVector, 1)
      } else{
        # otherwise, no switch
        switchVector <- append(switchVector, 0)
      }
      
    }else{
      # last two items are designated with a 2
      switchVector <- append(switchVector, 2)
    }
  }
  switchVector
}
