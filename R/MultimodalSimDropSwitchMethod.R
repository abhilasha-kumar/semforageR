multimodal_switch <- function(responses, sem_sim, phon_sim, alpha){
  # takes a list of fluency responses, their consecutive semantic and phonological similarities, and an alpha parameter that dictates the weight of semantic vs. phonological cue
  # and returns a vector of 0s and 1s corresponding to clusters and switches
  
  if ((alpha>1)|(alpha<0)){
    stop('Rise Threshold parameter must be within range [0,1]')
  }
  
  simphon <- (alpha * sem_sim) + ((1-alpha)*phon_sim)
  switchVector <- c(2)
  for (k in 2:length(responses)){
    # for all except last two items, check whether the similarity between items has dropped
    if (k<length(responses)-1){
      if ((simphon[k+1]>simphon[k]) & (simphon[k-1]>simphon[k])){
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
  
  
}