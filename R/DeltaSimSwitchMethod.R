
delta_similarity_switch <- function(responses, sims, rise_thresh, fall_thresh) {
  # takes a list of fluency responses, their consecutive similarities, a rise and fall threshold as input and returns a vector of 0s and 1s corresponding to clusters and switches
  
  if ((rise_thresh>1)|(rise_thresh<0)){
    stop('Rise Threshold parameter must be within range [0,1]')
  }
  if ((fall_thresh>1)|(fall_thresh<0)){
    stop('Fall Threshold parameter must be within range [0,1]')
  }
  
  # obtain consecutive semantic similarities b/w responses
  # z-score similarities within participant
  similaritiesZ <- (sims[-1]-mean(sims[-1]))/sd(sims[-1])
  medianSim <- median(similaritiesZ)
  meanSim = 0
  similaritiesZ <- append(similaritiesZ, NaN, 0)
      
  # define subject level threshold = median (zscored similarities)
  # firstSwitchSimThreshold <- meanSim
  firstSwitchSimThreshold <- medianSim
      
  switchVector <- c(2) # first item designated with 2
  # for second item, if similarity < median, then switch, else cluster
  if (similaritiesZ[2] < firstSwitchSimThreshold){
    switchVector <- append(switchVector, 1)
  } else{
    switchVector <- append(switchVector, 0)
  }
  currentState <- switchVector[2]
  previousState <- currentState
      
  # for all other items:
  for (n in 2:(length(responses)-1)){
    # consider n-1, n, n+1 items
    simPrecedingToCurrentWord <- similaritiesZ[n]
    simCurrentToNextWord <- similaritiesZ[n+1]
        
    if (previousState == 0){ #if previous state was a cluster
      if (fall_thresh < (simPrecedingToCurrentWord - simCurrentToNextWord)){ # similarity diff fell more than threshold
        currentState <- 1 # switch
      } else{
        currentState <- 0 # cluster
      }
    } else{ # previous state was a switch
      if (rise_thresh < (simCurrentToNextWord - simPrecedingToCurrentWord)){ # similarity diff is greater than our rise threshold
        currentState <- 0 # cluster
      } else{
        currentState <- 1 # switch
      }
    } 
        
    switchVector <- append(switchVector, currentState)
    previousState = currentState
  }

  switchVector
}


