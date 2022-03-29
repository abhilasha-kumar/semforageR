#install.packages("readxl")
#library("readxl")

delta_similarity_switch <- function(responses, sims) {
  # takes a list of fluency responses and their consecutive similarities as input and returns a vector of 0s and 1s corresponding to clusters and switches
  
  # define rise/fall thresholds: 0 to 1 in .25 increments
  simDeltaFallThreshold <- c(0, .25, .5, .75, 1)
  simDeltaRiseThreshold <- c(0, .25, .5, .75, 1)
  
  # create dataframe of switch vectors for each combination of thresholds
  switches <- data.frame(matrix(ncol = 0, nrow = length(responses)))
  
  for (f in simDeltaFallThreshold) {
    for (r in simDeltaRiseThreshold) {
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
          if (f < (simPrecedingToCurrentWord - simCurrentToNextWord)){ # similarity diff fell more than threshold
            currentState <- 1 # switch
          } else{
            currentState <- 0 # cluster
          }
        } else{ # previous state was a switch
          if (r < (simCurrentToNextWord - simPrecedingToCurrentWord)){ # similarity diff is greater than our rise threshold
            currentState <- 0 # cluster
          } else{
            currentState <- 1 # switch
          }
        } 
        
        switchVector <- append(switchVector, currentState)
        previousState = currentState
      }

      switches[, paste("f", toString(f), "r", toString(r))] <- switchVector
    }
  }
  switches
}

#data <- read_excel("animals_VFT_responses_fMRI_study_n30.xlsx")
#firstSub <- dplyr::filter(data, subject == 50001)

#delta_similarity_switch(firstSub$corpus_response_used_in_semantic_analysis, firstSub$context_semantic_similarity)

