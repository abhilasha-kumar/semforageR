troyer_switch <- function(responses) {
  # takes a list of fluency responses and their consecutive similarities as input and returns a vector of 0s and 1s corresponding to clusters and switches
  
  norms <- read.csv('troyernorms.csv') 
  switchVector <- c(2) # first item designated with 2
  
  for (k in 2:length(responses)){
    # check whether the current and previous item are in the same category
      item1 <- responses[k]
      item2 <- responses[k-1]
      category1 <- norms[norms$Animal == item1, "Category"]
      category2 <- norms[norms$Animal == item2, "Category"]
      if (length(intersect(category1, category2))==0){
        # if the items do not share any categories, it is a switch
        switchVector <- append(switchVector, 1)
      } else{
        # otherwise, no switch
        switchVector <- append(switchVector, 0)
      }
      
  }
  switchVector
}
#troyer_switch(c("cat", "dog", "mouse", "rat", "giraffe", "lion"))