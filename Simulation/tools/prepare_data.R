#Returns a dataframe containing Cues, Outcomes, and Frequency.

#  1. It takes as an input 'myexp'--csv file containing labels, affixes, and frequencies. 
#  2. Condition: "suffix" or else ("prefix")

#If condition is "suffix": object labels will serve as cues and affixes will serve as outcomes.
#If condition is "prefix": object labels will serve as outcomes and affixes will serve as cues (prefix condition).


prepare_data <- function(myexp, condition){
  
  if (condition == "suffix"){
    Cues <- myexp$Cues
    Outcomes <- myexp$Outcomes
    Frequency <- myexp$Frequency
  } else {
    Cues <- myexp$Outcomes
    Outcomes <- myexp$Cues
    Frequency <- myexp$Frequency
  }
  
  mystims = data.frame(Cues, Outcomes, Frequency)
  i <- sapply(mystims, is.factor)
  mystims[i] <- lapply(mystims[i], as.character)
  knitr::kable(mystims)
  
  return(mystims)
  
}

