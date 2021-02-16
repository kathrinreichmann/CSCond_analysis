### Generalization of evaluative conditioning, Modeling CS Variability


#packages
library(ggplot2)


#latent variables of participant
learnRate <- 0.5

#experimental variables
nTrials <- 4
nCat <- 4
nExemplar <- 5
nFeaturesPerCS <- 2

#stimuli in rating phase
Stimuli <- matrix(NA, nExemplar, nFeaturesPerCS)

for (CS in 1:nrow(Stimuli)){
  Stimuli[CS, 1:2] <- c(1, CS+1)
}

#stimuli in testing phase
FeatureStim <- unique(as.vector(Stimuli))
GSStim <- c(1,nrow(Stimuli) + 1)

#valence for each category
CatVal <- c()
for(cat in 1:nCat){
  Valence <- sample(c(-1,1), size = 1)
  CatVal[cat] <- Valence
}

#learning conditions
many <- 5
one <- 1


#### learning phase
#sum up valence for each stimulus feature

#one
#storage location for valence values
Expectation <- matrix(0, 1, nFeaturesPerCS)
plot(as.data.frame(Expectation))

category <- 1
nTrials <- 20

for (round in 1:nTrials){
  for (nrCS in 1:one){
    Valence <- CatVal[category]
    for (feat in 1:nFeaturesPerCS){
      featureNR <- Stimuli[nrCS, feat]
      if (featureNR %in% Expectation[,1]){
        Expectation[Expectation[,1] == featureNR] <- c(featureNR, Expectation[Expectation[,1] == featureNR][[2]] + Valence)
      } else {
        NewExpectation <- c(featureNR, Valence)
        Expectation <- rbind(Expectation, NewExpectation)
      }
    }
    Plot <- as.data.frame(Expectation)
    names(Plot) <- c("Feature", "ValenceValue")
    print(lines(Plot, col = "red"))
    Sys.sleep(0.5)
    
  }
}
ExpectationOne <- Expectation[-1,]


#many
Expectation <- matrix(0, 1, nFeaturesPerCS)
category <- 1
nTrails <- 4
for (round in 1:nTrials){
  for (nrCS in 1:many){
    Valence <- CatVal[category]
    for (feat in 1:nFeaturesPerCS){
      featureNR <- Stimuli[nrCS, feat]
      if (featureNR %in% Expectation[,1]){
        Expectation[Expectation[,1] == featureNR] <- c(featureNR, Expectation[Expectation[,1] == featureNR][[2]] + Valence)
      } else {
        NewExpectation <- c(featureNR, Valence)
        Expectation <- rbind(Expectation, NewExpectation)
      }
    }
    Plot <- as.data.frame(Expectation)
    names(Plot) <- c("Feature", "ValenceValue")
    print(lines(Plot, col = "blue"))
    Sys.sleep(0.5)
    
  }
}
ExpectationMany <- Expectation[-1,]

uF <- unique(as.vector(Stimuli))
Expectation <- rep(0, length(uF))

### Learning Phase
condition <- 1
if (condition == 1) {
  for (trial in 1:nTrials) {
    stim2present <- sample(1:nStim, size=1)
    expect <- Expectation[uF%in%Stimuli[stim2present,]]
    predError <- Valence[stim2present] - expect
    Expectation[uF%in%Stimuli[stim2present,]] <- Expectation[uF%in%Stimuli[stim2present,]] + learnRate*predError
  }
}


# keep this for condition 2, change later

Expectation <- matrix(0, 2, nStim)

condition <- 2
if (condition == 2) {
  for (trial in 1:nTrials) {
    stim2present <- sample(1:nStim, size=1)
    val2present <- Valence[stim2present]
    if (val2present==1) valIndex <- 1
    if (val2present==-1) valIndex <- 2
    expect <- Expectation[valIndex, ]
    #predError <- rep(-0.5, nStim)
    predError <- rep(0, nStim)
    predError[stim2present] <- predError[stim2present] + 1
    Expectation[valIndex, ] <- Expectation[valIndex, ] + learnRate*predError
  }
}



if (condition == 1) {     # CS before UCS
  Valence <- list(matrix(NA, length(unique(Stimuli[,1])), 2), matrix(NA, length(unique(Stimuli[,2])), 2))
  for (feature in 1:2) {
    Valence[[feature]][,1] <- unique(Stimuli[,feature])
    for (f in unique(Stimuli[, feature])) {
      Valence[[feature]][Valence[[feature]][,1]==f, 2] <- sum(Stimuli[Stimuli[,feature]==f,3])
    }
  }
  
  for (gen in 3:4) {
    if (Stimuli[gen,1] == Stimuli[1,1]) {
      Valence[1]
    } 
  }
}
if (condition == 2) {     # CS after UCS
  
}

