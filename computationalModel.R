### Generalization of evaluative conditioning, Modeling CS Variability


#packages
library(ggplot2)


#latent variables of participant
learnRate <- 0.8 #participants are not able to learn everything perfectly
repRate <- 1
decreaseFactorRepeat <- 0.95 #with every repeat, the laerning cure flattens

#experimental variables
nTrials <- 4
nCat <- 4
nExemplar <- 5
nFeaturesPerCS <- 2

#stimuli in rating phase
Stimuli <- matrix(NA, nExemplar, nFeaturesPerCS)

for (CS in 1:nrow(Stimuli)){
  Stimuli[CS, 1:nFeaturesPerCS] <- c(1, CS+1)
}

#stimuli in testing phase
FeatureStim <- unique(as.vector(Stimuli))
CSStim <- Stimuli[1,]
GSStim <- c(1,nExemplar*nFeaturesPerCS + 1)

#valence for each category
CatVal <- c()
for(cat in 1:nCat){
  Valence <- sample(c(-1,1), size = 1)
  CatVal[cat] <- Valence
}

#learning conditions
many <- 5
one <- 1


#### learning phase - information reduction
#sum up valence for each stimulus feature

#Plot results

#many
Expectation <- matrix(0, 1, nFeaturesPerCS)
category <- 1
nTrials <- 4
for (round in 1:nTrials){
  for (nrCS in 1:many){
    Valence <- CatVal[category]
    for (feat in 1:nFeaturesPerCS){
      featureNR <- Stimuli[nrCS, feat]
      if (featureNR %in% Expectation[,1]){
        Expectation[Expectation[,1] == featureNR] <- c(featureNR, Expectation[Expectation[,1] == featureNR][[2]] + Valence*learnRate*repRate)
      } else {
        NewExpectation <- c(featureNR, Valence*learnRate*repRate)
        Expectation <- rbind(Expectation, NewExpectation)
      }
    }
    Plot <- as.data.frame(Expectation)
    names(Plot) <- c("Feature", "ValenceValue")
    Plot <- Plot[-1,]
    plot(Plot)
    print(lines(Plot, col = "blue"))
    Sys.sleep(1)
  }
  repRate <- repRate*decreaseFactorRepeat
}
ExpectationMany <- Expectation[-1,]
ExpectationMany

#one
#storage location for valence values
Expectation <- matrix(0, 1, nFeaturesPerCS)

category <- 1
nTrials <- 20

for (round in 1:nTrials){
  for (nrCS in 1:one){
    Valence <- CatVal[category]
    for (feat in 1:nFeaturesPerCS){
      featureNR <- Stimuli[nrCS, feat]
      if (featureNR %in% Expectation[,1]){
        Expectation[Expectation[,1] == featureNR] <- c(featureNR, Expectation[Expectation[,1] == featureNR][[2]] + Valence*learnRate*repRate)
      } else {
        NewExpectation <- c(featureNR, Valence*learnRate*repRate)
        Expectation <- rbind(Expectation, NewExpectation)
      }
    }
    Plot <- as.data.frame(Expectation)
    Plot <- Plot[-1,]
    names(Plot) <- c("Feature", "ValenceValue")
    print(lines(Plot, col = "red"))
    Sys.sleep(1)
  }
  repRate <- repRate*decreaseFactorRepeat
}
ExpectationOne <- Expectation[-1,]
ExpectationOne

#### testing phase

# testing of features alone
#one condition
FeatureRatingOne <- matrix(0, 1, nFeaturesPerCS)
for (feat in 1:length(FeatureStim)){
  if (feat%in%ExpectationOne[,1]){
    newFeature <- ExpectationOne[ExpectationOne[,1] == feat,]
  } else {
    newFeature <- c (feat, 0)
  }
  FeatureRatingOne <- rbind(FeatureRatingOne, newFeature)
}

FeatureRatingOne <- FeatureRatingOne[-1,]
FeatureRatingOne
plot(FeatureRatingOne, type = "l")

#many condition
FeatureRatingMany <- matrix(0, 1, nFeaturesPerCS)
for (feat in 1:length(FeatureStim)){
  if (feat%in%ExpectationMany[,1]){
    newFeature <- ExpectationMany[ExpectationMany[,1] == feat,]
  } else {
    newFeature <- c (feat, 0)
  }
  FeatureRatingMany <- rbind(FeatureRatingMany, newFeature)
}
FeatureRatingMany <- FeatureRatingMany[-1,]
FeatureRatingMany
plot(FeatureRatingMany, type = "p")


## testing of CS

feat1 <- CSStim[1]
feat2 <- CSStim[2]

#formula: Rating = RatingFeature1 + RatingFeature2
CSresultMany <- FeatureRatingMany[feat1,2] + FeatureRatingMany[feat2,2]
CSresultMany

CSresultOne <- FeatureRatingOne[feat1,2] + FeatureRatingOne[feat2,2]
CSresultOne


#testing of GSs

feat1 <- GSStim[1]
feat2 <- GSStim[2]

#formula: Rating = RatingFeature1 + RatingFeature2
#many
if (feat1%in%FeatureRatingMany) {
  GSresultMany <- FeatureRatingMany[feat1,2]
} else {
  GSresultMany <- FeatureRatingMany[feat2,2]
}
GSresultMany

#one
if (feat1%in%FeatureRatingOne) {
  GSresultOne <- FeatureRatingOne[feat1,2]
} else {
  GSresultOne <- FeatureRatingOne[feat2,2]
}
GSresultOne


## using the differences between the features for generalization
## differences: distinguish between likely predictions and less likely predictions

#many
DiffExpectation <- matrix(NA, dim(FeatureRatingMany)[1], dim(FeatureRatingMany)[1])
  
for(column in 1:dim(FeatureRatingMany)[1]){
  for (row in 1:dim(FeatureRatingMany)[1]){
    if (row == column){
      DiffExpectation[row, column] <- 0
    } else {
      DiffExpectation[row, column] <- as.numeric(FeatureRatingMany[column, 2] - FeatureRatingMany[row, 2])
    
    }
  }
}

DiffExpectationMany <- DiffExpectation
DiffExpectationMany

#one
DiffExpectation <- matrix(NA, dim(FeatureRatingMany)[1], dim(FeatureRatingMany)[1])

for(column in 1:dim(FeatureRatingOne)[1]){
  for (row in 1:dim(FeatureRatingOne)[1]){
    if (row == column){
      DiffExpectation[row, column] <- 0
    } else {
      DiffExpectation[row, column] <- as.numeric(FeatureRatingOne[column, 2] - FeatureRatingOne[row, 2])
      
    }
  }
}

DiffExpectationOne <- DiffExpectation
DiffExpectationOne

#sum of differences



