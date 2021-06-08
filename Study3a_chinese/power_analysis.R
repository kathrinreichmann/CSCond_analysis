
### example from Lane & Hennes (2018)

#packages
library(arm)
library(lme4)
library(lmerTest)

#generate data

ex1.fake <- function(J, K){
  
  #set up predictors (use mean and variance )
  obs <- rep (seq (1, K, length = K), J) #K measurements per person
  person <- rep (1:J, each = K) #J person IDs
  type <- rnorm (J*K, 0, 1) #type_discrete (within)
  cond <- rnorm (J*K, 0, 1) # condition (between)
 
  #fixed effects (values from previous model)
  b0 <- 64.16   #true intercept value
  b1 <- -17.39  #true estimate type_discrete
  b2 <- 0.75   #true estimate condition
  b3 <- -22.91  #true estimate provision
  
  #random effects (taken from the previous paper. variance, not standard deviations!)
  vsub.b0 <- 2090.08 #random intercept
  vsub.b1 <- 67.68 #random slope lag anxiety
  vresid <- 3324.47  #btw person variance in anxiety
  
  
  #per person (use fixed effect as mean, and random effect as standard deviation)
  b0.int <- rnorm (J, b0, sqrt (vsub.b0)) # intercept for every participant
  b1.type <- rnorm (J, b1, sqrt (vsub.b1)) #slope for type for every participant

  #generate depend measure based on the true estimates of the model
  diff <- rnorm (J*K, b0.int[person]
                + b1.type[person]*type
                + b2 * cond
                + b3 * type * cond
                , sqrt (vresid)) #residual
  
  return (data.frame(person, obs, type, cond, diff))
}

#generate example dataset with 68 people and 21 measurement time points (Observations per participant)
data <- ex1.fake(J = 200, K = 8) 
#8 observations per person
# 200 participants


#model
lme.power <- lmer(diff ~ cond*type + (type | person), data = data)
summary(lme.power)

#loop data generation and analyze it
ex1.power <- function (J, K, n.sims = 1000){
  signif <- rep (NA, n.sims) #vector that will record if the effect of interest is significant
  for (s in 1:n.sims){
    fake <- ex1.fake(J, K)  #generate fake data set in every simulation round
    lme.power <- lmer(diff ~ cond*type + (type | person), data = data)
    est <- fixef (lme.power)["cond:type"] #save parameter estimate (parameter relevant for hypothesis)
    se <- se.fixef(lme.power)["cond:type"] #save standard error
    signif[s] <- (abs(est)-2*se) > 0 #calculate significance: returns TRUE/FALSE
  }
  power <- mean(signif)
  return (power)
}

#power analysis
ex1.power(J = 200, K = 8, n.sims = 1000)
