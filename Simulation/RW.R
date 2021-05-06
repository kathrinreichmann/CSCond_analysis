### Rescola-Wagner model

## define function

update_RW <- function (value, alpha = .3, beta = .3, lambda=1){
  #compute value of compound stimulus
  value_compound <- sum(value)
  
  #compute prediction error
  prediction_error <- lambda - value_compound
  
  #compute change in strength
  value_change <- alpha*beta * prediction_error
  
  #update the associative value
  value <- value + value_change
  
  #return the new value
  return(value)
}

#### (1) simulate a conditioning experiment

n_trials <- 20

strength <- numeric(n_trials)
#to store the associative strength

for (trial in 2:n_trials){
  strength[trial] <- update_RW(strength[trial-1])
  #-1: for first trial, associative strength stays 0
}
#"present" CS-US pairings
#update associative strength at the end of each trial

print(strength)
plot(strength)

#### (2) simulate extinction

n_trials <- 50
strength <- numeric (n_trials)
lambda <- .3 #initial reward value

#trial 1-25: learning phase
#trial 26-50: extinction phase

for (trial in 2:n_trials){
  
  #lambda = 0: CS presented without US
  if (trial > 25){
    lambda <- 0
  }
  
  #update associative strength with each trial
  strength[trial] <- update_RW(
    value = strength[trial - 1],
    lambda = lambda
  )
  
}

print(strength)
plot(strength)
#associative value is reverting to 0 when extinction phase starts:

#### (3) simulate blocking

n_trials <- 50
strength_A <- rep (0, n_trials)
strength_B <- rep (0, n_trials)

#trial 1-25: present A - US
#trial26-50: present AB - US

#alpha in learning phase, 0 because B is not presented
alpha <- c(.3, 0)

for (trial in 2:n_trials){
  
  if (trial > 15){
    alpha <- c(.3,.3) #blocking phase
  }
  
  #current associative strengths
  v_old <- c(strength_A[trial-1], strength_B[trial-1])
  
  #update associative strengths
  v_new <- update_RW(
    value = v_old,
    alpha = alpha
  )
  
  #record new strengths
  strength_A[trial] <- v_new[1]
  strength_B[trial] <- v_new[2]
  
}

plot(strength_A, col= "red")
points(strength_B, col = "blue")


#### (4) simulate evaluative Conditioning

n_trials <- 20
lambda <- 1 # positive valence

#low variability: 2 cues
alpha <- c(.3, .3)
strengthLow <- matrix(0, n_trials, 2)


for (trial in 2:n_trials){
  
  #current associative strength
  v_old <- c(strengthLow[trial-1, 1], strengthLow[trial-1, 2])
  print(v_old)
  #update associative strengths
  v_new <- update_RW(
    value = v_old,
    lambda = lambda,
    alpha = alpha
  )
  print(v_new)
  
  #encode new strengths
  strengthLow[trial,] <- v_new

}

plot(strengthLow[,1], col = "lightblue")
points(strengthLow[,2], col = "blue")

#high variability: 6 cues
alpha <- c(.3, .3, 0, 0, 0, 0)
strength <- matrix(0, n_trials, 6)


for (trial in 2:n_trials){
  
  #determine stimulus composition
  if (trial > 4 && trial < 8){
    alpha <- c (.3, 0, .3, 0, 0,0)
  } else if (trial > 8 && trial < 12){
    alpha <- c(.3, 0, 0, .3, 0, 0)
  } else if (trial > 12 && trial < 16){
    alpha <- c(.3, 0, 0, 0, .3, 0)    
  } else if (trial > 16 && trial < 20){
    alpha <- c(.3, 0, 0, 0, 0, .3)
  }
  
  #current associative strength
  v_old <- c(strength[trial-1, 1], 
             strength[trial-1, 2], 
             strength[trial-1, 3],
             strength[trial-1, 4],
             strength[trial-1, 5],
             strength[trial-1, 6])
  
  #update associative strengths
  v_new <- update_RW(
    value = v_old,
    lambda = lambda,
    alpha = alpha
  )
  
  #encode new strengths
  strength[trial,] <- v_new
  
}

plot(strength[,1], col = "lightblue")
points(strength[,2], col = "blue")
points(strength[,3], col = "red")
points(strength[,4], col = "orange")
points(strength[,5], col = "green")
points(strength[,6], col = "grey")

#high variability: 6 cues, random dislay of stimuli (check for blocking)
alpha <- c(.3, .3, 0, 0, 0, 0)
strength <- matrix(0, n_trials, 6)

#randomize display order
display_order <- rep(sample(1:5, replace = FALSE, size = 5), 4)

for (trial in 2:n_trials){
  
  #determine stimulus composition
  if (display_order[trial] == 1){
    alpha <- c(.3, .3, 0, 0, 0,0)
  } else if (display_order[trial] == 2){
    alpha <- c (.3, 0, .3, 0, 0,0)
  } else if (display_order[trial] == 3){
    alpha <- c(.3, 0, 0, .3, 0, 0)
  } else if (display_order[trial] == 4){
    alpha <- c(.3, 0, 0, 0, .3, 0)    
  } else {
    alpha <- c(.3, 0, 0, 0, 0, .3)
  }
  
  #current associative strength
  v_old <- c(strength[trial-1, 1], 
             strength[trial-1, 2], 
             strength[trial-1, 3],
             strength[trial-1, 4],
             strength[trial-1, 5],
             strength[trial-1, 6])
  
  #update associative strengths
  v_new <- update_RW(
    value = v_old,
    lambda = lambda,
    alpha = alpha
  )
  
  #encode new strengths
  strength[trial,] <- v_new
  
}

plot(strength[,1], col = "lightblue")
points(strength[,2], col = "blue")
points(strength[,3], col = "red")
points(strength[,4], col = "orange")
points(strength[,5], col = "green")
points(strength[,6], col = "grey")




