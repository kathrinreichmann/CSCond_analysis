## Basic Rescola Wagner Model

###basic function
update_RW <- function(value, alpha = .3, beta = .3, lambda = 1){
  
  #value of compound stimulus: sum of individual strengths
  value_compound <- sum(value)
  
  #prediction errror: diff. between max. associative strength that the US supports & current associative strength for the compound
  prediction_error <- lambda - value_compound
  
  #compute change in strength
  value_change <- alpha * beta * prediction_error
  
  #update associative value
  value <- value + value_change
  
  #return new value
  return (value)
}

### conditioning

n_trials <- 20
strength <- numeric(n_trials) #use to store associative strengths

for (trial in 2:n_trials){
  strength[trial] <- update_RW(strength[trial -1])
}
 plot(strength)

 
### extinction
n_trials <- 50
strength <- numeric(n_trials)
lambda <- .3

for (trial in 2:n_trials){
  
  #trial 1-25 extinction
  if (trial > 25){
    lambda <- 0
  }
  
  strength[trial] <- update_RW(
    value = strength[trial -1],
    lambda = lambda
  )
}
plot(strength)


### blocking
n_trials <- 50

#compound stimulus
strength_A <- rep(0, n_trials)
strength_B <- rep(0, n_trials)

#during first learning phase, B is not present (alpha = 0)
alpha <- c(0.3, 0)

for (trial in 2:n_trials){
  
  #after trial 15, both components of the stimulus are present
  if(trial > 15){
    alpha <- c(.3, .3)
  }
  
  #current associative strengths
  v_old <- c(strength_A[trial -1], strength_B[trial - 1])
  
  #old associative strengths
  v_new <- update_RW(
    value = v_old,
    alpha = alpha
  )
  
  #record the new strengths
  strength_A[trial] <- v_new[1]
  strength_B[trial] <- v_new[2]
  
}
plot(strength_A)
lines(strength_B)

