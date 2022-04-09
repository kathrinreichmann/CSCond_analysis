#####  generating predictions from the Rescola-Wagner model

##packages
library(tidyverse)
library(ggplot2)

## set wd
setwd("C:/Users/siskr01/GitHub/CSCond_analysis/analysis_All")

### Rescola-Wagner model

update_RW <- function (value, alpha, beta, lambda){
  
  #value of compound stimulus
  value_compound <- sum(value)
  
  #compute prediction error
  prediction_error <- lambda - value_compound
  
  #compute change in strength
  value_change <- alpha * beta * prediction_error
  
  #update associative value
  value <- value + value_change
  
  #return new value
  return(value)
  
}

### simulate learning phase: one vs. many
set.seed(88)

#number of trials per category:
n_trials <- 20

### associative strength for each stimulus component

#lambda <- 1 #positive valence
beta <- .3

conditions <- c("one", "many")
valence <- c("neg", "pos")

for (val in valence) {
  if (val == "pos"){
    
    lambda <- 1 #positive valence
    
    ##store results
    #calculate associative values for one condition
    strengthOne <- tibble(trial_nr = 1:n_trials,
                          valuesA = numeric(n_trials), #category-defining component
                          values1 = numeric(n_trials),
                          values2 = numeric(n_trials),
                          values3 = numeric(n_trials),
                          values4 = numeric(n_trials),
                          values5 = numeric(n_trials),
                          #always same stimulus presentation in One
                          stim_selected = 1,
    )
    
    #calculate associative values for many condition
    strengthMany <- tibble(trial_nr = 1:n_trials,
                           valuesA = numeric(n_trials),
                           values1 = numeric(n_trials),
                           values2 = numeric(n_trials),
                           values3 = numeric(n_trials),
                           values4 = numeric(n_trials),
                           values5 = numeric(n_trials),
                           #random stimulus presentation in Many
                           stim_selected = sample(rep(1:5, n_trials/4), size = n_trials, replace = FALSE))
    
    for (condition in conditions){
      if (condition == "one") {
        for (trial in 2:n_trials){
          
          #attention can only be devoted towards the first two elements of the stimulus
          alpha_manually <- c(.3, .3, 0, 0, 0, 0)
          
          ### predictive values for 20 trials, pairing each CS consistently with USs
          
          #get assiciative values from previous trial
          v_old <- c(strengthOne$valuesA[trial-1], 
                     strengthOne$values1[trial-1],
                     strengthOne$values2[trial-1],
                     strengthOne$values3[trial-1],
                     strengthOne$values4[trial-1],
                     strengthOne$values5[trial-1]
          )
          
          #calculate associative strengths
          v_new <- update_RW(v_old, alpha = alpha_manually , beta, lambda)
          print(v_new)
          
          ##store results
          strengthOne$valuesA[trial] <- v_new[1]
          strengthOne$values1[trial] <- v_new[2]
          strengthOne$values2[trial] <- v_new[3]
          strengthOne$values3[trial] <- v_new[4]
          strengthOne$values4[trial] <- v_new[5]
          strengthOne$values5[trial] <- v_new[6]
        }
        
      } else {
        #many condition
        
        for (trial in 2:n_trials) {
          
          #randomly select which combination of category-defining component and
          #unique component is displayed to participant
          
          if (strengthMany$stim_selected[trial] == 1){
            alpha_manually <- c(.3, .3, 0, 0, 0, 0)
          } else if (strengthMany$stim_selected[trial] == 2){
            alpha_manually <- c(.3, 0, .3, 0, 0, 0)
          } else if (strengthMany$stim_selected[trial] == 3){
            alpha_manually <- c(.3, 0, 0, .3, 0, 0)
          } else if (strengthMany$stim_selected[trial] == 4){
            alpha_manually <- c(.3, 0, 0, 0, .3, 0)
          } else {
            alpha_manually <- c(.3, 0, 0, 0, 0, .3)
          }
          
          ###predictive values for 20 trials, pairing each CS consistently with USs
          
          #get associative values from previous trial
          v_old <- c(strengthMany$valuesA[trial-1], 
                     strengthMany$values1[trial-1],
                     strengthMany$values2[trial-1],
                     strengthMany$values3[trial-1],
                     strengthMany$values4[trial-1],
                     strengthMany$values5[trial-1]
          )
          
          #calculate associative strengths
          v_new <- update_RW(v_old, alpha = alpha_manually , beta, lambda)
          
          #store results in strengthMany data frame
          strengthMany$valuesA[trial] <- v_new[1]
          strengthMany$values1[trial] <- v_new[2]
          strengthMany$values2[trial] <- v_new[3]
          strengthMany$values3[trial] <- v_new[4]
          strengthMany$values4[trial] <- v_new[5]
          strengthMany$values5[trial] <- v_new[6]
          
        }
      }
    }
    
    strengthOne
    strengthMany
    
    #### store outcomes: associative strength for cues at 20th trial
    associativeStrengthPos <- tibble(condition = rep(c("one", "many"),2),
                                  cue = c("same", "same", "varying", "varying"),
                                  val = rep(c("pos"), 4),
                                  strength = c(as.numeric(strengthOne[n_trials,2]),
                                               as.numeric(strengthMany[n_trials,2]),
                                               as.numeric(strengthOne[n_trials,3]),
                                               as.numeric(strengthMany[n_trials, 3]))) %>%
      arrange(condition)

    associativeStrengthPos
    
  } else {
    ##### same calculations for negative valence
    lambda <- -1 #negative valence
    
    ##store results
    #calculate associative values for one condition
    strengthOne <- tibble(trial_nr = 1:n_trials,
                          valuesA = numeric(n_trials), #category-defining component
                          values1 = numeric(n_trials),
                          values2 = numeric(n_trials),
                          values3 = numeric(n_trials),
                          values4 = numeric(n_trials),
                          values5 = numeric(n_trials),
                          #always same stimulus presentation in One
                          stim_selected = 1,
    )
    
    #calculate associative values for many condition
    strengthMany <- tibble(trial_nr = 1:n_trials,
                           valuesA = numeric(n_trials),
                           values1 = numeric(n_trials),
                           values2 = numeric(n_trials),
                           values3 = numeric(n_trials),
                           values4 = numeric(n_trials),
                           values5 = numeric(n_trials),
                           #random stimulus presentation in Many
                           stim_selected = sample(rep(1:5, n_trials/4), size = n_trials, replace = FALSE))
    
    for (condition in conditions){
      if (condition == "one") {
        for (trial in 2:n_trials){
          
          #attention can only be devoted towards the first two elements of the stimulus
          alpha_manually <- c(.3, .3, 0, 0, 0, 0)
          
          ### predictive values for 20 trials, pairing each CS consistently with USs
          
          #get assiciative values from previous trial
          v_old <- c(strengthOne$valuesA[trial-1], 
                     strengthOne$values1[trial-1],
                     strengthOne$values2[trial-1],
                     strengthOne$values3[trial-1],
                     strengthOne$values4[trial-1],
                     strengthOne$values5[trial-1]
          )
          
          #calculate associative strengths
          v_new <- update_RW(v_old, alpha = alpha_manually , beta, lambda)
          print(v_new)
          
          ##store results
          strengthOne$valuesA[trial] <- v_new[1]
          strengthOne$values1[trial] <- v_new[2]
          strengthOne$values2[trial] <- v_new[3]
          strengthOne$values3[trial] <- v_new[4]
          strengthOne$values4[trial] <- v_new[5]
          strengthOne$values5[trial] <- v_new[6]
        }
        
      } else {
        #many condition
        
        for (trial in 2:n_trials) {
          
          #randomly select which combination of category-defining component and
          #unique component is displayed to participant
          
          if (strengthMany$stim_selected[trial] == 1){
            alpha_manually <- c(.3, .3, 0, 0, 0, 0)
          } else if (strengthMany$stim_selected[trial] == 2){
            alpha_manually <- c(.3, 0, .3, 0, 0, 0)
          } else if (strengthMany$stim_selected[trial] == 3){
            alpha_manually <- c(.3, 0, 0, .3, 0, 0)
          } else if (strengthMany$stim_selected[trial] == 4){
            alpha_manually <- c(.3, 0, 0, 0, .3, 0)
          } else {
            alpha_manually <- c(.3, 0, 0, 0, 0, .3)
          }
          
          ###predictive values for 20 trials, pairing each CS consistently with USs
          
          #get associative values from previous trial
          v_old <- c(strengthMany$valuesA[trial-1], 
                     strengthMany$values1[trial-1],
                     strengthMany$values2[trial-1],
                     strengthMany$values3[trial-1],
                     strengthMany$values4[trial-1],
                     strengthMany$values5[trial-1]
          )
          
          #calculate associative strengths
          v_new <- update_RW(v_old, alpha = alpha_manually , beta, lambda)
          
          #store results in strengthMany data frame
          strengthMany$valuesA[trial] <- v_new[1]
          strengthMany$values1[trial] <- v_new[2]
          strengthMany$values2[trial] <- v_new[3]
          strengthMany$values3[trial] <- v_new[4]
          strengthMany$values4[trial] <- v_new[5]
          strengthMany$values5[trial] <- v_new[6]
          
        }
      }
    }
    
    strengthOne
    strengthMany
    
    #### store outcomes: associative strength for cues at 20th trial
    
    associativeStrengthNeg <- tibble(condition = rep(c("one", "many"),2),
                                     cue = c("same", "same", "varying", "varying"),
                                     val = rep(c("neg"), 4),
                                     strength = c(as.numeric(strengthOne[n_trials,2]),
                                                  as.numeric(strengthMany[n_trials, 2]),
                                                  as.numeric(strengthOne[n_trials,3]),
                                                  as.numeric(strengthMany[n_trials,3]))) %>%
      arrange(condition)
    associativeStrengthNeg
    
  }
}

### concat results
associativeStrength <- bind_rows(associativeStrengthPos, associativeStrengthNeg)
associativeStrength
write_csv(associativeStrength, "associativeStrength.csv")

##### plot how associative strengths develop over time
plotOne <-  strengthOne %>% 
  pivot_longer(!trial_nr, names_to = "CS", values_to = "values") %>% 
  mutate(CS = factor(CS)) %>% 
  filter(CS %in% c ("valuesA", "values1")) %>% 
  mutate(CS = factor(CS, levels = c("valuesA", "values1"), 
                     labels = c("category\ncomponent", "unique\ncomponent"))) %>%
  group_by(CS) %>% 
  ggplot(aes(x = trial_nr, y = values, color = CS)) +
    geom_point(aes (pch = CS), size = 3) +
    geom_line(aes(linetype = CS, color = CS), alpha = .8, size = 1)+
    facet_grid(. ~ CS) +
    ylim(0, 0.5) +
    ggtitle('One Condition\n') +
    xlab ('\n Trial Number') +
    ylab ('Associative Strength \n') +
    scale_color_brewer(palette = "Paired") +
    theme_bw() +
    labs(fill = "Valence") +
    theme(plot.title = element_text (hjust = 0.5, face = "bold", size = 22),
          text = element_text(size=20),
          strip.background = element_rect(colour="black", fill="white", 
                                          size=.7, linetype="solid"),
          legend.position = "none"
          )
plotOne
ggsave("figures/one.png", plotOne, width = 10, height = 4)

plotManyPrep <-  strengthMany %>% 
  select(-stim_selected) %>% 
  pivot_longer(!trial_nr, names_to = "CS", values_to = "values") %>%
  mutate(CS = factor(CS, 
                     levels = c("valuesA", "values1", "values2", "values3", "values4", "values5"), 
                     labels = c("CS", "contextual cues", "cue2", "cue3", "cue4", "cue5")),
         type = factor(CS,
                       levels = c("CS", "contextual cues", "cue2", "cue3", "cue4", "cue5"),
                       labels = c("category\ncomponent", "unique\ncomponent", "unique\ncomponent", "unique\ncomponent", 
                                  "unique\ncomponent", "unique\ncomponent"))) %>%  
  group_by(CS)
plotManyPrep 

plotMany <-  ggplot(plotManyPrep, aes(x = trial_nr, y = values)) +
  facet_grid(. ~ type) +
  geom_point(aes (color = CS, pch = type), size = 3) +
  geom_line(aes(linetype = CS, color = CS), alpha = .8)+
  ylim(0, 0.5) +
  ggtitle('Many condition\n') +
  xlab ('\n Trial Number') +
  ylab ('Associative Strength \n') +
  scale_color_brewer(palette = "Paired") +
  theme_bw() +
  labs(fill = "Valence") +
  theme(plot.title = element_text (hjust = 0.5, face = "bold", size = 22),
        text = element_text(size=20),
        strip.background = element_rect(colour="black", fill="white", 
                                        size=.7, linetype="solid"),
        legend.position = "none"
  )
plotMany
ggsave("figures/many.jpg", plotMany, width = 10, height = 4)


