
################### CS Variability: one-to-one vs. many-to-one
### january, 2021
### Kathrin Reichmann

### Indirect Measure

### content:

# Key Press:
# (1) Type x CS Variability
# (2) Type x CS Variability x Measure
# (3) CS Category
# (4) individual CS exemplars
# (5) individual targets

# Reaction Times:
# (1) Type x CS Variability
# (2) Type x CS Variability x Measure
# (3) including presentation order

### import:
#indirect.csv

### set working directory:
setwd("\\\\sn00.zdv.uni-tuebingen.de/siskr01/Documents/Github/CSCond_analysis/CSCond_analysis/data")
#setwd("C:/Users/reich/Documents/GitHub/CSCond_analysis/data")

library(dplyr)
#library(tidyverse)

#Plot
library(ggplot2)

#Analysis
library(afex)
library(lme4)

#Functions
CI <- function(x) qnorm(0.975)*sd(x)/sqrt(length(x))
se <- function(x) sd(x)/sqrt(length(x))

setwd("\\\\sn00.zdv.uni-tuebingen.de/siskr01/Documents/Github/CSCond_analysis/CSCond_analysis/data")
#setwd("C:/Users/reich/Documents/GitHub/CSCond_analysis/data")


#import data
indirect <- read.csv2('indirect.csv', header = TRUE)
str(indirect)

as_factor <- c("subject", "val", "condition_code", "condition", "measure", "measure_code", "type", "type_specific", "category", "cs_selected")

for (factor in as_factor){
  indirect[, factor] <- as.factor(indirect[,factor])
}

indirect$type_specific <- factor(indirect$type_specific, levels = c("CS", "GS same", "GS different", "Feature"))
str(indirect)

# calculate probabilities -------------------------------------------------


#generate variable prob
dataIndirect <- aggregate(response ~ subject + condition_code + condition + measure + val + type + type_specific, indirect, sum )
dataIndirect$nr_val <- aggregate(response ~ subject + condition_code + condition + measure + val + type + type_specific, indirect, length)[[8]]
dataIndirect$prob <- (dataIndirect$response/dataIndirect$nr_val) #prob: proportion "angenehm" of all stimuli in a combination of conditions
head(dataIndirect)

#generate variable diff
for (name in unique(dataIndirect$type_specific)){
  temp <- dataIndirect[dataIndirect$type_specific == name,];
  dataIndirect$diff[dataIndirect$type_specific == name] <- temp$prob[temp$val == "pos"] - temp$prob[temp$val == "neg"]
}

#columns for all "angenehm" and "unangenehm" answers
dataIndirect$an <- dataIndirect$response #column for "unangenehm" answer
dataIndirect$un <- dataIndirect$nr_val - dataIndirect$response #column for "angenehm" answer
head(dataIndirect)


### Plot difference scores: CS Variability x Type

barplotDiffData <- aggregate(diff ~ condition + type_specific, dataIndirect, mean)
barplotDiffData$se <- aggregate(diff ~ condition + type_specific, dataIndirect, se)[[3]]


barplotDiff <- ggplot(barplotDiffData, aes (x = type_specific, y = diff, fill = condition)) +
  geom_bar(stat = 'identity', position = position_dodge(), show.legend = TRUE) +
  geom_errorbar(aes(ymin= diff - se, ymax= diff + se), width=.2,
                position=position_dodge(.9)) +
  ggtitle("Indirect Measure") + 
  scale_fill_brewer(palette = "Paired") +
  theme_classic() +
  theme(plot.title = element_text (hjust = 0.5, face = "bold", size = 12)) +
  labs(fill = "CS Variability") +
  scale_y_continuous (name = "[Prob. 'an' | pos] - [Prob. 'an' | neg]", breaks = seq(0, 0.3, 0.1), limits = c(0, 0.3))
barplotDiff


################### one-sample t-tests
#H0: mu = 0
#H1: mu not equal to 0 -> EC effect is significant

#for GS different
t.test(dataIndirect$diff[dataIndirect$condition == "one_one" & dataIndirect$type_specific == "GS different"], mu = 0, alternative = "two.sided")
t.test(dataIndirect$diff[dataIndirect$condition == "many_one" & dataIndirect$type_specific == "GS different"], mu = 0, alternative = "two.sided")


################### (1) CS Variability x Type (CS vs. GS same)

#Hypothesis: interaction type x CS Variability


CSGSsame <- dataIndirect[dataIndirect$type_specific == "CS" | dataIndirect$type_specific == "GS same",]

#### mixed-model ANOVA with CS Variability x Type (CS vs. GS same)
indirect.type <- aggregate(diff ~ condition + type_specific + subject, CSGSsame, mean)
aov_specific <- aov_car(diff ~ condition*type_specific + Error(subject/type_specific), indirect.type, anova_table = list("pes"))
aov_specific

#2-way interaction n.s.

################### CS Variability x Type (CS vs. GS same vs. GS different vs. Feature)

#### mixed-model ANOVA with CS Variability x Type (CS vs. GS same vs. GS different vs. Feature)
indirect.type <- aggregate(diff ~ condition + type_specific + subject, dataIndirect, mean)
aov_specific <- aov_car(diff ~ condition*type_specific + Error(subject/type_specific), indirect.type, anova_table = list("pes"))
summary(aov_specific)
#Departure from Sphericity, use Greenhouse-Geisser correction

#interaction n.s.
#main effect type_specific
aggregate(diff ~ type_specific, dataIndirect, mean)


##????? as linear model
lm.type_specific <- lm(diff ~ type_specific*condition, dataIndirect)
summary(lm.type_specific)

lm.type_specific2 <- lmer(diff ~ type_specific + condition + (type_specific | subject), dataIndirect)
summary(lm.type_specific)

lm.type_specific3 <- lmer(diff ~ type_specific*condition + (type_specific | subject), dataIndirect)
summary(lm.type_specific3)

anova(lm.type_specific2, lm.type_specific3)



#simple slopes
#CS
lm1 <- lm(diff ~ condition, dataIndirect[dataIndirect$type_specific == "CS",])
summary(lm1)

lm2 <- lm(diff ~ condition, dataIndirect[dataIndirect$type_specific == "GS same",])
summary(lm2)

lm3 <- lm(diff ~ condition, dataIndirect[dataIndirect$type_specific == "GS different",])
summary(lm3)

lm4 <- lm(diff ~ condition, dataIndirect[dataIndirect$type_specific == "Feature",])
summary(lm4)


################### randomly select 2 CSs in "many-to-one"

new_one <- indirect[indirect$condition == "one_one",]
new_many <- indirect[indirect$condition == "many_one",]
new_many <- new_many[!new_many$type_specific == "CS",]
new_indirect <- rbind(new_one, new_many)

for (subject in unique(indirect$subject)){
  if (indirect$condition[indirect$subject == subject] == "many_one"){
    for (cat in 1:4){
      temp <- indirect[indirect$subject == subject & indirect$type_specific == "CS" & indirect$category == cat,]
      select <- temp[1:2,]
      new_indirect <- rbind(new_indirect, select)
    }
  }
  
}

#generate variable prob
dataIndirect_new <- aggregate(response ~ subject + condition_code + condition + measure + val + type + type_specific, new_indirect, sum )
dataIndirect_new$nr_val <- aggregate(response ~ subject + condition_code + condition + measure + val + type + type_specific, new_indirect, length)[[8]]
dataIndirect_new$prob <- (dataIndirect$response/dataIndirect$nr_val) #prob: proportion "angenehm" of all stimuli in a combination of conditions
head(dataIndirect)

#generate variable diff
for (name in unique(dataIndirect_new$type_specific)){
  temp <- dataIndirect_new[dataIndirect_new$type_specific == name,];
  dataIndirect_new$diff[dataIndirect_new$type_specific == name] <- temp$prob[temp$val == "pos"] - temp$prob[temp$val == "neg"]
}

#Plot
barplotDiffData <- aggregate(diff ~ condition + type_specific, dataIndirect_new, mean)
barplotDiffData$se <- aggregate(diff ~ condition + type_specific, dataIndirect_new, se)[[3]]


barplotDiff <- ggplot(barplotDiffData, aes (x = type_specific, y = diff, fill = condition)) +
  geom_bar(stat = 'identity', position = position_dodge(), show.legend = TRUE) +
  geom_errorbar(aes(ymin= diff - se, ymax= diff + se), width=.2,
                position=position_dodge(.9)) +
  ggtitle("Indirect Measure") + 
  scale_fill_brewer(palette = "Paired") +
  theme_classic() +
  theme(plot.title = element_text (hjust = 0.5, face = "bold", size = 12)) +
  labs(fill = "CS Variability") +
  scale_y_continuous (name = "[Prob. 'an' | pos] - [Prob. 'an' | neg]", breaks = seq(0, 0.3, 0.1), limits = c(0, 0.3))
barplotDiff

#### mixed-model ANOVA with CS Variability x Type (CS -only 2 CSs vs. GS same vs. GS different vs. Feature)
indirect.type <- aggregate(diff ~ condition + type_specific + subject, dataIndirect_new, mean)
aov_specific <- aov_car(diff ~ condition*type_specific + Error(subject/type_specific), indirect.type, anova_table = list("pes"))
summary(aov_specific)
#Departure from Sphericity, use Greenhouse-Geisser correction

################### calculate Generalized Linear Model (GLM)

#GLM on (un, an) condition * type_specific * val

glm1 <- glm(cbind(un, an) ~ 1, binomial, dataIndirect) #minimal model
glm2 <- glm(cbind(un, an) ~ condition + type_specific + val, binomial, dataIndirect)
glm3 <- glm(cbind(un, an) ~ condition* type_specific + val, binomial, dataIndirect)
glm4 <- glm(cbind(un, an) ~ condition* type_specific * val, binomial, dataIndirect) #saturated model
anova(glm1, glm2, glm3, glm4, test = "LRT")
summary(glm4)

#"WK angenehm bei one_one und CS"
predict(glm4, data.frame(condition = "one_one", type_specific = "CS", val = 'neg'), type = "resp")
predict(glm4, data.frame(condition = "one_one", type_specific = "CS", val = 'pos'), type = "resp")


################## Additional plots

#plot different valences
barplotDiffData <- aggregate(prob ~ val + type_specific + condition, dataIndirect, mean)
barplotDiffData$se <- aggregate(prob ~ val + type_specific + condition, dataIndirect, se)[[4]]

barplotPosNeg <- ggplot(barplotDiffData, aes (x = type_specific, y = prob, fill = condition)) +
  facet_grid(. ~ val)+
  geom_bar(stat = 'identity', position = position_dodge(), show.legend = TRUE) +
  geom_errorbar(aes(ymin= prob - se, ymax= prob + se), width=.2,
                position=position_dodge(.9)) +
    geom_hline(yintercept = 0.5, color = "red") +
  ggtitle("Positive and Negative Valence") + 
  theme(plot.title = element_text (hjust = 0.5, face = "bold", size = 12)) +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(name = "Prob. of 'Angenehm'")
barplotPosNeg

#boxplot
posnegIndirectData <- aggregate(prob ~ val + type_specific + condition, dataIndirect, mean)
posnegIndirect <- ggplot(dataIndirect, aes (x = type_specific, y = prob, fill = condition)) +
  facet_grid(. ~ val) +
  stat_boxplot(geom = "errorbar") +
  geom_boxplot(show.legend = TRUE) +
  ggtitle("Positive and Negative Pairings") + 
  theme(plot.title = element_text (hjust = 0.5, face = "bold", size = 12)) +
  scale_fill_brewer(palette = "Blues") +
  scale_y_continuous(name = "Prob. of 'Angenehm'")
posnegIndirect

################## (2) Variability x Type x Measure

#Plot
barplotDiffMeasure <- aggregate(diff ~ type_specific + measure + condition, dataIndirect, mean)
barplotDiffMeasure$se <- aggregate(diff ~ type_specific + measure + condition, dataIndirect, se)[[4]]

indirect.Measure <- ggplot(barplotDiffMeasure, aes (x = type_specific, y = diff, fill = condition)) +
  facet_grid(. ~ measure) +
  geom_bar(stat = 'identity', position = position_dodge(), show.legend = TRUE) +
  geom_errorbar(aes(ymin= diff - se, ymax= diff + se), width=.2,
                position=position_dodge(.9)) +
  ggtitle("Indirect Measure") + 
  theme(plot.title = element_text (hjust = 0.5, face = "bold", size = 12)) +
  scale_fill_brewer(palette = "Paired")  + 
  scale_y_continuous(name = "[Prob. 'an' | pos] - [Prob. 'an' | neg]")
indirect.Measure

#ANOVA
indirect.measure <- aggregate(diff ~ condition + measure + type_specific + subject, dataIndirect, mean)
aov_measure <- aov_car(diff ~ condition*type_specific*measure + Error(subject/type_specific), indirect.measure, anova_table = list("pes"))
aov_measure
summary(aov_measure)
#interaction n.s., no significant influence of 'measure'

################## (3) including category

#Plot
categoryIndirect <- aggregate(response ~  condition  + val + type_specific + category, indirect, sum )
categoryIndirect$nr_val <- aggregate(response ~ condition + val + type_specific + category, indirect, length)[[5]]
categoryIndirect$prob <- (categoryIndirect$response/categoryIndirect$nr_val)
head(categoryIndirect)

plot.categories <- ggplot(categoryIndirect, aes (x = category, y = prob, fill = val)) +
  stat_boxplot(geom = "errorbar") +
  geom_boxplot(show.legend = TRUE) +
  geom_hline(yintercept = 0.5, color = "red") +
  ggtitle("Categories") + 
  theme(plot.title = element_text (hjust = 0.5, face = "bold", size = 12)) +
  scale_fill_brewer(palette = "Blues") 
plot.categories

################## (4) including individual CSs

#Plot
CSselectedIndirect <- aggregate(response ~  val + cs_selected, indirect, sum )
CSselectedIndirect$nr_val <- aggregate(response ~ val + cs_selected, indirect, length)[[3]]
CSselectedIndirect$prob <- (CSselectedIndirect$response/CSselectedIndirect$nr_val)
head(CSselectedIndirect)

CsselectedPlot <- ggplot(CSselectedIndirect, aes (x = cs_selected, y = prob, col = val)) +
  geom_point(show.legend = TRUE) +
  ggtitle("Single CSs") + 
  theme(plot.title = element_text (hjust = 0.5, face = "bold", size = 12)) +
  scale_fill_brewer(palette = "Blues") 
CsselectedPlot

#4/GS2 
#1/GS1, 1/GS2, 1/GS3 

################## (5) including targets


targetsIndirect <- aggregate(response ~ target + val + condition + type_specific, indirect, sum)
targetsIndirect$nr_val <- aggregate(response ~ target + val + condition + type_specific, indirect, length) [[5]]
targetsIndirect$prob <- (targetsIndirect$response/targetsIndirect$nr_val)
head(targetsIndirect)

#generate variable diff
for (target in unique(targetsIndirect$target)){
  for (name in unique(targetsIndirect$type_specific)){
    temp <- targetsIndirect[targetsIndirect$target == target & targetsIndirect$type_specific == name,];
    targetsIndirect$diff[targetsIndirect$target == target & targetsIndirect$type_specific == name] <- temp$prob[temp$val == "pos"] - temp$prob[temp$val == "neg"]
  }
}

head(targetsIndirect)

## ?? include target as grouping factor in analysis
lmProbTargets <- lmer(diff ~ type_specific*condition*val + (type_specific|target), targetsIndirect)
summary(lmProbTargets)

#Plot
plot.targets <- aggregate(prob ~ target + val, targetsIndirect, mean)
plot.targets$target <- as.factor(plot.targets$target)

plotTarget <- ggplot(plot.targets, aes (x = target, y = prob, col = val)) +
  geom_point() +
  ggtitle("Targets on x-axis") + 
  theme(plot.title = element_text (hjust = 0.5, face = "bold", size = 12)) +
  scale_color_brewer(palette = "Set1") +
  scale_y_continuous(name = "Prob. 'angenehm'")
plotTarget

#Target 13 und 29 immer eher pos.
#Target 42 eher neg.

# reaction times ----------------------------------------------------------


#generate data set with reaction times
dataRT <- aggregate(rt ~ subject + condition_code + condition + measure + val + type + type_specific + nr_pres, indirect, mean )
dataRT <- dataRT[!dataRT$rt > 5000,]
head(dataRT)


################## (1) CS variability x type_specific

#Plot
plot.RT <- aggregate(rt ~ condition + type_specific, dataRT, mean)
plot.RT$se <- aggregate(rt ~ condition + type_specific, dataRT, se)[[3]]
head(plot.RT)


barplotRT <- ggplot(plot.RT, aes (x = type_specific, y = rt, fill = condition)) +
  geom_bar(stat = 'identity', position = position_dodge(), show.legend = TRUE) +
  geom_errorbar(aes(ymin= rt - se, ymax= rt + se), width=.2,
                position=position_dodge(.9)) +
  ggtitle("Indirect Measure") + 
  scale_fill_brewer(palette = "Paired") +
  theme_classic() +
  theme(plot.title = element_text (hjust = 0.5, face = "bold", size = 12)) +
  labs(fill = "CS Variability") +
  scale_y_continuous (name = "Reaction Times")
barplotRT

#Plot for each valence separately
plot.RT <- aggregate(rt ~ condition + type_specific + val, dataRT, mean)
plot.RT$se <- aggregate(rt ~ condition + type_specific + val, dataRT, se)[[4]]
head(plot.RT)


barplotRT <- ggplot(plot.RT, aes (x = type_specific, y = rt, fill = condition)) +
  facet_grid(. ~val) +
  geom_bar(stat = 'identity', position = position_dodge(), show.legend = TRUE) +
  geom_errorbar(aes(ymin= rt - se, ymax= rt + se), width=.2,
                position=position_dodge(.9)) +
  ggtitle("Indirect Measure") + 
  scale_fill_brewer(palette = "Paired") +
  theme_classic() +
  theme(plot.title = element_text (hjust = 0.5, face = "bold", size = 12)) +
  labs(fill = "CS Variability") +
  scale_y_continuous (name = "Reaction Times")
barplotRT

#Boxplot
barplotRT <- ggplot(dataRT, aes (x = type_specific, y = rt, fill = condition)) +
  stat_boxplot(geom = "errorbar") +
  geom_boxplot(show.legend = TRUE) +
  ggtitle("Indirect Measure") + 
  scale_fill_brewer(palette = "Paired") +
  theme_classic() +
  theme(plot.title = element_text (hjust = 0.5, face = "bold", size = 12)) +
  labs(fill = "CS Variability") +
  scale_y_continuous (name = "Reaction Times")
barplotRT

################## (2) CS variability x type_specific x measure

#Plot
plot.RT <- aggregate(rt ~ condition + type_specific + measure, dataRT, mean)
plot.RT$se <- aggregate(rt ~ condition + type_specific + measure, dataRT, se)[[4]]
head(plot.RT)


barplotRT <- ggplot(plot.RT, aes (x = type_specific, y = rt, fill = condition)) +
  facet_grid(. ~ measure) +
  geom_bar(stat = 'identity', position = position_dodge(), show.legend = TRUE) +
  geom_errorbar(aes(ymin= rt - se, ymax= rt + se), width=.2,
                position=position_dodge(.9)) +
  ggtitle("Indirect Measure") + 
  scale_fill_brewer(palette = "Paired") +
  theme_classic() +
  theme(plot.title = element_text (hjust = 0.5, face = "bold", size = 12)) +
  labs(fill = "CS Variability") +
  scale_y_continuous (name = "Reaction Times")
barplotRT


################## (3) including presentation order

#Plot
plot.RT <- aggregate(rt ~ nr_pres, dataRT, mean)
plot.RT$se <- aggregate(rt ~ nr_pres, dataRT, se)[[2]]
head(plot.RT)


#aggregate(prob ~ val + type_specific, dataIndirect, mean)
plot.presOrder <- ggplot(plot.RT, aes (x = nr_pres, y = rt)) +
  geom_point(show.legend = TRUE) +
  ggtitle("Presentation order of trails") + 
  theme(plot.title = element_text (hjust = 0.5, face = "bold", size = 12)) +
  scale_color_brewer(palette = "Set1") +
  scale_y_continuous(name = "Reaction Times") +
  scale_x_continuous(name = "Presentation Order")
plot.presOrder


