
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

indirect$type_specific <- factor(indirect$type_specific, levels = c("CS", "GS same", "GS different", "Feature", "Group"))
str(indirect)


#generate variabile prob
dataIndirect <- aggregate(response ~ subject + condition_code + condition + measure + val + type + type_specific, indirect, sum )
dataIndirect$nr_val <- aggregate(response ~ subject + condition_code + condition + measure + val + type + type_specific, indirect, length)[[8]]
dataIndirect$prob <- (dataIndirect$response/dataIndirect$nr_val) #prob: proportion "angenehm" of positive / negative stimuli in the respective condition
head(dataIndirect)

#generate variable diff
for (subj in unique(dataIndirect$subject)){
  for (name in unique(dataIndirect$type_specific)){
    temp <- dataIndirect[dataIndirect$subject == subj & dataIndirect$type_specific == name,];
    dataIndirect$diff[dataIndirect$subject == subj & dataIndirect$type_specific == name] <- temp$prob[temp$val == "pos"] - temp$prob[temp$val == "neg"]
  }
}

dataIndirect$an <- dataIndirect$response #column for "unangenehm" answer
dataIndirect$un <- dataIndirect$nr_val - dataIndirect$response #column for "angenehm" answer
head(dataIndirect)

barplotDiffData <- aggregate(diff ~ condition + type_specific, dataIndirect, mean)
barplotDiffData$se <- aggregate(diff ~ condition + type_specific, dataIndirect, se)[[3]]

barplotDiff <- ggplot(barplotDiffData, aes (x = type_specific, y = diff, fill = condition)) +
  geom_bar(stat = 'identity', position = position_dodge(), show.legend = TRUE) +
  geom_errorbar(aes(ymin= diff - se, ymax= diff + se), width=.2,
                position=position_dodge(.9)) +
  ggtitle("Differences") + 
  theme(plot.title = element_text (hjust = 0.5, face = "bold", size = 12)) +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous (name = "[Prob. 'angenehm' bei Pos] - [Prob. 'angenehm' bei Neg]", breaks = seq(-0.1, 0.3, 0.1), limits = c(-0.1, 0.3))
barplotDiff

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


indirect.type <- aggregate(diff ~ condition + type + subject, dataIndirect[dataIndirect$type_specific == "CS" | dataIndirect$type_specific == "GSold",], mean)
aov_specific <- aov_car(diff ~ condition*type + Error(subject/type), indirect.type, anova_table = list("pes"))
#aov_specific
#summary(aov_specific)

indirect.type_specific <- aggregate(diff ~ condition + type_specific + subject, dataIndirect, mean)
aov_specific2 <- aov_car(diff ~ condition*type_specific + Error(subject/type_specific), indirect.type_specific, anova_table = list("pes"))
aov_specific2
summary(aov_specific2)

#main effect type_specific
aggregate(diff ~ type_specific, dataIndirect, mean)

lm.type_specific <- lm(diff ~ type_specific*condition, dataIndirect)
#summary(lm.type_specific)

lm.type_specific2 <- lmer(diff ~ type_specific*condition + (1 | subject), dataIndirect)
#summary(lm.type_specific)

lm.type_specific3 <- lmer(diff ~ type_specific*condition + (type | subject), dataIndirect)
summary(lm.type_specific3)

anova(lm.type_specific2, lm.type_specific3)

indirect.measure <- aggregate(diff ~ condition + measure + type_specific + subject, dataIndirect, mean)
aov_measure <- aov_car(diff ~ condition*type_specific*measure + Error(subject/type_specific), indirect.measure, anova_table = list("pes"))
#aov_measure
#summary(aov_measure)

targetsIndirect <- aggregate(response ~ target + val + condition + type_specific, indirect, sum)
targetsIndirect$nr_val <- aggregate(response ~ target + val + condition + type_specific, indirect, length) [[5]]
targetsIndirect$prob <- (targetsIndirect$response/targetsIndirect$nr_val)
#targetsIndirect

#generate variable diff
#not all cells have data, yet!
for (target in unique(targetsIndirect$target)){
  for (name in unique(targetsIndirect$type_specific)){
    temp <- targetsIndirect[targetsIndirect$target == target & targetsIndirect$type_specific == name,];
    targetsIndirect$diff[targetsIndirect$target == target & targetsIndirect$type_specific == name] <- temp$prob[temp$val == "pos"] - temp$prob[temp$val == "neg"]
  }
}


head(targetsIndirect)

lmProbTargets <- lmer(diff ~ type_specific*condition*val + (type_specific|target), targetsIndirect)
#summary(lmProbTargets)

plot.targets <- aggregate(prob ~ target + val, targetsIndirect, mean)
plot.targets$target <- as.factor(plot.targets$target)

for(target in unique(plot.targets$target)){
  temp <- plot.targets[plot.targets$target == target,]
  plot.targets$diff[plot.targets$target == target] <- temp$prob[temp$val == "pos"] - temp$prob[temp$val == "neg"]
}
plotTarget <- ggplot(plot.targets[plot.targets$val == "neg",], aes (x = target, y = diff)) +
  geom_point() +
  ggtitle("Targets on x-axis") + 
  theme(plot.title = element_text (hjust = 0.5, face = "bold", size = 12)) +
  scale_color_brewer(palette = "Set1") +
  scale_y_continuous(name = "Difference (Prob. Angenehm | Pos) - (Prob Angenehm | Neg)")
#plotTarget

#GLM on (un, an)
glm1 <- glm(cbind(un, an) ~ 1, binomial, dataIndirect) #minimal model
glm2 <- glm(cbind(un, an) ~ condition + type_specific + val, binomial, dataIndirect)
glm3 <- glm(cbind(un, an) ~ condition* type_specific + val, binomial, dataIndirect)
glm4 <- glm(cbind(un, an) ~ condition* type_specific * val, binomial, dataIndirect) #saturated model
anova(glm1, glm2, glm3, glm4, test = "LRT")
summary(glm4)

#main effect condition
predict(glm4, data.frame(condition = "one_one", type_specific = "CS", val = 'neg'), type = "resp")
predict(glm4, data.frame(condition = "one_one", type_specific = "GSnew", val = 'pos'), type = "resp")
#"WK angenehm bei one_one und CS"

barplotDiffMeasure <- aggregate(prob ~ val + type_specific + measure, dataIndirect, mean)
barplotDiffMeasure$se <- aggregate(prob ~ val + type_specific + measure, dataIndirect, se)[[4]]

indirect.Measure <- ggplot(barplotDiffMeasure, aes (x = type_specific, y = prob, fill = measure)) +
  facet_grid(. ~ val) +
  geom_bar(stat = 'identity', position = position_dodge(), show.legend = TRUE) +
  geom_errorbar(aes(ymin= prob - se, ymax= prob + se), width=.2,
                position=position_dodge(.9)) +
  ggtitle("Measure") + 
  theme(plot.title = element_text (hjust = 0.5, face = "bold", size = 12)) +
  scale_fill_brewer(palette = "Paired")  + 
  scale_y_continuous(name = "Prob. of 'Angenehm'")+
    geom_hline(yintercept = 0.5, color = "red")
indirect.Measure

presOrder <- aggregate(response ~ nr_pres + val , indirect, sum)
presOrder$nr_val <- aggregate(response ~ nr_pres + val, indirect, length) [[3]]
presOrder$se <- aggregate(response ~ nr_pres + val, indirect, se) [[3]]
presOrder$prob <- (presOrder$response/presOrder$nr_val)
#head(presOrder)

#aggregate(prob ~ val + type_specific, dataIndirect, mean)
plot.presOrder <- ggplot(presOrder, aes (x = nr_pres, y = prob, col = val)) +
  geom_point(show.legend = TRUE) +
  geom_hline(yintercept = 0.5, color = "red") +
  ggtitle("Positive and Negative Pairings") + 
  theme(plot.title = element_text (hjust = 0.5, face = "bold", size = 12)) +
  scale_color_brewer(palette = "Set1") +
  scale_y_continuous(name = "Prob. of 'Angenehm'")
#plot.presOrder

presOrder <- aggregate(rt ~ condition + type_specific, indirect, mean)
presOrder$se <- aggregate(rt ~ condition + type_specific, indirect, se) [[2]]

#aggregate(prob ~ val + type_specific, dataIndirect, mean)
plot.presOrder <- ggplot(presOrder, aes (x = type_specific, y = rt, col = condition)) +
  geom_point(show.legend = TRUE) +
  ggtitle("Reaction Times") + 
  theme(plot.title = element_text (hjust = 0.5, face = "bold", size = 12)) +
  scale_color_brewer(palette = "Set1") +
  scale_y_continuous(name = "Presentation Order")
plot.presOrder

categoryIndirect <- aggregate(response ~  condition  + val +type_specific + category, indirect, sum )
categoryIndirect$nr_val <- aggregate(response ~ condition + val + type_specific + category, indirect, length)[[5]]
categoryIndirect$prob <- (categoryIndirect$response/categoryIndirect$nr_val)
#head(categoryIndirect)

plot.categories <- ggplot(categoryIndirect, aes (x = category, y = prob, fill = val)) +
  stat_boxplot(geom = "errorbar") +
  geom_boxplot(show.legend = TRUE) +
  geom_hline(yintercept = 0.5, color = "red") +
  ggtitle("Categories") + 
  theme(plot.title = element_text (hjust = 0.5, face = "bold", size = 12)) +
  scale_fill_brewer(palette = "Blues") 
plot.categories

temp <- direct[!direct$type_specific == "all",]
indirect$responseDirect <- temp$response
indirect$responseIndirect <- as.factor(indirect$response)

#predict direct responses with answers from indirect responses: for positive pairings

#plot(responseDirect ~ responseIndirect, indirect[indirect$val == "pos",])
lm.pos <- lm(responseDirect ~ responseIndirect,indirect[indirect$val == "pos",])
#summary(lm.pos)

#predict direct responses with answers from indirect responses: for positive pairings

#plot(responseDirect ~ responseIndirect, indirect[indirect$val == "neg",])
lm.neg <- lm(responseDirect ~ responseIndirect,indirect[indirect$val == "neg",])
#summary(lm.neg)

#calculate correlations
cor(indirect$responseDirect[indirect$val == "pos"], indirect$response[indirect$val == "pos"])
cor(indirect$responseDirect[indirect$val == "neg"], indirect$response[indirect$val == "neg"])
