################### CS Variability: one-to-one vs. many-to-one
### january, 2021
### Kathrin Reichmann

### Direct Measure

### content:
# (1) Type x CS Variability
# (2) Type x CS Variability x Measure
# (3) CS Category
# (4) individual CS exemplars


### import:
#direct.csv


library(dplyr)
library(tidyverse)

#Plot
library(ggplot2)

#Analysis
library(afex)
library(lme4)
library(effsize)

#Functions
CI <- function(x) qnorm(0.975)*sd(x)/sqrt(length(x))
se <- function(x) sd(x)/sqrt(length(x))

setwd("\\\\sn00.zdv.uni-tuebingen.de/siskr01/Documents/Github/CSCond_analysis/CSCond_analysis/data")
#setwd("C:/Users/reich/Documents/GitHub/CSCond_analysis/data")
direct <- read.csv2('direct.csv', header = TRUE)
str(direct)

as_factor <- c("subject", "val", "condition_code", "condition", "measure", "measure_code", "type", "type_specific", "category", "cs_selected")

for (factor in as_factor){
  direct[, factor] <- as.factor(direct[,factor])
}

direct$type_specific <- factor(direct$type_specific, levels = c("CS", "GS same", "GS different", "Feature", "Group"))

### calculate difference scores ---------------------------------------------

#aggregate scores for each subject
dataDirect <- aggregate(response ~ subject + condition + measure + val + type_specific, direct, mean)
dataDirect$nr_obs <- aggregate(response ~ subject + condition + measure + val + type_specific, direct, length)[[6]]

#calculate differences between positive and negative valence, considering each type of measure
for (name in unique(dataDirect$type_specific)){
    temp <- dataDirect[dataDirect$type_specific == name,];
    dataDirect$diff[dataDirect$type_specific == name] <- temp$response[temp$val == "pos"] - temp$response[temp$val == "neg"]
}

head(dataDirect)


### Plot difference scores: CS Variability x Type

direct.type_specific <- aggregate(diff ~ condition + type_specific, dataDirect, mean)
direct.type_specific$se <- aggregate(diff ~ condition + type_specific, dataDirect, se)[[3]]
#direct.type_specific

barplotDiff <- ggplot(direct.type_specific, aes (x = type_specific, y = diff, fill = condition)) +
  geom_bar(stat = 'identity', position = position_dodge(), show.legend = TRUE) +
  geom_errorbar(aes(ymin= diff - se, ymax= diff + se), width=.2,
                position=position_dodge(.9)) +
  ggtitle("Difference Scores") + 
  scale_fill_brewer(palette = "Paired") +
  scale_x_discrete(name = "\nType") +
  scale_y_continuous (name = "Rating [Pos] - Rating [Neg]\n", breaks = seq(0, 70, 10), limits = c(0, 70)) + 
  theme_classic() +
  labs(fill = "CS Variability") +
  theme(plot.title = element_text (hjust = 0.5, face = "bold", size = 12))
barplotDiff

################### one-sample t-tests
#H0: mu = 0
#H1: mu not equal to 0 -> EC effect is significant

#for GS different
t.test(dataDirect$diff[dataDirect$condition == "one_one" & dataDirect$type_specific == "GS different"], mu = 0, alternative = "two.sided")
t.test(dataDirect$diff[dataDirect$condition == "many_one" & dataDirect$type_specific == "GS different"], mu = 0, alternative = "two.sided")


################### (1) CS Variability x Type (CS vs. GS same)

#Hypothesis: interaction type x CS Variability

CSGSsame <- dataDirect[dataDirect$type_specific == "CS" | dataDirect$type_specific == "GS same",]

#### mixed-model ANOVA with CS Variability x Type (CS vs. GS same)
direct.type <- aggregate(diff ~ condition + type_specific + subject, CSGSsame, mean)
aov_specific <- aov_car(diff ~ condition*type_specific + Error(subject/type_specific), direct.type, anova_table = list("pes"))
aov_specific

#analyze 2-way interaction
CSGSsame.many <- CSGSsame[CSGSsame$condition == "many_one",]
CSGSsame.one <- CSGSsame[CSGSsame$condition == "one_one",]

#CS
t.test(CSGSsame.many$diff[CSGSsame.many$type_specific == "CS"], CSGSsame.one$diff[CSGSsame.one$type_specific == "CS"], paired = FALSE,  var.equal = FALSE)

#GS same
t.test(CSGSsame.many$diff[CSGSsame.many$type_specific == "GS same"], CSGSsame.one$diff[CSGSsame.one$type_specific == "GS same"], paired = FALSE,  var.equal = FALSE)


### lmer: type_specific as grouping variable

#baseline model
baseline <- lmer(diff ~ 1 + (type_specific|subject), CSGSsame)
baseline

#including main effect of condition
conditionM <- lmer(diff ~ condition + (type_specific|subject),  CSGSsame)
conditionM

#including main effect of type
typeM <- lmer(diff ~ condition + type_specific + (type_specific|subject),  CSGSsame)
typeM

#including interaction condition and type
type_condition <- lmer(diff ~ condition*type_specific + (type_specific|subject),  CSGSsame)
type_condition

#compare models
anova(baseline, conditionM, typeM, type_condition)
summary(type_condition)


################### CS Variability x Type (CS vs. GS same vs. GS different vs. Feature)

#Hypothesis: interaction type x CS Variability

CSGSFeat <- dataDirect[!dataDirect$type_specific == "Group",]

#### mixed-model ANOVA with CS Variability x Type (CS vs. GS same vs. GS different vs. Feature)
direct.type <- aggregate(diff ~ condition + type_specific + subject, CSGSFeat, mean)
aov_specific <- aov_car(diff ~ condition*type_specific + Error(subject/type_specific), direct.type, anova_table = list("pes"))
summary(aov_specific)
#Departure from Sphericity, use Greenhouse-Geisser correction

#simple slopes
#CS
lm1 <- lm(diff ~ condition, dataDirect[dataDirect$type_specific == "CS",])
summary(lm1)

lm2 <- lm(diff ~ condition, dataDirect[dataDirect$type_specific == "GS same",])
summary(lm2)

lm3 <- lm(diff ~ condition, dataDirect[dataDirect$type_specific == "GS different",])
summary(lm3)

lm4 <- lm(diff ~ condition, dataDirect[dataDirect$type_specific == "Feature",])
summary(lm4)

################### CS Variability for "ALL" ratings
Group <- dataDirect[dataDirect$type_specific == "Group",]
t.test(Group$diff[Group$condition == "many_one"], Group$diff[Group$condition == "one_one"], paired = FALSE, var.equal = TRUE)

#as linear model:
lm5 <- lm(diff ~ condition, dataDirect[dataDirect$type_specific == "Group",])
summary(lm5)


################## Additional plots

#Boxplots different conditions
diffDirect <- ggplot(dataDirect, aes (x = type_specific, y = diff, fill = condition)) +
  stat_boxplot(geom = "errorbar") +
  geom_boxplot(show.legend = TRUE) +
  ggtitle("Differences") + 
  theme(plot.title = element_text (hjust = 0.5, face = "bold", size = 12)) +
  scale_fill_brewer(palette = "Blues") +
  scale_y_continuous (name = "Pos - Neg", breaks = seq(-100, 200, 25), limits = c(-50, 100))
diffDirect

#Barplot for both valences separately
barplotDirect <- aggregate(response ~ type_specific + condition + val,direct, mean)
barplotDirect$se <- aggregate(response ~ type_specific + condition + val, direct, se)[[4]]

barplot.val <- ggplot(barplotDirect, aes (x = type_specific, y = response, fill = condition)) +
  facet_grid(. ~ val) +
  geom_bar(stat = 'identity', position = position_dodge(), show.legend = TRUE) +
  geom_errorbar(aes(ymin= response - se, ymax= response + se), width=.2,
                position=position_dodge(.9)) +
  ggtitle("Positive and Negative Pairings") + 
  theme(plot.title = element_text (hjust = 0.5, face = "bold", size = 12)) +
  scale_fill_brewer(palette = "Paired") 
barplot.val

#Boxplot positive pairings
positiveDirect <- ggplot(direct[direct$val == "pos",], aes (x = type_specific, y = response, fill = condition)) +
  stat_boxplot(geom = "errorbar") +
  geom_boxplot(show.legend = TRUE) +
  ggtitle("Positive Pairings") + 
  theme(plot.title = element_text (hjust = 0.5, face = "bold", size = 12)) +
  scale_fill_brewer(palette = "Blues") 
positiveDirect

#Boxplot negative pairings
negativeDirect <- ggplot(dataDirect[dataDirect$val == "neg",], aes (x = type_specific, y = response, fill = condition)) +
  stat_boxplot(geom = "errorbar") +
  geom_boxplot(show.legend = TRUE) +
  ggtitle("Negative Pairings") + 
  theme(plot.title = element_text (hjust = 0.5, face = "bold", size = 12)) +
  scale_fill_brewer(palette = "Blues") 
negativeDirect

################## (2) Variability x Type x Measure

barplotMeasure <- aggregate(diff ~ type_specific + condition + measure, dataDirect[dataDirect$val == "pos",], mean)
barplotMeasure$se <- aggregate(diff ~ type_specific + condition + measure, dataDirect[dataDirect$val == "pos",], se)[[4]]

plotMeasure <- ggplot(barplotMeasure, aes (x = type_specific, y = diff, fill = condition)) +
  facet_grid(. ~ measure) +
  geom_bar(stat = 'identity', position = position_dodge(), show.legend = TRUE) +
  geom_errorbar(aes(ymin= diff - se, ymax= diff + se), width=.2,
                position=position_dodge(.9)) +
  ggtitle("Differences") + 
  theme(plot.title = element_text (hjust = 0.5, face = "bold", size = 12)) +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous (name = "Pos - Neg", breaks = seq(-25, 125, 25), limits = c(-10, 100))
plotMeasure


################## (3) including category

categoryDirect <- aggregate(response ~ subject + condition_code + condition + measure_code + val + type + type_specific + category, direct, mean)
categoryDirect$nr_obs <- aggregate(response ~ subject + condition_code + condition + measure_code + val + type + type_specific + category, direct, length)[[8]]

categoryDirectPlot <- ggplot(categoryDirect, aes (x = category, y = response, fill = val)) +
  stat_boxplot(geom = "errorbar") +
  geom_boxplot(show.legend = TRUE) +
  ggtitle("Categories") + 
  theme(plot.title = element_text (hjust = 0.5, face = "bold", size = 12)) +
  scale_fill_brewer(palette = "Blues") 
categoryDirectPlot

#category3/CS9.png especially positive


################## (4) individual CS exemplars

csSelectedDirect <- aggregate(response ~ cs_selected + val, dat, mean)
csDirectPlot <- ggplot(csSelectedDirect, aes (x = cs_selected, y = response, col = val)) +
  geom_point(show.legend = TRUE)
  ggtitle("Single CSs") + 
  theme(plot.title = element_text (hjust = 0.5, face = "bold", size = 12)) +
  scale_fill_brewer(palette = "Blues") 
#csDirectPlot
