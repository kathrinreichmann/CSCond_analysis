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

### set working directory:
#setwd("\\\\sn00.zdv.uni-tuebingen.de/siskr01/Documents/Github/CSCond_analysis/CSCond_analysis/data")
setwd("C:/Users/reich/Documents/GitHub/CSCond_analysis/Study1_EC/data")

library(dplyr)
library(tidyverse)

#Plot
library(ggplot2)

#Analysis
library(afex)
library(lme4)
library(nlme)
library(effsize)

#Functions
CI <- function(x) qnorm(0.975)*sd(x)/sqrt(length(x))
se <- function(x) sd(x)/sqrt(length(x))


direct <- read.csv2('direct.csv', header = TRUE)
str(direct)

as_factor <- c("subject", "val", "condition_code", "condition", "measure", "measure_code", "type", "type_specific", "category", "cs_selected")

for (factor in as_factor){
  direct[, factor] <- as.factor(direct[,factor])
}

direct$type_specific <- factor(direct$type_specific, levels = c("CS", "GS same", "GS different", "Feature", "Group"))


# multilevel model --------------------------------------------------------
str(direct)

#(1) mean scores for every category and participant
centerDirect <- aggregate(response ~ subject + condition + category + type_specific + val, direct, mean)
#condition_code: 0 =  many
centerDirect[centerDirect$subject == "02a80kdxm7",] #check

#(2) use CS as predictor for GS same
multiLevel <- centerDirect[centerDirect$type_specific == "CS",]
respGS <- centerDirect[centerDirect$type_specific == "GS same",]
multiLevel$GS <- respGS$response
multiLevel$CS <- multiLevel$response
multiLevel$response <- NULL
multiLevel$type_specific <- NULL
head(multiLevel)
multiLevel[multiLevel$subject == "02a80kdxm7",] #check

#(3) plot the two different levels involved in the analysis

dotplot <- ggplot(multiLevel, aes (x = CS, y = GS, color = category)) +
  facet_grid(. ~ condition) +
  geom_point(show.legend = TRUE) +
  geom_smooth(method = 'lm') +
  scale_color_brewer(palette = "Paired") +
  scale_x_continuous(name = "\nCS Ratings") +
  scale_y_continuous (name = "GS (new) Ratings\n") + 
  theme_classic() +
  labs(fill = "Categories") +
  theme(plot.title = element_text (hjust = 0.5, face = "bold", size = 12))
dotplot

#(4) build up the models
#baseline
lme1 <- gls(GS ~ 1, data = multiLevel, method = "ML")

#add random intercept for category
lme2 <- lme(GS ~ 1, data = multiLevel, random = ~ 1|category, method = "ML")
summary(lme2)
anova(lme1, lme2)

#add CSs as predictor
lme3 <- lme(GS ~ CS, data = multiLevel, random = ~ 1|category, method = "ML")

#add random slopes
lme4 <- lme(GS ~ CS, random = ~ CS |category, data = multiLevel, method = "ML" )

#add interaction with condition
lme5 <- lme(GS ~ CS*condition, random = ~ CS |category, data = multiLevel, method = "ML" )

#(5) pick best model
anova(lme1, lme2, lme3, lme4, lme5)

#(6) results
summary(lme5)

#interpretations:


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

### Plot difference scores: CS x GS

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

baseline <- lme(diff ~ 1, random = ~ 1|subject/type_specific/condition, data = CSGSFeat, method = "ML")
conditionM <- update(baseline, .~. + condition)
typeM <- update(conditionM, .~. + type_specific)
interaction <- update(typeM, .~. + condition:type_specific)
anova(baseline, conditionM, typeM, interaction)


#analyze 2-way interaction
CSGSsame.many <- CSGSsame[CSGSsame$condition == "many_one",]
CSGSsame.one <- CSGSsame[CSGSsame$condition == "one_one",]

#CS
t.test(CSGSsame.many$diff[CSGSsame.many$type_specific == "CS"], CSGSsame.one$diff[CSGSsame.one$type_specific == "CS"], paired = FALSE,  var.equal = FALSE)

#GS same
t.test(CSGSsame.many$diff[CSGSsame.many$type_specific == "GS same"], CSGSsame.one$diff[CSGSsame.one$type_specific == "GS same"], paired = FALSE,  var.equal = FALSE)


### ?? lmer: type_specific as grouping variable

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

#MANOVA
manovaData <- subset(dataDirect, select = -c(subject, val, measure, response, nr_obs))
manovaData <- manovaData[!manovaData$type_specific == "Group",]
manovaData$CS <- dataDirect$diff[dataDirect$type_specific == "CS"]
manovaData$GSsame <- dataDirect$diff[dataDirect$type_specific == "GS same"]
manovaData$GSdifferent <- dataDirect$diff[dataDirect$type_specific == "GS different"]
manovaData$Feature <- dataDirect$diff[dataDirect$type_specific == "Feature"]
manovaData <- manovaData[1:400, ]
manovaData$type_specific <- NULL
manovaData$diff <- NULL

#homogeneity of covariance matrices
by(manovaData[,2:5], manovaData$condition, cov)

#multivariate outliers
library(mvoutlier)
aq.plot(manovaData[,2:5])

#put multiple outcomes in the model
outcome <- cbind(manovaData$CS, manovaData$GSsame, manovaData$GSdifferent, manovaData$Feature)

#calculate the model
conditionModel <- manova(outcome ~ condition, data = manovaData)
conditionModel
summary(conditionModel)
summary(conditionModel, test = "Wilks")
summary(conditionModel, test = "Hotelling")
summary(conditionModel, test = "Roy")

#Hypothesis: interaction type x CS Variability

CSGSFeat <- dataDirect[!dataDirect$type_specific ==
                         "Group",]

#### mixed-model ANOVA with CS Variability x Type (CS vs. GS same vs. GS different vs. Feature)
direct.type <- aggregate(diff ~ condition + type_specific + subject, CSGSFeat, mean)
aov_specific <- aov_car(diff ~ condition*type_specific + Error(subject/type_specific), direct.type, anova_table = list("pes"))
summary(aov_specific)
aov_specific
#Departure from Sphericity, use Greenhouse-Geisser correction

#??as linear model
baseline <- lme(diff ~ 1, random = ~ 1|subject/type_specific/condition, data = CSGSFeat, method = "ML")
conditionM <- update(baseline, .~. + condition)
typeM <- update(conditionM, .~. + type_specific)
interaction <- update(typeM, .~. + condition:type_specific)
anova(baseline, conditionM, typeM, interaction)
summary(interaction)

baseline <- lmer(diff ~ 1 + (type_specific|subject),  CSGSFeat, REML = FALSE)
conditionM <- lmer(diff ~ condition + (type_specific|subject),  CSGSFeat, REML = FALSE)
typeM <- lmer(diff ~ condition + type_specific + (type_specific|subject),  CSGSFeat, REML = FALSE)
type_condition <- lmer(diff ~ condition*type_specific + (type_specific|subject),  CSGSFeat, REML = FALSE)
anova(baseline, conditionM, typeM, type_condition)
summary(typeM)

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


################### randomly select 2 CSs in "many-to-one"

new_one <- direct[direct$condition == "one_one",]
new_many <- direct[direct$condition == "many_one",]
new_many <- new_many[!new_many$type_specific == "CS",]
new_direct <- rbind(new_one, new_many)

for (subject in unique(direct$subject)){
  if (direct$condition[direct$subject == subject] == "many_one"){
    for (cat in 1:4){
      temp <- direct[direct$subject == subject & direct$type_specific == "CS" & direct$category == cat,]
      select <- temp[1:2,]
      new_direct <- rbind(new_direct, select)
    }
  }
  
}

#calculate difference scores
dataDirect_new <- aggregate(response ~ subject + condition + measure + val + type_specific, new_direct, mean)
dataDirect_new$nr_obs <- aggregate(response ~ subject + condition + measure + val + type_specific, new_direct, length)[[6]]

#calculate differences between positive and negative valence, considering each type of measure
for (name in unique(dataDirect_new$type_specific)){
  temp <- dataDirect_new[dataDirect_new$type_specific == name,];
  dataDirect_new$diff[dataDirect_new$type_specific == name] <- temp$response[temp$val == "pos"] - temp$response[temp$val == "neg"]
}


# Plot 
direct.type_specific <- aggregate(diff ~ condition + type_specific, dataDirect_new, mean)
direct.type_specific$se <- aggregate(diff ~ condition + type_specific, dataDirect_new, se)[[3]]
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

# inference statistics
CSGSFeat_new <- dataDirect_new[!dataDirect_new$type_specific == "Group",]
direct.type <- aggregate(diff ~ condition + type_specific + subject, CSGSFeat_new, mean)
aov_specific <- aov_car(diff ~ condition*type_specific + Error(subject/type_specific), direct.type, anova_table = list("pes"))
summary(aov_specific)
#results don't change

#Boxplots different conditions
diffDirect <- ggplot(dataDirect_new, aes (x = type_specific, y = diff, fill = condition)) +
  stat_boxplot(geom = "errorbar") +
  geom_boxplot(show.legend = TRUE) +
  ggtitle("Differences") + 
  theme(plot.title = element_text (hjust = 0.5, face = "bold", size = 12)) +
  scale_fill_brewer(palette = "Blues") +
  scale_y_continuous (name = "Pos - Neg", breaks = seq(-100, 200, 25), limits = c(-50, 100))
diffDirect


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

#plots 
barplotMeasure <- aggregate(diff ~ type_specific + condition + measure, dataDirect, mean)
barplotMeasure$se <- aggregate(diff ~ type_specific + condition + measure, dataDirect, se)[[4]]

plotMeasure <- ggplot(barplotMeasure, aes (x = type_specific, y = diff, fill = condition)) +
  facet_grid(. ~ measure) +
  geom_bar(stat = 'identity', position = position_dodge(), show.legend = TRUE) +
  geom_errorbar(aes(ymin= diff - se, ymax= diff + se), width=.2,
                position=position_dodge(.9)) +
  ggtitle("Direct Measure") + 
  theme(plot.title = element_text (hjust = 0.5, face = "bold", size = 12)) +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous (name = "Rating[Pos] - Rating[Neg]", breaks = seq(-25, 125, 25), limits = c(-10, 100))
plotMeasure

boxplot <- ggplot(dataDirect, aes (x = type_specific, y = diff, fill = condition)) +
  facet_grid(. ~ measure) +
  stat_boxplot(geom = "errorbar") +
  geom_boxplot(show.legend = TRUE) +
  ggtitle("Direct Measure") + 
  theme(plot.title = element_text (hjust = 0.5, face = "bold", size = 12)) +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous (name = "Pos - Neg", breaks = seq(-25, 125, 25), limits = c(-10, 100))
boxplot

#mixed-model ANOVA variability x type (CS vs. GS same) vs measure  (direct first vs. indirect first)

CSGSsame <- dataDirect[dataDirect$type_specific == "CS" | dataDirect$type_specific == "GS same",]

#### mixed-model ANOVA with CS Variability x Type (CS vs. GS same)
direct.type <- aggregate(diff ~ condition + type_specific + measure + subject, CSGSsame, mean)
aov_specific <- aov_car(diff ~ condition*type_specific*measure + Error(subject/type_specific), direct.type, anova_table = list("pes"))
aov_specific

################## (3) including category

#calculate difference scores
categories <- aggregate(response ~ subject + condition + category + val + type_specific, direct, mean)
categories$nr_obs <- aggregate(response ~ subject + condition + measure + val + type_specific, direct, length)[[6]]

#calculate differences between positive and negative valence, considering each type of measure
for (name in unique(categories$type_specific)){
  temp <- categories[categories$type_specific == name,];
  categories$diff[categories$type_specific == name] <- temp$response[temp$val == "pos"] - temp$response[temp$val == "neg"]
}

categoryDirectPlot <- ggplot(categories, aes (x = type_specific, y = diff, fill = condition)) +
  facet_grid(. ~ category) +
  stat_boxplot(geom = "errorbar") +
  geom_boxplot(show.legend = TRUE) +
  ggtitle("Categories") + 
  theme(plot.title = element_text (hjust = 0.5, face = "bold", size = 12)) +
  scale_fill_brewer(palette = "Blues") 
categoryDirectPlot


################## (4) individual CS exemplars

csSelectedDirect <- aggregate(response ~ cs_selected + val, direct, mean)
csDirectPlot <- ggplot(csSelectedDirect, aes (x = cs_selected, y = response, col = val)) +
  geom_point(show.legend = TRUE) +
  ggtitle("Single CSs") + 
  theme(plot.title = element_text (hjust = 0.5, face = "bold", size = 12)) +
  scale_fill_brewer(palette = "Blues") 
csDirectPlot

#potentially problematic:
#1/GS1, 1/GS2, 1/GS3
#4/GS2
  
