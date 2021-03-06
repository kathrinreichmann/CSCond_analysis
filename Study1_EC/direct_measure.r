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




# hierarchical model (Vanbrabant et al., 2015) ----------------------------

## randomly select 1 CSs for many_one:
new_one <- direct[direct$condition == "one_one",]
new_many <- direct[direct$condition == "many_one",]
new_many <- new_many[!new_many$type_specific == "CS",]
new_direct <- rbind(new_one, new_many)

for (subject in unique(direct$subject)){
  if (direct$condition[direct$subject == subject] == "many_one"){
    for (cat in 1:4){
      temp <- direct[direct$subject == subject & direct$type_specific == "CS" & direct$category == cat,]
      select <- temp[1,]
      new_direct <- rbind(new_direct, select)
    }
  }
}

## like new experiment: no all; two instead of four categories
new_direct$type <- NULL
new_direct <- new_direct[!new_direct$type_specific == "Group",]
new_direct <- new_direct[!new_direct$type_specific == "Feature",]

##aggregate over targets and category

#with targets
HLMtarget <- aggregate(response ~ subject + condition + val + type_specific + category + cs_selected, new_direct, mean)
HLMtarget$nr_obs <- aggregate(response ~ subject + condition + val + type_specific + category + cs_selected, new_direct, length)[[7]]

#right now: do not take different targets into account (participants are nested within targets)
temp <- aggregate(response ~ subject + condition + val + type_specific + category, new_direct, mean)
temp$nr_obs <- aggregate(response ~ subject + condition + val + type_specific + category, new_direct, length)[[6]]
temp$nr_obs

## order data
temp <- temp[order(temp$subject, temp$val, temp$type_specific),]

## cbind positive and negative scores
HLMpos <- temp[temp$val == "pos",]
HLMpos$pos <- HLMpos$response
HLMpos$response <- NULL

HLMneg <- temp[temp$val == "neg",]
HLMneg$neg <- HLMneg$response
HLM <- cbind(HLMpos, HLMneg$neg, HLMneg$category)

HLM$val <- NULL
HLM$neg <- HLM$`HLMneg$neg`
HLM$`HLMneg$neg` <- NULL

## calculate difference scores
HLM$diff <- HLM$pos - HLM$neg


# continuous variable for generalization ----------------------------------


## ? use type as a continuous predictor
HLM$type_continuous <- factor(HLM$type_specific, labels = c("0", "1", "2"), levels = c("CS", "GS same", "GS different"))
HLM$type_continuous <- as.numeric(HLM$type_continuous)

## rename many_one to many and one_one to one
HLM$condition <- factor(HLM$condition, labels = c("many", "one"), levels = c("many_one", "one_one"))

##plot individual difference scores for condition and type

HLMdotplot <- aggregate(diff ~ subject + type_continuous + condition, HLM, mean)
means <- aggregate(diff ~ type_continuous + condition, HLM, mean)

dotplot <- ggplot(HLMdotplot, aes (x = type_continuous, y = diff, group = subject, color = condition, shape = condition)) +
  geom_line() +
  geom_point(show.legend = TRUE, alpha = .4) +
  geom_point(data = means, size = 4) +
  scale_color_brewer(palette = "Paired") +
  scale_x_continuous(name = "\nDimension subject i target j") +
  scale_y_continuous (name = "difference scores subject i target j\n") + 
  theme_classic() +
  ggtitle("Raw Data") +
  labs(fill = "condition\n subject i") +
  theme(plot.title = element_text (hjust = 0.5, face = "bold", size = 12))
dotplot


#random intercept model with fixed effect of stimulus dimension
model1 <- lmer(diff ~ type_continuous + (1|subject), data = HLM, REML = FALSE)

intercepts1 <- coef(model1)$subject[,1]
slopes1 <- coef(model1)$subject[,2]
intercept1Fix <- summary(model1)$coef[1, "Estimate"]
slope1Fix <- summary(model1)$coef[2, "Estimate"]

plotModel1 <- ggplot(HLM, aes (x = type_continuous, y = diff, group = subject, color = condition, shape = condition)) +
  geom_abline(slope = slopes1, intercept = intercepts1, alpha = .2) + 
  geom_point(data = means, size = 4) +
  geom_abline(slope = slope1Fix, intercept = intercept1Fix, color = "red", size = 1) +
  geom_point(show.legend = TRUE, alpha = .4) +
  scale_color_brewer(palette = "Paired") +
  scale_x_continuous (name = "\nDimension subject i target j") +
  scale_y_continuous (name = "difference scores subject i target j\n") + 
  theme_classic() +
  ggtitle("Data fitted under Model 1") +
  labs(fill = "condition\n subject i") +
  theme(plot.title = element_text (hjust = 0.5, face = "bold", size = 12))
plotModel1

#+ random slope
model2 <- lmer(diff ~ type_continuous + (type_continuous|subject), data = HLM, REML = FALSE)

intercepts2 <- coef(model2)$subject[,1]
slopes2 <- coef(model2)$subject[,2]
intercept2Fix <- summary(model2)$coef[1, "Estimate"]
slope2Fix <- summary(model2)$coef[2, "Estimate"]

plotModel2 <- ggplot(HLM, aes (x = type_continuous, y = diff, group = subject, color = condition, shape = condition)) +
  geom_abline(slope = slopes2, intercept = intercepts2, alpha = .2) + 
  geom_point(data = means, size = 4) +
  geom_abline(slope = slope2Fix, intercept = intercept2Fix, color = "red", size = 1) +
  geom_point(show.legend = TRUE, alpha = .4) +
  scale_color_brewer(palette = "Paired") +
  scale_x_continuous (name = "\nDimension subject i target j") +
  scale_y_continuous (name = "difference scores subject i target j\n") + 
  theme_classic() +
  ggtitle("Data fitted under Model 2") +
  labs(fill = "condition\n subject i") +
  theme(plot.title = element_text (hjust = 0.5, face = "bold", size = 12))
plotModel2

#+difference variable
model3 <- lmer(diff ~ type_continuous*condition + (type_continuous|subject), data = HLM, REML = FALSE)

intercepts3 <- coef(model3)$subject[,1]
slopes3 <- coef(model3)$subject[,2]
intercept3FixMany <- summary(model3)$coef[1, "Estimate"]
intercept3FixOne <- summary(model3)$coef[1, "Estimate"] + summary(model3)$coef[3, "Estimate"]
slope3FixMany <- summary(model3)$coef[2, "Estimate"]
slope3FixOne <- summary(model3)$coef[2, "Estimate"] + summary(model3)$coef[4, "Estimate"]

plotModel3 <- ggplot(HLM, aes (x = type_continuous, y = diff, group = subject, color = condition, shape = condition)) +
  geom_abline(slope = slopes3, intercept = intercepts3, alpha = .2) + 
  geom_abline(slope = slope3FixMany, intercept = intercept3FixMany, color = "lightblue", size = 1.5) +
  geom_abline(slope = slope3FixOne, intercept = intercept3FixOne, color = "steelblue", size = 1.5) +
  geom_point(show.legend = TRUE, alpha = .4) +
  geom_point(data = means, size = 4) +
  scale_color_brewer(palette = "Paired") +
  scale_x_continuous (name = "\nDimension subject i target j") +
  scale_y_continuous (name = "difference scores subject i target j\n") + 
  theme_classic() +
  ggtitle("Data fitted under Model 3") +
  labs(fill = "condition\n subject i") +
  theme(plot.title = element_text (hjust = 0.5, face = "bold", size = 12))
plotModel3


#model comparison
anova(model1, model2, model3)
#model 3 with best fit

#parameter interpretation
plot(model3)
summary(model3)


# including valence as factor, AV: evaluative ratings  ---------------------------------------------------
HLMrat <- aggregate(response ~ subject + condition + val + type_specific + category, new_direct, mean)
HLMrat$nr_obs <- aggregate(response ~ subject + condition + val + type_specific + category, new_direct, length)[[6]]
HLMrat$nr_obs

## ? use type as a continuous predictor
HLMrat$type_continuous <- factor(HLMrat$type_specific, labels = c("0", "1", "2"), levels = c("CS", "GS same", "GS different"))
HLMrat$type_continuous <- as.numeric(HLMrat$type_continuous)

## rename many_one to many and one_one to one
HLMrat$condition <- factor(HLMrat$condition, labels = c("many", "one"), levels = c("many_one", "one_one"))

##plot individual difference scores for condition and type

HLMdotplot <- aggregate(response ~ subject + type_continuous + condition + val, HLMrat, mean)
means <- aggregate(response ~ type_continuous + condition + val, HLMrat, mean)

dotplot <- ggplot(HLMdotplot, aes (x = type_continuous, y = response, group = subject, color = condition, shape = condition)) +
  facet_grid(. ~ val) +
  geom_line() +
  geom_point(show.legend = TRUE, alpha = .4) +
  geom_point(data = means, size = 4) +
  scale_color_brewer(palette = "Paired") +
  scale_x_continuous(name = "\nDimension subject i target j") +
  scale_y_continuous (name = "evaluative ratings subject i target j\n") + 
  theme_classic() +
  ggtitle("Raw Data") +
  labs(fill = "condition\n subject i") +
  theme(plot.title = element_text (hjust = 0.5, face = "bold", size = 12))
dotplot


#random intercept model with fixed effect of stimulus dimension
model1 <- lmer(response ~ type_continuous*val + (val|subject), data = HLMrat, REML = FALSE)
summary(model1)

intercepts1 <- coef(model1)$subject[,1]
slopes1 <- coef(model1)$subject[,3]
intercept1Fix <- summary(model1)$coef[1, "Estimate"]
slope1Fix <- summary(model1)$coef[2, "Estimate"]

plotModel1 <- ggplot(HLMrat, aes (x = type_continuous, y = response, group = subject, color = condition, shape = condition)) +
  facet_grid(. ~ val) +
  geom_abline(slope = slopes1, intercept = intercepts1, alpha = .2) + 
  geom_point(data = means, size = 4) +
  geom_abline(slope = slope1Fix, intercept = intercept1Fix, color = "red", size = 1) +
  geom_point(show.legend = TRUE, alpha = .4) +
  scale_color_brewer(palette = "Paired") +
  scale_x_continuous (name = "\nDimension subject i target j") +
  scale_y_continuous (name = "difference scores subject i target j\n") + 
  theme_classic() +
  ggtitle("Data fitted under Model 1") +
  labs(fill = "condition\n subject i") +
  theme(plot.title = element_text (hjust = 0.5, face = "bold", size = 12))
plotModel1

#+ random slope
model2 <- lmer(diff ~ type_continuous + (type_continuous|subject), data = HLM, REML = FALSE)

intercepts2 <- coef(model2)$subject[,1]
slopes2 <- coef(model2)$subject[,2]
intercept2Fix <- summary(model2)$coef[1, "Estimate"]
slope2Fix <- summary(model2)$coef[2, "Estimate"]

plotModel2 <- ggplot(HLM, aes (x = type_continuous, y = diff, group = subject, color = condition, shape = condition)) +
  geom_abline(slope = slopes2, intercept = intercepts2, alpha = .2) + 
  geom_point(data = means, size = 4) +
  geom_abline(slope = slope2Fix, intercept = intercept2Fix, color = "red", size = 1) +
  geom_point(show.legend = TRUE, alpha = .4) +
  scale_color_brewer(palette = "Paired") +
  scale_x_continuous (name = "\nDimension subject i target j") +
  scale_y_continuous (name = "difference scores subject i target j\n") + 
  theme_classic() +
  ggtitle("Data fitted under Model 2") +
  labs(fill = "condition\n subject i") +
  theme(plot.title = element_text (hjust = 0.5, face = "bold", size = 12))
plotModel2

#+difference variable
model3 <- lmer(diff ~ type_continuous*condition + (type_continuous|subject), data = HLM, REML = FALSE)

intercepts3 <- coef(model3)$subject[,1]
slopes3 <- coef(model3)$subject[,2]
intercept3FixMany <- summary(model3)$coef[1, "Estimate"]
intercept3FixOne <- summary(model3)$coef[1, "Estimate"] + summary(model3)$coef[3, "Estimate"]
slope3FixMany <- summary(model3)$coef[2, "Estimate"]
slope3FixOne <- summary(model3)$coef[2, "Estimate"] + summary(model3)$coef[4, "Estimate"]

plotModel3 <- ggplot(HLM, aes (x = type_continuous, y = diff, group = subject, color = condition, shape = condition)) +
  geom_abline(slope = slopes3, intercept = intercepts3, alpha = .2) + 
  geom_abline(slope = slope3FixMany, intercept = intercept3FixMany, color = "lightblue", size = 1.5) +
  geom_abline(slope = slope3FixOne, intercept = intercept3FixOne, color = "steelblue", size = 1.5) +
  geom_point(show.legend = TRUE, alpha = .4) +
  geom_point(data = means, size = 4) +
  scale_color_brewer(palette = "Paired") +
  scale_x_continuous (name = "\nDimension subject i target j") +
  scale_y_continuous (name = "difference scores subject i target j\n") + 
  theme_classic() +
  ggtitle("Data fitted under Model 3") +
  labs(fill = "condition\n subject i") +
  theme(plot.title = element_text (hjust = 0.5, face = "bold", size = 12))
plotModel3


#model comparison
anova(model1, model2, model3)
#model 3 with best fit

#parameter interpretation
plot(model3)
summary(model3)

# polynomial HLM ----------------------------------------------------------


#+ quadratic effect of stimulus dimension
model4 <- lmer(diff ~ type_continuous*condition + I(type_continuous^2)*condition + (I(type_continuous^2)|subject), data = HLM, REML = FALSE)
anova(model3, model4)

intercepts4 <- coef(model4)$subject[,1]
slopes4 <- coef(model4)$subject[,2]
intercept4Fix <- summary(model4)$coef[1, "Estimate"]
slope4Fix <- summary(model4)$coef[2, "Estimate"]

plotModel4 <- ggplot(HLM, aes (x = type_continuous, y = diff, group = subject, color = condition, shape = condition)) +
  geom_abline(slope = slopes4, intercept = intercepts4, alpha = .2) + 
  geom_point(data = means, size = 4) +
  geom_abline(slope = slope4Fix, intercept = intercept4Fix, color = "red", size = 1) +
  geom_point(show.legend = TRUE, alpha = .4) +
  scale_color_brewer(palette = "Paired") +
  scale_x_continuous (name = "\nDimension subject i target j") +
  scale_y_continuous (name = "difference scores subject i target j\n") + 
  theme_classic() +
  ggtitle("Data fitted under Model 4") +
  labs(fill = "condition\n subject i") +
  theme(plot.title = element_text (hjust = 0.5, face = "bold", size = 12))
plotModel4

#+quadratic random effect
model5 <- lmer(diff ~ type_continuous + I(type_continuous^2)*condition + (1+ type_continuous|subject), data = HLM, REML = FALSE)
anova(model4, model5)

#+individual difference variable
model6 <- lmer(diff ~ type_continuous*condition + I(type_continuous^2) + (type_continuous|subject), data = HLM, REML = FALSE)
anova(model1, model2, model3, model4, model5, model6)

model7 <- lmer(diff ~ type_continuous*condition + I(type_continuous^2)*condition + (I(type_continuous^2)|subject), data = HLM, REML = FALSE)
anova(model1, model2, model3, model4, model5, model6, model7)



# categorical variable for generaliztation --------------------------------

##categorical variable: generalization as discrete

HLM$type_discrete <- factor(HLM$type_specific, labels = c("CS", "GS1", "GS2"), levels = c("CS", "GS same", "GS different"))


### plot raw data

HLMdotplot <- aggregate(diff ~ subject + type_discrete + condition, HLM, mean)
means <- aggregate(diff ~ type_discrete + condition, HLM, mean)

HLMdotplotVar1 <- HLMdotplot[!HLMdotplot$type_discrete == "GS2",]
means1 <- means[!means$type_discrete == "GS2",]

dotplot1 <- ggplot(HLMdotplotVar1, aes (x = type_discrete, y = diff, group = subject, color = condition, shape = condition)) +
  geom_line() +
  geom_point(show.legend = TRUE, alpha = .4) +
  geom_point(data = means1, size = 4) +
  scale_color_brewer(palette = "Paired") +
  scale_x_discrete(name = "\nDimension subject i target j") +
  scale_y_continuous (name = "difference scores subject i target j\n") + 
  theme_classic() +
  ggtitle("Raw Data") +
  labs(fill = "condition\n subject i") +
  theme(plot.title = element_text (hjust = 0.5, face = "bold", size = 12))
dotplot1

HLMdotplotVar2 <- HLMdotplot[!HLMdotplot$type_discrete == "GS1",]
means2 <- means[!means$type_discrete == "GS1",]

dotplot2 <- ggplot(HLMdotplotVar2, aes (x = type_discrete, y = diff, group = subject, color = condition, shape = condition)) +
  geom_line() +
  geom_point(show.legend = TRUE, alpha = .4) +
  geom_point(data = means2, size = 4) +
  scale_color_brewer(palette = "Paired") +
  scale_x_discrete(name = "\nDimension subject i target j") +
  scale_y_continuous (name = "difference scores subject i target j\n") + 
  theme_classic() +
  ggtitle("Raw Data") +
  labs(fill = "condition\n subject i") +
  theme(plot.title = element_text (hjust = 0.5, face = "bold", size = 12))
dotplot2

##### random intercept model with fixed effect of stimulus type
model1 <- lmer(diff ~ type_discrete + (1|subject), data = HLM, REML = FALSE)
summary(model1)

intercepts1 <- coef(model1)$subject[,1]
slopes1 <- coef(model1)$subject[,2]
intercept1Fix <- summary(model1)$coef[1, "Estimate"]
slope1Fix1 <- summary(model1)$coef[1, "Estimate"] - summary(model1)$coef[2, "Estimate"]
slope1Fix2 <- summary(model1)$coef[1, "Estimate"] - summary(model1)$coef[3, "Estimate"]

plotModel1 <- ggplot(HLM, aes (x = type_discrete, y = diff, group = subject, color = condition, shape = condition)) +
  geom_abline(slope = slopes1, intercept = intercepts1, alpha = .02) +  
  geom_abline(slope = slope1Fix1, intercept = intercept1Fix, color = "red", size = 1) +
  geom_abline(slope = slope1Fix2, intercept = intercept1Fix, color = "blue", size = 1) +
  geom_point(data = means, size = 4) +
  geom_point(show.legend = TRUE, alpha = .4) +
  scale_color_brewer(palette = "Paired") +
  scale_x_discrete (name = "\nDimension subject i target j") +
  scale_y_continuous (name = "difference scores subject i target j\n") + 
  theme_classic() +
  ggtitle("Data fitted under Model 1") +
  labs(fill = "condition\n subject i") +
  theme(plot.title = element_text (hjust = 0.5, face = "bold", size = 12))
plotModel1

#+ random slope
model2 <- lmer(diff ~ type_discrete + (type_discrete|subject), data = HLM, REML = FALSE)

#+difference variable
model3 <- lmer(diff ~ type_discrete*condition + (type_discrete|subject), data = HLM, REML = FALSE)

intercepts <- coef(model3)$subject[,1]
slopes <- coef(model3)$subject[,2]
summary(model3)$coef[, "Estimate"]

plotModel3 <- ggplot(HLM, aes (x = type_continuous, y = diff, group = subject, color = condition, shape = condition)) +
  geom_abline(slope = slopes, intercept = intercepts, color = "blue") +  
  geom_point(show.legend = TRUE, alpha = .4) 
plotModel3


#model comparison
anova(model1, model2, model3)
#model 3 with best fit

#parameter interpretation
plot(model3)
summary(model3)


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
  
