################### CS Variability: one-to-one vs. many-to-one
### junde, 2021
### Kathrin Reichmann

### Plots and analysis for retreat

library(dplyr)
library(tidyverse)
library(ggplot2)
library(lme4)
library(sjstats)
library(mvoutlier) #outlier analysis

#Functions
CI <- function(x) qnorm(0.975)*sd(x)/sqrt(length(x))
se <- function(x) sd(x)/sqrt(length(x))


# generalization: direct evaluative measure -------------------------------

### set working directory:
#setwd("\\\\sn00.zdv.uni-tuebingen.de/siskr01/Documents/Github/CSCond_analysis/CSCond_analysis/data")
setwd("C:/Users/reich/Documents/GitHub/CSCond_analysis/Study1_EC/data")


direct <- read.csv2('direct.csv', header = TRUE)
str(direct)

as_factor <- c("subject", "val", "condition_code", "condition", "measure", "measure_code", "type", "type_specific", "category", "cs_selected")

for (factor in as_factor){
  direct[, factor] <- as.factor(direct[,factor])
}

direct$type_specific <- factor(direct$type_specific, levels = c("CS", "GS same", "GS different", "Feature", "Group"))


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

#right now: do not take different targets into account (participants are nested within targets)
temp <- aggregate(response ~ subject + condition + val + type_specific + category, new_direct, mean)
temp$nr_obs <- aggregate(response ~ subject + condition + val + type_specific + category, new_direct, length)[[6]]

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

#look only at "CS" and "GS"
HLM <- HLM[!HLM$type_specific =="GS different", ]
HLM <- HLM[!HLM$type_specific == "Group",]
HLM <- HLM[!HLM$type_specific == "Feature",]

## rename many_one to many and one_one to one
HLM$condition <- factor(HLM$condition, labels = c("many", "one"), levels = c("many_one", "one_one"))

##categorical variable: generalization as discrete
HLM$type_discrete <- factor(HLM$type_specific, labels = c("CS", "GS"), levels = c("CS", "GS same"))

#reverse dummy coding for condiiton
HLM$condition <- factor(HLM$condition, labels = c("one", "many"), levels = c("one", "many"))

###### Plot data

means <- aggregate(diff ~ condition + type_discrete, HLM, mean)
means$se <- aggregate(diff ~ condition + type_discrete, HLM, se)[[3]]
means

#barplot with standard errors
barplotDiff <- ggplot(means, aes (x = type_discrete, y = diff, fill = condition)) +
  geom_bar(stat = 'identity', position = position_dodge(), show.legend = TRUE) +
  geom_errorbar(aes(ymin= diff - se, ymax= diff + se), width=.2,
                position=position_dodge(.9)) +
  ggtitle("Mean Differences (with Standard Errors)") + 
  scale_fill_brewer(palette = "Set2") +
  scale_x_discrete(name = "\nType") +
  scale_y_continuous (name = "Mean Difference Scores\n", breaks = seq(0, 100, 10), limits = c(0, 100)) + 
  theme_classic() +
  labs(fill = "Condition") +
  theme(plot.title = element_text (hjust = 0.5, face = "bold", size = 14),
        text = element_text(size=14))
barplotDiff

####### Data analysis

#rANOVA

aov.out <- aov(diff ~ condition*type_discrete + Error(subject + subject:type_discrete), HLM)
summary(aov.out)

#effect size
eta_sq(aov.out, partial = TRUE)

#simple slopes
lm <- lm (diff ~ condition*type_specific, HLM)
summary(lm)

#CS
lm1 <- lm(diff ~ condition, HLM[HLM$type_discrete == "CS",])
t.test(diff ~ condition, HLM[HLM$type_discrete== "CS",])
summary(lm1)


lm2 <- lm(diff ~ condition, HLM[HLM$type_discrete == "GS",])
t.test(diff ~ condition, HLM[HLM$type_discrete == "GS",])
summary(lm2)


###################### 2nd Alternative to analyze data:

#(1) mean scores for every category and participant
#with category:
#centerDirect <- aggregate(response ~ subject + condition + type_specific + val + category, new_direct, mean)

#without category:
centerDirect <- aggregate(response ~ subject + condition + type_specific + val, new_direct, mean)

#look only at "CS" and "GS"
centerDirect <- centerDirect[!centerDirect$type_specific =="GS different", ]
centerDirect <- centerDirect[!centerDirect$type_specific == "Group",]
centerDirect <- centerDirect[!centerDirect$type_specific == "Feature",]


## rename many_one to many and one_one to one
centerDirect$condition <- factor(centerDirect$condition, labels = c("many", "one"), levels = c("many_one", "one_one"))

##categorical variable: generalization as discrete
centerDirect$type_discrete <- factor(centerDirect$type_specific, labels = c("CS", "GS"), levels = c("CS", "GS same"))

#reverse dummy coding for condiiton
centerDirect$condition <- factor(centerDirect$condition, labels = c("one", "many"), levels = c("one", "many"))

#(2) use CS as predictor for GS

multiLevel <- centerDirect[centerDirect$type_discrete == "CS",]
respGS <- centerDirect[centerDirect$type_discrete == "GS",]
multiLevel$GS <- respGS$response
multiLevel$CS <- multiLevel$response
multiLevel$response <- NULL
multiLevel$type_specific <- NULL
multiLevel$type_discrete <- NULL
head(multiLevel)
#z-standardize GS and CS
#multiLevel$GS <- scale (multiLevel$GS, center = TRUE, scale = TRUE)
#multiLevel$CS <- scale (multiLevel$CS, center = TRUE, scale = TRUE)

#condition_code: 0 =  many


dotplot <- ggplot(multiLevel, aes (x = CS, y = GS, color = condition)) +
  facet_grid(. ~ condition) +
  geom_point(show.legend = TRUE) +
  ggtitle("Direct Evaluative Ratings\n") + 
  geom_smooth(method = 'lm', aes (color = condition)) +
  scale_color_brewer(palette = "Set2") +
  scale_x_continuous(name = "\nCS Ratings") +
  scale_y_continuous (name = "GS Ratings\n") + 
  theme_classic() +
  labs(color = "Condition") +
  theme(plot.title = element_text (hjust = 0.5, face = "bold", size = 14),
        text = element_text(size=14))
dotplot

#calculate multiple regression

model1 <- lm(GS ~ CS, data = multiLevel)
summary(model1)

#report this model in the presentation


#Effektkodierung -0.5, 0.5
multiLevel$condition_effect <- 0
for (line in 1:dim(HLM)[1]){
  if (multiLevel$condition[line] == "one"){
    multiLevel$condition_effect[line] <- -0.5
  } else {
    multiLevel$condition_effect[line] <- 0.5
  }
}
multiLevel$condition_effect
str(multiLevel)

#options(contrasts = c("contr.sum", "contr.poly"))

model2 <- lm(GS ~ CS*condition_effect, data = multiLevel)
summary(model2)
anova(model2)

model.matrix(GS ~ CS*condition_effect, data = multiLevel)
plot(model2)

#fixed effect
fix1CS <- data.frame(diff = summary(model2)$coef[1, "Estimate"], type_discrete = "CS")
fix1GS <- data.frame(diff = summary(model2)$coef[1, "Estimate"] + summary(model1)$coef[2, "Estimate"], type_discrete = "GS")
fix1 <- rbind(fix1CS, fix1GS)
fix1

dotplot <- ggplot(multiLevel, aes (x = CS, y = GS, color = condition)) +
  facet_grid(. ~ condition) +
  geom_abline(intercept = summary(model2)$coef[1, "Estimate"], slope = summary(model2)$coef[2, "Estimate"], color = "red" ) +
  geom_abline(intercept = (summary(model2)$coef[1, "Estimate"] + summary(model2)$coef[3, "Estimate"]), slope = (summary(model2)$coef[2, "Estimate"] + summary(model2)$coef[4, "Estimate"]), color = "blue" ) +
  geom_abline(intercept = (summary(model2)$coef[1, "Estimate"] - summary(model2)$coef[3, "Estimate"]), slope = (summary(model2)$coef[2, "Estimate"] - summary(model2)$coef[4, "Estimate"]), color = "black" ) +
  geom_point(show.legend = TRUE) +
  ggtitle("Direct Evaluative Ratings\n") + 
  geom_smooth(method = 'lm', aes (color = condition)) +
  scale_color_brewer(palette = "Set2") +
  scale_x_continuous(name = "\nCS Ratings") +
  scale_y_continuous (name = "GS Ratings\n") + 
  theme_classic() +
  labs(color = "Condition") +
  theme(plot.title = element_text (hjust = 0.5, face = "bold", size = 14),
        text = element_text(size=14))
dotplot


#Effektkodierung -1 und 1 (use for presentation)
options(contrasts = c("contr.sum", "contr.poly"))

model2 <- lm(GS ~ CS*condition, data = multiLevel)
summary(model2)
anova(model2)

model.matrix(GS ~ CS*condition, data = multiLevel)
plot(model2)

#simple slope for condition: Many
options(contrasts = c("contr.SAS","contr.poly")) #Dummykodierung

model2 <- lm(GS ~ CS*condition, data = multiLevel)
summary(model2)
anova(model2)

model.matrix(GS ~ CS*condition, data = multiLevel)
plot(model2)


dotplot <- ggplot(multiLevel, aes (x = CS, y = GS, color = condition)) +
  facet_grid(. ~ condition) +
  geom_abline(intercept = summary(model2)$coef[1, "Estimate"], slope = summary(model2)$coef[2, "Estimate"], color = "red" ) +
  geom_abline(intercept = (summary(model2)$coef[1, "Estimate"] + summary(model2)$coef[3, "Estimate"]), slope = (summary(model2)$coef[2, "Estimate"] + summary(model2)$coef[4, "Estimate"]), color = "blue" ) +
  geom_point(show.legend = TRUE) +
  ggtitle("Direct Evaluative Ratings\n") + 
  geom_smooth(method = 'lm', aes (color = condition)) +
  scale_color_brewer(palette = "Set2") +
  scale_x_continuous(name = "\nCS Ratings") +
  scale_y_continuous (name = "GS Ratings\n") + 
  theme_classic() +
  labs(color = "Condition") +
  theme(plot.title = element_text (hjust = 0.5, face = "bold", size = 14),
        text = element_text(size=14))
dotplot


#simple slope for Many
options(contrasts = c("contr.treatment","contr.poly")) #Dummykodierung

model2 <- lm(GS ~ CS*condition, data = multiLevel)
summary(model2)
anova(model2)

model.matrix(GS ~ CS*condition, data = multiLevel)
plot(model2)


#### keep this model in mind for later
model3 <- lm(GS ~ CS*condition_effect*val, data = multiLevel)
summary(model3)



#condition_code: 0 =  many

dotplot <- ggplot(multiLevel, aes (x = CS, y = GS, color = val)) +
  facet_grid(. ~ condition) +
  geom_point(show.legend = TRUE) +
  ggtitle("Direct Evaluative Ratings\n") + 
  geom_smooth(method = 'lm', aes (color = val)) +
  scale_color_brewer(palette = "Paired") +
  scale_x_continuous(name = "\nCS Ratings") +
  scale_y_continuous (name = "GS Ratings\n") + 
  theme_classic() +
  labs(color = "Valence") +
  theme(plot.title = element_text (hjust = 0.5, face = "bold", size = 14),
        text = element_text(size=14))
dotplot

anova(model1, model2, model3)



# components: direct evaluative measure -----------------------------------

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

#reverse dummy coding for condiiton
HLM$condition <- factor(HLM$condition, labels = c("one", "many"), levels = c("one_one", "many_one"))
str(HLM)

## discard levels of type we don't need
HLM <- HLM[!HLM$type_specific == "Group",]
HLM <- HLM[!HLM$type_specific == "GS different",]
HLM <- HLM[!HLM$type_specific == "GS same",]
HLM <- HLM[!HLM$type_specific == "CS",]


##categorical variable: generalization as discrete
HLM$type_discrete <- factor(HLM$type_specific, labels = c("predictive"), levels = c("Feature"))

means <- aggregate(diff ~ type_discrete + condition, HLM, mean)
means$se <- aggregate(diff ~ type_discrete + condition, HLM, se)[[3]]
means

##add hypothetical data
means$type_discrete <- as.character(means$type_discrete)
hypoMany <- c("non-predictive", "many", 18, 0)
hypoOne <- c("non-predictive", "one", 22, 0)
means <- rbind(means, hypoMany, hypoOne)
means$type_discrete <- as.factor(means$type_discrete)
means$diff <- as.numeric(means$diff)
means$se <- as.numeric(means$se)
means

#barplot with standard errors
barplotDiff <- ggplot(means, aes (x = condition, y = diff, fill = type_discrete)) +
  geom_bar(stat = 'identity', position = position_dodge(), show.legend = TRUE) +
  geom_errorbar(aes(ymin= diff - se, ymax= diff + se), width=.2,
                position=position_dodge(.9)) +
  ggtitle("Evaluative Ratings for Components\n") + 
  scale_fill_brewer(palette = "Set3") +
  scale_x_discrete(name = "\nCondition") +
  scale_y_continuous (name = "Mean Difference Scores\n", breaks = seq(0, 100, 10), limits = c(0, 100)) + 
  theme_classic() +
  labs(fill = "Component") +
  theme (plot.title = element_text (hjust = 0.5, face = "bold", size = 16),
         text = element_text(size=14))
barplotDiff

# generalization: indirect evaluative measure -----------------------------


# recognition memory: DRM -------------------------------------------------

setwd("C:/Users/reich/Documents/GitHub/CSCond_analysis/Study2_DRM/data_preprocessed")
#setwd("\\\\sn00.zdv.uni-tuebingen.de/siskr01/Documents/Github/CScond_Exp2/data")

memory1 <- read.csv2('memory1.csv', header = TRUE)
str(memory1)

#exclude timeouts
table(memory1$timeout)
memory1 <- memory1[memory1$timeout == "false",]
table(memory1$timeout)

#convert variables
memory1$type <- factor(memory1$type)
memory1$memoryResp <- as.numeric(memory1$memoryResp)

#delete the columns we don't need
memory1 <- subset(memory1, select = -c(X, trial_index, task, rt, timeout, memoryCorrect, cs_selected, nr_pres))

#exclude levels of "type" we don't want to look at
memory1 <- memory1[!memory1$type == "CSpred",]
memory1 <- memory1[!memory1$type == "CSnonpred",]
memory1 <- memory1[!memory1$type == "GSnew",]
memory1 <- memory1[!memory1$type == "distractor",]

#rename factors and levels: condition
memory1$condition1 <- factor(memory1$condition1, labels = c("one", "many", "fill"), levels = c("one_one", "many_one", "many_fill"))

#rename factors and levels: condition
memory1$type <- factor(memory1$type, labels = c("CS", "GS"), levels = c("CS", "GSold"))

#subject to factor for rANOVA
memory1$subject <- as.factor(memory1$subject)
head(memory1)

### (1) calculate proportions of "old" responses
# correct recognition of studied items (CSs) -> true recognition
# incorrect response "old" to critical lures (GSs) -> false recognition
#0 new
#1 old

memory1Prop <- aggregate(memoryResp ~ subject + condition1 + type, memory1, sum)
memory1Prop$nr <- aggregate(memoryResp ~ subject + condition1 + type, memory1, length)[[4]]
memory1Prop$prop <- (memory1Prop$memoryResp/memory1Prop$nr)
head(memory1Prop)

# (1) plot proportion of "old" responses - barplot

barplotData <- aggregate(prop ~ condition1 + type, memory1Prop, mean)
barplotData$se <- aggregate(prop ~ condition1 + type, memory1Prop, se)[[3]]
barplotData$sd <- aggregate(prop ~ condition1 + type, memory1Prop, sd)[[3]]
barplotData

##Plot generalization stimuli
plotmemory1PropOld <- ggplot(barplotData, aes (x = type, y = prop, fill = condition1)) +
  geom_bar (stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin= prop - se, ymax= prop + se), width=.2,
                position=position_dodge(.9)) +
  ggtitle("Recognition Memory\n") + 
  scale_fill_brewer(palette = "Set2") +
  theme(plot.title = element_text (hjust = 0.5, face = "bold", size = 12)) +
  labs(fill = "Condition") +
  scale_y_continuous (name = "Proportion of 'old' responses\n") +
  scale_x_discrete(name = "\nType") +
  theme_classic()+
  theme (plot.title = element_text (hjust = 0.5, face = "bold", size = 16),
         text = element_text(size=14))
plotmemory1PropOld


#ANOVA
aov.out2 <- aov(prop ~ condition1*type + Error(subject + subject:type), memory1Prop)
aov.out2
summary(aov.out2)

#effect size
eta_sq(aov.out2, partial = TRUE)



lm1 <- lmer(prop ~ condition1*type + (1|subject), memory1Prop)
summary(lm1)

library(car)
anova(lm1)
plot(lm1)
X <- model.matrix(lm1)
C(X) %*% X
cov(X)

#simple slopes

#dummy coding:
options(contrasts = c("contr.SAS", "contr.poly"))

#many as reference category
memory1Prop$condition1 <- factor(memory1Prop$condition1, labels = c("one", "fill", "many"), levels = c("one", "fill", "many"))

#GS, ref. category is many
memory1Prop$type <- factor(memory1Prop$type, labels = c("CS", "GS"), levels = c("CS", "GS"))
lm1 <- lm(prop ~ condition1*type, memory1Prop)
summary(lm1)
model.matrix(lm1)


#GS, ref. category is many
memory1Prop$type <- factor(memory1Prop$type, labels = c("GS", "CS"), levels = c("GS", "CS"))

lm2 <- lm(prop ~ condition1*type, memory1Prop)
summary(lm2)
model.matrix(lm2)

###### logistic regression

#calculate log odds
memory1Prop$odds <- round(memory1$propAdj/(1-memory1Prop$propAdj), 4)
memory1Prop$logodds <- round(log(memory1Prop$odds), 4)

memory1$condition1 <- factor(memory1$condition1, levels = c("many", "one", "fill"))


glm <- glm(memoryResp ~ condition1*type, memory1, family = 'binomial')
summary(glm)

glm1 <- glm(memoryResp ~ condition1, memory1[memory1$type == "CS",], family = binomial (link = 'logit'))
summary(glm1)
exp(1.5629)/(1+exp(1.5629)) #many_one
exp(-0.1134)/(1+exp(-0.1134)) #many_fill -> weird result!!
exp(1.2344)/(1+exp(1.2344))#one_one

glm2 <- glm(memoryResp ~ condition1, memory1[memory1$type == "GS",], family = binomial (link = 'logit'))
summary(glm2)
exp(-0.09607)/(1+exp(-0.09607)) #many_one
exp(-0.33737)/(1+exp(-0.33737)) #many_fill
exp(-1.37723)/(1+exp(-1.37723))#one_one

glm3 <- glm(memoryResp ~ condition1, memory1[memory1$type == "Distractor",], family = 'binomial')
summary(glm3)
exp(-2.1535)/(1+exp(-2.1535)) #many_one
exp(0.5491)/(1+exp(0.5491)) #one_one --> weird result!!
exp(-0.9145)/(1+exp(-0.9145))#many_fill




# recognition memory: components ------------------------------------------


setwd("C:/Users/reich/Documents/GitHub/CSCond_analysis/Study2_DRM/data_preprocessed")
#setwd("\\\\sn00.zdv.uni-tuebingen.de/siskr01/Documents/Github/CScond_Exp2/data")

memory1 <- read.csv2('memory1.csv', header = TRUE)
str(memory1)

#exclude timeouts
table(memory1$timeout)
memory1 <- memory1[memory1$timeout == "false",]
table(memory1$timeout)

#variables as factors
memory1$type <- factor(memory1$type)
memory1$memoryResp <- as.numeric(memory1$memoryResp)

#delete the columns we don't need
memory1 <- subset(memory1, select = -c(X, trial_index, task, rt, timeout, memoryCorrect, cs_selected, nr_pres))

#exclude levels of "type" we don't want to look at
memory1 <- memory1[!memory1$type == "CS",]
memory1 <- memory1[!memory1$type == "GSold",]
memory1 <- memory1[!memory1$type == "GSnew",]
memory1 <- memory1[!memory1$type == "distractor",]

#rename factors and levels: condition
memory1$condition1 <- factor(memory1$condition1, labels = c("one", "many", "fill"), levels = c("one_one", "many_one", "many_fill"))

#rename factors and levels: condition
memory1$type <- factor(memory1$type, labels = c("non-predictive", "predictive"), levels = c("CSnonpred", "CSpred"))

#subject to factor for rANOVA
memory1$subject <- as.factor(memory1$subject)
head(memory1)

### (1) calculate proportions of "old" responses
# correct recognition of studied items (CSs) -> true recognition
# incorrect response "old" to critical lures (GSs) -> false recognition
#0 new
#1 old

memory1Prop <- aggregate(memoryResp ~ subject + condition1 + type, memory1, sum)
memory1Prop$nr <- aggregate(memoryResp ~ subject + condition1 + type, memory1, length)[[4]]
memory1Prop$prop <- (memory1Prop$memoryResp/memory1Prop$nr)
head(memory1Prop)

# (1) plot proportion of "old" responses - barplot

barplotData <- aggregate(prop ~ condition1 + type, memory1Prop, mean)
barplotData$se <- aggregate(prop ~ condition1 + type, memory1Prop, se)[[3]]
barplotData$sd <- aggregate(prop ~ condition1 + type, memory1Prop, sd)[[3]]
barplotData

##Plot generalization stimuli
plotmemory1PropOld <- ggplot(barplotData, aes (x = type, y = prop, fill = condition1)) +
  geom_bar (stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin= prop - se, ymax= prop + se), width=.2,
                position=position_dodge(.9)) +
  ggtitle("Recognition Memory\n") + 
  scale_fill_brewer(palette = "Set2") +
  theme(plot.title = element_text (hjust = 0.5, face = "bold", size = 12)) +
  labs(fill = "Condition") +
  scale_y_continuous (name = "Proportion of 'old' responses\n", limits = c(0.0, 1.0)) +
  scale_x_discrete(name = "\nType") +
  theme_classic()+
  theme (plot.title = element_text (hjust = 0.5, face = "bold", size = 16),
         text = element_text(size=14))
plotmemory1PropOld


#ANOVA

options(contrasts = c("contr.sum", "contr.poly"))

aov.out2 <- aov(prop ~ condition1*type + Error(subject + subject:type), memory1Prop)
aov.out2
summary(aov.out2)

#effect size
eta_sq(aov.out2, partial = TRUE)


options(contrasts = c("contr.sum", "contr.poly"))
lm1 <- lm(prop ~ condition1*type, memory1Prop)
summary(lm1)

library(car)
anova(lm1)
plot(lm1)
X <- model.matrix(lm1)
C(X) %*% X
cov(X)

#simple slopes

#options(contrasts = c("contr.sum", "contr.poly"))
options(contrasts = c("contr.SAS", "contr.poly"))
memory1Prop$condition1 <- factor(memory1Prop$condition1, labels = c("one", "fill", "many"), levels = c("one", "fill", "many"))


str(memory1)
#predictive, ref.category many
lm1 <- lm(prop ~ condition1*type, memory1Prop)
summary(lm1)
model.matrix(lm1)

#non-predictive, ref.category many
memory1Prop$type <- factor(memory1Prop$type, labels = c("predictive", "non-predictive"), levels = c("predictive", "non-predictive"))

lm2 <- lm(prop ~ condition1*type, memory1Prop)
summary(lm2)
model.matrix(lm1)

lm2 <- lm(prop ~ condition1, memory1Prop[memory1Prop$type == "predictive",])
t.test(prop ~ condition1, memory1Prop[memory1Prop$type == "predictive",])
summary(lm2)

