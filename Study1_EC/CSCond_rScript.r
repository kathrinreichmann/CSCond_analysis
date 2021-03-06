setwd("C:/Users/reich/Documents/GitHub/FLO_replication/CScond_online/data")

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

filenames <- dir()
dat <- data.frame()
for (i in filenames){
  dat <- rbind(dat, read.table(i, header = TRUE, sep = ",", encoding = "UTF-8"))
}

comments_one <- dat$responses[dat$trial_type == "survey-text" & dat$condition == "one_one"]
#comments_one

comments_many <- dat$responses[dat$trial_type == "survey-text" & dat$condition == "many_one"]
#comments_many

# total before excluding participants
length(unique(dat$subject))

dat$subject <- factor(dat$subject)
dat$response <- as.numeric(dat$response)
dat$responses <- as.numeric(dat$responses)
dat$rt <- as.numeric(dat$rt)

#chinese speaking participants
#0 = ja
get_tables <- aggregate(response ~ subject + chinese, dat, mean)
table(get_tables$chinese)
dat <- dat[!dat$chinese == "0",]

#participants who did not pay attention
get_tables <- aggregate(response ~ subject + hinschauen, dat, mean)
table(get_tables$hinschauen) #1 = nicht hinschauen
dat <- dat[!dat$hinschauen == "1",]
#table(dat$hinschauen)

#who were distracted
get_tables <- aggregate(response ~ subject + ablenkung, dat, mean)
table(get_tables$ablenkung) # 0 = abgelenkt
dat <- dat[!dat$ablenkung == "0",]
#table(dat$ablenkung)

#who did not follow instructions
get_tables <- aggregate(response ~ subject + gewissenhaft, dat, mean)
table(get_tables$gewissenhaft) # 1 = nicht gewissenhaft

#always clicked the same answer in direct measure
subjects <- ggplot(dat, aes (x= subject, y = response, col = subject)) +
                geom_point(show.legend = FALSE)
#subjects

#answered too slowly in indirect measure (rt > 3 SDs)
exclude <- aggregate(rt ~ subject, dat, mean)
exclusion_sd <- sd(na.omit(dat$rt))*3
exclusion_sd
exclude[exclude$rt > exclusion_sd,]

#total after excluding participants
length(unique(dat$subject))

#condition
get_tables <- aggregate(response ~ subject + condition, dat, mean)
table(get_tables$condition)

#measure
get_tables <- aggregate(response ~ subject + measure, dat, mean)
table(get_tables$measure)

#data.frame(colnames(dat))
dat[,1:5] <- NULL
dat$codeword <- NULL
dat$url <- NULL

as_factor <- c("subject", "val", "condition_code", "condition", "measure", "measure_code", "type", "type_specific", "category", "cs_selected")

as.factor(gsub(".png$", "", dat$target))-> dat$target
as.factor(gsub("targets/", "", dat$target))-> dat$target
as.factor(gsub(".png$", "", dat$cs_selected))-> dat$cs_selected
as.factor(gsub("category", "", dat$cs_selected))-> dat$cs_selected

demographics <- dat[c('subject', 'age', 'gender', 'education', 'chinese', 'handedness')]
demographics <- aggregate(. ~ subject, demographics, median);


table(demographics$education)
#education
#1	Studierende/r
#2	Berufstätig
#3	Arbeitssuchend
#4	keine Angabe

##gender
#0	weiblich
#1	männlich
#2	divers
#3	keine Angabe
table(demographics$gender)
#149/201
#48/201

summary(demographics$age)

#extract relevant columns
direct <- dat[dat$task == "direct" | dat$task == "all",] 
direct <- direct[c('response', 'subject', 'condition', 'condition_code', 'measure', 'measure_code', 'val', 'type', 'type_specific', 'category', 'cs_selected')]

for (factor in as_factor){
  direct[, factor] <- as.factor(direct[,factor])
}

direct$type_specific <- factor(direct$type_specific, levels = c('CS', 'GSold', 'GSnew', 'abstract', 'all'))
direct$response <- as.numeric(direct$response)
#str(direct)

#export files
#setwd("\\\\sn00.zdv.uni-tuebingen.de/siskr01/Documents/Github/Exp1a_data_analysis/CSCond_analysis/data")
setwd("C:/Users/reich/Documents/GitHub/CSCond_analysis")
write.csv2(direct, file = 'direct.csv')

#aggregate scores for each subject
dataDirect <- aggregate(response ~ subject + condition + measure + val + type_specific, direct, mean)
dataDirect$nr_obs <- aggregate(response ~ subject + condition + measure + val + type_specific, direct, length)[[6]]
#dataDirect

#calculate differences between positive and negative, for each subject when type_specific is the same
for (subj in unique(dataDirect$subject)){
  for (name in unique(dataDirect$type_specific)){
    temp <- dataDirect[dataDirect$subject == subj & dataDirect$type_specific == name,];
    dataDirect$diff[dataDirect$subject == subj & dataDirect$type_specific == name] <- temp$response[temp$val == "pos"] - temp$response[temp$val == "neg"]
  }
}

head(dataDirect)

direct.type_specific <- aggregate(diff ~ condition + type_specific, dataDirect, mean)
direct.type_specific$sd <- aggregate(diff ~ condition + type_specific, dataDirect, sd)[[3]]
direct.type_specific$se <- aggregate(diff ~ condition + type_specific, dataDirect, se)[[3]]
direct.type_specific$CI <- aggregate(diff ~ condition + type_specific, dataDirect, CI)[[3]]
#direct.type_specific

barplotDiff <- ggplot(direct.type_specific, aes (x = type_specific, y = diff, fill = condition)) +
  geom_bar(stat = 'identity', position = position_dodge(), show.legend = TRUE) +
  geom_errorbar(aes(ymin= diff - se, ymax= diff + se), width=.2,
                position=position_dodge(.9)) +
  ggtitle("Differences") + 
  theme(plot.title = element_text (hjust = 0.5, face = "bold", size = 12)) +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous (name = "Pos - Neg", breaks = seq(-25, 100, 25), limits = c(-10, 100))
barplotDiff

diffDirect <- ggplot(dataDirect, aes (x = type_specific, y = diff, fill = condition)) +
  stat_boxplot(geom = "errorbar") +
  geom_boxplot(show.legend = TRUE) +
  ggtitle("Differences") + 
  theme(plot.title = element_text (hjust = 0.5, face = "bold", size = 12)) +
  scale_fill_brewer(palette = "Blues") +
  scale_y_continuous (name = "Pos - Neg", breaks = seq(-100, 200, 25), limits = c(-50, 100))
diffDirect

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

direct.type <- aggregate(diff ~ condition + type_specific + subject, dataDirect[dataDirect$type_specific == "CS" | dataDirect$type_specific == "GSold",], mean)
aov_specific <- aov_car(diff ~ condition*type_specific + Error(subject/type_specific), direct.type, anova_table = list("pes"))
#summary(aov_specific)

#main effect type
#aggregate(diff ~ type_specific, direct.type, mean)

#interaction effect
#aggregate(diff ~ type_specific + condition, direct.type, mean)

aov_specific2 <- aov_car(diff ~ condition*type_specific + Error(subject/type_specific), dataDirect,  anova_table = list("pes"))
aov_specific2
#summary(aov_specific2)

#interaction effect & main effects
aggregate(diff ~ condition + type_specific, dataDirect, mean)
aggregate(diff ~ condition, dataDirect, mean)
aggregate(diff ~ type_specific, dataDirect, mean)

#difference CS
#n.s.
type1 <- lm(diff ~ condition, dataDirect[dataDirect$type_specific == "CS",] )
#summary(type1)
#anova(type1)

#difference GSold
#s.
type2 <- lm(diff ~ condition, dataDirect[dataDirect$type_specific == "GSold",] )
#summary(type1)
#anova(type2)

#difference GSnew
#s.
type3 <- lm(diff ~ condition, dataDirect[dataDirect$type_specific == "GSnew",] )
#summary(type1)
#anova(type3)

#difference abstract
#s.
type4 <- lm(diff ~ condition, dataDirect[dataDirect$type_specific == "abstract",] )
#summary(type1)
#anova(type4)

#difference all
#s.
type5 <- lm(diff ~ condition, dataDirect[dataDirect$type_specific == "all",] )
#summary(type1)
# anova(type5)

lmDiff1 <- lm(diff ~ condition*type, dataDirect[dataDirect$type_specific == "CS" | dataDirect$type_specific == "GSold",])
#summary(lmDiff1)
#plot(lmDiff1)

lmDiff2 <- lm(diff ~ condition*type_specific, dataDirect)
#summary(lmDiff2)
#plot(lmDiff2)

#random intercept
lmDiffSubj1 <- lmer(diff ~ condition*type_specific + (1 | subject), dataDirect)
#summary(lmDiffSubj1)

#random slopes and random intercept
#lmDiffSubj2 <- lmer(diff ~ condition*type_specific + (type_specific | subject), dataDirect)
#summary(lmDiffSubj2)

#model comparison
#anova(lmDiffSubj1, lmDiffSubj2)

#aov_specific3 <- aov_car(diff ~ condition*type_specific*measure + Error(subject/type_specific), dataDirect, anova_table = list("pes"))
#aov_specific3
#summary(aov_specific3)

lmMeas1 <- lm(diff ~ condition*type*measure, dataDirect[dataDirect$type_specific == "CS" | dataDirect$type_specific == "GSold",])
#summary(lmMeas1)

positiveDirect <- ggplot(direct[direct$val == "pos",], aes (x = type_specific, y = response, fill = condition)) +
  stat_boxplot(geom = "errorbar") +
  geom_boxplot(show.legend = TRUE) +
  ggtitle("Positive Pairings") + 
  theme(plot.title = element_text (hjust = 0.5, face = "bold", size = 12)) +
  scale_fill_brewer(palette = "Blues") 
#positiveDirect

negativeDirect <- ggplot(dataDirect[dataDirect$val == "neg",], aes (x = type_specific, y = response, fill = condition)) +
  stat_boxplot(geom = "errorbar") +
  geom_boxplot(show.legend = TRUE) +
  ggtitle("Negative Pairings") + 
  theme(plot.title = element_text (hjust = 0.5, face = "bold", size = 12)) +
  scale_fill_brewer(palette = "Blues") 
#negativeDirect


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

csSelectedDirect <- aggregate(response ~ cs_selected + val, dat, mean)
csDirectPlot <- ggplot(csSelectedDirect, aes (x = cs_selected, y = response, col = val)) +
  geom_point(show.legend = TRUE)
  ggtitle("Single CSs") + 
  theme(plot.title = element_text (hjust = 0.5, face = "bold", size = 12)) +
  scale_fill_brewer(palette = "Blues") 
#csDirectPlot

#extract relevant columns
indirect <- dat[dat$task == "indirect",]
indirect <- indirect[c('indirect', 'rt', 'subject', 'condition', 'condition_code', 'measure', 'measure_code', 'val', 'type', 'type_specific', 'category', 'cs_selected', 'target', 'nr_pres')]

as_factor <- append(as_factor, "target")
for (factor in as_factor){
  indirect[, factor] <- as.factor(indirect[,factor])
}

indirect$response <- as.numeric(indirect$indirect)
indirect$indirect <- NULL
indirect$nr_pres <- as.numeric(indirect$nr_pres)
indirect$rt <- as.numeric(indirect$rt)
str(indirect)

indirect$type_specific <- factor(indirect$type_specific, levels = c('CS', 'GSold', 'GSnew', 'abstract'))

#export files
#setwd("\\\\sn00.zdv.uni-tuebingen.de/siskr01/Documents/Github/Exp1a_data_analysis/CSCond_analysis/data")
setwd("C:/Users/reich/Documents/GitHub/CSCond_analysis")
write.csv2(indirect, file = 'indirect.csv')

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
    targetsIndirect$diff[targetsIndirect$subject == subj & targetsIndirect$type_specific == name] <- temp$prob[temp$val == "pos"] - temp$prob[temp$val == "neg"]
  }
}


lmProbTargets <- lmer(diff ~ type_specific*condition*val + (type_specific|target), targetsIndirect)
#summary(lmProbTargets)

plot.targets <- aggregate(prob ~ target + val, targetsIndirect, mean)
plot.targets$target <- as.factor(plot.targets$target)

for(target in unique(plot.targets$target)){
  temp <- plot.targets[plot.targets$target == target,]
  plot.targets$diff[plot.targets$target == target] <- temp$prob[temp$val == "pos"] - temp$prob[temp$val == "neg"]
}
barplotTarget <- ggplot(plot.targets[plot.targets$val == "neg",], aes (x = target, y = diff)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  ggtitle("Targets on x-axis") + 
  theme(plot.title = element_text (hjust = 0.5, face = "bold", size = 12)) +
  scale_color_brewer(palette = "Set1") +
  scale_y_continuous(name = "Difference (Prob. Angenehm | Pos) - (Prob Angenehm | Neg)")
#barplotTarget

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

presOrder <- aggregate(rt ~ nr_pres + val + condition , indirect, mean)
presOrder$se <- aggregate(rt ~ nr_pres + val + condition, indirect, se) [[2]]

#aggregate(prob ~ val + type_specific, dataIndirect, mean)
plot.presOrder <- ggplot(presOrder, aes (x = nr_pres, y = rt, col = val)) +
  facet_grid(. ~ condition) +
  geom_bar(stat = 'identity', position = position_dodge(), show.legend = TRUE) +
  geom_errorbar(aes(ymin= rt - se, ymax= prob + se), width=.2,
                position=position_dodge(.9)) +
  geom_point(show.legend = TRUE) +
  ggtitle("Reaction Times") + 
  theme(plot.title = element_text (hjust = 0.5, face = "bold", size = 12)) +
  scale_color_brewer(palette = "Set1") +
  scale_y_continuous(name = "Presentation Order")
plot.presOrder

presOrder <- aggregate(rt ~ type_specific + condition , indirect, mean)
presOrder$se <- aggregate(rt ~ type_specific + condition, indirect, se) [[2]]

#aggregate(prob ~ val + type_specific, dataIndirect, mean)
plot.presOrder <- ggplot(presOrder, aes (x = type_specific, y = rt, col = condition)) +
  geom_bar(stat = 'identity', position = position_dodge(), show.legend = TRUE) +
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
