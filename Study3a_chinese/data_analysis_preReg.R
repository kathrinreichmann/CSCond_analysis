################### CS Variability: one-to-one vs. many-to-one
### july, 2021
### Kathrin Reichmann

### Plots and analysis for retreat

library(dplyr)
library(tidyverse)
library(ggplot2)
library(lme4)
library(sjstats)
library(lmerTest)

#Functions
CI <- function(x) qnorm(0.975)*sd(x)/sqrt(length(x))
se <- function(x) sd(x)/sqrt(length(x))

### set working directory:
setwd("C:/Users/siskr01/GitHub/CSCond_analysis/Study1_EC/data")
DRM <- "C:/Users/siskr01/GitHub/CSCond_analysis/Study2_DRM/data_preprocessed"

# generalization: direct evaluative measure -------------------------------

#read table (pre-processed data)
direct <- read.csv2('direct.csv', header = TRUE)
str(direct)

as_factor <- c("subject", "val", "condition", "measure", "type", "type_specific", "category", "cs_selected")

for (factor in as_factor){
  direct[, factor] <- as.factor(direct[,factor])
}

direct$type_specific <- factor(direct$type_specific, levels = c("CS", "GS same", "GS different", "Feature", "Group"))

#delete variables we don't need
direct$X <- NULL
direct$type <- NULL

## randomly select 1 CS for many_one:
new_one <- direct[direct$condition == "one_one",]
new_many <- direct[direct$condition == "many_one",]

#remove the original CS evaluations
new_many <- new_many[!new_many$type_specific == "CS",]
new_direct <- rbind(new_one, new_many)

for (subject in unique(direct$subject)){
  if (direct$condition[direct$subject == subject] == "many_one"){
    for (cat in 1:4){
      temp <- direct[direct$subject == subject & direct$type_specific == "CS" & direct$category == cat,]
      select <- temp[1,]
      #concat only 1 CS rating per participant
      new_direct <- rbind(new_direct, select)
    }
  }
}
head(new_direct)

temp <- new_direct

# order data
temp <- temp[order(temp$subject, temp$val, temp$type_specific),]

# omit information we don't need
temp <- temp[!temp$type_specific =="GS different", ]
temp <- temp[!temp$type_specific == "Group",]
temp <- temp[!temp$type_specific == "Feature",]

## rename many_one to many and one_one to one
temp$condition <- factor(temp$condition, labels = c("one", "many"), levels = c("one_one", "many_one"))

##categorical variable: generalization as discrete
temp$type_specific <- factor(temp$type_specific, labels = c("CS", "GS"), levels = c("CS", "GS same"))

##### Data Analysis: Multilevel models (with valence)

### Effektkodierung -0.5, 0.5

#condition
temp$condition_effect <- 0
for (line in 1:dim(temp)[1]){
  if (temp$condition[line] == "one"){
    temp$condition_effect[line] <- -0.5
  } else {
    temp$condition_effect[line] <- 0.5
  }
}
temp$condition_effect

#valence
temp$val_effect <- 0
for (line in 1:dim(temp)[1]){
  if (temp$val[line] == "neg"){
    temp$val_effect[line] <- -0.5
  } else {
    temp$val_effect[line] <- 0.5
  }
}
temp$condition_effect
str(temp)

#type
temp$type_effect <- 0
for (line in 1:dim(temp)[1]){
  if (temp$type_specific[line] == "CS"){
    temp$type_effect[line] <- -0.5
  } else {
    temp$type_effect[line] <- 0.5
  }
}
temp$type_specific
str(temp)

#set default to dummy coding
options(contrasts = c("contr.treatment", "contr.poly"))
direct <- temp
direct <- direct[order(direct$subject, direct$val, direct$type_specific, direct$category),]

#analysis 1: difference scores
directNeg <- direct[direct$val == "neg",]
directPos <- direct$response[direct$val == "pos"]

directDiff <- cbind(directNeg, directPos)
directDiff$diff <- directDiff$directPos - directDiff$response
directDiff[directDiff$subject == "02a80kdxm7",]


#set default to dummy coding
options(contrasts = c("contr.treatment", "contr.poly"))

## specify models
lmer1Diff <- lmer(diff ~ condition_effect*type_effect 
                  + (1|subject), 
                  directDiff, REML = FALSE)

lmer2Diff <- lmer(diff ~ condition_effect*type_effect 
                  + (type_effect|subject), 
                  directDiff, REML = FALSE)

anova(lmer1Diff, lmer2Diff)
summary(lmer1Diff)
#choose model1
#significant two-way interaction

aggregate(response ~ condition*type_specific, directDiff, mean)
aggregate(response ~ condition*type_specific, directDiff, sd)

#model1: analyze simple slopes
lmer1_1Diff <- lmer(diff ~ type_effect + condition_effect:type_specific
                    + (type_effect|subject),
                    directDiff, REML = FALSE)
summary(lmer1_1Diff)

#plot model1
boxplot2 <- ggplot(directDiff, aes (x = type_specific, y = diff, color = condition)) +
  geom_boxplot() +
  ggtitle("Direct Evaluative Ratings\n") + 
  scale_color_brewer(palette = "Set2") +
  scale_x_discrete(name = "\n Valence") +
  scale_y_continuous (name = "Evaluative Ratings\n") + 
  theme_classic() +
  labs(color = "Condition") +
  theme(plot.title = element_text (hjust = 0.5, face = "bold", size = 14),
        text = element_text(size=14))
boxplot2

## use three-way interaction for power analysis

#analysis 2: with valence as a factor

## specify models
lmer1 <- lmer(response ~ val_effect*condition_effect*type_effect 
              + (val_effect*type_effect|subject), 
              direct, REML = FALSE)
summary(lmer1)
model.matrix(response ~ val_effect*condition_effect*type_effect, direct)
#significant three-way interaction

aggregate(response ~ val*condition*type_specific, direct, mean)
aggregate(response ~ val*condition*type_specific, direct, sd)


#model2
lmer1_1 <- lmer(response ~ val_effect*type_effect + condition_effect:val:type_specific
                + (val_effect*type_effect|subject),
                direct, REML = FALSE)
summary(lmer1_1)

#plot model1

boxplot1 <- ggplot(direct, aes (x = val, y = response, color = condition)) +
  facet_grid(. ~ type_specific) +
  geom_boxplot() +
  geom_line(show.legend = TRUE, aes (x = val, color = condition)) +
  ggtitle("Direct Evaluative Ratings\n") + 
  geom_smooth(method = "lm", color = "black", se = FALSE) +
  scale_color_brewer(palette = "Set2") +
  scale_x_discrete(name = "\n Valence") +
  scale_y_continuous (name = "Evaluative Ratings\n") + 
  theme_classic() +
  labs(color = "Condition") +
  theme(plot.title = element_text (hjust = 0.5, face = "bold", size = 14),
        text = element_text(size=14))
boxplot1

# AMP ---------------------------------------------------------------------

indirect <- read.csv2("indirect.csv", header = TRUE)

#delete columns we don't need
indirect$X <- NULL
indirect$type <- NULL
str(indirect)

as_factor <- c("subject", "val", "condition", "measure", "type_specific", "category", "cs_selected", "target")

for (factor in as_factor){
  indirect[, factor] <- as.factor(indirect[,factor])
}

indirect$type_specific <- factor(indirect$type_specific, levels = c("CS", "GS same", "GS different", "Feature", "Group"))


## randomly select 1 CS for many_one:
new_one <- indirect[indirect$condition == "one_one",]
new_many <- indirect[indirect$condition == "many_one",]

#remove the original CS evaluations
new_many <- new_many[!new_many$type_specific == "CS",]
new_indirect <- rbind(new_one, new_many)

for (subject in unique(indirect$subject)){
  if (indirect$condition[indirect$subject == subject] == "many_one"){
    for (cat in 1:4){
      temp <- indirect[indirect$subject == subject & indirect$type_specific == "CS" & indirect$category == cat,]
      select <- temp[1,]
      #concat only 1 CS rating per participant
      new_indirect <- rbind(new_indirect, select)
    }
  }
}
head(new_indirect)

temp <- new_indirect

# order data
temp <- temp[order(temp$subject, temp$val, temp$type_specific),]

# omit information we don't need
temp <- temp[!temp$type_specific =="GS different", ]
temp <- temp[!temp$type_specific == "Feature",]

## rename many_one to many and one_one to one
temp$condition <- factor(temp$condition, labels = c("one", "many"), levels = c("one_one", "many_one"))

##categorical variable: generalization as discrete
temp$type_specific <- factor(temp$type_specific, labels = c("CS", "GS"), levels = c("CS", "GS same"))
head(temp)

## effect code variables
#condition
temp$condition_effect <- 0
for (line in 1:dim(temp)[1]){
  if (temp$condition[line] == "one"){
    temp$condition_effect[line] <- -0.5
  } else {
    temp$condition_effect[line] <- 0.5
  }
}
temp$condition_effect

#valence
temp$val_effect <- 0
for (line in 1:dim(temp)[1]){
  if (temp$val[line] == "neg"){
    temp$val_effect[line] <- -0.5
  } else {
    temp$val_effect[line] <- 0.5
  }
}
temp$condition_effect
str(temp)

#type
temp$type_effect <- 0
for (line in 1:dim(temp)[1]){
  if (temp$type_specific[line] == "CS"){
    temp$type_effect[line] <- -0.5
  } else {
    temp$type_effect[line] <- 0.5
  }
}
temp$type_specific
str(temp)

#set default to dummy coding
options(contrasts = c("contr.treatment", "contr.poly"))

### specify models
indirect <- temp
indirect$response <- as.numeric(indirect$response)
indirect <- indirect[order(indirect$subject, indirect$val, indirect$type_specific, indirect$category),]

indirectProp <- aggregate(response ~ subject + val_effect + val + type_effect + type_specific
                          + condition + condition_effect, indirect, sum)
indirectProp$length <- aggregate(response ~ subject + val_effect + val + type_effect + type_specific
                                 + condition + condition_effect, indirect, length)[[8]]
indirectProp$prop <- indirectProp$response/indirectProp$length
hist(indirectProp$prop)
head(indirectProp)

##### analysis 1: with difference scores
indirectNeg <- indirectProp[indirectProp$val == "neg",]
indirectPos <- indirectProp$prop[indirectProp$val == "pos"]

indirectDiff <- cbind(indirectNeg, indirectPos)
names(indirectDiff)[names(indirectDiff) == "prop"] <- "propNeg"
names(indirectDiff)[names(indirectDiff) == "indirectPos"] <- "propPos"

indirectDiff$diff <- indirectDiff$propPos - indirectDiff$propNeg
indirectDiff[indirectDiff$subject == "02a80kdxm7",]

aggregate(diff ~ condition*type_specific, indirectDiff, mean)
aggregate(diff ~ condition*type_specific, indirectDiff, se)

#model specification
lmer1Diff <- lmer(diff ~ condition_effect*type_effect + (1|subject), indirectDiff, REML = FALSE)

#lmer2Diff <- lmer(diff ~ condition_effect*type_effect + (type_effect|subject), indirectDiff, REML=FALSE)
#model 2 not specifiable due to missing observations

#anova(lmer1Diff, lmer2Diff)

plot(lmer1Diff)
summary(lmer1Diff)

#simple slopes
lmer1Diff_1 <- lmer(diff ~ type_effect + type_specific:condition_effect + (1|subject), indirectDiff, REML = FALSE)
summary(lmer1Diff_1)

#plot1
indirect_plot1 <- aggregate(diff ~ condition*type_specific, indirectDiff, mean)
indirect_plot1$se <- aggregate(diff ~ condition*type_specific, indirectDiff, se) [[3]]
indirect_plot1

barplot1 <- ggplot(indirect_plot1, aes (y = diff, x = type_specific, fill = condition)) +
  geom_col(position = position_dodge()) +
  geom_errorbar(aes(ymin= diff - se, ymax= diff + se), width=.2,
                position=position_dodge(.9)) +
  ggtitle("AMP\n") + 
  scale_color_brewer(palette = "Set2") +
  scale_x_discrete(name = "\n Type") +
  scale_y_continuous (name = "Difference Scores\n") + 
  theme_classic() +
  labs(fill = "Condition") +
  theme(plot.title = element_text (hjust = 0.5, face = "bold", size = 14),
        text = element_text(size=14))
barplot1

####analysis 2: with valence as additional variable

#random intercept for participants
glmer1 <- glmer(response ~ val_effect*condition_effect*type_effect 
                + (1|subject), 
                indirect, binomial)
summary(glmer1)

#random intercept for targets
glmer2 <- glmer(response ~ val_effect*condition_effect*type_effect 
                + (1|subject) + (1|target), 
                indirect, binomial)
summary(glmer2)

#random slopes for participants (fails to converge)
'glmer3 <- glmer(response ~ val_effect*condition_effect*type_effect 
                + (val_effect*type_effect|subject) + (1|target), 
                indirect, binomial)'
summary(glmer3)

#random slopes for targets (converges, but does not lead to better model fit)
'glmer4 <- glmer(response ~ val_effect*condition_effect*type_effect 
              + (val_effect*type_effect|subject) + (val_effect*type_effect|target), 
              indirect, binomial)'
anova(glmer1, glmer2, glmer3, glmer4)
#model choice: glmer2

#analyze simple slopes (even though three-way-interaction is not significant)
glmer2_2 <- glmer(response ~ val_effect*type_effect + val:type_specific:condition_effect
                 + (1|subject) + (1|target), indirect, binomial)
summary(glmer2_2)
#significant three-way interaction

#plot model1
indirect_plot2 <- aggregate(prop ~ condition + val + type_specific, indirectProp, mean)
indirect_plot2$se <- aggregate(prop ~ condition + val + type_specific, indirectProp, se)[[4]]
indirect_plot2

barplot2 <- ggplot(indirect_plot2, aes (y = prop, x = type_specific, fill = val)) +
  facet_grid(. ~ condition) +
  geom_col(position = position_dodge()) +
  geom_errorbar(aes(ymin= prop - se, ymax= prop + se), width=.2,
                position=position_dodge(.9)) +
  geom_hline(yintercept = 0.5, col = "black") +
  ggtitle("Direct Evaluative Ratings\n") + 
  scale_color_brewer(palette = "Set2") +
  scale_x_discrete(name = "\n Valence") +
  scale_y_continuous (name = "Proportion pleasant\n") + 
  theme_classic() +
  labs(color = "Condition") +
  theme(plot.title = element_text (hjust = 0.5, face = "bold", size = 14),
        text = element_text(size=14))
barplot2




# recognition memory ------------------------------------------------------
setwd(DRM)

memory <- read.csv2("memory1.csv", header = TRUE)
str(memory)

#delete columns we don't need
memory$X <- NULL
memory$memoryCorrect <- NULL
memory$trial_index <- NULL
memory$task <- NULL
memory$rt <- NULL

#rename condition
names(memory)[names(memory) == "condition1"] <- "condition"

#delete trials with timeout
memory <- memory[!memory$timeout == "true",]
str(memory)
#as factor
as_factor <- c("subject", "condition", "type", "category", "cs_selected")

for (factor in as_factor){
  memory[, factor] <- as.factor(memory[,factor])
}

memory$type <- factor(memory$type, levels = c("CS", "GS same", "GS different", "Feature", "Group"))


## randomly select 1 CS for many_one:
new_one <- memory[memory$condition == "one_one",]
new_many <- memory[memory$condition == "many_one",]

#remove the original CS evaluations
new_many <- new_many[!new_many$type_specific == "CS",]
new_memory <- rbind(new_one, new_many)

for (subject in unique(memory$subject)){
  if (memory$condition[memory$subject == subject] == "many_one"){
    for (cat in 1:4){
      temp <- memory[memory$subject == subject & memory$type_specific == "CS" & memory$category == cat,]
      select <- temp[1,]
      #concat only 1 CS rating per participant
      new_memory <- rbind(new_memory, select)
    }
  }
}
head(new_memory)

temp <- new_memory

# order data
temp <- temp[order(temp$subject, temp$val, temp$type_specific),]

# omit information we don't need
temp <- temp[!temp$type_specific =="GS different", ]
temp <- temp[!temp$type_specific == "Feature",]

## rename many_one to many and one_one to one
temp$condition <- factor(temp$condition, labels = c("one", "many"), levels = c("one_one", "many_one"))

##categorical variable: generalization as discrete
temp$type_specific <- factor(temp$type_specific, labels = c("CS", "GS"), levels = c("CS", "GS same"))
head(temp)

## effect code variables
#condition
temp$condition_effect <- 0
for (line in 1:dim(temp)[1]){
  if (temp$condition[line] == "one"){
    temp$condition_effect[line] <- -0.5
  } else {
    temp$condition_effect[line] <- 0.5
  }
}
temp$condition_effect

#valence
temp$val_effect <- 0
for (line in 1:dim(temp)[1]){
  if (temp$val[line] == "neg"){
    temp$val_effect[line] <- -0.5
  } else {
    temp$val_effect[line] <- 0.5
  }
}
temp$condition_effect
str(temp)

#type
temp$type_effect <- 0
for (line in 1:dim(temp)[1]){
  if (temp$type_specific[line] == "CS"){
    temp$type_effect[line] <- -0.5
  } else {
    temp$type_effect[line] <- 0.5
  }
}
temp$type_specific
str(temp)

#set default to dummy coding
options(contrasts = c("contr.treatment", "contr.poly"))

### specify models
memory <- temp
memory$response <- as.numeric(memory$response)
memory <- memory[order(memory$subject, memory$val, memory$type_specific, memory$category),]

memoryProp <- aggregate(response ~ subject + val_effect + val + type_effect + type_specific
                        + condition + condition_effect, memory, sum)
memoryProp$length <- aggregate(response ~ subject + val_effect + val + type_effect + type_specific
                               + condition + condition_effect, memory, length)[[8]]
memoryProp$prop <- memoryProp$response/memoryProp$length
hist(memoryProp$prop)
head(memoryProp)

##### analysis 1: with difference scores
memoryNeg <- memoryProp[memoryProp$val == "neg",]
memoryPos <- memoryProp$prop[memoryProp$val == "pos"]

memoryDiff <- cbind(memoryNeg, memoryPos)
names(memoryDiff)[names(memoryDiff) == "prop"] <- "propNeg"
names(memoryDiff)[names(memoryDiff) == "memoryPos"] <- "propPos"

memoryDiff$diff <- memoryDiff$propPos - memoryDiff$propNeg
memoryDiff[memoryDiff$subject == "02a80kdxm7",]

aggregate(diff ~ condition*type_specific, memoryDiff, mean)
aggregate(diff ~ condition*type_specific, memoryDiff, se)

#model specification
lmer1Diff <- lmer(diff ~ condition_effect*type_effect + (1|subject), memoryDiff, REML = FALSE)

#lmer2Diff <- lmer(diff ~ condition_effect*type_effect + (type_effect|subject), memoryDiff, REML=FALSE)
#model 2 not specifiable due to missing observations

#anova(lmer1Diff, lmer2Diff)

plot(lmer1Diff)
summary(lmer1Diff)

#simple slopes
lmer1Diff_1 <- lmer(diff ~ type_effect + type_specific:condition_effect + (1|subject), memoryDiff, REML = FALSE)
summary(lmer1Diff_1)

#plot1
memory_plot1 <- aggregate(diff ~ condition*type_specific, memoryDiff, mean)
memory_plot1$se <- aggregate(diff ~ condition*type_specific, memoryDiff, se) [[3]]
memory_plot1

barplot1 <- ggplot(memory_plot1, aes (y = diff, x = type_specific, fill = condition)) +
  geom_col(position = position_dodge()) +
  geom_errorbar(aes(ymin= diff - se, ymax= diff + se), width=.2,
                position=position_dodge(.9)) +
  ggtitle("AMP\n") + 
  scale_color_brewer(palette = "Set2") +
  scale_x_discrete(name = "\n Type") +
  scale_y_continuous (name = "Difference Scores\n") + 
  theme_classic() +
  labs(fill = "Condition") +
  theme(plot.title = element_text (hjust = 0.5, face = "bold", size = 14),
        text = element_text(size=14))
barplot1

####analysis 2: with valence as additional variable

#random intercept for participants
glmer1 <- glmer(response ~ val_effect*condition_effect*type_effect 
                + (1|subject), 
                memory, binomial)
summary(glmer1)

#random intercept for targets
glmer2 <- glmer(response ~ val_effect*condition_effect*type_effect 
                + (1|subject) + (1|target), 
                memory, binomial)
summary(glmer2)

#random slopes for participants (fails to converge)
'glmer3 <- glmer(response ~ val_effect*condition_effect*type_effect 
                + (val_effect*type_effect|subject) + (1|target), 
                memory, binomial)'
summary(glmer3)

#random slopes for targets (converges, but does not lead to better model fit)
'glmer4 <- glmer(response ~ val_effect*condition_effect*type_effect 
              + (val_effect*type_effect|subject) + (val_effect*type_effect|target), 
              memory, binomial)'
anova(glmer1, glmer2, glmer3, glmer4)
#model choice: glmer2

#analyze simple slopes (even though three-way-interaction is not significant)
glmer2_2 <- glmer(response ~ val_effect*type_effect + val:type_specific:condition_effect
                  + (1|subject) + (1|target), memory, binomial)
summary(glmer2_2)
#significant three-way interaction

#plot model1
memory_plot2 <- aggregate(prop ~ condition + val + type_specific, memoryProp, mean)
memory_plot2$se <- aggregate(prop ~ condition + val + type_specific, memoryProp, se)[[4]]
memory_plot2

barplot2 <- ggplot(memory_plot2, aes (y = prop, x = type_specific, fill = val)) +
  facet_grid(. ~ condition) +
  geom_col(position = position_dodge()) +
  geom_errorbar(aes(ymin= prop - se, ymax= prop + se), width=.2,
                position=position_dodge(.9)) +
  geom_hline(yintercept = 0.5, col = "black") +
  ggtitle("Direct Evaluative Ratings\n") + 
  scale_color_brewer(palette = "Set2") +
  scale_x_discrete(name = "\n Valence") +
  scale_y_continuous (name = "Proportion pleasant\n") + 
  theme_classic() +
  labs(color = "Condition") +
  theme(plot.title = element_text (hjust = 0.5, face = "bold", size = 14),
        text = element_text(size=14))
barplot2








##aggregate scores for each category :
temp2 <- aggregate(response ~ subject + category + type_specific + condition + val, temp, mean)

#without category
temp2 <- aggregate(response ~ subject + type_specific + condition + val, temp, mean)


#without subjects
#temp2 <- aggregate(response ~ type_specific + condition + val, temp, mean)

#wide format: GS and CS as different columns
multiLevel <- temp2[temp2$type_specific == "CS",]
respGS <- temp2[temp2$type_specific == "GS",]
multiLevel$GS <- respGS$response
multiLevel$CS <- multiLevel$response

#delete variables we don't need
multiLevel$response <- NULL
multiLevel$type_specific <- NULL
multiLevel$type_discrete <- NULL
head(multiLevel)

#condition_code: 0 =  many

dotplot <- ggplot(multiLevel, aes (x = CS, y = GS)) +
  facet_grid(. ~ condition) +
  geom_point(show.legend = TRUE, aes (color = val)) +
  ggtitle("Direct Evaluative Ratings\n") + 
  geom_smooth(method = "lm", color = "black", se = FALSE) +
  geom_smooth(method = 'lm', aes(color = val, linetype = val), se = FALSE) +
  scale_color_brewer(palette = "Set2") +
  scale_x_continuous(name = "\nCS Ratings") +
  scale_y_continuous (name = "GS Ratings\n") + 
  theme_classic() +
  labs(color = "Valence", linetype = "Valence") +
  theme(plot.title = element_text (hjust = 0.5, face = "bold", size = 14),
        text = element_text(size=14))
dotplot


barplotCS <- ggplot(multiLevel, aes (x = condition, y = CS, color = val)) +
  geom_boxplot() +
  facet_grid(.~ category) +
  ggtitle("Direct Evaluative Ratings\n") + 
  scale_color_brewer(palette = "Set2") +
  scale_x_discrete(name = "\nCS Ratings") +
  scale_y_continuous (name = "GS Ratings\n") + 
  theme_classic() +
  labs(color = "Condition") +
  theme(plot.title = element_text (hjust = 0.5, face = "bold", size = 14),
        text = element_text(size=14))
barplotCS

### Effektkodierung -0.5, 0.5

#condition
multiLevel$condition_effect <- 0
for (line in 1:dim(multiLevel)[1]){
  if (multiLevel$condition[line] == "one"){
    multiLevel$condition_effect[line] <- -0.5
  } else {
    multiLevel$condition_effect[line] <- 0.5
  }
}
multiLevel$condition_effect

#valence
multiLevel$val_effect <- 0
for (line in 1:dim(multiLevel)[1]){
  if (multiLevel$val[line] == "neg"){
    multiLevel$val_effect[line] <- -0.5
  } else {
    multiLevel$val_effect[line] <- 0.5
  }
}
multiLevel$condition_effect
str(multiLevel)

#calculate multiple regression
model1 <- lm(GS ~ CS, data = multiLevel)
summary(model1)

model2 <- lm(GS ~ CS*val_effect, data = multiLevel)
summary(model2)
anova(model2)
model.matrix(GS ~ CS*val_effect, data = multiLevel)
#plot(model2)

model3 <- lm(GS ~ CS*condition_effect*val_effect, data = multiLevel)
summary(model3)
anova(model3)
model.matrix(GS ~ CS*condition_effect*val_effect, data = multiLevel)
#plot(model2)

anova(model1, model2, model3)

#analyze specific effects by dummy coding (setting to 0)

#test CS*condition interaction for both positive and negative valence
multiLevel$val_rev <- factor(multiLevel$val, levels = c("pos", "neg"))

#0 = negativ, 1 = postiv
model3_1 <- lm(GS ~ CS*condition_effect*val, data = multiLevel)
summary(model3_1)
model.matrix(GS ~ CS*condition_effect*val, data = multiLevel)

#0 = positiv, 0 = negativ 
model3_2 <- lm(GS ~ CS*condition_effect, data = multiLevel[multiLevel$val == "neg",])
summary(model3_2)
model.matrix(GS ~ CS*condition*val, data = multiLevel)

#calculate multilevel Model
#z-standardize GS and CS
multiLevel$GS_z <- scale (multiLevel$GS, center = TRUE, scale = TRUE)
multiLevel$CS_z <- scale (multiLevel$CS, center = TRUE, scale = TRUE)

lmer1 <- lmer(GS_z ~ CS_z + (1|category), multiLevel)
summary(lmer1)

lmer2 <- lmer(GS_z ~ CS_z + (CS_z | category), multiLevel)
summary(lmer2)

lmer3 <- lmer(GS_z ~ CS_z*condition_effect + (CS_z |category), multiLevel)
summary(lmer3)

lmer4 <- lmer(GS_z ~ CS_z*condition_effect*val_effect + (CS_z|val), multiLevel )
summary(lmer4)
anova(lmer4)
anova(lmer1, lmer2, lmer3, lmer4)

lmer4_1 <- lmer(GS_z ~ CS_z*condition_effect*val + (val|subject), multiLevel )
summary(lmer4_1)

lmer4_1 <- lmer(GS_z ~ CS_z*condition_effect*val_rev + (val_rev + CS_z|subject), multiLevel )
summary(lmer4_1)

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




# recognition memory: type RTs --------------------------------------------

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
memory1 <- subset(memory1, select = -c(X, trial_index, task, memory, memoryResp, timeout, memoryCorrect, cs_selected, nr_pres))

#exclude levels of "type" we don't want to look at
memory1 <- memory1[!memory1$type == "CSnonpred",]
memory1 <- memory1[!memory1$type == "CSpred",]
memory1 <- memory1[!memory1$type == "GSnew",]
memory1 <- memory1[!memory1$type == "distractor",]

#rename factors and levels: condition
memory1$condition1 <- factor(memory1$condition1, labels = c("one", "many", "fill"), levels = c("one_one", "many_one", "many_fill"))

#rename factors and levels: condition
memory1$type <- factor(memory1$type, labels = c("CS", "GS"), levels = c("CS", "GSold"))

#subject to factor for rANOVA
memory1$subject <- as.factor(memory1$subject)
memory1$rt <- as.numeric(memory1$rt)
head(memory1)

plot(memory1$rt)

#rt on logarithmic scale
plot(log(memory1$rt))
memory1$rt_log <- log(memory1$rt)

barplotData <- aggregate(rt_log ~ condition1 + type, memory1, median)
barplotData$se <- aggregate(rt_log ~ condition1 + type, memory1, se)[[3]]
barplotData$mean <- aggregate(rt_log ~ condition1 + type, memory1, mean)[[3]]
barplotData

##Plot generalization stimuli
plotmemory1PropOld <- ggplot(barplotData, aes (x = type, y = mean, color = condition1)) +
  #geom_bar(stat = "identity", position = position_dodge()) +
  #geom_point() + 
  geom_errorbar(aes(ymin= rt_log - se, ymax= rt_log + se), width=.2,
                position=position_dodge(.9)) +
  ggtitle("Recognition Memory\n") + 
  scale_fill_brewer(palette = "Set2") +
  theme(plot.title = element_text (hjust = 0.5, face = "bold", size = 12)) +
  labs(fill = "Condition") +
  #scale_y_continuous (name = "Proportion of 'old' responses\n", limits = c(0.0, 7.0)) +
  scale_x_discrete(name = "\nType") +
  theme_classic()+
  theme (plot.title = element_text (hjust = 0.5, face = "bold", size = 16),
         text = element_text(size=14))
plotmemory1PropOld

plotBoxplot <- ggplot(memory1, aes (x = type, y = rt, color = condition1)) +
  #geom_bar(stat = "identity", position = position_dodge()) +
  geom_boxplot() +  
  #geom_errorbar(aes(ymin= rt_log - se, ymax= rt_log + se), width=.2,
  #position=position_dodge(.9)) +
  ggtitle("Recognition Memory\n") + 
  scale_color_brewer(palette = "Set2") +
  theme(plot.title = element_text (hjust = 0.5, face = "bold", size = 12)) +
  labs(color = "Condition") +
  #scale_y_continuous (name = "Proportion of 'old' responses\n", limits = c(0.0, 7.0)) +
  scale_x_discrete(name = "\nType") +
  theme_classic()+
  theme (plot.title = element_text (hjust = 0.5, face = "bold", size = 16),
         text = element_text(size=14))
plotBoxplot


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




# recognition memory: components RT ---------------------------------------


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
memory1 <- subset(memory1, select = -c(X, trial_index, task, memory, memoryResp, timeout, memoryCorrect, cs_selected, nr_pres))

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
memory1$rt <- as.numeric(memory1$rt)
head(memory1)

plot(memory1$rt)

#rt on logarithmic scale
plot(log(memory1$rt))
memory1$rt_log <- log(memory1$rt)

barplotData <- aggregate(rt_log ~ condition1 + type, memory1, median)
barplotData$se <- aggregate(rt_log ~ condition1 + type, memory1, se)[[3]]
barplotData$mean <- aggregate(rt_log ~ condition1 + type, memory1, mean)[[3]]
barplotData

##Plot generalization stimuli
plotmemory1PropOld <- ggplot(barplotData, aes (x = type, y = mean, color = condition1)) +
  #geom_bar(stat = "identity", position = position_dodge()) +
  #geom_point() + 
  geom_errorbar(aes(ymin= rt_log - se, ymax= rt_log + se), width=.2,
                position=position_dodge(.9)) +
  ggtitle("Recognition Memory\n") + 
  scale_fill_brewer(palette = "Set2") +
  theme(plot.title = element_text (hjust = 0.5, face = "bold", size = 12)) +
  labs(fill = "Condition") +
  #scale_y_continuous (name = "Proportion of 'old' responses\n", limits = c(0.0, 7.0)) +
  scale_x_discrete(name = "\nType") +
  theme_classic()+
  theme (plot.title = element_text (hjust = 0.5, face = "bold", size = 16),
         text = element_text(size=14))
plotmemory1PropOld

plotBoxplot <- ggplot(memory1, aes (x = type, y = rt, color = condition1)) +
  #geom_bar(stat = "identity", position = position_dodge()) +
  geom_boxplot() +  
  #geom_errorbar(aes(ymin= rt_log - se, ymax= rt_log + se), width=.2,
                #position=position_dodge(.9)) +
  ggtitle("Recognition Memory\n") + 
  scale_color_brewer(palette = "Set2") +
  theme(plot.title = element_text (hjust = 0.5, face = "bold", size = 12)) +
  labs(color = "Condition") +
  #scale_y_continuous (name = "Proportion of 'old' responses\n", limits = c(0.0, 7.0)) +
  scale_x_discrete(name = "\nType") +
  theme_classic()+
  theme (plot.title = element_text (hjust = 0.5, face = "bold", size = 16),
         text = element_text(size=14))
plotBoxplot
