################### Pre-Test DRM 

### february, 2021
### Kathrin Reichmann


setwd("C:/Users/reich/Documents/GitHub/CScond_PreTest/data")
#setwd("\\\\sn00.zdv.uni-tuebingen.de/siskr01/Documents/Github/CScond_Exp2/data")


library(dplyr)
library(tidyverse)
library(ggplot2)
library(lme4)
library(nlme)

##### import: 
#single data files

##### export:
#direct.csv
#memory1.csv
#memory2.csv

#Functions
CI <- function(x) qnorm(0.975)*sd(x)/sqrt(length(x))
se <- function(x) sd(x)/sqrt(length(x))

# read files --------------------------------------------------------------


filenames <- dir()
dat <- data.frame()
for (i in filenames){
  dat <- rbind(dat, read.table(i, header = TRUE, sep = ",", encoding = "UTF-8"))
}

rm (filenames, i)
length(unique(dat$subject))

comments_one <- dat$responses[dat$trial_type == "survey-text" & dat$condition2 == "one_one"]
#comments_one

comments_many <- dat$responses[dat$trial_type == "survey-text" & dat$condition2 == "many_one"]
#comments_many

# exclude participants --------------------------------------------------------

#total number of subjects before data exclusion
length(unique(dat$subject))

#chinese speaking participants
#0 = ja, 2 = keine Angabe
get_tables <- aggregate(response ~ subject + chinese, dat, mean)
table(get_tables$chinese)
dat <- dat[!dat$chinese == 0,]

#participants who did not pay attention
get_tables <- aggregate(response ~ subject + hinschauen, dat, mean)
table(get_tables$hinschauen) #1 = nicht hinschauen
dat <- dat[!dat$hinschauen == 1,]

#who were distracted
get_tables <- aggregate(response ~ subject + ablenkung, dat, mean)
table(get_tables$ablenkung) # 0 = abgelenkt
#dat <- dat[!dat$ablenkung == 0,]

#who did not follow instructions
get_tables <- aggregate(response ~ subject + gewissenhaft, dat, mean)
table(get_tables$gewissenhaft) # 1 = nicht gewissenhaft
dat <- dat[!dat$gewissenhaft == 1,]

#total number of subjects after data exclusion
length(unique(dat$subject))
#Auschluss von 32 VP


# demographic data --------------------------------------------------------

demographics <- dat[c('subject', 'age', 'gender', 'education')]
demographics <- aggregate(. ~ subject, demographics, median);

table(demographics$gender)
table(demographics$education)

age$mean <- as.numeric(demographics$age) %>%
  mean()

age$sd <- as.numeric(demographics$age) %>%
  sd()
age

# prep data for three different tasks -----------------------------------

#delete irrelevant columns
olddat <- dat
dat <- dat[c("trial_index", "task", "subject", "condition1", "condition2", "type", "category", "response", "rt", "timeout", "memory", "memoryResp", "nr_pres","memoryCorrect", "cs_selected")]

#variable cs_selected
as.factor(gsub(".png$", "", dat$cs_selected))-> dat$cs_selected
as.factor(gsub(".jpg$", "", dat$cs_selected))-> dat$cs_selected
as.factor(gsub("category", "", dat$cs_selected))-> dat$cs_selected

#transform to factor
as_factor <- c("subject", "condition1", "condition2", "type", "category", "timeout", "nr_pres","cs_selected")

for (factor in as_factor){
  dat[, factor] <- as.factor(dat[,factor])
}
str(dat)

#separate data for three tasks
direct <- dat[dat$task == "direct",]
direct <- direct[c("trial_index", "task", "subject", "type", "category", "response", "rt", "cs_selected")]

memory1 <- dat[dat$task == "memory1",]
memory1 <- memory1[c("trial_index", "task", "subject", "condition1", "type", "category", "rt", "timeout", "memory", "memoryResp", "nr_pres","memoryCorrect", "cs_selected")]

memory2 <- dat[dat$task == "memory2",]
memory2 <- memory2[c("trial_index", "task", "subject", "condition2", "type", "category", "rt", "timeout", "memory", "memoryResp", "nr_pres","memoryCorrect", "cs_selected")]

#export data files
setwd("C:/Users/reich/Documents/GitHub/CSCond_analysis/Study2_DRM/data_preprocessed")

write.csv2(direct, file = 'direct.csv')
write.csv2(memory1, file = 'memory1.csv')
write.csv2(memory2, file = 'memory2.csv')

# learning phases ---------------------------------------------------------

#presentation order and stimulus assignment
learning1 <- dat[dat$task == "learning1",]
learning1 <- learning1[c("subject", "trial_index", "cs_selected")]
learning1

learning2 <- dat[dat$task == "learning2",]
learning2 <- learning2[c("subject", "trial_index", "cs_selected")]
learning2$trial_index <- learning2$trial_index - 75


# "evaluation" filler task -----------------------------------------------------------

direct$response <- as.numeric(direct$response)
plotDirect <- aggregate(response ~ cs_selected + category, direct, mean)
aggregate(response ~ cs_selected + category, direct, sd)
plotDirect$se <- aggregate(response ~ cs_selected + category, direct, se)[[3]]


plotDirect <- ggplot(plotDirect, aes (x = cs_selected, y = response, fill = category)) +
  geom_bar(stat = 'identity', position = position_dodge(), show.legend = TRUE) +
  geom_errorbar(aes(ymin= response - se, ymax= response + se), width=.2,
                position=position_dodge(.9)) +
  ggtitle("Stimulus rating") + 
  scale_fill_brewer(palette = "Paired") +
  theme_classic() +
  theme(plot.title = element_text (hjust = 0.5, face = "bold", size = 12)) +
  labs(fill = "Categories") +
  scale_y_continuous (name = "rating")
plotDirect


# memory1 task ------------------------------------------------------------
#exclude timeouts
table(memory1$timeout)
memory1 <- memory1[memory1$timeout == "false",]
table(memory1$timeout)

#convert variables
memory1$type <- factor(memory1$type)
memory1$memoryResp <- as.numeric(memory1$memoryResp)

#check condition assignment
table(memory1$condition1)

#total numbers (total number of observations: 680)
total1 <- aggregate(memoryResp ~ condition1 + type, memory1, sum)
total1$percent <- total1$memoryResp/680

table(memory1$memoryResp[memory1$type == "CS"])
table(memory1$memoryResp[memory1$type == "CS"])

### (1) calculate proportions of "old" responses
# correct recognition of studied items (CSs) -> true recognition
# incorrect response "old" to critical lures (GSs) -> false recognition
#0 new
#1 old

memory1Prop <- aggregate(memoryResp ~ subject + condition1 + type, memory1, sum)
memory1Prop$nr <- aggregate(memoryResp ~ subject + condition1 + type, memory1, length)[[4]]
memory1Prop$prop <- (memory1Prop$memoryResp/memory1Prop$nr)

# (1) plot proportion of "old" responses - barplot

barplotData <- aggregate(prop ~ condition1 + type, memory1Prop, mean)
barplotData$se <- aggregate(prop ~ condition1 + type, memory1Prop, se)[[3]]
barplotData$sd <- aggregate(prop ~ condition1 + type, memory1Prop, sd)[[3]]
barplotData

##Plot generalization stimuli
barplotData1 <- barplotData[!barplotData$type == "CSpred",]
barplotData1 <- barplotData1[!barplotData1$type == "CSnonpred",]

plotmemory1PropOld <- ggplot(barplotData1, aes (x = type, y = prop, fill = condition1)) +
  geom_bar (stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin= prop - se, ymax= prop + se), width=.2,
                position=position_dodge(.9)) +
  ggtitle("DRM - Chinese Characters") + 
  scale_fill_brewer(palette = "Dark2") +
  theme(plot.title = element_text (hjust = 0.5, face = "bold", size = 12)) +
  labs(fill = "Condition") +
  scale_y_continuous (name = "Proportion of 'old' responses\n") +
  theme_classic() 
plotmemory1PropOld

#Violin plot: generalization stimuli
memory2Prop <- memory1Prop[!memory1Prop$type == "CSpred",]
memory2Prop <- memory2Prop[!memory2Prop$type == "CSnonpred",]

violin1PropOld <- ggplot(memory2Prop, aes (x = type, y = prop, col = condition1)) +
  geom_violin (draw_quantiles = c(0.25, 0.5, 0.75)) +
  ggtitle("DRM - Chinese Characters") + 
  scale_color_brewer(palette = "Dark2") +
  theme(plot.title = element_text (hjust = 0.5, face = "bold", size = 12)) +
  labs(fill = "Condition") +
  scale_y_continuous (name = "Proportion of 'old' responses\n") +
  theme_classic() 
violin1PropOld


##Plot modified CSs
barplotData2 <- barplotData[!barplotData$type == "GSnew",]
barplotData2 <- barplotData2[!barplotData2$type == "GSold",]
barplotData2 <- barplotData2[!barplotData2$type == "distractor",]


plotmemory1PropOld <- ggplot(barplotData2, aes (x = type, y = prop, fill = condition1)) +
  geom_bar (stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin= prop - se, ymax= prop + se), width=.2,
                position=position_dodge(.9)) +
  ggtitle("DRM - Chinese Characters") + 
  scale_fill_brewer(palette = "Dark2") +
  theme(plot.title = element_text (hjust = 0.5, face = "bold", size = 12)) +
  labs(fill = "Condition") +
  scale_y_continuous (name = "Proportion of 'old' responses\n") +
  theme_classic() 
plotmemory1PropOld


##Violin plot
memory3Prop <- memory1Prop[!memory1Prop$type == "GSold",]
memory3Prop <- memory3Prop[!memory3Prop$type == "GSnew",]

violin2PropOld <- ggplot(memory3Prop, aes (x = type, y = prop, col = condition1)) +
  geom_violin (draw_quantiles = c(0.25, 0.5, 0.75)) +
  ggtitle("DRM - Chinese Characters") + 
  scale_color_brewer(palette = "Dark2") +
  theme(plot.title = element_text (hjust = 0.5, face = "bold", size = 12)) +
  labs(fill = "Condition") +
  scale_y_continuous (name = "Proportion of 'old' responses\n") +
  theme_classic() 
violin2PropOld

# (2) plot proportion of "old" responses - barplot for every category

memory4Prop <- aggregate(memoryResp ~ subject + condition1 + type + category, memory1, sum)
memory4Prop$nr <- aggregate(memoryResp ~ subject + condition1 + type + category, memory1, length)[[5]]
memory4Prop$prop <- (memory4Prop$memoryResp/memory4Prop$nr)


barplotCat <- aggregate(prop ~ condition1 + type + category, memory4Prop, mean)
barplotCat$se <- aggregate(prop ~ condition1 + type + category, memory4Prop, se)[[4]]
barplotCat <- barplotCat[!barplotCat$type == "CSnonpred",]
barplotCat <- barplotCat[!barplotCat$type == "CSpred",]
head(barplotCat)

plotmemory1PropOld <- ggplot(barplotCat, aes (x = type, y = prop, fill = condition1)) +
  facet_grid(.~ category) +
  geom_bar (stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin= prop - se, ymax= prop + se), width=.2,
                position=position_dodge(.9)) +
  ggtitle("DRM - Chinese Characters") + 
  scale_fill_brewer(palette = "Dark2") +
  theme(plot.title = element_text (hjust = 0.5, face = "bold", size = 12)) +
  labs(fill = "Condition") +
  scale_y_continuous (name = "Proportion of 'old' responses\n") +
  theme_classic() 
plotmemory1PropOld


### (3) test for significance: type of stimulus * condition

# (3.1) condition x Type (CS vs. GS same vs. GS different vs. distractor)

#MANOVA
manovaData <- subset(memory1Prop, select = -c(memoryResp, nr))
manovaData <- manovaData[!manovaData$type == "CSpred",]
manovaData <- manovaData[!manovaData$type == "CSnonpred",]
manovaData$CS <- memory1Prop$prop[memory1Prop$type == "CS"]
manovaData$GSold <- memory1Prop$prop[memory1Prop$type == "GSold"]
manovaData$GSnew <- memory1Prop$prop[memory1Prop$type == "GSnew"]
manovaData$distractor <- memory1Prop$prop[memory1Prop$type == "distractor"]
manovaData <- manovaData[1:length(unique(manovaData$subject)), ]
manovaData$type <- NULL
manovaData$prop <- NULL
manovaData$subject <- NULL
head(manovaData)


#homogeneity of covariance matrices
by(manovaData[,2:5], manovaData$condition1, cov)

#multivariate outliers
library(mvoutlier)
aq.plot(manovaData[,2:5])

#put multiple outcomes in the model
outcome <- cbind(manovaData$CS, manovaData$GSold, manovaData$GSnew, manovaData$distractor)

#calculate the model
conditionModel <- manova(outcome ~ condition1, data = manovaData)
summary(conditionModel)
summary(conditionModel, test = "Wilks")
summary(conditionModel, test = "Hotelling")
summary(conditionModel, test = "Roy")

#Hypothesis: interaction type x CS Variability

#simple slopes
memory1Prop$condition1 <- factor(memory1Prop$condition1, levels = c("many_one", "many_fill", "one_one"))
  
#CS
lm1 <- lm(prop ~ condition1, memory1Prop[memory1Prop$type == "CS",])
summary(lm1)

lm2 <- lm(prop ~ condition1, memory1Prop[memory1Prop$type == "GSold",])
summary(lm2)

lm3 <- lm(prop ~ condition1, memory1Prop[memory1Prop$type == "GSnew",])
summary(lm3)

lm4 <- lm(prop ~ condition1, memory1Prop[memory1Prop$type == "distractor",])
summary(lm4)



head(memory1Prop)

#test for true recognition
trueRec1 <- memory1Prop[memory1Prop$type == "CS",]

#test for false recognition
falseRec1 <- memory1Prop[memory1Prop$type == "GS",]


##### singal detection analysis: adjust  false alarm rates of 0 and hit rates of 1 MacMillan & Creelman (1991) 1/2n correction -> see Gunter et al., 2007; Huff & Bodner, 2013
#1/2n if hit rate = 1, n is the number of targets (here: 4); if false alarm rate = 0, n is the number of GSs (here: 4)
memory1Prop$propAdj <- memory1Prop$prop
nItems <- 4

for(line in 1:dim(memory1Prop)[1]){
  if (memory1Prop$prop[line] == 0){
    memory1Prop$propAdj[line] <- 1/(2*nItems) 
  } else if (memory1Prop$prop[line] == 1){
    memory1Prop$propAdj[line] <- 1 - (1/(2*nItems)) 
  }
}

#logistic regression

#calculate log odds
memory1Prop$odds <- round(memory1Prop$propAdj/(1-memory1Prop$propAdj), 4)
memory1Prop$logodds <- round(log(memory1Prop$odds), 4)
memory1Prop$logodds <- round(log(memory1Prop$odds), 4)
#

glm1 <- glm(memoryResp ~ condition1, memory1[memory1$type == "CS",], family = 'binomial')
summary(glm1)
exp(0.1134) #odds
exp(0.1134)/(1+exp(0.1134)) #probabilities


#z-standardization of scores
memory1Prop$propZ <- scale(memory1Prop$propAdj, center = TRUE, scale = TRUE)

### (2) studied items d' (discriminability, index of the amount of memory information encoded for list items relative to list item controls)
#z[hit rate studied items = CS] - z[false alarm rate studied word controls = distractor] -> see Gunter et al., 2007; Huff & Bodner, 2013
#-1: always responding "old" to distractor and "new" to CS, see Colbert & McBride, 2007
# 1: always responding "new" to distractor and "old" to CS

#create variable
memory1studiedD <- memory1Prop[memory1Prop$type == "CS",]
names(memory1studiedD)[names(memory1studiedD) == "propZ"] <- "propZCS"
memory1studiedD$propZDis <- memory1Prop$propZ[memory1Prop$type == "distractor"]
memory1studiedD$d <- memory1studiedD$propZCS - memory1studiedD$propZDis
memory1studiedD$type <- NULL
memory1studiedD

#create plot
plotStudiedD <- ggplot(memory1studiedD, aes (x = condition1, y = d)) +
  geom_bar(stat = 'identity', position = position_dodge(), show.legend = TRUE) +
  #geom_errorbar(aes(ymin= response - se, ymax= response + se), width=.2,
  #              position=position_dodge(.9)) +
  ggtitle("DRM - Chinese Characters") + 
  scale_fill_brewer(palette = "Paired") +
  theme_classic() +
  theme(plot.title = element_text (hjust = 0.5, face = "bold", size = 12)) +
  scale_y_continuous (name = "sensitivty (d') of CSs")
plotStudiedD

#test differences between conditions
#


### (3) critical lure d' (discriminability, index of the amount of memory information encoded for critical lures when the corresponding list was studied relative to when it was not studied)
#z[hit rate critical lures = GS] - z[false alarm rate critical lure controls = distractor]-> see Gunter et al., 2007; Huff & Bodner, 2013
#-1: always responding "old" to distractor and "new" to GS, see Colbert & McBride, 2007
# 1: always responding "new" to distractor and "old" to GS

memory1unstudiedD <- memory1Prop[memory1Prop$type == "GSold",]
names(memory1unstudiedD)[names(memory1unstudiedD) == "propZ"] <- "propZCS"
memory1unstudiedD$propZDis <- memory1Prop$propZ[memory1Prop$type == "distractor"]
memory1unstudiedD$d <- memory1unstudiedD$propZCS - memory1unstudiedD$propZDis
memory1unstudiedD$type <- NULL
memory1unstudiedD

plotUnstudiedD <- ggplot(memory1unstudiedD, aes (x = condition1, y = d)) +
  geom_bar(stat = 'identity', position = position_dodge(), show.legend = TRUE) +
  #geom_errorbar(aes(ymin= response - se, ymax= response + se), width=.2,
  #              position=position_dodge(.9)) +
  ggtitle("DRM - Chinese Characters") + 
  scale_fill_brewer(palette = "Paired") +
  theme_classic() +
  theme(plot.title = element_text (hjust = 0.5, face = "bold", size = 12)) +
  scale_y_continuous (name = "sensitivty (d') of GSs")
plotUnstudiedD

### (4) lambda (bias/ "monitoring"; index of how stringent strategic monitoring standards were at test) 
#z[1 - false alarm rate distractor] -> see Gunter et al., 2007; Huff & Bodner, 2013
#higher lambda scores: index of more rigorous strategic monitoring standards applied to test (less often "old" response)
memory1Lambda <- memory1Prop[memory1Prop$type == "distractor",]
memory1Lambda$distr <- 1 - memory1Lambda$propAdj
memory1Lambda$lambda <- scale(memory1Lambda$distr, center = TRUE, scale = TRUE)
memory1Lambda


# memory2 task ------------------------------------------------------------
#exclude timeouts
table(memory2$timeout)
memory2 <- memory2[memory2$timeout == "false",]
table(memory2$timeout)

#convert variables
memory2$type <- factor(memory2$type)
memory2$memoryResp <- as.numeric(memory2$memoryResp)

#check condition assignment
table(memory2$condition2)

### (1) calculate proportions of "old" responses
# correct recognition of studied items (CSs) -> true recognition
# incorrect response "old" to critical lures (GSs) -> false recognition
#0 new
#1 old

memory2Prop <- aggregate(memoryResp ~ subject + condition2 + type, memory2, sum)
memory2Prop$nr <- aggregate(memoryResp ~ subject + condition2 + type, memory2, length)[[4]]
memory2Prop$prop <- (memory2Prop$memoryResp/memory2Prop$nr)
head(memory2Prop)

memory2Prop$se <- aggregate(prop ~ subject + condition2 + type, memory2Prop, se)[[4]]

#plot proportion of "old" responses - boxplot
aggregate(prop ~ condition2 + type, memory2Prop, mean)
aggregate(prop ~ condition2 + type, memory2Prop, se)

barplotData <- aggregate(prop ~ condition2 + type, memory2Prop, mean)
barplotData$se <- aggregate(prop ~ condition2 + type, memory2Prop, se)[[3]]

#plot proportion of "old" responses - barplot
plotmemory2PropOld <- ggplot(barplotData, aes (x = type, y = prop, fill = condition2)) +
  geom_bar (stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin= prop - se, ymax= prop + se), width=.2,
                position=position_dodge(.9)) +
  ggtitle("DRM - Food Items") + 
  scale_fill_brewer(palette = "Dark2") +
  theme(plot.title = element_text (hjust = 0.5, face = "bold", size = 12)) +
  labs(fill = "Condition") +
  scale_y_continuous (name = "Proportion of 'old' responses\n") +
  theme_classic() 
plotmemory2PropOld



