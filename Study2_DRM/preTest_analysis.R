################### Pre-Test DRM 

### february, 2021
### Kathrin Reichmann


setwd("C:/Users/reich/Documents/GitHub/CScond_PreTest/data")
#setwd("\\\\sn00.zdv.uni-tuebingen.de/siskr01/Documents/Github/CScond_Exp2/data")


library(dplyr)
library(tidyverse)
library(ggplot2)

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


#delete irrelevant columns
olddat <- dat
dat <- dat[c("trial_index", "task", "subject", "condition1", "condition2", "type", "category", "response", "rt", "timeout", "memory", "memoryResp", "nr_pres","memoryCorrect", "cs_selected")]

#variable cs_selected
as.factor(gsub(".png$", "", dat$cs_selected))-> dat$cs_selected
as.factor(gsub(".jpg$", "", dat$cs_selected))-> dat$cs_selected
as.factor(gsub("category", "", dat$cs_selected))-> dat$cs_selected

#transform to factor
as_factor <- c("subject", "condition", "type", "category", "timeout", "nr_pres","cs_selected")

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
setwd("C:/Users/reich/Documents/GitHub/CScond_PreTest/analysis")

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
dim(memory1)
memory1 <- memory1[memory1$timeout == "false",]
dim(memory1)
memory1$type <- factor(memory1$type)
memory1$memoryResp <- as.numeric(memory1$memoryResp)

### (1) calculate proportions of "old" responses
# correct recognition of studied items (CSs) -> true recognition
# incorrect response "old" to critical lures (GSs) -> false recognition
#0 new
#1 old

memory1Prop <- aggregate(memoryResp ~ subject + condition1 + type, memory1, sum)
memory1Prop$nr <- aggregate(memoryResp ~ subject + condition1 + type, memory1, length)[[4]]
memory1Prop$prop <- (memory1Prop$memoryResp/memory1Prop$nr)
memory1Prop

memory1Prop$se <- aggregate(prop ~ subject + condition1 + type, memory1Prop, se)[[4]]

#plot proportion of "old" responses
plotmemory1Corrrect <- ggplot(memory1Prop, aes (x = type, y = prop, fill = condition1)) +
  geom_bar(stat = 'identity', position = position_dodge(), show.legend = TRUE) +
  #geom_errorbar(aes(ymin= response - se, ymax= response + se), width=.2,
  #              position=position_dodge(.9)) +
  ggtitle("DRM - Chinese Characters") + 
  scale_fill_brewer(palette = "Paired") +
  theme_classic() +
  theme(plot.title = element_text (hjust = 0.5, face = "bold", size = 12)) +
  labs(fill = "Condition") +
  scale_y_continuous (name = "proportion of 'old' responses")
plotmemory1Corrrect

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
dim(memory2)
memory2 <- memory2[memory2$timeout == "false",]
dim(memory2)
memory2$type <- factor(memory2$type)
memory2$memoryResp <- as.numeric(memory2$memoryResp)




