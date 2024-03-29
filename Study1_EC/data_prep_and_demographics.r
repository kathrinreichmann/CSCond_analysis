################### CS Variability: one-to-one vs. many-to-one
### january, 2021
### Kathrin Reichmann

### Data preparation & demographics


setwd("C:/Users/reich/Documents/GitHub/FLO_replication/CScond_online/data")
#setwd("\\\\sn00.zdv.uni-tuebingen.de/siskr01/Documents/Github/CSCond_online/data")


library(dplyr)
library(tidyverse)
library(ggplot2)

##### import: 
#single data files

##### export:
#direct.csv
#indirect.csv


# read files --------------------------------------------------------------


filenames <- dir()
dat <- data.frame()
for (i in filenames){
  dat <- rbind(dat, read.table(i, header = TRUE, sep = ",", encoding = "UTF-8"))
}
rm (filenames, i)
unique(dat$subject)

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


# exclude participants ----------------------------------------------------


#chinese speaking participants
#0 = ja
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
dat <- dat[!dat$ablenkung == 0,]

#who did not follow instructions
get_tables <- aggregate(response ~ subject + gewissenhaft, dat, mean)
table(get_tables$gewissenhaft) # 1 = nicht gewissenhaft

#answered too slowly in indirect measure (rt > 3 SDs)
exclude <- aggregate(rt ~ subject, dat, mean)
#exclusion_sd <- sd(na.omit(dat$rt))*3
#exclusion_sd
#exclude[exclude$rt > exclusion_sd,]

#exclude data with NA as subject (1 VP)
dat <- dat[!is.na(dat$subject == TRUE),]

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


# demographics ------------------------------------------------------------


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


# export for direct measure analysis --------------------------------------


#extract relevant columns
direct <- dat[dat$task == "direct" | dat$task == "all",] 
direct <- direct[c('response', 'subject', 'condition', 'condition_code', 'measure', 'measure_code', 'val', 'type', 'type_specific', 'category', 'cs_selected')]

for (factor in as_factor){
  direct[, factor] <- as.factor(direct[,factor])
}

direct$type_specific <- factor(direct$type_specific, levels = c('CS', 'GSold', 'GSnew', 'abstract', 'all'), labels = c('CS', 'GS same', 'GS different', "Feature", "Group"))
direct$response <- as.numeric(direct$response)
str(direct)

unique(direct$subject)

#export files
#setwd("\\\\sn00.zdv.uni-tuebingen.de/siskr01/Documents/Github/CSCond_analysis/CSCond_analysis/data")
setwd("C:/Users/reich/Documents/GitHub/CSCond_analysis/data")
write.csv2(direct, file = 'direct.csv')
write_csv(direct, "direct_Study1.csv")


# export for indirect measure analysis ------------------------------------


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

indirect$type_specific <- factor(indirect$type_specific, levels = c('CS', 'GSold', 'GSnew', 'abstract'), labels = c('CS', 'GS same', 'GS different', "Feature"))

#export files
#setwd("\\\\sn00.zdv.uni-tuebingen.de/siskr01/Documents/Github/CSCond_analysis/CSCond_analysis/data")
setwd("C:/Users/reich/Documents/GitHub/CSCond_analysis/data")
write.csv2(indirect, file = 'indirect.csv')
write_csv(indirect, "indirect_Study1.csv")