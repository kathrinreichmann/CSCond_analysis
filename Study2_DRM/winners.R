#### Experiment: CS DRM Pre-Test, 6.5.2021


#### set directory
setwd("C:/Users/reich/Documents/GitHub/CScond_PreTest/mails")

#### set variabls
nrWinners <- 12

#### read tables
filenames <- dir()
dat <- data.frame()
for (i in filenames){
  dat <- rbind(dat, read.table(i, header = TRUE, sep = ",", encoding = "UTF-8"))
}

dat <- na.omit(dat)

dim(dat)
dat$mail

#### select winners

winners <- sample(1:dim(dat)[1], nrWinners, replace=FALSE)
winners
#28  92 139 129 192 159  61  67 143 117 182 178

win <- NULL
loose <- dat

for (winner in 1:nrWinners) {
  store <- dat[winners[winner], ]
  win <- rbind(win, store)
  loose <- loose[-winners[winner], ]
}

#### export winners and losers
setwd("C:/Users/reich/Documents/GitHub/CSCond_analysis/Study2_DRM/data_preprocessed")
write.csv2(win$mail, "winners.csv")
write.csv2(loose$mail, "loosers.csv")
