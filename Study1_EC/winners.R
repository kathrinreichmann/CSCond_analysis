setwd("\\\\sn00.zdv.uni-tuebingen.de/siskr01/Documents/Github/CScond_online/mail")


#### read tables
filenames <- dir()
dat <- data.frame()
for (i in filenames){
  dat <- rbind(dat, read.table(i, header = TRUE, sep = ",", encoding = "UTF-8"))
}

dat <- na.omit(dat)

dim(dat)
dat$mail
winners <- sample(1:240, 6, replace=FALSE)
winners
#141  39 158 230  14  31

for (winner in winners){
  print(dat$mail[winner])
}

#winners
#Nina.mk@gmx.de
#madeline.fink@gmx.de
#fabienne.stehle@student.uni-tuebingen.de
#malte.loetz@googlemail.com
#horst.wagner@student.uni-tuebingen.de
#wischmobdancer@gmail.com