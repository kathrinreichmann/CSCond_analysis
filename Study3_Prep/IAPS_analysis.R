## IAPS analysis

setwd("C:/Users/siskr01/GitHub/CSCond_analysis/Study3_Prep")

dat <- read.csv2('IAPS_US.csv', header = TRUE, sep = '\t')

#dat <- dat[dat$old_new == "repl" || dat$old_new == "old",]
dat <- dat[dat$old_new == "new" || dat$old_new == "old",]

datNeg <- dat[dat$val == "neg",]
datPos <- dat[dat$val == "pos",]

##descriptives
summary(datNeg[,3:7])
summary(datPos[,3:7])

## test valence
t.test(datNeg$Pleasure, datPos$Pleasure, paired = FALSE, var.equal = TRUE)
plot(datNeg$Pleasure, datPos$Pleasure)

## test arousal
t.test(datNeg$Arousal, datPos$Arousal, paired = FALSE, var.equal = TRUE)
plot(datNeg$Arousal, datPos$Arousal)

## test dominance
t.test(datNeg$Dominance, datPos$Dominance, paired = FALSE, var.equal = TRUE)
plot(datNeg$Dominance, datPos$Dominance)
