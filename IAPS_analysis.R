## IAPS analysis

setwd()

dat <- read.csv2('IAPS_US.csv', header = TRUE, sep = ';')

datNeg <- dat[dat$val == "neg",]
datPos <- dat[dat$val == "pos",]


## test valence
t.test(datNeg$Pleasure, datPos$Pleasure, paired = FALSE, var.equal = TRUE)

## test arousal
t.test(datNeg$Arousal, datPos$Arousal, paired = FALSE, var.equal = TRUE)

## test dominance
t.test(datNeg$Dominance, datPos$Dominance, paired = FALSE, var.equal = TRUE)
plot(datNeg$Dominance, datPos$Dominance)