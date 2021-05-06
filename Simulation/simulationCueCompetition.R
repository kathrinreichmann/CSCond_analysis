rm(list = ls());
library(ndl);
library(ggplot2);
library(knitr);
library(ggpubr);
library(ggsci);
library(grid);
library(gridExtra);
library(cowplot);
library(plyr);
library(wesanderson);
library(data.table);
library(tinytex);
library(RColorBrewer);

#local directory
localGitDir <- 'C:/Users/reich/Documents/GitHub/CSCond_analysis/Simulation'
setwd(localGitDir);


#load RW functions
source("tools/my_rescorlawagner_suffix.R")
source("tools/my_rescorlawagner_prefix.R")
source("tools/prepare_data.R")

plot.bar <- function(mystims, saliency, condition){
  if(condition == "suffix"){
    print('suffix condition')
    mybar <- my_rescorlawagner_suffix(mystims, saliency, type_plot = "bar")
    
    round(mybar$Equilibriums,2)->mybar$Equilibriums
    ggbarplot(mybar, "SingleCues", "Equilibriums",
              fill = "SingleCues",
              label = TRUE) +
      scale_fill_manual(values = c("red" = "#e41a1c", "blue" = "#377eb8",
                                   "purple"="#9400D3",
                                   "d1" = "#4daf4a", "d2" = "#999999", 
                                   "d3" = "#ef8a62", "d4" = "#998ec3",
                                   "d5" = "#000000")) +
      ylim((min(mybar$Equilibriums)-.1),1) +
      labs(x="", y="Associative strength")+
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position = "none"); 
    return(mybar)
    
  } else {
    print('prefix condition') 
    mybar <- my_rescorlawagner_prefix(mystims, saliency, type_plot = "bar")
    round(mybar$Equilibriums,2)->mybar$Equilibriums
    ggbarplot(mybar, "unique.CueOutcome.", "Equilibriums",
              fill = "unique.CueOutcome.",
              label = TRUE) +
      scale_fill_manual(values = c("red" = "#e41a1c", "blue" = "#377eb8",
                                   "purple"="#9400D3",
                                   "d1" = "#4daf4a", "d2" = "#999999", 
                                   "d3" = "#ef8a62", "d4" = "#998ec3",
                                   "d5" = "#000000")) +
      ylim((min(mybar$Equilibriums)-.1),1) +
      labs(x="", y="Associative strength")+
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position = "none"); 
    
  }
}

plot_line <- function(mystims, saliency, condition){
  if(condition == "suffix"){
    print('suffix condition')
    myline = my_rescorlawagner_suffix(mystims, saliency, type_plot = "line")
  }else{
    print('prefix condition');
    myline = my_rescorlawagner_prefix(mystims, saliency, type_plot = "line")
  }
  
  p <- ggplot(data=myline, aes(x = Time, y = Weight, colour = CueOutcome)) + 
    geom_line() + 
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank()) + 
    #ylim(-0.4,1) +
    labs(x="Trial", y="Associative strength", colour="Feature") +
    scale_color_manual(values = c("red" = "#e41a1c", "blue" = "#377eb8",
                                  "purple" = "#9400D3",
                                  "d1" = "#4daf4a", "d2" = "#999999", 
                                  "d3" = "#ef8a62", "d4" = "#998ec3",
                                  "d5" = "#000000"));
  return(p)
}







#### file 1 ####
#This dataset simulates a scenario where participants don't recognize the low saliency feature as discriminating
#so they perceive all the features to be shared by all categories
myexp = read.csv(paste(localGitDir,"/input/exp1.csv", sep = ''));
myexp

mystims <- prepare_data(myexp, condition = "suffix")

traceOutcomes=c("bim")
traceCues=c("blue", "red", "purple","d1", "d2", "d3")

saliency <- rep(0.1, length(traceCues));

plot_line(mystims, saliency, "suffix")
plot.bar(mystims, saliency, "suffix")

mystims <- prepare_data(myexp, condition = "prefix")

traceOutcomes=c("blue", "red",  "d1", "d2", "d3")
traceCues=c("dep")

saliency <- rep(0.1, length(traceOutcomes));

plot_line(mystims, saliency, "prefix")

#interesting! Participants learn that the most reliable cue is the color/bodyshape

#let's hypothesize, that participants perceive low saliency features of low freq exemplars, but 
#don't perceive the low frequency exemplars as such
#In this case, only the FL learning should be able to disentangle the features in the hf cond,
#but it should be super confused by the low frequency

myexp<-data.frame(
  Cues = c(rep("blue_d1",3),"red_d2",
           rep("purple_d3",3),"blue_d2",
           rep("red_d4",3), "purple_d2"),
  Outcomes = c(rep("dep",4),
               rep("bim",4),
               rep("tob",4)),
  Frequency = rep(250,12))

traceOutcomes=c("dep")
traceCues=c("blue", "red", "purple","d1", "d2", "d3", "d4")

saliency <- rep(0.1, length(traceCues));
mystims <- prepare_data(myexp, condition = "suffix")

scenario1<-plot_line(mystims, saliency, "suffix")
plot.bar(mystims, saliency, "suffix")

traceOutcomes=c("blue", "red", "purple","d1", "d2", "d3", "d4")
traceCues=c("dep")

saliency <- rep(0.1, length(traceOutcomes));
mystims <- prepare_data(myexp, condition = "prefix")

plot_line(mystims, saliency, "prefix")
plot.bar(mystims, saliency, "prefix")



#another possibility, is that categories are perceived identical among high and low freq
#In this case, both learnings behaves the same
myexp<-data.frame(
  Cues = c(rep("blue_d1",3),"red_d2",
           rep("purple_d3",3),"blue_d1",
           rep("red_d2",3), "purple_d3"),
  Outcomes = c(rep("dep",4),
               rep("bim",4),
               rep("tob",4)),
  Frequency = rep(250,12))

traceOutcomes=c("dep")
traceCues=c("blue", "red", "purple","d1", "d2", "d3")

saliency <- rep(0.1, length(traceCues));
mystims <- prepare_data(myexp, condition = "suffix")

plot_line(mystims, saliency, "suffix")
plot.bar(mystims, saliency, "suffix")

traceOutcomes=c("blue", "red", "purple","d1", "d2", "d3")
traceCues=c("dep")

saliency <- rep(0.1, length(traceOutcomes));
mystims <- prepare_data(myexp, condition = "prefix")

plot_line(mystims, saliency, "prefix")
plot.bar(mystims, saliency, "prefix")

#### file 1b ####
# Predictions as found in the FLO paper
myexp = read.csv(paste(localGitDir,"input/exp1b.csv", sep = ''));

mystims <- prepare_data(myexp, condition = "suffix")

traceOutcomes=c( "dep")
traceCues=c("blue", "red",  "purple", "d1", "d2", "d5", "d3", "d4")

saliency <- rep(0.1, length(traceCues));

FL_learningCurve<- plot_line(mystims, saliency, "suffix")
FL_learningCurve
plot.bar(mystims, saliency, "suffix")

mystims <- prepare_data(myexp, condition = "prefix")

traceOutcomes=c("blue", "red",  "d1", "d2", "d5")
traceCues=c("dep")

saliency <- rep(0.1, length(traceOutcomes));

plot_line(mystims, saliency, "prefix")
plot.bar(mystims, saliency, "prefix")

ggarrange(scenario1, FL_learningCurve)


