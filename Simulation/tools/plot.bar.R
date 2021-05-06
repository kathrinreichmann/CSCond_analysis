#This function plots the equilibrium association weight between a cue/outcome pair as barchart. It calls my_rescorlawagner. 

plot.bar <- function(mystims, saliency, condition){
  if(condition == "suffix"){
    print('suffix condition')
    mybar <- my_rescorlawagner_suffix(mystims, saliency, type_plot = "bar")
    
    round(mybar$Equilibriums,2)->mybar$Equilibriums
    ggbarplot(mybar, "SingleCues", "Equilibriums",
              fill = "SingleCues",
              label = TRUE) +
      scale_fill_manual(values = c("red" = "#e41a1c", "blue" = "#377eb8",
                                   "d1" = "#4daf4a", "d2" = "#999999", 
                                   "d3" = "#ef8a62", "d4" = "#998ec3")) +
      ylim((min(mybar$Equilibriums)-.1),1) +
      labs(x="", y="Associative strength")+
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position = "none"); 
    
  } else {
    print('prefix condition') 
    mybar <- my_rescorlawagner_prefix(mystims, saliency, type_plot = "bar")
    round(mybar$Equilibriums,2)->mybar$Equilibriums
    ggbarplot(mybar, "unique.CueOutcome.", "Equilibriums",
              fill = "unique.CueOutcome.",
              label = TRUE)  +
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
