#This function plots association weights between a cue/outcome pair over time (trials). It calls rescorlawagner_FL/prefix. 

col_pal = c("red3", "orange1", "forestgreen", "purple3", "blue", "gold", "sienna4", "darkgrey", "cyan4")

plot_line <- function(mystims, saliency, condition){
  if(condition == "suffix"){
    print('FL condition')
    myline = my_rescorlawagner_suffix(mystims, saliency, type_plot = "line")
  }else{
    print('LF condition');
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
    labs(x="Trial", y="Associative strength", colour="Feature") #+
    scale_color_manual(values = c("red" = "#e41a1c", "blue" = "#377eb8",
                                  "d1" = "#4daf4a", "d2" = "#999999", 
                                  "d3" = "#ef8a62", "d4" = "#998ec3"));
  return(p)
}
