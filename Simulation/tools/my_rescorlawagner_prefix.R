my_rescorlawagner_prefix <- function(mystims, saliency, type_plot){
  
  Time <- c()
  Weight <- c()
  CueOutcome <- c()
  Equilibriums <- c()
  SingleCues <- c() 
  
  for (i in 1:length(traceOutcomes)){
    traceCue <- traceCues[1]
    SingleCues <- append(SingleCues, traceCue, after=length(SingleCues))
    traceOutcome <- traceOutcomes[i]
    
    # RESCORLA-WAGNER MODEL HAPPENS HERE
    my_rw = RescorlaWagner(mystims, nruns=1,
                           traceCue = traceCues[1],
                           traceOutcome = traceOutcomes[i], 
                           beta1 = saliency[i])
    
    # For each cue, get equilibrium weight (to be plotted)
    e <- my_rw$equilibriumWeight
    Equilibriums <- append(Equilibriums, e)
    
    # For each cue, get the associative weight at each trial (to be plotted)
    weights <- my_rw$weightvector
    Weight <- append(Weight, weights)
    print(tail(weights, 1))
    
    # For each trial, write a string saying which cue it was (for plotting)
    cue_outcome <- c()
    for (x in 1:length(weights)){
      cue_outcome <- append(cue_outcome, paste(traceOutcome))
    }
    CueOutcome <- append(CueOutcome, cue_outcome)
    
    # Create a vector with x values (trial number, for plotting)
    xvals <- c()
    for (x in 1:length(weights)) {
      xvals <- append(xvals, x, after=length(xvals))
    }
    Time <- append(Time, xvals, after=length(Time))
  }
  
  myline <- data.frame(CueOutcome, Time, Weight)
  mybar <- data.frame(SingleCues, Equilibriums, unique(CueOutcome))
  
  if (type_plot == "line")
    
  {
    return(myline)
    
  } else {
    
    return(mybar)
  }
}
