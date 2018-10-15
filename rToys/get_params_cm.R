## CM parameters
get_parameters <- function(dd,domain){
  # rm(cm_ticks_values,cm_ticks_labels,x,y,z,convert2kHZ,polyorder,nBins,nObservations)
  
  if ("linear" %in% domain) {
    cm_ticks_values <- c(0,5,10,15)
    cm_ticks_labels <- cm_ticks_values
    x=dd$Frequency_kHz
    convert2kHZ <- nlinear
  } 
  else if ("nLog" %in% domain) {
    x=log(dd$Frequency_kHz)
    cm_ticks_values <- c(-3,-1.5, 0,1.5, 3)
    cm_ticks_labels <- exp(cm_ticks_values)
    # cm_ticks_labels <- log(cm_ticks_values)
    convert2kHZ <- exp
  } 
  else if ("bLog" %in% domain) {
    x=log10(dd$Frequency_kHz)
    cm_ticks_values <- c( -1, 0, 1)
    cm_ticks_labels <- 10^(cm_ticks_values)
    # cm_ticks_labels <- log10(cm_ticks_values)
    convert2kHZ <- function(x){10^x}
  } 
  else if ("nERBs" %in% domain) {
    x <- dd$Frequency_nERB
    cm_ticks_values <- c(0,10,20,30,40)
    cm_ticks_labels <- nERB2kHz(cm_ticks_values)
    
    # cm_ticks_labels <- nERB(cm_ticks_values)
    convert2kHZ <- nERB2kHz  
  } 
  else if ("nDLFs" %in% domain) {
    # x=dd$Frequency_nDLF
    x=nDLF(dd$Frequency_kHz)
    cm_ticks_values <- c(2500,5000,7500,10000,12500)
    # cm_ticks_values <- c(-4000,-3000,-2000,-1000)
    cm_ticks_labels <- nDLF2kHz(cm_ticks_values)
    # cm_ticks_labels <- nDLF(cm_ticks_values)
    convert2kHZ <- nDLF2kHz    
  }
  else if ("nDLFs_fyr" %in% domain) {
    # x=dd$Frequency_nDLF
    x=nDLF_fyr(dd$Frequency_kHz)
    # cm_ticks_values <- c(2500,5000,7500,10000,12500)
    cm_ticks_values <- c(-4000,-3000,-2000,-1000)
    # cm_ticks_values <- c(-4154.895, -3330.212, -2442.507, -1699.991, -1142.992)
    cm_ticks_labels <- nDLF2kHz_fyr(cm_ticks_values)
    # cm_ticks_labels <- nDLF(cm_ticks_values)
    convert2kHZ <- nDLF2kHz_fyr    
  }
  else {print("what's going on?")}
  
  
  cm_ticks_labels <- round(cm_ticks_labels,digits=2)
  y <- dd$CorticalDistance_norm
  # y <- dd$CorticalDistance_mm
  # z <- nERB2kHz(dd$TuningWidth)
  z <- dd$TuningWidth
  
  return(list(x = x,
              y = y,
              z = z,
              cm_ticks_values = cm_ticks_values,
              cm_ticks_labels = cm_ticks_labels,
              convert2kHZ = convert2kHZ))
}