bin_cortical_distance <- function(params,polyorder,nBins,nObservations,save_name){
  
  # x = frequency
  # y = cortical distance
  # z = point image
  # slope = Cortical Magnification
  # pi = point image
  
  # cm_tick_values <- params[cm_ticks_values]
  # cm_ticks_labels <- params[cm_ticks_labels]
  # convert2kHZ <- params[convert2kHZ]
  # polyorder <- params[polyorder]
  # nBins <- params[nBins]
  # nObservations <- params[nObservations]
  # 
  # # put data in data frame
  # dd<-data.frame(x=params[x])
  # dd$y=params[y]
  # dd$z=params[z]
  
  cm_ticks_values <- params$cm_ticks_values
  cm_ticks_labels <- round(params$cm_ticks_labels,1)
  convert2kHZ <- params$convert2kHZ
  # polyorder <- params$polyorder
  # nBins <- params$nBins
  # nObservations <- params$nObservations
  
  # put data in data frame
  dd<-data.frame(x=params$x)
  dd$y=params$y
  # dd$z=params$z
  
  # fit nth order poly to data
  fit <- lm(y ~ poly(x,polyorder,raw=TRUE), dd)
  dd$fitted <- fitted(fit)

  # plot everything
  
  cortical_distance_raw <- x_bin(dd$x,dd$y,nBins,nObservations)
  cortical_distance_fitted <- x_bin(dd$x,dd$fitted,nBins,nObservations)
  
  # convert to kHz becuases binned in x spacing domain ie nERB 
  cortical_distance_raw$x_mn <- convert2kHZ(cortical_distance_raw$x_mn)
  
  cm <- data.frame(x=cortical_distance_raw$x_mn)
  cm$y <- cortical_distance_raw$y_mn
  cm$yf <- cortical_distance_fitted$y_mn
  cm$mag <- rep(save_name,length(cortical_distance_raw$x_mn))
  
  return(cm)
  
  # return(list(x = cortical_distance_raw$x_mn,
  #             y = cortical_distance_raw$y_mn,
  #             yf = cortical_distance_fitted$y_mn,
  #             mag = rep(save_name,length(cortical_distance_raw$x_mn))))
}