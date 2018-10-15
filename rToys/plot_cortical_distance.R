plot_cortical_distance<- function(params,polyorder,nBins,nObservations,save_name){
  text_size = 9
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
  dd$z=params$z
  
  # fit nth order poly to data
  fit <- lm(y ~ poly(x,polyorder,raw=TRUE), dd)
  
  dd$fitted <- fitted(fit)
  
  # take deriverative of fitted function to get slope as a function of x
  # dd$slope <- model.matrix(fit) %*% matrix(deriv_coef(fit), ncol=1)
  
  # convert to kHz becuases fitted in x spacing domain ie nERB 
  # dd$slope <- convert2kHZ(dd$slope)
  
  # calculate point image
  # dd$pi=dd$slope*dd$z
  
  # plot everything
  
  cortical_distance_raw <- x_bin(dd$x,dd$y,nBins,nObservations)
  cortical_distance_fitted <- x_bin(dd$x,dd$fitted,nBins,nObservations)
  
  
  cd <- ggplot(cortical_distance_raw, aes(x=x_mn)) + 
    geom_point(aes(y = y_mn), colour="black") +
    geom_line(aes(y = y_mn), colour="black") + 
    geom_errorbar(aes(ymin =y_mn-y_se,ymax =y_mn+y_se))+
    labs( x = "Frequency (kHz)", y = "Cortical Distance (mm)") +
    theme(legend.text=element_text(size=text_size)) +
    geom_line(data=cortical_distance_fitted, aes(x=x_mn,y = y_mn), colour="red")
  
  cd <- cd + scale_x_continuous(breaks = cm_ticks_values, label = cm_ticks_labels)
 
  return(cd) 
}
  