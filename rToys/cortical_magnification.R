cortical_magnification <- function(x,y,z,cm_tick_values,cm_ticks_labels,convert2kHZ,polyorder,nBins,nObservations){
  
  # x = frequency
  # y = cortical distance
  # z = point image
  # slope = Cortical Magnification
  # pi = point image
  
  # put data in data frame
  dd<-data.frame(x=x)
  dd$y=y
  dd$z=z
  
  # fit nth order poly to data
  fit <- lm(y ~ poly(x,polyorder,raw=TRUE), dd)
  
  dd$fitted <- fitted(fit)
  
  # take deriverative of fitted function to get slope as a function of x
  dd$slope <- model.matrix(fit) %*% matrix(deriv_coef(fit), ncol=1)
  
  # convert to kHz becuases fitted in x spacing domain ie nERB 
  # dd$slope <- convert2kHZ(dd$slope)
  
  # calculate point image
  dd$pi=dd$slope*dd$z
  
  # plot everything

  cortical_distance_raw <- f_bin(dd$x,dd$y,nBins,nObservations)
  cortical_distance_fitted <- f_bin(dd$x,dd$fitted,nBins,nObservations)
  
  cd <- ggplot(cortical_distance_raw, aes(x=x_mn)) + 
    geom_point(aes(y = y_mn), colour="black") +
    geom_errorbar(aes(ymin =y_mn-y_se,ymax =y_mn+y_se))+
    scale_x_continuous(breaks = cm_ticks_values, label = cm_ticks_labels) +
    labs( x = "Frequency (kHz)", y = "Cortical Distance (mm)") +
    geom_line(data=cortical_distance_fitted, aes(x=x_mn,y = y_mn), colour="red")
  
  
  tuning_width <- f_bin(dd$x,dd$z,nBins,nObservations)
  
  tw <- ggplot(tuning_width, aes(x=x_mn)) + 
    geom_point(aes(y = y_mn), colour="black") +
    geom_errorbar(aes(ymin =y_mn-y_se,ymax =y_mn+y_se))+
    scale_x_continuous(breaks = cm_ticks_values, label = cm_ticks_labels) +
    labs( x = "Frequency (kHz)", y = "Tuning Width (kHz)")
  
  cortical_magnification <- f_bin(dd$x,dd$slope,nBins,nObservations)
  
  cmf <- ggplot(cortical_magnification, aes(x=x_mn)) + 
    geom_point(aes(y = y_mn), colour="black") +
    geom_errorbar(aes(ymin =y_mn-y_se,ymax =y_mn+y_se))+
    scale_x_continuous(breaks = cm_ticks_values, label = cm_ticks_labels) +
    labs( x = "Frequency (kHz)", y = "CMF (mm/kHz)")
  
  point_image <- f_bin(dd$x,dd$pi,nBins,nObservations)
  
  pi <- ggplot(point_image, aes(x=x_mn)) + 
    geom_point(aes(y = y_mn), colour="black") +
    geom_errorbar(aes(ymin =y_mn-y_se,ymax =y_mn+y_se))+
    scale_x_continuous(breaks = cm_ticks_values, label = cm_ticks_labels) +
    labs( x = "Frequency (kHz)", y = "PI (mm)")
  
  plot_grid(cd, tw, cmf, pi, labels = c("A", "B", "C", "D"), ncol = 2)
  
}

