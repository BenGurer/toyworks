cortical_magnification <- function(x,y,z,cm_tick_values,cm_ticks_labels,convert2kHZ){
  
  # x = frequency
  # y = cortical distance
  # z = point image
  # slope = Cortical Magnification
  # pi = point image
  
  # put data in data frame
  dd<-data.frame(x=x)
  dd$y=y
  dd$z=z
  
  # fit 2nd order poly to data
  fit <- lm(y ~ poly(x,3,raw=TRUE), dd)
  dd$fitted <- fitted(fit)
  
  # take deriverative of fitted function to get slope as a function of x
  dd$slope <- model.matrix(fit) %*% matrix(deriv_coef(fit), ncol=1)
  
  # convert to kHz becuases fitted in x spacing domain ie nERB 
  # dd$slope <- convert2kHZ(dd$slope)
  dd$slope <- dd$slope
  
  # calculate point image
  dd$pi=dd$slope*dd$z
  
  # plot everything
  
  cortical_distance <- ggplot(dd, aes(x=x)) +
    geom_point(aes(y = y), colour="black", alpha = 0.1) +
    geom_line(aes(y = fitted), colour="red") +
    geom_line(aes(y = slope), colour="green") +
    scale_x_continuous(breaks = cm_ticks_values, label = cm_ticks_labels) +
    labs( x = "Frequency (kHz)", y = "Cortical Distance (mm)")
  
  tuning_width <- ggplot(dd, aes(x=x)) +
    geom_point(aes(y = z), colour="black", alpha = 0.1) +
    geom_point(aes(y = pi), colour="red", alpha = 0.1) +
    scale_x_continuous(breaks = cm_ticks_values, label = cm_ticks_labels) +
    labs( x = "Frequency (kHz)", y = "Tuning Width (kHz)")
  
  cortical_magnification <- f_bin(dd$x,dd$slope,20,8)
  
  cmf <- ggplot(cortical_magnification, aes(x=x_mn)) + 
    geom_point(aes(y = y_mn), colour="black") +
    geom_errorbar(aes(ymin =y_mn-y_se,ymax =y_mn+y_se))+
    scale_x_continuous(breaks = cm_ticks_values, label = cm_ticks_labels) +
    labs( x = "Frequency (kHz)", y = "CMF (mm/kHz)")
  
  point_image <- f_bin(dd$x,dd$pi,20,8)
  
  pi <- ggplot(point_image, aes(x=x_mn)) + 
    geom_point(aes(y = y_mn), colour="black") +
    geom_errorbar(aes(ymin =y_mn-y_se,ymax =y_mn+y_se))+
    scale_x_continuous(breaks = cm_ticks_values, label = cm_ticks_labels) +
    labs( x = "Frequency (kHz)", y = "PI (mm)")
  
  plot_grid(cortical_distance, tuning_width, cmf, pi, labels = c("A", "B", "C", "D"), ncol = 2)
  
}

