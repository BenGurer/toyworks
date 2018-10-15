cal_cortical_magnification <- function(params,polyorder,nBins,nObservations,save_name){
  
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
  cm_ticks_labels <- params$cm_ticks_labels
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
    scale_x_continuous(breaks = cm_ticks_values, label = cm_ticks_labels) +
    labs( x = "Frequency (kHz)", y = "Cortical Distance (mm)") +
    geom_line(data=cortical_distance_fitted, aes(x=x_mn,y = y_mn), colour="red")
  
  
  tuning_width <- x_bin(dd$x,dd$z,nBins,nObservations)
  
  # tw <- ggplot(tuning_width, aes(x=x_mn)) + 
  #   geom_point(aes(y = y_mn), colour="black") +
  #   geom_errorbar(aes(ymin =y_mn-y_se,ymax =y_mn+y_se))+
  #   scale_x_continuous(breaks = cm_ticks_values, label = cm_ticks_labels) +
  #   labs( x = "Frequency (kHz)", y = "Tuning Width (nERB)")

  
  # cortical_magnification <- cal_slope(cortical_distance_fitted$x_mn,cortical_distance_fitted$y_mn,tuning_width$y_mn,convert2kHZ)
  cortical_magnification <- cal_slope(cortical_distance_raw$x_mn,cortical_distance_raw$y_mn,tuning_width$y_mn,cortical_distance_raw$x_sd,cortical_distance_raw$y_sd,tuning_width$y_sd,nObservations,convert2kHZ)
  
  # fit 2nd order poly to cortical magnification data
  fit_cm <- lm(dy ~ poly(mx,2,raw=TRUE), cortical_magnification)
  cortical_magnification$fy <- fitted(fit_cm)
  
  # fit 2nd order poly to tuning width data
  fit_tw <- lm(mz ~ poly(mx,2,raw=TRUE), cortical_magnification)
  cortical_magnification$fz <- fitted(fit_tw)
  
  # point image
  # cortical_magnification$pi = cortical_magnification$dy * cortical_magnification$mz
  cortical_magnification$pi_f = cortical_magnification$fy * cortical_magnification$mz
  cortical_magnification$pi_ff = cortical_magnification$fy * cortical_magnification$fz
  cortical_magnification$pi_se = (sqrt(cortical_magnification$mysd^2 + cortical_magnification$mzsd^2))/sqrt(nObservations)
  
  
  tw <- ggplot(cortical_magnification, aes(x=mx)) +
    geom_point(aes(y = mz)) +
    geom_line(aes(y = mz)) +
    geom_errorbar(aes(ymin = mz - mzse, ymax = mz + mzse)) +
    scale_x_continuous(breaks = cm_ticks_values, label = cm_ticks_labels) +
    labs( x = "Frequency (kHz)", y = "Tuning Width (nERB)") +
    geom_line(aes(y = fz), colour="red")
    
  cmf <- ggplot(cortical_magnification, aes(x=mx)) +
    geom_point(aes(y = dy)) +
    geom_line(aes(y = dy)) +
    geom_errorbar(aes(ymin = dy - myse, ymax = dy + myse)) +
    scale_x_continuous(breaks = cm_ticks_values, label = cm_ticks_labels) +
    labs( x = "Frequency (kHz)", y =  "CMF (mm/nERB)") +
    geom_line(aes(y = fy), colour="red")
  
  # pi <- ggplot(cortical_magnification, aes(x=mx)) +
  #   geom_point(aes(y = pi_f), colour="black") +
  #   geom_errorbar(aes(ymin =pi_f-z_se,ymax =pi_f+z_se))+
  #   scale_x_continuous(breaks = cm_ticks_values, label = cm_ticks_labels) +
  #   labs( x = "Frequency (kHz)", y = "PI (mm)")
  
  pi <- ggplot(cortical_magnification, aes(x=mx)) +
    geom_point(aes(y = pi), colour="black") +
    geom_line(aes(y = pi), colour="black") +
    geom_line(aes(y = pi_ff), colour="red") +
    scale_x_continuous(breaks = cm_ticks_values, label = cm_ticks_labels) +
    labs( x = "Frequency (kHz)", y = "PI (mm)")
  
  # error bars for point image
  # geom_errorbar(aes(ymin =pi-pi_se ,ymax =pi+pi_se))+
  
  
  final_plot <- plot_grid(cd, tw, cmf, pi, labels = c("A", "B", "C", "D"), ncol = 2)

  final_plot

  # save plot
  save_plot(str_c(save_name,".png"), final_plot, device = "png", base_height = NULL,
          base_aspect_ratio = 1.1, base_width =  6.9291339,dpi = 450)
   
}

# bin data in equal size bins
x_bin <- function(x,y,nBins,N){
  
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  f1 <- min(x, na.rm = TRUE)
  f2 <- max(x, na.rm = TRUE)
  
  x_bins <- seq(f1, f2, length=nBins)
  
  x_mn <- numeric(nBins-1)
  y_mn <- numeric(nBins-1)
  x_sd <- numeric(nBins-1)
  y_sd <- numeric(nBins-1)
  x_se <- numeric(nBins-1)
  y_se <- numeric(nBins-1)
  
  for (i in c(1:(length(x_bins)-1))){
    index_l = x_bins[i]
    index_u = x_bins[i+1]
    index = x > index_l & x <= index_u
    
    x_mn[i] <- mean(x[index], na.rm=TRUE)
    y_mn[i] <- mean(y[index], na.rm=TRUE)
    
    x_sd[i] <- sd(x[index], na.rm=TRUE)
    y_sd[i] <- sd(y[index], na.rm=TRUE)
    
    # Calculate standard error of the mean
    x_se[i] <- x_sd[i]/sqrt(N)
    y_se[i] <- y_sd[i]/sqrt(N)
    
  }
  
  return(data.frame(x_mn=x_mn,
                    y_mn=y_mn,
                    x_sd=x_sd,
                    y_sd=y_sd,
                    x_se=x_se,
                    y_se=y_se))
  
}


## calculate slope
cal_slope <- function(x,y,z,xsd,ysd,zsd,N,convert2kHZ){
  
  nBins = length(x) - 1
  
  slope <- numeric(nBins)
  mx <- numeric(nBins)
  my <- numeric(nBins)
  mz <- numeric(nBins)
  
  mxsd <- numeric(nBins)
  mysd <- numeric(nBins)
  mzsd <- numeric(nBins)
  
  mxse <- numeric(nBins)
  myse <- numeric(nBins)
  mzse <- numeric(nBins)
  
  dx <- numeric(nBins)
  dy <- numeric(nBins)
  pi <- numeric(nBins)

  
  for (i in c(1:nBins)){
    dx[i] <- convert2kHZ(x)[i+1] - convert2kHZ(x)[i]
    # dx[i] <- x[i+1] - x[i]
    dy[i] <- y[i+1] - y[i]
    
    mx[i] <- (x[i+1] + x[i])/2
    my[i] <- (y[i+1] + y[i])/2
    mz[i] <- (z[i+1] + z[i])/2
    
    # mxsd[i] <- (xsd[i+1] + xsd[i])/2
    # mysd[i] <- (ysd[i+1] + ysd[i])/2
    # mzsd[i] <- (zsd[i+1] + zsd[i])/2
    
    mxsd[i] <- sqrt(xsd[i+1]^2 + xsd[i]^2)
    mysd[i] <- sqrt(ysd[i+1]^2 + ysd[i]^2)
    mzsd[i] <- sqrt(zsd[i+1]^2 + zsd[i]^2)
    
    mxse[i] <- mxsd[i]/sqrt(N)
    myse[i] <- mysd[i]/sqrt(N)
    mzse[i] <- mzsd[i]/sqrt(N)
    
  }
  
  for (i in c(1:nBins)){
    # CMF[i] <- 1/(dCD[i]/df[i])
    # slope[i] <- (dy[i]/convert2kHZ(dx[i]))
    slope[i] <- (dy[i]/dx[i])
    # pi[i]<- mz[i]*slope[i]
    pi[i]<- dy[i]*mz[i]
  }
  
  return(data.frame(mx=mx,
                    my=my,
                    mz= mz,
                    mxsd = mxsd,
                    mysd = mysd,
                    mzsd = mzsd,
                    mxse = mxse,
                    myse = myse,
                    mzse = mzse,
                    slope=slope,
                    dx=dx,
                    dy=dy,
                    pi=pi
  ))
  
}
# cmf <- ggplot(cortical_magnification, aes(x=mx)) +
#   geom_line(aes(y = slope)) +
#   scale_x_continuous(breaks = cm_ticks_values, label = cm_ticks_labels) +
#   labs( x = "Frequency (kHz)", y = "CMF (mm/kHz)")

# pi <- ggplot(cortical_magnification, aes(x=mx)) +
#   geom_line(aes(y = pi)) +
#   scale_x_continuous(breaks = cm_ticks_values, label = cm_ticks_labels) +
#   labs( x = "Frequency (kHz)", y = "PI (mm)")
# pi <- ggplot(cortical_magnification, aes(x=mx)) +
#   geom_line(aes(y = dy * mz)) +
#   scale_x_continuous(breaks = cm_ticks_values, label = cm_ticks_labels) +
#   labs( x = "Frequency (kHz)", y = "Cortical Distance (mm)")


# dd$pi=cortical_magnification$slope*dd$z

# cortical_magnification <- x_bin(dd$x,dd$slope,nBins,nObservations)

# cmf <- ggplot(cortical_magnification, aes(x=x_mn)) + 
#   geom_point(aes(y = y_mn), colour="black") +
#   geom_errorbar(aes(ymin =y_mn-y_se,ymax =y_mn+y_se))+
#   scale_x_continuous(breaks = cm_ticks_values, label = cm_ticks_labels) +
#   labs( x = "Frequency (kHz)", y = "CMF (mm/kHz)")

# point_image <- x_bin(dd$x,dd$pi,nBins,nObservations)

# pi <- ggplot(point_image, aes(x=x_mn)) +
#   geom_point(aes(y = y_mn), colour="black") +
#   geom_errorbar(aes(ymin =y_mn-y_se,ymax =y_mn+y_se))+
#   scale_x_continuous(breaks = cm_ticks_values, label = cm_ticks_labels) +
#   labs( x = "Frequency (kHz)", y = "PI (mm)")
