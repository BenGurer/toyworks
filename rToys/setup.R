# set up script
library(tidyverse)
library(cowplot)
library(corrplot)
library(scales) # to access break formatting functions
library(RColorBrewer)
library(modelr)
library(plyr); library(dplyr)

f_bin<-function(x,y,nBins,N){
  
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
    x_se[i] <- x_sd[i]/N
    y_se[i] <- y_sd[i]/N
    
  }
  
  return(data.frame(x_mn=x_mn,
                    y_mn=y_mn,
                    x_sd=x_sd,
                    y_sd=y_sd,
                    x_se=x_se,
                    y_se=y_se))
  
}

nlinear <- function(kHz) {
  kHz}

# convert nERB to kHz
nERB2kHz <- function(nERB) {
  
  # converts nERB (ERB numbers) to  kHz
  A <- 24.7/1000
  B <- 4.37
  kHz <- 1/B*(exp(A*B*nERB)-1)
  
}

nERB <- function(kHz) {
  
  # converts nERB (ERB numbers) to  kHz
  A <- 24.7/1000 
  B <- 4.37
  nERB <- 1/((A*B)*log(B*kHz+1))
  
}

fitnERB <- function(kHz,m) {
  
  # converts nERB (ERB numbers) to  kHz
  A <- 24.7/1000 
  B <- 4.37
  nERB <- (1/((A*B)*log(B*kHz+1))) + m
  
}

nDLF <- function(f) {
  f = f*1000 # convert from kHz to Hz
  a = 0.0214; k = -0.15; m = 5.056; SL = 45;
  nDLF = (2*(a*sqrt(f) + (a*sqrt(f) - log(exp(k + m/SL + a * sqrt(f)))) * log(SL * log(exp((k + (m/SL) + a * sqrt(f))))))) / a^2;
}
nDLF2kHz <- function(d) {
  x = seq(0.02, 20, by = 0.001)
  y <- nDLF(x)
  kHz <- spline(y,x,xout=d)
  return(kHz$y)
}
ERB <- function(f) {
  A = 24.7/1000; B = 4.37;
  ERB = A*(B*f+1);
}

DLF <- function(f) {
  a = 0.0214; k = -0.15; m = 5.056; SL = 40;
  f = f.*1000; # convert from kHz to Hz
  
  DLF = log10(a.*(sqrt(f)) + k + m.*(SL^-1))
}

f_poly <- function(CorticalDistance, a, b, c) {
  
  pred <- a*CorticalDistance^2 + b*CorticalDistance + c
  
}
f_lin <- function(CorticalDistance, m, c) {
  
  pred <- m * CorticalDistance + c
  
}

f_binMean <- function(frequencies,cortical_distances,tuning_widths,nBins){
  
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  f1 <- min(frequencies, na.rm = TRUE)
  f2 <- max(frequencies, na.rm = TRUE)
  
  f_bins <- seq(f1, f2, length=nBins)
  
  f_mean_bin <- numeric(nBins-1)
  cd_mean_bin <- numeric(nBins-1)
  tw_mean_bin <- numeric(nBins-1)
  
  f_sd_bin <- numeric(nBins-1)
  cd_sd_bin <- numeric(nBins-1)
  tw_sd_bin <- numeric(nBins-1)
  
  f_se_bin <- numeric(nBins-1)
  cd_se_bin <- numeric(nBins-1)
  tw_se_bin <- numeric(nBins-1)
  
  for (i in c(1:(length(f_bins)-1))){
    index_l = f_bins[i]
    index_u = f_bins[i+1]
    index = frequencies > index_l & frequencies <= index_u
    N = 14
    
    f_mean_bin[i] <- mean(frequencies[index], na.rm=TRUE)
    cd_mean_bin[i] <- mean(cortical_distances[index], na.rm=TRUE)
    tw_mean_bin[i] <- mean(tuning_widths[index], na.rm=TRUE)
    
    f_sd_bin[i] <- sd(frequencies[index], na.rm=TRUE)
    cd_sd_bin[i] <- sd(cortical_distances[index], na.rm=TRUE)
    tw_sd_bin[i] <- sd(tuning_widths[index], na.rm=TRUE)
    
    # Calculate standard error of the mean
    f_se_bin[i] <- f_sd_bin[i]/N
    cd_se_bin[i] <- cd_sd_bin[i]/N
    tw_se_bin[i] <- tw_sd_bin[i]/N
    
  }
  
  return(data.frame(frequencies_mean_bin=f_mean_bin,
                    cortical_distances_mean_bin=cd_mean_bin,
                    tuning_widths_mean_bin=tw_mean_bin,
                    frequencies_sd_bin=f_sd_bin,
                    cortical_distances_sd_bin=cd_sd_bin,
                    tuning_widths_sd_bin=tw_sd_bin,
                    frequencies_se_bin=f_se_bin,
                    cortical_distances_se_bin=cd_se_bin,
                    tuning_widths_se_bin=tw_se_bin))
}

cal_CMF <- function(frequencies,cortical_distances,tuning_width){
  
  nBins = length(frequencies) - 1
  
  CMF <- numeric(nBins)
  f <- numeric(nBins)
  TW <- numeric(nBins)
  PI <- numeric(nBins)
  df <- numeric(nBins)
  dCD <- numeric(nBins)
  CD <- numeric(nBins)
  
  for (i in c(1:nBins)){
    df[i] <- frequencies[i+1]-frequencies[i]
    dCD[i] <- cortical_distances[i+1] - cortical_distances[i]
    # CMF[i] <- 1/((cortical_distances[i+1] - cortical_distances[i])/df[i])
    
    f[i] <- (frequencies[i+1]+frequencies[i])/2
    CD[i] <- (cortical_distances[i+1]+cortical_distances[i])/2
    TW[i] <- (tuning_width[i+1]+tuning_width[i])/2
  }
  
  for (i in c(1:nBins)){
    CMF[i] <- 1/(dCD[i]/df[i])
    CMF[i] <- (dCD[i]/df[i])
    PI[i]<- TW[i]*CMF[i]
  }
  
  return(data.frame(x=f,
                    y=CD,
                    z=TW,
                    slope=CMF,
                    df=df,
                    dCD=dCD,
                    PI=PI
  ))
  
}

## Summarizes data.
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

deriv_coef<-function(x) {
  x <- coef(x)
  stopifnot(names(x)[1]=="(Intercept)")
  y <- x[-1]
  stopifnot(all(grepl("^poly", names(y))))
  px <- as.numeric(gsub("poly\\(.*\\)","",names(y)))
  rr <- setNames(c(y * px, 0), names(x))
  rr[is.na(rr)] <- 0
  rr
}

