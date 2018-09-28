# Group analysis: CM
# set working directory to correct folder
# setwd("E:/OneDrive - The University of Nottingham/data/hearingLossSimulation/groupAnalysis")

setwd("C:/Users/bengu/Google Drive/data/CorticalMagnification/groupAnalysis")

cm_dataset = read.csv('CorticalMagnification.csv')

row.has.na <- apply(cm_dataset, 1, function(x){any(is.na(x))})
sum(row.has.na)
cm_dataset <- cm_dataset[!row.has.na,]

cm_dataset %>% filter(Analysis == 'pRF') ->  cm_dataset_2plot
filter(cm_dataset_2plot,SubjectID == 2) ->  sub_cm_dataset_2plot


cm_dataset %>% filter(Analysis == 'pRF') %>% 
  filter(Location %in% c("posterior")) ->  cm_dataset_2plot_loc
# anterior
# posterior

cm_dataset %>% filter(Analysis == 'pRF') %>% 
  filter(between(Frequency_nERB, 10, 20)) %>% 
  filter(Hemisphere %in% c("Left")) ->  cm_dataset_2plot_hemi

# for(i in dataList){for(j in i){print(j)}}

# sapply(filenames, FUN = analyze)

domain_names <- c("linear","nLog","bLog","nERB","nDLF")
params <- get_parameters(cm_dataset,domain_names[1])

polyorder<- 1
nBins <- 40
nObservations <- 8

for(name in domain_names){
  
  rm(params)
  params <- get_parameters(cm_dataset_2plot_hemi,name)
  print(name)
  cal_cortical_magnification(params,polyorder,nBins,nObservations,name)
  
  }


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
    cm_ticks_values <- c(-3, 0, 3)
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
  else if ("nERB" %in% domain) {
    x <- dd$Frequency_nERB
    cm_ticks_values <- c(10,20,30,40)
    cm_ticks_labels <- nERB2kHz(cm_ticks_values)

    # cm_ticks_labels <- nERB(cm_ticks_values)
    convert2kHZ <- nERB2kHz  
    } 
  else if ("nDLF" %in% domain) {
    x=dd$Frequency_nDLF
    cm_ticks_values <- c(2500,5000,7500,10000,12500)
    cm_ticks_labels <- nDLF2kHz(cm_ticks_values)
    # cm_ticks_labels <- nDLF(cm_ticks_values)
    convert2kHZ <- nDLF2kHz    
    }
  else {print("what's going on?")}

  
  cm_ticks_labels <- round(cm_ticks_labels,digits=2)
  y <- dd$CorticalDistance_norm
  z <- nERB2kHz(dd$TuningWidth)
  
  return(list(x = x,
         y = y,
         z = z,
         cm_ticks_values = cm_ticks_values,
         cm_ticks_labels = cm_ticks_labels,
         convert2kHZ = convert2kHZ,
         polyorder = 3,
         nBins = 20,
         nObservations = 8))
}

# kHz
cm_ticks_values <- c(0,5,10)
cm_ticks_labels <- round(cm_ticks_labels,digits=2)

x=cm_dataset_2plot$Frequency_kHz
y=cm_dataset_2plot$CorticalDistance_mm
z=cm_dataset_2plot$TuningWidth

cortical_magnification_linear(x,y,z,cm_tick_values,cm_ticks_labels,nERB)

fitnERB


model <- lm(y ~ nERB(x), dd)


cm_ticks_values <- c(2500,5000,7500,10000,12500)
cm_ticks_labels <- nDLF2kHz(cm_ticks_values)
cm_ticks_labels <- round(cm_ticks_labels,digits=2)
convert2kHZ <- nDLF2kHz

x=cm_dataset_2plot$Frequency_nDLF
y=cm_dataset_2plot$CorticalDistance_mm
z=cm_dataset_2plot$TuningWidth

# nERB
cm_ticks_values <- c(10,20,30,40)
cm_ticks_labels <- nERB2kHz(cm_ticks_values)
cm_ticks_labels <- round(cm_ticks_labels,digits=2)

x <- cm_dataset_2plot$Frequency_nERB
y <- cm_dataset_2plot$CorticalDistance_mm
z <- nERB2kHz(cm_dataset_2plot$TuningWidth)

polyorder = 3
convert2kHZ = nERB2kHz

cortical_magnification(x,y,z,cm_ticks_values,cm_ticks_labels,convert2kHZ,polyorder,20,8)
# check if tuning witdth is kHz or nERB

# log kHz
cm_ticks_values <- c(-3, -2, -1, 0, 1, 2, 3)
cm_ticks_labels <- exp(cm_ticks_values)
cm_ticks_labels <- round(cm_ticks_labels,digits=2)

x=log(cm_dataset_2plot$Frequency_kHz)
y=cm_dataset_2plot$CorticalDistance_mm
z=nERB2kHz(cm_dataset_2plot$TuningWidth)

polyorder = 6
convert2kHZ = exp

cortical_magnification(x,y,z,cm_ticks_values,cm_ticks_labels,convert2kHZ,polyorder,20,8)

# log kHz
cm_ticks_values <- c(0,5,10,20)
cm_ticks_labels <- exp(cm_ticks_values)
cm_ticks_labels <- round(cm_ticks_labels,digits=2)

x=cm_dataset_2plot$Frequency_kHz
y=cm_dataset_2plot$CorticalDistance_mm
z=nERB2kHz(cm_dataset_2plot$TuningWidth)

polyorder = 3
convert2kHZ = exp

cortical_magnification(x,y,z,cm_ticks_values,cm_ticks_labels,convert2kHZ,polyorder,20,8)

# bin data
# fit function - use nls - function can be anything I want - possibly try on khz data and use nerb and ndlf functions
# interp
# get values at each frequency bin centre
# convert to kHz
# subtract - this is the diferential and CMF

model <- nls( Frequency_nERB ~ f_poly(CorticalDistance_mm, a,b,c), cm_dataset_2plot, start = list(a = 1, b = 1, c = 0))
tidy(model)
glance(model)
model <- nls( Frequency_nERB ~ f_lin(CorticalDistance_mm, m,c), cm_dataset_2plot, start = list(m = 1, c = 0))


test <- predict(model)

plot(predict(model,1:10))
fitted(model)

dd<-data.frame(x=x)
dd$y=y
dd$z=z
dd$fitted <- predict(model)

dd$fitted <- fitted(model)

a_x = seq(0, 40, by = 0.01)
a_y <- f_poly(x,coef(model)[1],coef(model)[2],coef(model)[3])
dd$fitted <- approx(x,a_y,x)

ggplot(dd, aes(x=x)) +
  geom_line(aes(y = y), colour="red")

ggplot(dd, aes(x=x)) +
  geom_point(aes(y = y), colour="black", alpha = 0.1) +
  geom_point(aes(y = fitted), colour="red")

a<-coef(model)[1]
b<-coef(model)[2]
k<-coef(model)[3]
plot(x,y)
lines(x<-c(1:10),a+b*x^k,col='red')


x = seq(0.02, 20, by = 0.01)
y <- nDLF(x)
kHz = approx(x,y,d)

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












# nERB 15 bins
result <- f_binMean(cm_dataset_2plot$Frequency_nERB,cm_dataset_2plot$CorticalDistance_norm,cm_dataset_2plot$TuningWidth,20)
# log 15 bins
result <- f_binMean(log10(cm_dataset_2plot$Frequency_kHz),cm_dataset_2plot$CorticalDistance_mm,nERB2kHz(cm_dataset_2plot$TuningWidth),15)
# nDLF 15 bins
result <- f_binMean(cm_dataset_2plot$Frequency_nDLF,cm_dataset_2plot$CorticalDistance_mm,cm_dataset_2plot$TuningWidth,20)



ggplot(result, aes(x=frequencies_mean_bin, y = cortical_distances_mean_bin))+
  geom_ribbon(aes(ymin=cortical_distances_mean_bin-cortical_distances_se_bin, ymax=cortical_distances_mean_bin+cortical_distances_se_bin), alpha=0.1, linetype="blank") +
  geom_line() +
  geom_point() +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank())

dd<-data.frame(x=result$frequencies_mean_bin)
dd$y=result$cortical_distances_mean_bin
dd$z=result$tuning_widths_mean_bin

fit <- lm(y ~ poly(x,3,raw=TRUE), dd)
dd$fitted <- fitted(fit)

ggplot(dd, aes(x=x)) + 
  geom_line(aes(y = y), colour="red") + 
  geom_line(aes(y = fitted), colour="blue")


dd$slope <- model.matrix(fit) %*% matrix(deriv_coef(fit), ncol=1)

ggplot(dd, aes(x=x)) + 
  geom_line(aes(y = y), colour="red") + 
  geom_line(aes(y = fitted), colour="blue") + 
  geom_line(aes(y = slope), colour="green")

ggplot(dd, aes(x=x)) + 
  geom_line(aes(y = slope), colour="green")

p <- nls( Frequency_nERB ~ f_poly(CorticalDistance_norm, a,b,c), cm_dataset_2plot, start = list(a = 1, b = 1, c = 0))

ggplot(dd, aes(x=x)) + 
  geom_line(aes(y = y), colour="red") + 
  geom_line(aes(y = fitted), colour="blue") + 
  geom_line(aes(y = slope), colour="green")

ggplot(dd, aes(x=x)) + 
  geom_line(aes(y = slope*z), colour="black")

ggplot(dd, aes(x=x)) + 
  geom_line(aes(y = z), colour="black")

# not binned data

# linear kHz
dd<-data.frame(x=cm_dataset_2plot$Frequency_kHz)
dd$y=cm_dataset_2plot$CorticalDistance_mm
dd$z=cm_dataset_2plot$TuningWidth


cm_ticks_values <- c(0,5,10,15)
cm_ticks_labels <- round(cm_ticks_labels,digits=2)

fit <- lm(y ~ poly(x,2,raw=TRUE), dd)
dd$fitted <- fitted(fit)

dd$slope <- model.matrix(fit) %*% matrix(deriv_coef(fit), ncol=1)
# dd$slope <- exp(dd$slope)
dd$pi=dd$slope*dd$z

ggplot(dd, aes(x=x)) + 
  geom_line(aes(y = slope), colour="green")

ggplot(dd, aes(x=x)) + 
  geom_point(aes(y = y), colour="red") + 
  geom_line(aes(y = fitted), colour="blue") + 
  geom_line(aes(y = slope), colour="green")

ggplot(dd, aes(x=x)) + 
  geom_point(aes(y = slope*z), colour="black")

test <-result <- f_bin(dd$x,dd$slope,20,8)

ggplot(test, aes(x=x_mn)) + 
  geom_point(aes(y = y_mn), colour="black") +
  geom_errorbar(aes(ymin =y_mn-y_se,ymax =y_mn+y_se))+
  scale_x_continuous(breaks = cm_ticks_values, label = cm_ticks_labels) +
  labs( x = "Frequency (kHz)", y = "CMF (mm/kHz)")

ggplot(dd, aes(x=x)) + 
  geom_point(aes(y = z), colour="black")+
  scale_x_continuous(breaks = cm_ticks_values, label = cm_ticks_labels) +
  labs( x = "Frequency (kHz)", y = "Tuning Width (kHz)")

point_image <-result <- f_bin(dd$x,dd$pi,20,8)
ggplot(point_image, aes(x=x_mn)) + 
  geom_point(aes(y = y_mn), colour="black") +
  geom_errorbar(aes(ymin =y_mn-y_se,ymax =y_mn+y_se))+
  scale_x_continuous(breaks = cm_ticks_values, label = cm_ticks_labels) +
  labs( x = "Frequency (kHz)", y = "PI (mm)")

# nERB
cm_ticks_values <- c(10,20,30,40)
cm_ticks_labels <- nERB2kHz(cm_ticks_values)
cm_ticks_labels <- round(cm_ticks_labels,digits=2)

dd<-data.frame(x=cm_dataset_2plot$Frequency_nERB)
dd$y=cm_dataset_2plot$CorticalDistance_mm
dd$z=cm_dataset_2plot$TuningWidth

fit <- lm(y ~ poly(x,2,raw=TRUE), dd)
dd$fitted <- fitted(fit)

dd$slope <- model.matrix(fit) %*% matrix(deriv_coef(fit), ncol=1)
dd$slope <- nERB2kHz(dd$slope)
dd$pi=dd$slope*dd$z

ggplot(dd, aes(x=x)) + 
  geom_line(aes(y = slope), colour="green")

ggplot(dd, aes(x=x)) + 
  geom_point(aes(y = y), colour="red") + 
  geom_line(aes(y = fitted), colour="blue") + 
  geom_line(aes(y = slope), colour="green")

ggplot(dd, aes(x=x)) + 
  geom_point(aes(y = slope*z), colour="black")

test <-result <- f_bin(dd$x,dd$slope,20,8)

ggplot(test, aes(x=x_mn)) + 
  geom_point(aes(y = y_mn), colour="black") +
  geom_errorbar(aes(ymin =y_mn-y_se,ymax =y_mn+y_se)) +
  scale_x_continuous(breaks = cm_ticks_values, label = cm_ticks_labels) +
  labs( x = "Frequency (kHz)", y = "CMF (mm/kHz)")

ggplot(dd, aes(x=x)) + 
  geom_point(aes(y = z), colour="black")+
  scale_x_continuous(breaks = cm_ticks_values, label = cm_ticks_labels) +
  labs( x = "Frequency (kHz)", y = "Tuning Width (kHz)")

point_image <-result <- f_bin(dd$x,dd$pi,20,8)
ggplot(point_image, aes(x=x_mn)) + 
  geom_point(aes(y = y_mn), colour="black") +
  geom_errorbar(aes(ymin =y_mn-y_se,ymax =y_mn+y_se))+
  scale_x_continuous(breaks = cm_ticks_values, label = cm_ticks_labels) +
  labs( x = "Frequency (kHz)", y = "PI (mm)")

# nDLF
cm_ticks_values <- c(10,20,30,40)
cm_ticks_labels <- nDLF2kHz(cm_ticks_values)
cm_ticks_labels <- round(cm_ticks_labels,digits=2)

dd<-data.frame(x=cm_dataset_2plot$Frequency_nDLF)
dd$y=cm_dataset_2plot$CorticalDistance_mm
dd$z=cm_dataset_2plot$TuningWidth

fit <- lm(y ~ poly(x,2,raw=TRUE), dd)
dd$fitted <- fitted(fit)

dd$slope <- model.matrix(fit) %*% matrix(deriv_coef(fit), ncol=1)
dd$slope <- nDLF2kHz(dd$slope)
dd$pi=dd$slope*dd$z

ggplot(dd, aes(x=x)) + 
  geom_line(aes(y = slope), colour="green")

ggplot(dd, aes(x=x)) + 
  geom_point(aes(y = y), colour="red") + 
  geom_line(aes(y = fitted), colour="blue") + 
  geom_line(aes(y = slope), colour="green")

ggplot(dd, aes(x=x)) + 
  geom_point(aes(y = slope*z), colour="black")

test <-result <- f_bin(dd$x,dd$slope,20,8)

ggplot(test, aes(x=x_mn)) + 
  geom_point(aes(y = y_mn), colour="black") +
  geom_errorbar(aes(ymin =y_mn-y_se,ymax =y_mn+y_se)) +
  scale_x_continuous(breaks = cm_ticks_values, label = cm_ticks_labels) +
  labs( x = "Frequency (kHz)", y = "CMF (mm/kHz)")

ggplot(dd, aes(x=x)) + 
  geom_point(aes(y = z), colour="black")+
  scale_x_continuous(breaks = cm_ticks_values, label = cm_ticks_labels) +
  labs( x = "Frequency (kHz)", y = "Tuning Width (kHz)")

point_image <-result <- f_bin(dd$x,dd$pi,20,8)
ggplot(point_image, aes(x=x_mn)) + 
  geom_point(aes(y = y_mn), colour="black") +
  geom_errorbar(aes(ymin =y_mn-y_se,ymax =y_mn+y_se))+
  scale_x_continuous(breaks = cm_ticks_values, label = cm_ticks_labels) +
  labs( x = "Frequency (kHz)", y = "PI (mm)")


# log kHz
cm_ticks_values <- c(-3, -2, -1, 0, 1, 2, 3)
cm_ticks_labels <- exp(cm_ticks_values)
cm_ticks_labels <- round(cm_ticks_labels,digits=2)

dd<-data.frame(x=log(cm_dataset_2plot$Frequency_kHz))
dd$y=cm_dataset_2plot$CorticalDistance_mm
dd$z=cm_dataset_2plot$TuningWidth

fit <- lm(y ~ poly(x,3,raw=TRUE), dd)
dd$fitted <- fitted(fit)

dd$slope <- model.matrix(fit) %*% matrix(deriv_coef(fit), ncol=1)
dd$slope <- exp(dd$slope)
dd$pi=dd$slope*dd$z

ggplot(dd, aes(x=x)) + 
  geom_line(aes(y = slope), colour="green")

ggplot(dd, aes(x=x)) + 
  geom_point(aes(y = y), colour="red") + 
  geom_line(aes(y = fitted), colour="blue") + 
  geom_line(aes(y = slope), colour="green")

ggplot(dd, aes(x=x)) + 
  geom_point(aes(y = slope*z), colour="black")

test <-result <- f_bin(dd$x,dd$slope,20,8)

ggplot(test, aes(x=x_mn)) + 
  geom_point(aes(y = y_mn), colour="black") +
  geom_errorbar(aes(ymin =y_mn-y_se,ymax =y_mn+y_se))+
  scale_x_continuous(breaks = cm_ticks_values, label = cm_ticks_labels) +
  labs( x = "Frequency (kHz)", y = "CMF (mm/kHz)")

ggplot(dd, aes(x=x)) + 
  geom_point(aes(y = z), colour="black")+
  scale_x_continuous(breaks = cm_ticks_values, label = cm_ticks_labels) +
  labs( x = "Frequency (kHz)", y = "Tuning Width (kHz)")

point_image <-result <- f_bin(dd$x,dd$pi,20,8)

ggplot(point_image, aes(x=x_mn)) + 
  geom_point(aes(y = y_mn), colour="black") +
  geom_errorbar(aes(ymin =y_mn-y_se,ymax =y_mn+y_se))+
  scale_x_continuous(breaks = cm_ticks_values, label = cm_ticks_labels) +
  labs( x = "Frequency (kHz)", y = "PI (mm)")

# log10 kHz
cm_ticks_values <- c(-1, -0.5, 0, 0.5, 1)
cm_ticks_labels <- 10^(cm_ticks_values)
cm_ticks_labels <- round(cm_ticks_labels,digits=2)

dd<-data.frame(x=log10(cm_dataset_2plot$Frequency_kHz))
dd$y=cm_dataset_2plot$CorticalDistance_mm
dd$z=cm_dataset_2plot$TuningWidth

fit <- lm(y ~ poly(x,2,raw=TRUE), dd)
dd$fitted <- fitted(fit)

dd$slope <- model.matrix(fit) %*% matrix(deriv_coef(fit), ncol=1)
dd$slope <- 10^(dd$slope)
dd$pi=dd$slope*dd$z

ggplot(dd, aes(x=x)) + 
  geom_line(aes(y = slope), colour="green")

ggplot(dd, aes(x=x)) + 
  geom_point(aes(y = y), colour="red") + 
  geom_line(aes(y = fitted), colour="blue") + 
  geom_line(aes(y = slope), colour="green")

ggplot(dd, aes(x=x)) + 
  geom_point(aes(y = slope*z), colour="black")

test <-result <- f_bin(dd$x,dd$slope,20,8)

ggplot(test, aes(x=x_mn)) + 
  geom_point(aes(y = y_mn), colour="black") +
  geom_errorbar(aes(ymin =y_mn-y_se,ymax =y_mn+y_se))+
  scale_x_continuous(breaks = cm_ticks_values, label = cm_ticks_labels) +
  labs( x = "Frequency (kHz)", y = "CMF (mm/kHz)")

ggplot(dd, aes(x=x)) + 
  geom_point(aes(y = z), colour="black")+
  scale_x_continuous(breaks = cm_ticks_values, label = cm_ticks_labels) +
  labs( x = "Frequency (kHz)", y = "Tuning Width (kHz)")

point_image <-result <- f_bin(dd$x,dd$pi,20,8)

ggplot(point_image, aes(x=x_mn)) + 
  geom_point(aes(y = y_mn), colour="black") +
  geom_errorbar(aes(ymin =y_mn-y_se,ymax =y_mn+y_se))+
  scale_x_continuous(breaks = cm_ticks_values, label = cm_ticks_labels) +
  labs( x = "Frequency (kHz)", y = "PI (mm)")




CMF_result <- cal_CMF(result$frequencies_mean_bin,result$cortical_distances_mean_bin,result$tuning_widths_mean_bin)

ggplot(CMF_result, aes(x=frequencies, y = cortical_magnification))+
  geom_point()

ggplot(CMF_result, aes(x=frequencies, y = cortical_distances))+
  geom_point()

ggplot(CMF_result, aes(x=frequencies, y = cortical_diff))+
  geom_point()

ggplot(CMF_result, aes(x=frequencies, y = tuning_width))+
  geom_point()

ggplot(CMF_result, aes(x=frequencies, y = point_image))+
  geom_point()



#######


stim_min_nERB = 3.3589
stim_max_nERB = 33.1892
stim_middle_nERB = 18.2740

ticks_values <- c(0, 10, 20, 30, 40)
ticks_labels <- nERB2kHz(ticks_values)
ticks_labels <- round(ticks_labels,digits=2)

# Importing the dataset
# filenames <- 'Comparisions_tuning_curves.csv','Comparisions_beta_weights.csv'
ve_dataset = read.csv('Comparisions_voxel_estimates.csv')

ve_dataset %>% 
  filter(estimation == 'population Centre Frequency')  %>%
  filter(roi %in% c("Left", "Right")) ->  ve_dataset_histo

ggplot(data = ve_dataset_histo, mapping = aes(x = frequency_nERB, fill = acquistion, colour = NULL)) + 
  geom_density(alpha=0.3) +
  labs(title = "Distribution of preferred frequency", x = "Frequency (kHz)", y = "Voxel Count") +
  guides(fill=guide_legend(title=NULL))+
  labs(fill="Condition") + 
  scale_x_continuous(breaks = ticks_values, label = ticks_labels) +
  theme(legend.justification=c(1,0), 
        legend.position=c(1, 0.75),  
        legend.background = element_blank(),
        legend.key = element_blank()) 

cm_dataset = read.csv('CorticalMagnification.csv')

row.has.na <- apply(cm_dataset, 1, function(x){any(is.na(x))})
sum(row.has.na)
cm_dataset <- cm_dataset[!row.has.na,]

# cm_dataset %>% filter(Analysis == 'pRF')   %>%
#   filter(Frequency_nERB > stim_min_nERB+5 & Frequency_nERB < stim_max_nERB-5)   %>%
#   filter(r2 > 0.1) ->  cm_dataset_2plot

cm_dataset %>% filter(Analysis == 'pRF') ->  cm_dataset_2plot


ggplot(data = cm_dataset_2plot, mapping = aes(x = Frequency_nERB, y = CorticalDistance_mm, color = r2)) + 
  geom_point(alpha = 1/20) +
  ylim(0, NA) +
  geom_smooth(method = "lm", formula = y ~ x, size = 1)

ggplot(data = cm_dataset_2plot, mapping = aes(x = Frequency_nDLF, y = CorticalDistance_mm, color = r2)) + 
  geom_point(alpha = 1/20) +
  ylim(0, NA) +
  geom_smooth(method = "lm", formula = y ~ x, size = 1)

ggplot(data = cm_dataset_2plot, mapping = aes(x = log10(Frequency_kHz), y = CorticalDistance_mm, color = r2)) + 
  geom_point(alpha = 1/20) +
  ylim(0, 20) +
  geom_smooth(method = "lm", formula = y ~ x, size = 1) + 
  facet_wrap(~ Analysis, nrow = 2)

ggplot(data = cm_dataset_2plot, mapping = aes(x = Frequency_nERB, y = TuningWidth, color = r2)) + 
  geom_point(alpha = 1/20) +
  geom_smooth(method = "lm", formula = y ~ x, size = 1) + 
  facet_wrap(~ Analysis, nrow = 2)

ggplot(data = cm_dataset_2plot, mapping = aes(x = log10(Frequency_kHz), y = log10(nERB2kHz(TuningWidth)), color = r2)) + 
  geom_point(alpha = 1/20) +
  geom_smooth(method = "lm", formula = y ~ x, size = 1) + 
  facet_wrap(~ Analysis, nrow = 2)

#cm_dataset_2plot %>% mutate(CMF = CorticalDistance/Frequency_nERB) -> cm_dataset_2plot

#cm_dataset_2plot %>% mutate(PointImage = TuningWidth*CMF) -> cm_dataset_2plot
# 
# ggplot(data = cm_dataset_2plot, mapping = aes(x = Frequency_nERB, y = CMF, color = r2)) + 
#   geom_point(alpha = 1/20) +
#   geom_smooth(method = "lm", formula = y ~ poly(x, 2), size = 1) + 
#   facet_wrap(~ SubjectID, nrow = 2)
# 
# ggplot(data = cm_dataset_2plot, mapping = aes(x = Frequency_nERB, y = PointImage, color = r2)) + 
#   geom_point(alpha = 1/20) +
#   geom_smooth(method = "lm", formula = y ~ poly(x, 2), size = 1) + 
#   facet_wrap(~ SubjectID, nrow = 2)

# cm_dataset_2plot %>% mutate(CMF_kHz_mm = CorticalDistance_mm/Frequency_kHz) -> cm_dataset_2plot
# cm_dataset_2plot %>% mutate(PointImage_kHz_mm = nERB2kHz(TuningWidth)*CMF_kHz_mm) -> cm_dataset_2plot

ticks_values <- c(-1, -0.5, 0, 0.5, 1)
ticks_labels <- 10^(ticks_values)
ticks_labels <- round(ticks_labels,digits=2)





p <- nls( Frequency ~ f(CorticalDistance, a,b,c), d, start = list(a = 1, b = 1, c = 0))
q <- nls( log(Frequency) ~ f(CorticalDistance, a,b,c), d, start = list(a = 1, b = 1, c = 0))

l <- nls( Frequency_kHz ~ f_lin(CorticalDistance_norm, m,c), cm_dataset_2plot, start = list(m = 1, c = 0))
l <- nls( log(Frequency_kHz) ~ f_lin(CorticalDistance_norm, m,c), cm_dataset_2plot, start = list(m = 1, c = 0))
l <- nls( Frequency_nERB ~ f_lin(CorticalDistance_norm, m,c), cm_dataset_2plot, start = list(m = 1, c = 0))
l <- nls( Frequency_nDLF ~ f_lin(CorticalDistance_norm, m,c), cm_dataset_2plot, start = list(m = 1, c = 0))

p <- nls( Frequency_kHz ~ f_poly(CorticalDistance_norm, a,b,c), cm_dataset_2plot, start = list(a = 1, b = 1, c = 0))
tidy(p)
glance(p)
p <- nls( log(Frequency_kHz) ~ f_poly(CorticalDistance_norm, a,b,c), cm_dataset_2plot, start = list(a = 1, b = 1, c = 0))
tidy(p)
glance(p)
p <- nls( Frequency_nERB ~ f_poly(CorticalDistance_norm, a,b,c), cm_dataset_2plot, start = list(a = 1, b = 1, c = 0))
tidy(p)
glance(p)
p <- nls( Frequency_nDLF ~ f_poly(CorticalDistance_norm, a,b,c), cm_dataset_2plot, start = list(a = 1, b = 1, c = 0))
tidy(p)
glance(p)


q <- nls( log(Frequency) ~ f(CorticalDistance, a,b,c), d, start = list(a = 1, b = 1, c = 0))

tidy(p)
glance(p)



cm_dataset_2plot_sub <- filter(cm_dataset_2plot,SubjectID == 1)
result <- f_binMean(cm_dataset_2plot_sub$Frequency_nERB,cm_dataset_2plot_sub$CorticalDistance_mm,cm_dataset_2plot_sub$TuningWidth,50)
result <- f_binMean(cm_dataset_2plot$Frequency_nERB,cm_dataset_2plot$CorticalDistance_mm,cm_dataset_2plot$TuningWidth,10)

result <- f_binMean(cm_dataset_2plot$Frequency_nERB,cm_dataset_2plot$CorticalDistance_norm,cm_dataset_2plot$TuningWidth,10)
result <- f_binMean(log10(cm_dataset_2plot$Frequency_kHz),cm_dataset_2plot$CorticalDistance_norm,cm_dataset_2plot$TuningWidth,10)
result <- f_binMean(cm_dataset_2plot$Frequency_nDLF,cm_dataset_2plot$CorticalDistance_norm,cm_dataset_2plot$TuningWidth,10)

# nERB 15 bins
result <- f_binMean(cm_dataset_2plot$Frequency_nERB,cm_dataset_2plot$CorticalDistance_mm,cm_dataset_2plot$TuningWidth,20)
# nDLF 15 bins
result <- f_binMean(cm_dataset_2plot$Frequency_nDLF,cm_dataset_2plot$CorticalDistance_norm,cm_dataset_2plot$TuningWidth,15)
# lin 50 bins
result <- f_binMean(cm_dataset_2plot$Frequency_kHz,cm_dataset_2plot$CorticalDistance_mm,nERB2kHz(cm_dataset_2plot$TuningWidth),50)
# log 15 bins
result <- f_binMean(log10(cm_dataset_2plot$Frequency_kHz),cm_dataset_2plot$CorticalDistance_norm,nERB2kHz(cm_dataset_2plot$TuningWidth),15)
# log 50 bins
result <- f_binMean(log10(cm_dataset_2plot$Frequency_nDLF),cm_dataset_2plot$CorticalDistance_norm,nERB2kHz(cm_dataset_2plot$TuningWidth),50)


ggplot(result, aes(x=frequencies_mean_bin, y = tuning_widths_mean_bin))+
  geom_errorbar(aes(ymin=tuning_widths_mean_bin-tuning_widths_se_bin, ymax=tuning_widths_mean_bin+tuning_widths_se_bin)) +
  geom_point()

ggplot(result, aes(x=frequencies_mean_bin, y = cortical_distances_mean_bin))+
  geom_errorbar(aes(ymin=cortical_distances_mean_bin-cortical_distances_se_bin, ymax=cortical_distances_mean_bin+cortical_distances_se_bin)) +
  geom_point()


CMF_result <- cal_CMF(result$frequencies_mean_bin,result$cortical_distances_mean_bin,result$tuning_widths_mean_bin)

ggplot(CMF_result, aes(x=frequencies, y = cortical_magnification))+
   geom_point()

ggplot(CMF_result, aes(x=frequencies, y = cortical_distances))+
  geom_point()

ggplot(CMF_result, aes(x=frequencies, y = cortical_diff))+
  geom_point()

ggplot(CMF_result, aes(x=frequencies, y = tuning_width))+
  geom_point()

ggplot(CMF_result, aes(x=frequencies, y = point_image))+
  geom_point()
