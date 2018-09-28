# Group analysis: 7T
library(tidyverse)
library(cowplot)
library(corrplot)
library(scales) # to access break formatting functions
library(RColorBrewer)
library(modelr)
library(plyr); library(dplyr)

# convert nERB to kHz
nERB2kHz <- function(nERB) {
  
  # converts nERB (ERB numbers) to  kHz
  A = 24.7/1000; B = 4.37;
  kHz <- 1/B*(exp(A*B*nERB)-1);
  
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

# set working directory to correct folder
# setwd("E:/OneDrive - The University of Nottingham/data/hearingLossSimulation/groupAnalysis")

setwd("C:/Users/bengu/Google Drive/data/CorticalMagnification/groupAnalysis")
# HRF

hrf_dataset = read.csv('HRF_est.csv')

hrf_dataset_sum <-summarySE(hrf_dataset,measurevar="hrf_Est",groupvars=c("hrf_Time"))

# labels
hrf_bin_values <- select(hrf_dataset_sum,hrf_Time) %>% distinct(hrf_Time) %>% t() %>% drop()
hrf_bin_values <- hrf_bin_values[c(1,3,5,7,9)]
hrf_ticks_labels <- round(hrf_bin_values,digits=2)
text_size = 9

# plot data
hrf_plot <- ggplot(hrf_dataset_sum, aes(x=hrf_Time, y=hrf_Est)) + 
  geom_ribbon(aes(ymin=hrf_Est-se, ymax=hrf_Est+se), alpha=0.1, linetype="blank") +
  geom_line() +
  geom_point()

# add labels
hrf_plot <- hrf_plot +
  scale_x_continuous(breaks = hrf_bin_values, label = hrf_ticks_labels) +
  labs( x = "Time (seconds)", y = "Normalised Beta Weight (arb. units)") +
  theme(legend.position="none") +
  scale_colour_brewer(palette = "Set1") + 
  scale_fill_brewer(palette = "Set1")

# set sizes
hrf_plot <- hrf_plot +
  theme(axis.text.x = element_text(colour="grey20",size=text_size,angle=0,hjust=0.5,vjust=0,face="plain"),
        axis.text.y = element_text(colour="grey20",size=text_size,angle=0,hjust=0,vjust=0.5,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=text_size,angle=0,hjust=0.5,vjust=0.5,face="plain"),
        axis.title.y = element_text(colour="grey20",size=text_size,angle=90,hjust=0.5,vjust=0.5,face="plain"))

hrf_plot

# save plot
save_plot("7T_HRF.png", hrf_plot, device = "png", base_height = NULL,
          base_aspect_ratio = 1.1, base_width =  3.34646,dpi = 450)
# 
# ggplot(data = hrf_dataset, mapping = aes(x = hrf_Time, y = hrf_EstNorm, color = as.character(Subject))) + 
#   geom_point() +
#   geom_smooth()
# 
# ggplot(data = hrf_dataset, mapping = aes(x = hrf_Time, y = hrf_EstNorm)) + 
#   geom_point() +
#   geom_smooth()
# 
# ggplot(data = hrf_dataset, mapping = aes(x = hrf_Time, y = hrf_EstNorm)) + 
#   geom_point() +
#   geom_line() +
#   facet_wrap(~ Subject, nrow = 2)


# hrf tw
hrftw_dataset = read.csv('HRFTW_est.csv')

hrftw_dataset <- mutate(hrftw_dataset,abs_freq = abs(hrf_freq))
  
hrftw_dataset_sum <- summarySE(hrftw_dataset,measurevar="hrf_Est",groupvars=c("hrf_Time", "abs_freq"))



# labels
hrf_bin_values <- select(hrf_dataset_sum,hrf_Time) %>% distinct(hrf_Time) %>% t() %>% drop()
hrf_bin_values <- hrf_bin_values[c(1,3,5,7,9)]
hrf_ticks_labels <- round(hrf_bin_values,digits=2)
text_size = 9

# plot data
# hrftw_plot <- ggplot(hrftw_dataset_sum, aes(x=hrf_Time, y=hrf_Est)) + 
#   geom_ribbon(aes(ymin=hrf_Est-se, ymax=hrf_Est+se), alpha=0.1, linetype="blank") +
#   geom_line() +
#   geom_point() +
#   facet_wrap(~ abs(abs_freq), ncol= 1) +
#   theme(strip.background = element_blank(),
#         strip.text.x = element_blank())

# plot data
hrftw_plot <- ggplot(hrftw_dataset_sum, aes(x=hrf_Time, y=hrf_Est, colour= as.character(1-(abs_freq/10)), fill= as.character(1-(abs_freq/10)))) + 
  geom_ribbon(aes(ymin=hrf_Est-se, ymax=hrf_Est+se), alpha=0.5, linetype="blank") +
  geom_line() +
  geom_point() +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank())

# add labels
hrftw_plot <- hrftw_plot +
  scale_x_continuous(breaks = hrf_bin_values, label = hrf_ticks_labels) +
  labs( x = "Time (seconds)", y = "Beta Weight (arb. units)") +
  theme(strip.background =element_rect(fill="white")) + 
  scale_fill_manual(name="Distance from pCF", 
                      labels = c("7", 
                                 "6", 
                                 "5",
                                 "4", 
                                 "3", 
                                 "2", 
                                 "1", 
                                 "0"), 
                    values = c("0.3"="#fff7fb", 
                               "0.4"="#ece7f2", 
                               "0.5"="#d0d1e6",
                               "0.6"="#a6bddb", 
                               "0.7"="#74a9cf", 
                               "0.8"="#3690c0",
                               "0.9"="#0570b0", 
                               "1"="#034e7b")) +
  scale_colour_manual(name="Distance from pCF", 
                    labels = c("7", 
                               "6", 
                               "5",
                               "4", 
                               "3", 
                               "2", 
                               "1", 
                               "0"), 
                    values = c("0.3"="#fff7fb", 
                               "0.4"="#ece7f2", 
                               "0.5"="#d0d1e6",
                               "0.6"="#a6bddb", 
                               "0.7"="#74a9cf", 
                               "0.8"="#3690c0",
                               "0.9"="#0570b0", 
                               "1"="#034e7b"))


# set sizes
hrftw_plot <- hrftw_plot +
  theme(axis.text.x = element_text(colour="grey20",size=text_size,angle=0,hjust=0.5,vjust=0,face="plain"),
        axis.text.y = element_text(colour="grey20",size=text_size,angle=0,hjust=0,vjust=0.5,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=text_size,angle=0,hjust=0.5,vjust=0.5,face="plain"),
        axis.title.y = element_text(colour="grey20",size=text_size,angle=90,hjust=0.5,vjust=0.5,face="plain"))

hrftw_plot

# save plot
save_plot("7T_hrftw.png", hrftw_plot, device = "png", base_height = NULL,
          base_aspect_ratio = 1.1, base_width = 3.34646,dpi = 450)



hrftw_dataset_sum2 <- summarySE(hrftw_dataset,measurevar="hrf_Est",groupvars=c("hrf_Time", "hrf_freq"))

ggplot(hrftw_dataset_sum2, aes(x=hrf_Time, y=hrf_freq, z = hrf_Est))+ 
  geom_raster(aes(fill = hrf_Est)) +
  geom_contour(colour = "white")


ggplot(data = hrftw_dataset, mapping = aes(x = hrf_Time, y = hrf_Est, color = as.character(Subject))) + 
  geom_line() +
  facet_wrap(~ hrf_freq)

ggplot(data = hrftw_dataset, mapping = aes(x = hrf_Time, y = hrf_EstNorm)) + 
  geom_smooth() +
  facet_wrap(~ hrf_freq)

# hrf params av
hrfparamsav_dataset = read.csv('HRFparams_av.csv')

ggplot(data = filter(hrfparamsav_dataset,Function_name != "diffofGamma"), mapping = aes(x = Time, y = Data, color = Function_name)) + 
  geom_line() + 
  stat_summary(data = hrf_dataset, aes(x=hrf_Time, y = hrf_EstNorm, color ="Deconvolution"), fun.y=mean, geom="line")
