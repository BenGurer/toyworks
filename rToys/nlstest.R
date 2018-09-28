# looking at non-linear least squares.
#
# 

library(tidyverse)

fname <- "CorticalMagnification.csv"
d <- read_csv(fname)

# quick plot

d %>% 
  filter(Analysis == "pRF") %>% 
  ggplot(aes(x = CorticalDistance, y = Frequency)) + geom_point(alpha=0.1) +
   geom_smooth() + geom_smooth(method = "lm", color="red")

# fit a function to the data

# nls(formula, data, start, control, algorithm,
#     trace, subset, weights, na.action, model,
#     lower, upper, ...)

f <- function(CorticalDistance, a, b, c) {
  
  pred <- a*CorticalDistance^2 + b *CorticalDistance + c
  
}



p <- nls( Frequency ~ f(CorticalDistance, a,b,c), d, start = list(a = 1, b = 1, c = 0))
q <- nls( log(Frequency) ~ f(CorticalDistance, a,b,c), d, start = list(a = 1, b = 1, c = 0))
# erb(Frequncy)
# package broom for tidy summary of model fits...



##
# grab all filenames.
filenames <- list.files(pattern="*.csv")

# function that takes as an input -> filename... and produces PDF of ggplot



