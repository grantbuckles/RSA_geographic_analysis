##################
# Geographic Randomization - Statistical Analysis of Randomization
# Created by Grant Buckles
# Last Updated September 22, 2016
##################

library(stringr)

#Extract sample from randomization
dat1 <- filter(dat, address_treat != "")

