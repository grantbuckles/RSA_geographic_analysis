##################
# Geographic Randomization - Merge
# Created by Grant Buckles
# Last Updated July 12, 2016
##################

rm(list=ls())

library(foreign)
library(dplyr)

#setwd("/Users/grantbuckles/Desktop/RSA_geographic_analysis")
data_dir <- "/Users/aaronerlich/Dropbox/RSA_RCT_SHARE/PK/data_raw"
allocation <- read.csv(file.path(data_dir, "AllGroups646.csv"))
allocation_orig <- read.csv(file.path(data_dir,"recontactSample2014-04-14_oneRowFinal.csv"))
master <- read.csv(file.path(data_dir, "contacts-export20140617.csv"))

#Check AllGroups646.csv against original allocation---------------------------
allocation <- rename(allocation, msisdn = MSISDN)
allocation_orig <- rename(allocation_orig, msisdn = numbers)
check_merged <- full_join(allocation, allocation_orig)

filter(check_merged, Name != "A", group == "group1")
#only 1 error - misisdn = 27798910129 assigned to C
filter(check_merged, Name != "B", group == "group2")
#only 1 error - misisdn = 27760320831 assigned to A
filter(check_merged, Name != "C", group == "group3")
#only 1 error - misisdn = 27835134217 assigned to B

#Merge with master file---------------------------
master$msisdn <- (gsub("[[:punct:]]", "", master$msisdn))
allocation$msisdn <- as.character(allocation$msisdn)
dat <- full_join(master, allocation)

#Rename treatment variable---------------------------
dat <- rename(dat, address_treat = Name)

#Spot check 
select(dat, raw_user_address, raw_user_address_2, msisdn, address_treat)[c(225:250,1325:1350),]
