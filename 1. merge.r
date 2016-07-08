##################
# Geographic Randomization - Merge
# Created by Grant Buckles
# Last Updated July 6, 2016
##################

rm(list=ls())

library(foreign)
library(dplyr)

setwd("/Users/grantbuckles/Desktop/South Africa - Geographic Analayis")
alloc<-read.csv("AllGroups646.csv")
alloc.org<-read.csv("recontactSample2014-04-14_oneRowFinal.csv")
master<-read.csv("contacts-export20140617.csv")

##Check AllGroups646.csv against original allocation

alloc <- rename(alloc, msisdn = MSISDN)
alloc.org <- rename(alloc.org, msisdn = numbers)
merge.check<-full_join(alloc, alloc.org)

filter(merge.check, Name != "A", group == "group1")
#only 1 error - misisdn = 27798910129 assigned to C
filter(merge.check, Name != "B", group == "group2")
#only 1 error - misisdn = 27760320831 assigned to A
filter(merge.check, Name != "C", group == "group3")
#only 1 error - misisdn = 27835134217 assigned to B

##Merge with master file
master$msisdn<-(gsub("[[:punct:]]","",master$msisdn))
alloc$msisdn<-as.character(alloc$msisdn)
dat<-full_join(master, alloc)

#Rename treatment variable
dat<-rename(dat,address_treat = Name)

#Spot check 
select(dat,raw_user_address,raw_user_address_2,msisdn,Name)[c(225:250,1325:1350),]
