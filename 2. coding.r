##################
# Geographic Randomization - Coding
# Created by Grant Buckles
# Last Updated July 8, 2016
##################

#create variable for address category
dat$add.cat<-NA

#Coding responses 0-10
levels(dat$raw_user_address)[1:10]
dat$add.cat[dat$raw_user_address=="______________________________+1_"]<-41
dat$add.cat[dat$raw_user_address=="_P.O.BOX  647.TSHAULU 0987 .NEMAKONDE T 59 .0713037300_"]<-32 #wrong format
dat$add.cat[dat$raw_usder_address=="," ]<-41
dat$add.cat[dat$raw_usder_address==",1"]<-41
dat$add.cat[dat$raw_usder_address== ";"]<-41
dat$add.cat[dat$raw_usder_address==";)"]<-41
dat$add.cat[dat$raw_usder_address==": 013833 Msogwaba"]<-32 #wrong format: colon at the beginning
dat$add.cat[dat$raw_usder_address==": 1001 mokone block stinkwater"]<-32 #wrong format: colon at the beginning
dat$add.cat[dat$raw_usder_address== ":?"]<-41
dat$add.cat[dat$raw_usder_address== ":("]<-41
