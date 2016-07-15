##################
# Geographic Randomization - Coding
# Created by Grant Buckles
# Last Updated July 15, 2016
##################

library(stringr)

sum(dat$raw_user_address != "")
#observations in raw_user_address to code: 21927 
sum(dat$raw_user_address_2 != "")
#observations in raw_user_address_2 to code: 1265

#Incomplete: only a P.O. Box was entered---------------------------------------
box_strings <- c("^p o box [0-9]+$", "^p\\.o box [0-9]+$", 
                 "^p\\.o\\.box [0-9]+$", "^po box [0-9]+$", 
                 "^po\\. box [0-9]+$", "^po\\.box [0-9]+$","^pobox[0-9]+$" )
incomplete_1 <- function(x){
 grepl(paste(box_strings, collapse = "|"), x)
}
dat$pob_only <- incomplete_1(dat$raw_user_address)

summary(dat$pob_only)
select(filter(dat, pob_only == TRUE), raw_user_address)

#Incomplete: only a ward number was entered------------------------------------
ward_strings <- c("^(ward [0-9]+)$", "^(ward[0-9]+)$")

incomplete_2 <- function(x){
  grepl(paste(ward_strings, collapse = "|"), x)
}
dat$ward_only <- incomplete_2(dat$raw_user_address)

summary(dat$ward_only)
select(filter(dat, ward_only == TRUE), raw_user_address)

#Incomplete: only a zip code was entered---------------------------------------
zip_strings <- c("^[0-9]{4}$")

incomplete_3 <- function(x){
  grepl(paste(zip_strings, collapse = "|"), x)
}
dat$zipcode_only <- incomplete_3(dat$raw_user_address)

summary(dat$zipcode_only)
select(sample_n(filter(dat, zipcode_only == TRUE), 50), raw_user_address)

#Incomplete: only a street number was entered----------------------------------
addnum_strings <- c("^[0-9]{1,3}$")

incomplete_4 <- function(x){
  grepl(paste(addnum_strings, collapse = "|"), x)
}
dat$stnum_only <- incomplete_4(dat$raw_user_address)

summary(dat$stnum_only)
select(sample_n(filter(dat, stnum_only == TRUE), 50), raw_user_address)



#Incorrect: a phone number was entered (10 digits)-----------------------------
phonenum_strings <- c("^[0-9]{10}$")

incomplete_10 <- function(x){
  grepl(paste(phonenum_strings, collapse = "|"), x)
}
dat$phone_address <- incomplete_10(dat$raw_user_address)

summary(dat$phone_address)
select(sample_n(filter(dat, phone_address == TRUE), 50), raw_user_address)

#Incorrect: a random number was entered (not a street number or phone number---
addnum_strings <- c("^[0-9]{5,15}$")
#exclude phone numbers
excludenum_strings <- c("^[0-9]{10}$")

incomplete_11 <- function(x){
  grepl(paste(addnum_strings, collapse = "|"), x) & 
  !grepl(paste(excludenum_strings, collapse = "|"), x)
}
dat$random_num <- incomplete_11(dat$raw_user_address)

summary(dat$random_num)
select(sample_n(filter(dat, random_num == TRUE), 50), raw_user_address)
