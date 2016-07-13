##################
# Geographic Randomization - Coding
# Created by Grant Buckles
# Last Updated July 13, 2016
##################

library(stringr)

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

#Incomplete: only a zip code was entered-----------------------------------------
zip_strings <- c("^[0-9]{4}$")

incomplete_3 <- function(x){
  grepl(paste(zip_strings, collapse = "|"), x)
}
dat$zipcode_only <- incomplete_3(dat$raw_user_address)

summary(dat$zipcode_only)
select(filter(dat, zipcode_only == TRUE), raw_user_address)

#Incomplete: only a street number was entered----------------------------------
addnum_strings <- c("^([0-9]+).$")
#exclude zip codes and phone numbers
excludenum_strings <- c("^[0-9]{4}$", "^[0-9]{10}$")

incomplete_4 <- function(x){
  grepl(paste(addnum_strings, collapse = "|"), x) & 
  !grepl(paste(excludenum_strings, collapse = "|"), x)
}
dat$stnum_only <- incomplete_4(dat$raw_user_address)

summary(dat$stnum_only)
select(filter(dat, stnum_only == TRUE), raw_user_address)[0:100,]
#need to exlude large numbers too

#Incorrect: a phone number was entered----------------------------------
phonenum_strings <- c("^[0-9]{10}$")

incomplete_10 <- function(x){
  grepl(paste(phonenum_strings, collapse = "|"), x)
}
dat$phone_address <- incomplete_10(dat$raw_user_address)

summary(dat$phone_address)
select(filter(dat, phone_address == TRUE), raw_user_address)

