##################
# Geographic Randomization - Coding
# Created by Grant Buckles
# Last Updated August 26, 2016
##################

library(stringr)

#remove test address
dat <- filter(dat, raw_user_address != "12 main street pretoria")

#observations in raw_user_address to code 
sum(dat$raw_user_address != "")
#observations in raw_user_address_2 to code
sum(dat$raw_user_address_2 != "")

#Successful entries (i.e. those successfully assigned a value for "ward")------ 
successes <- select(filter(dat, ward != "", ward != "unknown"), 
                    raw_user_address, raw_user_address_2, ward)

#success on first try----------------------------------------------------------
success_1 <- function(x){
  !grepl(" ", x)
}
successes$initial_entry <- success_1(successes$raw_user_address_2)
summary(successes$initial_entry)

#Next, categorize the ways in which these errors were produced.----------------

##reason for success: full address 
true_1 <- function(x){
  grepl("^[0-9]{1,5}[[:space:]]?[[:alpha:]]+", x)
}
successes$true_full <- true_1(successes$raw_user_address)
summary(successes$true_full)

##reason for success: zip code 
true_2 <- function(x){
  grepl("[0-9]{4}", x) &
    !grepl("^[0-9]{1,5}[[:space:]]?[[:alpha:]]+", x) 
  }
successes$true_zip <- true_2(successes$raw_user_address)
summary(successes$true_zip)

#reason for success: city name/other area identifier only
exclude_strings <- c("^[0-9]{4}$", "[0-9]{4}$", "[0-9]{4}",
                      "^[0-9]{1,5}[[:space:]]?[[:alpha:]]+")
true_3 <- function(x){
  grepl("[[:alpha:]]{3,30}", x) &
    !grepl(paste(exclude_strings, collapse = "|"), x)
}
successes$true_city <- true_3(successes$raw_user_address)
summary(successes$true_city)

#Some of these successes are clearly erroneous - namely those based off of 
#entries of numbers that are uninformative (i.e. not zip codes or ward numbers)
#and two character words.

false_strings <- c("^[0-9]{1,3}$", "^[[:punct:]]$", "^[[:alpha:]]{1,2}$")
false_1 <- function(x){
  grepl(paste(false_strings, collapse = "|"), x)
}
successes$false_success <- false_1(successes$raw_user_address)
summary(successes$false_success)

#Uncategorized reasons for a successful match
successes$misc <- FALSE
successes$misc[successes$true_full == FALSE & 
                 successes$true_zip == FALSE & 
                 successes$true_city == FALSE & 
                 successes$false_success == FALSE] <- TRUE
summary(successes$misc)

#Code errors

#Remove successes from the data------------------------------------------------------
dat_1 <- select(filter(dat, raw_user_address != ""),
                    raw_user_address, raw_user_address_2, ward)
errors <- filter(dat_1, ward == "" | ward != "" & 
                   raw_user_address_2 != "")

#Incomplete: only a P.O. Box was entered---------------------------------------
box_strings <- c("^p o box [0-9]+$", "^p\\.o box [0-9]+$", 
                 "^p\\.o\\.box [0-9]+$", "^po box [0-9]+$", 
                 "^po\\. box [0-9]+$", "^po\\.box [0-9]+$","^pobox[0-9]+$" )
incomplete_1 <- function(x){
 grepl(paste(box_strings, collapse = "|"), x)
}
errors$pob_only <- incomplete_1(errors$raw_user_address)

summary(errors$pob_only)
select(filter(errors, pob_only == TRUE), raw_user_address)

#Incomplete: only a ward number was entered------------------------------------
ward_strings <- c("^(ward [0-9]+)$", "^(ward[0-9]+)$")

incomplete_2 <- function(x){
  grepl(paste(ward_strings, collapse = "|"), x)
}
errors$ward_only <- incomplete_2(errors$raw_user_address)

summary(errors$ward_only)
select(filter(errors, ward_only == TRUE), raw_user_address)

#Incomplete: only a zip code was entered---------------------------------------
incomplete_3 <- function(x){
  grepl("^[0-9]{4}$", x)
}
errors$zipcode_only <- incomplete_3(errors$raw_user_address)

summary(errors$zipcode_only)
select(sample_n(filter(errors, zipcode_only == TRUE), 50), raw_user_address)

#Incomplete: only a street number was entered----------------------------------
addnum_strings <- c("^[0-9]{1,3}$")

incomplete_4 <- function(x){
  grepl("^[0-9]{1,3}$", x)
}
errors$stnum_only <- incomplete_4(errors$raw_user_address)

summary(errors$stnum_only)
select(sample_n(filter(errors, stnum_only == TRUE), 50), raw_user_address)


#Combining incomplete responses into single indicator
errors$incomplete <- NULL
errors$incomplete[errors$pob_only == TRUE |errors$ward_only == TRUE |
                    errors$zipcode_only == TRUE | 
                    errors$stnum_only == TRUE] <- TRUE
summary(errors$incomplete)


#Incorrect: a phone number was entered (10 digits)-----------------------------
incomplete_10 <- function(x){
  grepl("^[0-9]{10}$", x)
}
errors$phone_address <- incomplete_10(errors$raw_user_address)

summary(errors$phone_address)
select(sample_n(filter(errors, phone_address == TRUE), 50), raw_user_address)

#Incorrect: a random number was entered (not a street number or phone number---
addnum_strings <- c("^[0-9]{5,15}$")
#exclude phone numbers
excludenum_strings <- c("^[0-9]{10}$")

incomplete_11 <- function(x){
  grepl(paste(addnum_strings, collapse = "|"), x) & 
  !grepl(paste(excludenum_strings, collapse = "|"), x)
}
errors$random_num <- incomplete_11(errors$raw_user_address)

summary(errors$random_num)
select(sample_n(filter(errors, random_num == TRUE), 50), raw_user_address)

#Combining incorrect responses into single indicator
errors$incorrect <- NULL
errors$incorrect[errors$phone_address == TRUE |errors$random_num == TRUE] <- TRUE
summary(errors$incorrect)
