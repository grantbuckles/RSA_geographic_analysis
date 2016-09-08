##################
# Geographic Randomization - Coding
# Created by Grant Buckles
# Last Updated August 31, 2016
##################

library(stringr)

#remove test address
dat <- filter(dat, raw_user_address != "12 main street pretoria")

#observations in raw_user_address to code 
sum(dat$raw_user_address != "")
#observations in raw_user_address_2 to code
sum(dat$raw_user_address_2 != "")

#-------------------------------------------------------------------------------
  
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

#-------------------------------------------------------------------------------
  
#Code errors

#Remove successes from the data------------------------------------------------
dat_1 <- select(filter(dat, raw_user_address != ""),
                    raw_user_address, raw_user_address_2, ward)
errors <- filter(dat_1, ward == "" | ward != "" & 
                   raw_user_address_2 != "")

#-------------------------------------------------------------------------------
  

#Incomplete: only a ward number was entered------------------------------------
ward_strings <- c("^(ward [0-9]+)$", "^(ward[0-9]+)$")

incomplete_1 <- function(x){
  grepl(paste(ward_strings, collapse = "|"), x, ignore.case = TRUE )
}
errors$ward_only <- incomplete_1(errors$raw_user_address)

summary(errors$ward_only)
#select(filter(errors, ward_only == TRUE), raw_user_address)

#Incomplete: only a zip code was entered---------------------------------------
incomplete_2 <- function(x){
  grepl("^[0-9]{4}$", x)
}
errors$zipcode_only <- incomplete_2(errors$raw_user_address)

summary(errors$zipcode_only)
#select(sample_n(filter(errors, zipcode_only == TRUE), 50), raw_user_address)

#Incomplete: only a street number was entered----------------------------------
incomplete_3 <- function(x){
  grepl("^[0-9]{1,3}$", x)
}
errors$stnum_only <- incomplete_3(errors$raw_user_address)

summary(errors$stnum_only)
#select(sample_n(filter(errors, stnum_only == TRUE), 50), raw_user_address)

#Incomplete: a zip code + other information was entered------------------------
zip_plus_strings <- c("^[[:alpha:]]{1,20} [0-9]{4}$",
                      "^[[:alpha:]]{1,20} [[:alpha:]]{1,20} [0-9]{4}$")

incomplete_4 <- function(x){
  grepl(paste(zip_plus_strings, collapse = "|"), x, ignore.case = TRUE ) &
    !grepl("^box [0-9]{4}$", x, ignore.case = TRUE)
}
errors$zipcode_plus <- incomplete_4(errors$raw_user_address)

summary(errors$zipcode_plus)
#select(filter(errors, zipcode_plus == TRUE), raw_user_address)

#Incomplete: right format, not enough information
#An entry must have a minimum of three words after the street number: 
#street name = 2 words (i.e. main street, mandela road) + a city name
#Thus, any correctly formatted entry with less than three words is coded 
#as correct but incomplete

incomplete_5 <- function(x){
  grepl("^[0-9]{1,5}[[:space:]]?[[:alpha:]]{1,20}[[:space:]]?[[:alpha:]]{1,20}$", x)
}
errors$strnum_plus <- incomplete_5(errors$raw_user_address)

summary(errors$strnum_plus)
#select(filter(errors, strnum_plus == TRUE), raw_user_address)


#Combining incomplete responses into single indicator
errors$incomplete <- FALSE
errors$incomplete[errors$ward_only == TRUE | errors$zipcode_only == TRUE | 
                    errors$stnum_only == TRUE | errors$zipcode_plus ==TRUE | 
                    errors$strnum_plus ==TRUE] <- TRUE
summary(errors$incomplete)

#-------------------------------------------------------------------------------

#Incorrect: a phone number was entered (10 digits)-----------------------------
incorrect_10 <- function(x){
  grepl("^[0-9]{10}$", x)
}
errors$phone_address <- incorrect_10(errors$raw_user_address)

summary(errors$phone_address)
#select(sample_n(filter(errors, phone_address == TRUE), 50), raw_user_address)

#Incorrect: a random number was entered (not a street number or phone number)---
addnum_strings <- c("^[0-9]{5,15}$")
#exclude phone numbers
excludenum_strings <- c("^[0-9]{10}$")

incorrect_11 <- function(x){
  grepl(paste(addnum_strings, collapse = "|"), x) & 
  !grepl(paste(excludenum_strings, collapse = "|"), x)
}
errors$random_num <- incorrect_11(errors$raw_user_address)

summary(errors$random_num)
#select(sample_n(filter(errors, random_num == TRUE), 50), raw_user_address)

#Incorrect: correct format + enough words to be complete, but not recognized 
#(can be interpreted as a misspelling or fake address)
incorrect_12 <- function(x){
  grepl("^[0-9]{1,5}[[:space:]]?[[:alpha:]]+", x) &
  !grepl("^[0-9]{1,5}[[:space:]]?[[:alpha:]]{1,20}[[:space:]]?[[:alpha:]]{1,20}$", x)
}
errors$unrecog <- incorrect_12(errors$raw_user_address)

summary(errors$unrecog)
#select(filter(errors, unrecog == TRUE), raw_user_address)

#Incorrect: a P.O. Box was entered first--------------------------------------
box_strings <- c("^p o box [0-9]+", "^p\\.o box [0-9]+", 
                 "^p\\.o\\.box [0-9]+", "^po box [0-9]+", 
                 "^po\\. box [0-9]+", "^po\\.box [0-9]+","^pobox[0-9]+", 
                 "^box [0-9]+", "^po bx[0-9]+", "^p\\.box[0-9]+",
                 "^P\\.O\\. BOX+",  "^P\\.0\\. +")
incorrect_13 <- function(x){
  grepl(paste(box_strings, collapse = "|"), x, ignore.case = TRUE)
}
errors$pob_only <- incorrect_13(errors$raw_user_address)

summary(errors$pob_only)
#select(filter(errors, pob_only == TRUE), raw_user_address)

#Incorrect: a ward number was entered first or misspelled----------------------
ward_strings2 <- c("^ward", "^wart", "^ward$", "^ward\\.", "^ward[0-9]",
                   "^word", "^wrd")

incorrect_14 <- function(x){
  grepl(paste(ward_strings2, collapse = "|"), x, ignore.case = TRUE) &
  !grepl(paste(ward_strings, collapse = "|"), x, ignore.case = TRUE)
}
errors$ward_first <- incorrect_14(errors$raw_user_address)

summary(errors$ward_first)
#select(filter(errors, ward_first == TRUE), raw_user_address)


#Combining incorrect responses into single indicator
errors$incorrect <- FALSE
errors$incorrect[errors$phone_address == TRUE | errors$random_num == TRUE |
                 errors$unrecog == TRUE | errors$pob_only == TRUE |
                   errors$ward_first == TRUE] <- TRUE
summary(errors$incorrect)


#-------------------------------------------------------------------------------


#Extraneous: entered a party name
party_strings <- c("\\bANC\\b", "\\beff\\b", "\\bda\\b", "\\bIFP\\b")

extraneous_10 <- function(x){
  grepl(paste(party_strings, collapse = "|"), x, ignore.case = TRUE) 
}
errors$party_name <- extraneous_10(errors$raw_user_address)

summary(errors$party_name)
#select(filter(errors, party_name == TRUE), raw_user_address)

#Extraneous: expletives
expl_strings <- c("\\bfuck\\b", "\\bfuk\\b", "\\bshit\\b", "\\bhell\\b")

extraneous_11 <- function(x){
  grepl(paste(expl_strings, collapse = "|"), x, ignore.case = TRUE) 
}
errors$expletive <- extraneous_11(errors$raw_user_address)

summary(errors$expletive)
#select(filter(errors, expletive == TRUE), raw_user_address)

#Extraneous: single punctuation or letter
single_strings <- c("^[[:punct:]]{1,3}$", "^[[:alpha:]]{1}$")

extraneous_12 <- function(x){
  grepl(paste(single_strings, collapse = "|"), x)
}
errors$singles <- extraneous_12(errors$raw_user_address)

summary(errors$singles)
#select(filter(errors, singles == TRUE), raw_user_address)

#Extraneous: personal responses
response_strings <- c("\\bhi\\b", "\\bhello\\b", "\\bhy\\b", "\\bha\\b", 
                      "\\thanks\\b", "\\thnx\\b", 
                      "\\bhelp\\b", "^i ", "^y ", "^why ", "^who ",
                      "^when ", "^where ", "^what ", "^wat ", "^wt ")

extraneous_13 <- function(x){
  grepl(paste(response_strings, collapse = "|"), x, ignore.case = TRUE)
}
errors$yesno <- extraneous_13(errors$raw_user_address)

summary(errors$yesno)
#select(filter(errors, yesno == TRUE), raw_user_address)

#Extraneous: entered response related to other portions of survey
other_strings <- c("^[0-9]\\.", "^\\*", "^back$", "^exit$",
                   "\\byes\\b", "\\byebo\\b", "^no$", "^nee$",
                   "\\bja\\b", "\\bmaybe\\b",
                   "\\bok\\b", "\\bokay\\b", "\\bvip\\b",
                   "\\boky\\b", "\\bokey\\b")

extraneous_14 <- function(x){
  grepl(paste(other_strings, collapse = "|"), x, ignore.case = TRUE)
}
errors$other <- extraneous_14(errors$raw_user_address)

summary(errors$other)
#select(filter(errors, other == TRUE), raw_user_address)


#Combining extraneous responses into single indicator
errors$extraneous <- FALSE
errors$extraneous[errors$party_name == TRUE | errors$expletive == TRUE | 
               errors$singles == TRUE | errors$yesno == TRUE |
               errors$other == TRUE] <- TRUE
summary(errors$extraneous)

#------------------------------------------------------------------
remaining <- select(filter(errors, incorrect == FALSE, incomplete == FALSE, extraneous == FALSE), raw_user_address)
