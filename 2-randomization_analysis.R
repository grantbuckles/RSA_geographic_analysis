##################
# Geographic Randomization - Statistical Analysis of Randomization
# Created by Grant Buckles
# Last Updated October 20, 2016
##################

library(stringr)
library(xtable)
library(multcomp)
library(timeDate)
library(chron)
require(descr)

#Setup for .tex output
workd <- "/Users/grantbuckles/Desktop/RSA_geographic_analysis"
dir.create(paste0("tex_", Sys.time()))
texFile <- tail(list.files(path=workd, pattern ="tex_2016.*"),1)[1]
sink(file = paste0(texFile, "/analysis_dump.txt"), append = FALSE, type = c("output", "message"),
     split = TRUE)
invisible(file.copy(from="output_template", to=paste0(texFile,"/output_template", Sys.Date(),".tex")))

#cat("Text")
#print(xtable(table1,caption = 'Caption'),  
#      caption.placement = 'top')

#------------------------------------------------------------------------------
# Prepare data
#------------------------------------------------------------------------------

## Extract randomized sample----------------------------------------------------
dat1 <- filter(dat, address_treat != "")

### Remove those created after April 17
dat1 <- filter(dat1, as.Date(created_at) < '2014-04-17')

### Remove duplicates (2nd entries associated with a number)
dat1 <- filter(dat1, duplicated(dat1$msisdn) == FALSE)

## Create measures of engagement-----------------------------------------------

### Count variable (all instances of engagement)
#### This variables uses the 'it_' prefix variables (columns 49-109), which 
#### capture the time/date a response was entered; the variables that capture 
#### the time/date that a question was asked are excluded.

dat1$total.entries <- rowSums(dat1[c(49:56, 58, 61:63, 65, 67, 69, 71, 73:87, 89:109)] != "")
table(dat1$total.entries)

### Create dichotomous variable of engagement

dat1$engaged <- NULL
dat1$engaged[dat1$total.entries > 0] <- 1
dat1$engaged[dat1$total.entries == 0] <- 0

#------------------------------------------------------------------------------
# Descriptive statistics
#------------------------------------------------------------------------------

## 1. Differences in address entry between B & C groups------------------------

### Create variable: Entered an address (doest not consider address quality)
dat2 <- filter(dat1, address_treat == "B" | address_treat == "C")

dat2$engage_address[dat2$raw_user_address != ""] <- "Entered Address"
dat2$engage_address[dat2$raw_user_address == ""] <- "Did Not Enter Address"
#check variables
select(dat2, engage_address, raw_user_address, address_treat)[0:50,]

### Descriptive statistics
address_entry <- crosstab(dat2$engage_address, dat2$address_treat,
         dnn = c("Opinion", "Education"), prop.c = TRUE, prop.r = FALSE, 
         prop.t = FALSE, prop.chisq = FALSE, chisq = FALSE, plot = FALSE)
address_entry
print(xtable(address_entry))

## 2. Furthur engagement-------------------------------------------

enga <- crosstab(dat1$engaged, dat1$address_treat,
         dnn = c("Engaged", "Treatment Group"), prop.c = TRUE, prop.r = TRUE, 
         prop.t = FALSE, prop.chisq = FALSE, chisq = FALSE, plot = FALSE)
enga
print(xtable(enga))


#------------------------------------------------------------------------------
# Analysis
#------------------------------------------------------------------------------

## 1. Test of Proportions------------------------------------------------------

### Example: Engagement Question 
prop.test(table(dat1$engaged, dat1$address_treat)[,c(1,2)], 
          correct=FALSE)
prop.test(table(dat1$engaged, dat1$address_treat)[,c(2,3)], 
          correct=FALSE)
prop.test(table(dat1$engaged, dat1$address_treat)[,c(1,3)], 
          correct=FALSE)

cat("The difference between Group A (no lookup) and the other two groups is statistically 
    significant.")

## 2. Logistic Regression------------------------------------------------------

### Variables 
dat1$feeling_therm_rd1[dat1$pre_thermometer_round_1_reply == ""] <- 0
dat1$feeling_therm_rd1[dat1$pre_thermometer_round_1_reply != ""] <- 1
dat1$feeling_therm_rd1 <- as.numeric(dat1$feeling_therm_rd1)

dat1$black[dat1$answerwin_question_race == "black_african"] <- 1
dat1$black[dat1$answerwin_question_race != "black_african"] <- 0

dat1$male[dat1$answerwin_question_gender == "male"] <- 1
dat1$male[dat1$answerwin_question_gender != "male"] <- 0

dat1$twenties[dat1$answerwin_question_age == "20-29"] <- 1
dat1$twenties[dat1$answerwin_question_age != "20-29"] <- 0

### Plain Logit - Feeling Thermometer Response

mod1.1 <- glm(feeling_therm_rd1 ~ black + male + twenties,
              family=binomial(link="probit"), data = dat1)
summary(mod1.1)

#Note: Adjust P-values for Multiple Comparisons
