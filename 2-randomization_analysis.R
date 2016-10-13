##################
# Geographic Randomization - Statistical Analysis of Randomization
# Created by Grant Buckles
# Last Updated October 11, 2016
##################

library(stringr)
library(xtable)
library(multcomp)
require(descr)
#Extract sample from randomization
dat1 <- filter(dat, address_treat != "")

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

## 2. Additional forms of engagement-------------------------------------------

### Demographics - Age

age <- crosstab(dat1$answerwin_question_age, dat1$address_treat,
         dnn = c("Opinion", "Education"), prop.c = TRUE, prop.r = TRUE, 
         prop.t = FALSE, prop.chisq = FALSE, chisq = FALSE, plot = FALSE)
age
print(xtable(age))


### Demographics - Gender

gender <- crosstab(dat1$answerwin_question_gender, dat1$address_treat,
         dnn = c("Opinion", "Education"), prop.c = TRUE, prop.r = TRUE, 
         prop.t = FALSE, prop.chisq = FALSE, chisq = FALSE, plot = FALSE)
gender
print(xtable(gender))


### Demographics - Race

race <- crosstab(dat1$answerwin_question_race, dat1$address_treat,
                   dnn = c("Opinion", "Education"), prop.c = TRUE, 
                 prop.r = TRUE, prop.t = FALSE, prop.chisq = FALSE, 
                 chisq = FALSE, plot = FALSE)
race
print(xtable(race))


### VIP - Complete
dat1$completedVIP[dat1$vip_complete == ""] <- "Did Not Complete"
dat1$completedVIP[dat1$vip_complete != ""] <- "Completed"

VIP <- crosstab(dat1$completedVIP, dat1$address_treat,
                 dnn = c("Opinion", "Education"), prop.c = TRUE, 
                prop.r = FALSE, prop.t = FALSE, prop.chisq = FALSE, 
                chisq = FALSE, plot = FALSE)
VIP
print(xtable(VIP))



### VIP - Completed First Question (minimal engagemnt)

VIP_q1 <- crosstab(dat1$vip_question_1, dat1$address_treat,
                dnn = c("Opinion", "Education"), prop.c = TRUE, prop.r = FALSE,
                prop.t = FALSE, prop.chisq = FALSE, chisq = FALSE, 
                plot = FALSE)
VIP_q1
print(xtable(VIP_q1))


### Whatsup? - Complete

dat1$completed_whatsup[dat1$whatsup_complete == ""] <- "Did Not Complete"
dat1$completed_whatsup[dat1$whatsup_complete != ""] <- "Completed"

whatsup <- crosstab(dat1$completed_whatsup, dat1$address_treat,
                dnn = c("Opinion", "Education"), prop.c = TRUE, prop.r = FALSE,
                prop.t = FALSE, prop.chisq = FALSE, chisq = FALSE, 
                plot = FALSE)
whatsup
print(xtable(whatsup))

### Whatsup? - Completed First Question (minimal engagemnt)

whatsup_q1 <- crosstab(dat1$whatsup_question_food_to_eat, dat1$address_treat,
                    dnn = c("Opinion", "Education"), prop.c = TRUE, 
                    prop.r = FALSE, prop.t = FALSE, prop.chisq = FALSE, 
                    chisq = FALSE, plot = FALSE)
whatsup_q1
print(xtable(whatsup_q1))

### Feeling Thermometer - Round 1
dat1$feeling_therm_rd1[dat1$pre_thermometer_round_1_reply == ""] <- "Did Not Answer"
dat1$feeling_therm_rd1[dat1$pre_thermometer_round_1_reply != ""] <- "Answered"

therm_1 <- crosstab(dat1$feeling_therm_rd1, dat1$address_treat,
                       dnn = c("Opinion", "Education"), prop.c = TRUE, 
                    prop.r = FALSE, prop.t = FALSE, prop.chisq = FALSE, 
                    chisq = FALSE, plot = FALSE)
therm_1
print(xtable(therm_1))

### Feeling Thermometer - Round 2
dat1$feeling_therm_rd2[dat1$pre_thermometer_round_2_reply == ""] <- "Did Not Answer"
dat1$feeling_therm_rd2[dat1$pre_thermometer_round_2_reply != ""] <- "Answered"

therm_2 <- crosstab(dat1$feeling_therm_rd2, dat1$address_treat,
                    dnn = c("Opinion", "Education"), prop.c = TRUE, 
                    prop.r = FALSE, prop.t = FALSE, prop.chisq = FALSE, 
                    chisq = FALSE, plot = FALSE)
therm_2
print(xtable(therm_2))

### Engagement Question

engaged <- crosstab(dat1$engagement_question, dat1$address_treat,
                    dnn = c("Opinion", "Education"), prop.c = TRUE, 
                    prop.r = FALSE, prop.t = FALSE, prop.chisq = FALSE, 
                    chisq = FALSE, plot = FALSE)
engaged
print(xtable(engaged))

#------------------------------------------------------------------------------
# Analysis
#------------------------------------------------------------------------------

## 1. Test of Proportions------------------------------------------------------

### Example: Engagement Question 
prop.test(table(dat1$engagement_question, dat1$address_treat)[,c(1,2)], 
          correct=FALSE)
prop.test(table(dat1$engagement_question, dat1$address_treat)[,c(2,3)], 
          correct=FALSE)
prop.test(table(dat1$engagement_question, dat1$address_treat)[,c(1,3)], 
          correct=FALSE)

### Example: Feeling Thermometer
prop.test(table(dat1$feeling_therm_rd1, dat1$address_treat)[,c(1,2)], 
          correct=FALSE)
prop.test(table(dat1$feeling_therm_rd1, dat1$address_treat)[,c(2,3)], 
          correct=FALSE)
prop.test(table(dat1$feeling_therm_rd1, dat1$address_treat)[,c(1,3)], 
          correct=FALSE)

### Example: Gender 
prop.test(table(dat1$answerwin_question_gender, dat1$address_treat)[,c(1,2)], 
          correct=FALSE)
prop.test(table(dat1$answerwin_question_gender, dat1$address_treat)[,c(2,3)], 
          correct=FALSE)
prop.test(table(dat1$answerwin_question_gender, dat1$address_treat)[,c(1,3)], 
          correct=FALSE)

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
