##################
# Geographic Randomization - Statistical Analysis of Randomization
# Created by Grant Buckles
# Last Updated October 21, 2016
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

cat("Sample Text")


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

#Configure .tex output for table
#print(xtable(table(dat1$total.entries)))

### Create dichotomous variable of engagement

dat1$engaged <- NULL
dat1$engaged[dat1$total.entries > 0] <- 1
dat1$engaged[dat1$total.entries == 0] <- 0

#Configure .tex output for table
#print(xtable(table(dat1$engaged)))

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

#.text output
#print(xtable(enga))

#------------------------------------------------------------------------------
# Analysis
#------------------------------------------------------------------------------

## 1. Test of Proportions------------------------------------------------------

### Example: Engagement Question 
test1 <- prop.test(table(dat1$engaged, dat1$address_treat)[,c(1,2)], 
          correct=FALSE)
test2 <- prop.test(table(dat1$engaged, dat1$address_treat)[,c(2,3)], 
          correct=FALSE)
test3 <- prop.test(table(dat1$engaged, dat1$address_treat)[,c(1,3)], 
          correct=FALSE)

cat("The difference between Group A (no lookup) and the other two groups is statistically 
    significant.")

## 2. Logistic Regression------------------------------------------------------

### Variables 

#### Number of days in system (April 17 - date created)
dat1$days[as.Date(dat1$created_at) == "2014-04-07"] <- 10
dat1$days[as.Date(dat1$created_at) == "2014-04-08"] <- 9
dat1$days[as.Date(dat1$created_at) == "2014-04-09"] <- 8
dat1$days[as.Date(dat1$created_at) == "2014-04-10"] <- 7
dat1$days[as.Date(dat1$created_at) == "2014-04-11"] <- 6
dat1$days[as.Date(dat1$created_at) == "2014-04-12"] <- 5
dat1$days[as.Date(dat1$created_at) == "2014-04-13"] <- 4
dat1$days[as.Date(dat1$created_at) == "2014-04-14"] <- 3

#### Channel category - Treatment 2 (Subsidy)
dat1$control[dat1$USSD_number == "*120*4729#"] <- 1
dat1$control[dat1$USSD_number != "*120*4729#"] <- 0

#### Channel category - Treatment 1 (Lottery)
dat1$control[dat1$USSD_number == "*120*4729#"] <- 1
dat1$control[dat1$USSD_number != "*120*4729#"] <- 0

### Plain Logit - Feeling Thermometer Response

mod1.1 <- glm(engaged ~ days +,
              family=binomial(link="probit"), data = dat1)
summary(mod1.1)

#Note: Adjust P-values for Multiple Comparisons
