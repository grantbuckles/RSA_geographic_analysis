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
sink(file = paste0(texFile, "/analysis_dump.txt"), append = FALSE, 
     type = c("output", "message"),
     split = TRUE)
invisible(file.copy(from="output_template", 
                    to=paste0(texFile,"/output_template", Sys.Date(),".tex")))

cat("\\pagebreak")
cat(paste("\\section{Any Form of Engagment with the System}")) 
cat(paste("\\subsection{Descriptive Statistics}")) 

#------------------------------------------------------------------------------
# Prepare data - Any Form of Engagement with the System
#------------------------------------------------------------------------------

## Extract randomized sample---------------------------------------------------
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

dat1$total.entries <- rowSums(dat1[c(49:56, 58, 61:63, 65, 67, 69, 71, 73:87, 
                                     89:109)] != "")
table1 <- as.table(table(dat1$total.entries, dnn = c("Engaged - 
                                                     Number of Responses")))
print(xtable(table1, caption = "Distribution of Engagement"))
cat("\n\n")

### Create dichotomous variable of engagement

dat1$engaged <- NULL
dat1$engaged[dat1$total.entries > 0] <- 1
dat1$engaged[dat1$total.entries == 0] <- 0

table2 <- as.table(table(dat1$engaged, dnn = c("Engaged - No/Yes")))
print(xtable(table2, caption ="Distribution of Engagement"))
cat("\n\n\\pagebreak\n")


#------------------------------------------------------------------------------
# Descriptive statistics
#------------------------------------------------------------------------------

## 1. Differences in address entry between B & C groups------------------------

### Create variable: Entered an address (doest not consider address quality)
dat2 <- filter(dat1, address_treat == "B" | address_treat == "C")

dat2$engage_address[dat2$raw_user_address != ""] <- "Entered Address"
dat2$engage_address[dat2$raw_user_address == ""] <- "Did Not Enter Address"

### Descriptive statistics
address_entry <- crosstab(dat2$engage_address, dat2$address_treat,
         dnn = c("Opinion", "Education"), prop.c = TRUE, prop.r = FALSE, 
         prop.t = FALSE, prop.chisq = FALSE, chisq = FALSE, plot = FALSE)
print(xtable(address_entry, caption = "Address Entry Following Redo (Group B 
             vs. Group C)"))
cat("\n\n")

## 2. Furthur engagement-------------------------------------------

enga <- crosstab(dat1$engaged, dat1$address_treat,
         dnn = c("Engaged", "Treatment Group"), prop.c = TRUE, prop.r = TRUE, 
         prop.t = FALSE, prop.chisq = FALSE, chisq = FALSE, plot = FALSE)
print(xtable(enga, caption = "Engagement among control and treatment groups"))
cat("\n\n")

#------------------------------------------------------------------------------
# Analysis
#------------------------------------------------------------------------------

cat("\\pagebreak")
cat(paste("\\subsection{Analysis}")) 

## 1. Test of Proportions------------------------------------------------------

### Example: Engagement Question 
test1 <- prop.test(table(dat1$engaged, dat1$address_treat)[,c(1,2)], 
          correct=FALSE)
p.value1 <- as.table(test1$p.value)
print(xtable(p.value1, caption = "P-value - Test of Proportion: Group A vs. 
             Group B"))

test2 <- prop.test(table(dat1$engaged, dat1$address_treat)[,c(2,3)], 
          correct=FALSE)
p.value2 <- as.table(test2$p.value)
print(xtable(p.value2, caption = "P-value - Test of Proportions: Group B vs. 
             Group C"))

test3 <- prop.test(table(dat1$engaged, dat1$address_treat)[,c(1,3)], 
          correct=FALSE)
p.value3 <- as.table(test3$p.value)
print(xtable(p.value3, caption = "P-value - Test of Proportions: Group A vs. 
             Group C"))

cat("The difference between Group A (no lookup) and the other two groups is 
    statistically significant.")


## 2. Linear Probability Model-------------------------------------------------

### Variables 

#### Redo - Category A (No geographic lookup)
dat1$nolookup[dat1$address_treat == "A"] <- 1
dat1$nolookup[dat1$address_treat != "A"] <- 0

#### Redo - Category B (Lookup, no prime)
dat1$noprime[dat1$address_treat == "B"] <- 1
dat1$noprime[dat1$address_treat != "B"] <- 0

#### Redo - Category c (Lookup, with prime)
dat1$prime[dat1$address_treat == "C"] <- 1
dat1$prime[dat1$address_treat != "C"] <- 0

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
dat1$subsidy[dat1$USSD_number == "*120*4729#"] <- 1
dat1$subsidy[dat1$USSD_number != "*120*4729#"] <- 0

#### Channel category - other 1 (magazine?)
dat1$other1[dat1$USSD_number == "*120*7692*1#"] <- 1
dat1$other1[dat1$USSD_number != "*120*7692*1#"] <- 0

#### Channel category - Control
dat1$control[dat1$USSD_number == "*120*7692*2#"] <- 1
dat1$control[dat1$USSD_number != "*120*7692*2#"] <- 0

#### Channel category - Treatment 1 (Lottery)
dat1$lottery[dat1$USSD_number == "*120*7692*3#"] <- 1
dat1$lottery[dat1$USSD_number != "*120*7692*3#"] <- 0

#### Channel category - other 2 (???)
dat1$other2[dat1$USSD_number == "*120*7692*8#"] <- 1
dat1$other2[dat1$USSD_number != "*120*7692*8#"] <- 0

#### Channel category - other 3 (???)
dat1$other3[dat1$USSD_number == "*120*7692#"] <- 1
dat1$other3[dat1$USSD_number != "*120*7692#"] <- 0

### Model

mod1.1 <- lm(engaged ~ nolookup + prime +
                days +
                lottery + subsidy + control,
             data = dat1)
xtable(summary(mod1.1), caption = "Linear Probability Model of Engagement")

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

cat("\\pagebreak")
cat("\\pagebreak")
cat(paste("\\section{Engagement with Answer \\& Win Questions}")) 
cat(paste("\\subsection{Descriptive Statistics}")) 

#------------------------------------------------------------------------------
# Prepare data - Engagment = Answered an Answer and Win Question
#------------------------------------------------------------------------------

## Create measures of engagement-----------------------------------------------

### Count variable (# of Answer & Win Questions Answered)

dat1$aw_total.entries <- rowSums(dat1[c(49:52)] != "")
table1.1 <- as.table(table(dat1$aw_total.entries, dnn = c("Engaged - 
                                                  Number of Responses")))
print(xtable(table1.1, caption = "Distribution of Engagement"))
cat("\n\n")

### Create dichotomous variable of engagement

dat1$aw_engaged <- NULL
dat1$aw_engaged[dat1$aw_total.entries > 0] <- 1
dat1$aw_engaged[dat1$aw_total.entries == 0] <- 0

table2.1 <- as.table(table(dat1$aw_engaged, dnn = c("Engaged - No/Yes")))
print(xtable(table2.1, caption ="Distribution of Engagement"))
cat("\n\n\\pagebreak\n")

## Descriptives of engagement--------------------------------------------------

enga.1 <- crosstab(dat1$aw_engaged, dat1$address_treat,
                 dnn = c("Engaged", "Treatment Group"), prop.c = TRUE,
                 prop.r = TRUE, prop.t = FALSE, prop.chisq = FALSE, 
                 chisq = FALSE, plot = FALSE)
print(xtable(enga.1, caption = "Engagement among control and treatment groups"))
cat("\n\n")

#------------------------------------------------------------------------------
# Analysis
#------------------------------------------------------------------------------

cat("\\pagebreak")
cat(paste("\\subsection{Analysis}")) 

## 1. Test of Proportions------------------------------------------------------

### Example: Engagement Question 
test1.1 <- prop.test(table(dat1$aw_engaged, dat1$address_treat)[,c(1,2)], 
                   correct=FALSE)
p.value1.1 <- as.table(test1.1$p.value)
print(xtable(p.value1.1, caption = "P-value - Test of Proportion: Group A vs. 
             Group B"))

test2.1 <- prop.test(table(dat1$aw_engaged, dat1$address_treat)[,c(2,3)], 
                   correct=FALSE)
p.value2.1 <- as.table(test2.1$p.value)
print(xtable(p.value2.1, caption = "P-value - Test of Proportions: Group B vs. 
             Group C"))

test3.1 <- prop.test(table(dat1$aw_engaged, dat1$address_treat)[,c(1,3)], 
                   correct=FALSE)
p.value3.1 <- as.table(test3.1$p.value)
print(xtable(p.value3.1, caption = "P-value - Test of Proportions: Group A vs. 
             Group C"))

cat("The difference between Group A (no lookup) and the other two groups is 
    statistically significant.")


## 2. Linear Probability Model-------------------------------------------------

mod2.1 <- lm(aw_engaged ~ nolookup + prime +
               days +
               lottery + subsidy + control,
             data = dat1)
xtable(summary(mod2.1), caption = "Linear Probability Model of Engagement")


