###########################
## ADULT CIGARETTE USE AND TOBACCO PREVALENCE
## DATA FROM CALIFORNIA HEALTH AND HUMAN SERVICES
## LOCATED ON https://data.world/chhs/54344616-780e-4223-a253-e3baf8c76033
## PROJECT BY: CHRISTOPHER VIDETTO

## First lets import the libraries we need to create a connection
## to the database that has the cigarette use data I uploaded

library(DBI)
library(odbc)
#This line looks for drivers on local machine
sort(unique(odbcListDrivers()[[1]]))


con <- DBI::dbConnect(odbc::odbc(),
                      Driver    = "MySQL ODBC 8.0 ANSI Driver", 
                      Server    = "3.84.158.190",
                      Database = "ahividetto",
                      UID       = "ahividettochris",
                      PWD       = "ahi2020",
                      Port      = "3306")
datafromdb <- dbReadTable(con, "cigarette_use")


## We see that the data set contains five unique variables. The year, the gender, the 
## percentage of adults who have used tobacco, the upper 95th percentile, and the 
## lower 95th percentile.
## 
## The continuous variable I will use to conduct a t-test will be the PERCENT feature 
## because this represents the percent of population that has used tobacco products
## Two 2 groups investigated will be Male and Female to determine if there is 
## a statistical difference in the PERCENT of tobacco users between them.

## First, lets clean up the dataframe because there is an extra index column
df2 = subset(datafromdb, select = -c(index))

## Now lets install the required libraries
library(dplyr)
library(ggpubr)
library(PairedData)

## Next we have to perform a distribution plot for PERCENT feature of each group

male <- df2[df2$GENDER == "Male",]
female <- df2[df2$GENDER == "Female",]

ggdensity(male$PERCENT, 
          main = "Male Distribution",
          xlab = "Percentage")
          
ggdensity(female$PERCENT, 
          main = "Female Distribution",
          xlab = "Percentage")
          
malePERCENT = male["PERCENT"]
femalePERCENT = female["PERCENT"]


############################################################
############################################################
########
########  Assumption 1: Are the two samples independents?

# Yes, the samples of men and women are not related.

########
######## Assumption 2: Does data from each of the 2 groups follow a normal distribution?

# Yes, use Shapiro test to confirm statistically

shapiro.test(male$PERCENT)
shapiro.test(female$PERCENT)

## data:  male$PERCENT
W = 0.96048, p-value = 0.2364
data:  female$PERCENT
W = 0.93191, p-value = 0.0319

## From the output, the p-value > 0.01 is implying that the distribution of the 
## data are not significantly different from normal distribution. In other words, 
## we can assume the normality.

########
######## Assumption 3. Do the two populations have the same variances?

var(malePERCENT)
## 17.18546

var(femalePERCENT)
## 23.27793

## F-test = Variance of sample #1 / variance of sample #2
## F-test = MaleVariance/FemaleVariance
## 17.18546/23.27793
## F-test = 0.7382727
# The p-value of F-test is p = 0.7382727. It’s greater than the significance 
# level alpha = 0.05. In conclusion, there is no significant difference between 
# the variances of the two sets of data. 

######## All 3 assumptions have been met, so We can use a parametric test 

## Therefore, based on these findings we use the classic t-test 
## which assumes equality of the two variances.

######## Lets create a hypothesis for the t-Test

Alternative hypothesis:
Hl:  u women - u men != 0

Null Hypothesis
Ho: u women - u men = 0

######## Create the two variables we are comparing 

malePERCENT = male["PERCENT"]
femalePERCENT = female["PERCENT"]

# Compute unpaired two-samples t-test
# Compute t-test
res <- t.test(malePERCENT, femalePERCENT, var.equal = TRUE)
res

#In the result above :
# t is the t-test statistic value (t = 5.0913),
# df is the degrees of freedom (df= 68),
# p-value is the significance level of the t-test (p-value = 0.000003019).
# conf.int is the confidence interval of the mean at 95% (conf.int = [3.3287, 7.6198]);
# sample estimates is he mean value of the sample (mean = 19.057, 13.583).

# Interpretation of the result
# The p-value of the test is 0.000003019, which is less than the significance 
# level alpha = 0.05. We can conclude that PERCENTAGE of male tobacco users
# is significantly different from PERCENTAGE of female tobacco users.
