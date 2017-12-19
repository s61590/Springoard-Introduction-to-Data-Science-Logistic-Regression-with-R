Regression with binary outcomes
## ═════════════════════════════════

## Logistic regression
## ───────────────────────

##   This far we have used the `lm' function to fit our regression models.
##   `lm' is great, but limited–in particular it only fits models for
##   continuous dependent variables. For categorical dependent variables we
##   can use the `glm()' function.

##   For these models we will use a different dataset, drawn from the
##   National Health Interview Survey. From the [CDC website]:

##         The National Health Interview Survey (NHIS) has monitored
##         the health of the nation since 1957. NHIS data on a broad
##         range of health topics are collected through personal
##         household interviews. For over 50 years, the U.S. Census
##         Bureau has been the data collection agent for the National
##         Health Interview Survey. Survey results have been
##         instrumental in providing data to track health status,
##         health care access, and progress toward achieving national
##         health objectives.

##   Load the National Health Interview Survey data:

NH11 <- readRDS("NatHealth2011.rds")
labs <- attributes(NH11)$labels

##   [CDC website] http://www.cdc.gov/nchs/nhis.htm

## Logistic regression example
## ───────────────────────────────

##   Let's predict the probability of being diagnosed with hypertension
##   based on age, sex, sleep, and bmi

str(NH11$hypev) # check stucture of hypev
levels(NH11$hypev) # check levels of hypev
# collapse all missing values to NA
NH11$hypev <- factor(NH11$hypev, levels=c("2 No", "1 Yes"))
# run our regression model
hyp.out <- glm(hypev~age_p+sex+sleep+bmi,
               data=NH11, family="binomial")
coef(summary(hyp.out))

## Logistic regression coefficients
## ────────────────────────────────────

##   Generalized linear models use link functions, so raw coefficients are
##   difficult to interpret. For example, the age coefficient of .06 in the
##   previous model tells us that for every one unit increase in age, the
##   log odds of hypertension diagnosis increases by 0.06. Since most of us
##   are not used to thinking in log odds this is not too helpful!

##   One solution is to transform the coefficients to make them easier to
##   interpret

hyp.out.tab <- coef(summary(hyp.out))
hyp.out.tab[, "Estimate"] <- exp(coef(hyp.out))
hyp.out.tab

## Generating predicted values
## ───────────────────────────────

##   In addition to transforming the log-odds produced by `glm' to odds, we
##   can use the `predict()' function to make direct statements about the
##   predictors in our model. For example, we can ask "How much more likely
##   is a 63 year old female to have hypertension compared to a 33 year old
##   female?".

# Create a dataset with predictors set at desired levels
predDat <- with(NH11,
                expand.grid(age_p = c(33, 63),
                            sex = "2 Female",
                            bmi = mean(bmi, na.rm = TRUE),
                            sleep = mean(sleep, na.rm = TRUE)))
# predict hypertension at those levels
cbind(predDat, predict(hyp.out, type = "response",
                       se.fit = TRUE, interval="confidence",
                       newdata = predDat))

##   This tells us that a 33 year old female has a 13% probability of
##   having been diagnosed with hypertension, while and 63 year old female
##   has a 48% probability of having been diagnosed.

## Packages for  computing and graphing predicted values
## ─────────────────────────────────────────────────────────

##   Instead of doing all this ourselves, we can use the effects package to
##   compute quantities of interest for us (cf. the Zelig package).
install.packages("effects")
library(effects)
plot(allEffects(hyp.out))

## Exercise: logistic regression
## ───────────────────────────────────

##   Use the NH11 data set that we loaded earlier.

##   1. Use glm to conduct a logistic regression to predict ever worked
##      (everwrk) using age (age_p) and marital status (r_maritl).
##   2. Predict the probability of working for each level of marital
##      status.

##   Note that the data is not perfectly clean and ready to be modeled. You
##   will need to clean up at least some of the variables before fitting
##   the model.
library(dplyr)
library(effects)

# Examine variables
str(NH11[c("everwrk", "age_p", "r_maritl")])

# everwrk
levels(NH11$everwrk)
summary(NH11$everwrk)
#Many missing values(18974 NAs)
# converting NH11$everwrk into a factor variable
NH11$everwrk <- factor(NH11$everwrk, c("1 Yes", "2 No"))

# age_p
hist(NH11$age_p)
summary(NH11$age_p)
# Mean 48, median 47, min 18, max 85, 1st Q 33, 3rd Q 62

levels(NH11$r_maritl)
summary(NH11$r_maritl)
NH11$r_maritl <- factor(NH11$r_maritl, 
                        c("1 Married - spouse in household",
                          "2 Married - spouse not in household",
                          "4 Widowed", "5 Divorced", "6 Separated",
                          "7 Never married", "8 Living with partner"))
# Create logistic regression model
wrk_mod <- glm(everwrk ~ age_p + r_maritl, data = NH11, family = "binomial")
summary(wrk_mod)

# Predict everwrk for each level of r_maritl
predData_maritl <- with(NH11, expand.grid(r_maritl = levels(r_maritl),
                                          age_p = mean(age_p, na.rm = TRUE)))
wrkPred_maritl <- predict(wrk_mod, type = "response", newdata = predData_maritl)
cbind(predData_maritl, wrkPred_maritl)
# Console results of this function. Predicted value of everwrk(wrkPred_maritl)
#is given for each level of r_maritl. 
#                            r_maritl age_p       wrkPred_maritl
#     1 Married - spouse in household 48.10983     0.13302104
# 2 Married - spouse not in household 48.10983     0.13885509
#                          4 Widowed 48.10983     0.23309985
#                          5 Divorced 48.10983     0.06884153
#                         6 Separated 48.10983     0.11893040
#                     7 Never married 48.10983     0.17786252
#              8 Living with partner 48.10983     0.08963577
#           9 Unknown marital status 48.10983     0.18557430



