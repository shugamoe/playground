###############################################################################
###############    Sports Analytics Homework 2    #############################
###############           Question 3              ##############################
###############           Spring 2017             #############################  
###############################################################################

# This R code will estimate the logit models for Question 3 of Homework 2 and run Wald tests
# for some different hypotheses. The code will output the results of the Wald tests and the 
# logit models. You should carefully look through the results.   
# You must set the working directory to where you saved the field goal data.


# Load package for running Wald tests.
library(car)
# Load package for data frame manipulation
library(tidyverse)

####################################
#####  Import Field Goal Data  #####
####################################
# Set the working directory.
# Load the Field Goal data into R and store data in a data frame.
fg_df <- read.csv("NFL Field Goals 2000-2012.csv")

# Create a data frame which only includes kickers who kicked at least 200 times 
# over the period 2000-2012
over_200_kickers <- fg_df%>%
  group_by(NAME) %>%
  summarise(attempts = n()) %>%
  filter(attempts >= 200) %>%
  .$NAME

fg_df <- fg_df %>%
  filter(NAME %in% over_200_kickers)

# Create season dummy variables. The first command creates a matrix of dummy 
# variables while the second adds it to our data frame.
fg_df$SEASON <- factor(fg_df$SEASON)

#Create quadratic and cubic distance variables
fg_df$DIST2 <- fg_df$DIST ^ 2
fg_df$DIST3 <- fg_df$DIST ^ 3


####################################
###########  Method 1  #############
####################################
# Create kicker dummy variable and cold variable interaction effects.
#? kickercold.dum <- kicker.dum * replicate(dim(kicker.dum)[2], fg_df$COLD49)
#? fg_df$KICKERCOLD.DUM <- model.matrix( ~kickercold.dum)[, (2:(length(kickercold.dum[1,]) + 1))]

# Estimate model of Method 1. Note that we add the - 1 sign so that the model 
# does not have an intercept.  If we include an intercept then we must drop 
# one of the kicker dummy variables.
logit.3.1 <- glm(MAKE ~ NAME + NAME:COLD49 + DIST + DIST2 + DIST3 + 
                   SEASON + GRASS + WINDY + ALTITUDE + PRECIP - 1, 
                 family = "binomial", data = fg_df)
summary.glm(logit.3.1)
logLik(logit.3.1)
coeff.3.1 <- names(coef(logit.3.1))

# Jointly test whether or not the kicker/cold interaction terms are equal to 
# each other. We first create our restriction matrix R1 and vector r1.  We 
# conduct the Wald test by using the linearHypothesis function of the "car" 
# library; this function requires a fitted model object (e.g., logit.3.1), a 
# restriction matrix (i.e., R), and a right-hand side for the hypothesis 
# restrictions (i.e., r). The results are stored in the object we've labled 
# "wald.test.3.1a".
kicker_cold_locs1 <- grep(":COLD49", coeff.3.1)
R1 <- matrix(0, nrow = length(kicker_cold_locs1) - 1, 
             ncol = length(coeff.3.1))
R1[, first(kicker_cold_locs1):last(kicker_cold_locs1)] <- (
  -1 / length(kicker_cold_locs1))
R1[, first(kicker_cold_locs1):tail(kicker_cold_locs1, 2)[1]] <- (
  R1[,first(kicker_cold_locs1):tail(kicker_cold_locs1, 2)[1]] + 
    diag(length(kicker_cold_locs1) - 1))

r1 <- matrix(0, nrow = length(kicker_cold_locs1) - 1, ncol = 1)
wald.test.3.1a <- linearHypothesis(logit.3.1, R1, r1)

# Jointly test whether or not the kicker/cold interaction terms are equal to zero. 
wald.test.3.1b <- linearHypothesis(logit.3.1, coeff.3.1[kicker_cold_locs1])

# Jointly test the kicker fixed effects are all equal to each other.
kicker_locs1 <- grep("^NAME", coeff.3.1, value = TRUE) %>%
  grep("[^(COLD49)]$", .)
R3 <- matrix(0, nrow = length(kicker_locs1) - 1, 
             ncol = length(coeff.3.1))
R3[, first(kicker_locs1):last(kicker_locs1)] <- (
  -1 / length(kicker_locs1))
R3[, first(kicker_locs1):tail(kicker_locs1, 2)[1]] <- (
  R3[,first(kicker_locs1):tail(kicker_locs1, 2)[1]] + 
    diag(length(kicker_locs1) - 1))

r3 <- matrix(0, nrow = length(kicker_locs1) - 1, ncol = 1)
wald.test.3.1c <- linearHypothesis(logit.3.1, hypothesis.matrix = R3, rhs = r3)


####################################
###########  Method 2  #############
####################################
# Create subset of data with only attempts in cold weather for kickers with at
# least 200 attempts.
fg_df <- fg_df[fg_df$COLD49 == 1,]

# Estimate model of Method 2 on this data.  
# Note that we add the -1 sign so that the model does not have an intercept. 
# If we include an intercept then we must drop one of the kicker dummy variables. 
# Note, COLD49 is not included in this regression since all attempts in this 
# data are in cold weather.
logit.3.2<-glm(MAKE ~ NAME + DIST + DIST2 + DIST3 + SEASON + GRASS + WINDY + 
                 ALTITUDE + PRECIP - 1, family = "binomial", data = fg_df)
summary.glm(logit.3.2)
logLik(logit.3.2)
coeff.3.2 <- names(coef(logit.3.2))

# Jointly test whether or not the kicker fixed effects are equal to the average fixed effect.
kicker_locs2 <- grep("^NAME", coeff.3.2, value = TRUE) %>%
  grep("[^(COLD49)]$", .)
R4 <- matrix(0, nrow = length(kicker_locs2) - 1, 
             ncol = length(coeff.3.2))
R4[, first(kicker_locs2):last(kicker_locs2)] <- (
  -1 / length(kicker_locs2))
R4[, first(kicker_locs2):tail(kicker_locs2, 2)[1]] <- (
  R4[,first(kicker_locs2):tail(kicker_locs2, 2)[1]] + 
    diag(length(kicker_locs2) - 1))

r4 <- matrix(0, nrow = length(kicker_locs2) - 1, ncol = 1)
wald.test.3.2 <- linearHypothesis(logit.3.2, hypothesis.matrix = R4, rhs = r4)

####################################
#########  Save Output   ###########
####################################
# Write Wald test output to a csv file.
wald.tests <- data.frame(matrix(nrow = 4, ncol = 4))
colnames(wald.tests) <- c('Model', 'Hypothesis', 'Wald.Statistic', 'Pr(>Chisq)')
wald.tests$Model <- c(1,1,1,2)
wald.tests$Hypothesis <- c('Interaction terms all equal to each other.', 
                           'Interaction terms all equal to zero.', 
                           'Kicker fixed effects all equal to each other.', 
                           'Kicker fixed effects all equal to each other.')
wald.tests$Wald.Statistic <- c(wald.test.3.1a$Chisq[2], 
                               wald.test.3.1b$Chisq[2], 
                               wald.test.3.1c$Chisq[2], 
                               wald.test.3.2$Chisq[2])
wald.tests$`Pr(>Chisq)` <- c(wald.test.3.1a$`Pr(>Chisq)`[2], 
                             wald.test.3.1b$`Pr(>Chisq)`[2], 
                             wald.test.3.1c$`Pr(>Chisq)`[2], 
                             wald.test.3.2$`Pr(>Chisq)`[2])
write.csv(wald.tests, "Question 3 - Wald test results.csv", row.names = FALSE)


# Write regression output to csv files.
write.csv(summary(logit.3.1)$coefficients, "Question 3 - Method 1.csv")
write.csv(summary(logit.3.2)$coefficients, "Question 3 - Method 2.csv")
