###################################################################
###############    Sports Analytics Homework 3    #################
###############           Question 1               ###############  
###############           Spring 2017            ###############  
###################################################################

# This R code will use the 2000-2016 NFL field goal data constructed from the Armchair Analysis 
# data set to estimate models (i) and (ii) on kick attempts from the 2000-2011 seasons. 
# This code will NOT provide the shrinkage estimation coefficients for model (iii). Rather, 
# you are responsible for calculating these. The R code will calculate the w_i^2 and
# the WT values necessary for you to construct the shrinkage estimation coefficients using the 
# equation in Note 4 of Question 4.
# The code will NOT perform the out-of-sample prediction on the 2012-2016 data for you. 
# For the out-of-sample prediction, you have two options:
#   1) Extend the R code below to perform the out-of-sample prediction for models (i)-(iii).
#   2) Perform the out-of-sample prediction for models (i)-(iii) outside of R (e.g., in Excel).
#      The R code will output the regression estimates for models (i)-(ii) to allow you to do this.


# Load package for running Wald tests.
library(car)
# Load package for dataframe manipulation
library(tidyverse)

####################################
#####  Import Field Goal Data  #####
####################################
# Set the working directory

# Set the working directory.
# Load the Field Goal data into R and store data in a data frame.
fg_df <- read.csv("NFL Field Goals 2000-2016.csv")

# Create a data frame which only includes kickers who kicked at least 200 times 
# over the period 2000-2012
over_200_kickers <- fg_df%>%
  group_by(NAME, FKICKER) %>%
  filter(SEASON <= 2011) %>%
  summarise(attempts = n()) %>%
  filter(attempts >= 200 | FKICKER == "MV-0100" ) %>%
  .$NAME

fg_df <- fg_df %>%
  filter(NAME %in% over_200_kickers)

#Create quadratic and cubic distance variables
fg_df$DIST2 <- fg_df$DIST ^ 2
fg_df$DIST3 <- fg_df$DIST ^ 3

#Split fg_df into kicks taken over the period 2000-2011 and 2012-2016 for out of sample predictions
fg_df.2000to2011 <- fg_df %>% filter(SEASON <= 2011)
fg_df.2012to2016 <- fg_df %>% filter(SEASON >= 2012)

# Create season dummy variables. The first command creates a matrix of dummy 
# variables while the second adds it to our data frame.
fg_df.2000to2011$SEASON <- factor(fg_df.2000to2011$SEASON)
fg_df.2012to2016$SEASON <- factor(fg_df.2012to2016$SEASON)

#########################
#####  Model (i)   ######
#########################
# Estimate Model i.
logit.1 <- glm(MAKE ~ DIST + GRASS + COLD49 + WINDY + ALTITUDE + PRECIP, 
               family = "binomial", data = fg_df.2000to2011)
summary.glm(logit.1)
logLik(logit.1)

# Save model coefficient names
coeff.1 <- names(coef(logit.1))

#########################
#####  Model (ii)  ######
#########################
# Estimate Model ii.  Note that we add the - 1 sign so that the model does not have an intercept.  If we include
# an intercept then we must drop one of the kicker dummy variables.
logit.2 <- glm(MAKE ~ NAME + DIST + DIST2 + DIST3 + SEASON + GRASS + COLD49 + 
                 WINDY + ALTITUDE + PRECIP - 1, 
               family = "binomial", data = fg_df.2000to2011)
summary.glm(logit.2)
logLik(logit.2)

# Save model coefficients names
coeff.2 <- names(coef(logit.2))

# Display deviation of kicker fixed effect from mean fixed effect
(coef(logit.2) - mean(coef(logit.2)))


#########################
#####  Model (iii)  #####
#########################
# Jointly test whether or not the kicker fixed effects are equal to the average 
# fixed effect. We first create our restriction matrix R and vector r.  The 
# results are stored in the object we've labled "wald.test".
kicker_locs <- grep("^NAME", coeff.2)

R <- matrix(0, nrow = length(kicker_locs) - 1, ncol = length(coeff.2))
R <- matrix(0, nrow = length(kicker_locs) - 1, 
             ncol = length(coeff.2))
R[, first(kicker_locs):last(kicker_locs)] <- (
  -1 / length(kicker_locs))
R[, first(kicker_locs):tail(kicker_locs, 2)[1]] <- (
  R[,first(kicker_locs):tail(kicker_locs, 2)[1]] + 
    diag(length(kicker_locs) - 1))

r <- matrix(0, nrow = length(kicker_locs) - 1, ncol = 1)
wald.test <- linearHypothesis(logit.2, R, r)


# Shrink the fixed effects from model ii. The new vector of coefficients is 
# called coeff.3 See section on shrinkage estimation in the notes for further 
# details.
mat_dim <- length(kicker_locs)
first_kicker_coef <- first(kicker_locs)
last_kicker_coef <- tail(kicker_locs, 1)

stat <- wald.test$Chisq[2] / wald.test$Df[2] - 1
R1 <- matrix(-1 / mat_dim, nrow = mat_dim, ncol = mat_dim)
R1 <- R1 + diag(mat_dim)
var <- diag(R1 %*% vcov(logit.2)[first_kicker_coef:last_kicker_coef,
                                 first_kicker_coef:last_kicker_coef]
            %*% R1)
precision <- diag(R1 %*% vcov(logit.2)[first_kicker_coef:last_kicker_coef,
                                       first_kicker_coef:last_kicker_coef]
                  %*% R1) ^ -1
# Insert your code here to complete the calculation of the shrunken fixed effects.
# Alternatively, use the output below to do this calculation outside of R (e.g., in Excel).


#########################
#######  Output  ########
#########################
# Output results of Models (i) and (ii).
write.csv(summary(logit.1)$coefficients, "Model i results.csv")
write.csv(summary(logit.2)$coefficients, "Model ii results.csv")
# Output WT for calculation of shrinkage estimation coefficients for Model (iii).
paste('WT =', wald.test$Chisq[2], 'and the corresponding p-value is', wald.test$`Pr(>Chisq)`[2])
write.csv(cbind(summary(logit.2)$coefficients[first_kicker_coef:last_kicker_coef, 0], var), "w_i_squared.csv")


########################################
######  Out-of-Sample Regression  ######
########################################
# Insert your code here to calculate out-of-sample predictions on the 2012-2016 data.
# Alternatively, use the above output to construct out-of-sample predictions outside of R.