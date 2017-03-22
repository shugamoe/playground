###################################################################
###############   Sports Analytics Problem Set 1    ###############
###############           Spring 2017               ###############  
###################################################################

# This R code will estimate the logit models for problem set 1. You should only need to change one
# line of code: you must set the working directory to where you saved the field goal data.

####################################
#####  Import Field Goal Data  #####
####################################

# Set the working directory to where you saved the homework folder from chalk
setwd("~/briefcase/sports/hw1/")

# Use this command to load the Field Goal data into R.  The data will be stored in what R refers
# to as a data frame.  Here I've titled that data frame df
df <- read.csv("../data/NFL FIeld Goals 2000-2011.csv")


####################################################
#####  Question 2 (a): Duplicate Clark et al.  #####
####################################################

# Duplicate the logistic regression results from the Clark et al. paper.
# The results will be stored in the logit.1 object.
# The syntax is as follows: MAKE is the response (dependent variable), the other
# variables DIST, GRASS, etc. are the independent (explanatory) variables, family indicates
# you would like the model to be estimated using a binomial logit model, and data indicates which
# data set/ data frame you would like to use to estimate the model.
logit.1 <- glm(MAKE ~ DIST + GRASS + COLD49 + WINDY + ALTITUDE + PRECIP, family = "binomial", data = df)
summary.glm(logit.1)
logLik(logit.1)


#####################################################################
#####  Question 2 (a): Additional Logit Model and Lift Curves   #####
#####################################################################

# Create new distance variables
# You don't necessarily need to make new ones, you could also just enter in DIST ^ 2 and DIST ^ 3
# in the logit.2 formula.
df$DIST2 <- df$DIST ^ 2
df$DIST3 <- df$DIST ^ 3
 
# Create kicker experience variables
# Also don't have to make new ones here but we do anyway.
df$KICKER.EXP <- df$FG.OF.CAREER
df$KICKER.EXP2 <- df$KICKER.EXP ^ 2
df$KICKER.EXP3 <- df$KICKER.EXP ^ 3

# Estimate logit model with additional distance and season variables
logit.2 <- glm(MAKE ~ factor(SEASON) + DIST + DIST2 + DIST3 + KICKER.EXP + KICKER.EXP2 + KICKER.EXP3 +
               GRASS + COLD49 + WINDY + ALTITUDE + PRECIP, family = "binomial", data = df)
summary.glm(logit.2)
logLik(logit.2)

# The code below creates lift curves of perfect information, no information, and of the two logistic models
# that were created above.
library(caret) # You will need to install the 'caret' package first

results <- data.frame(make = df$MAKE, predicted_prob_1 = logit.1$fitted.values, predicted_prob_2 = logit.2$fitted.values)

# Calculating lift info takes a while, be patient
# lift_info <- caret::lift(factor(make) ~ predicted_prob_1 + predicted_prob_2, data = results, plot = 'lift', class = 1)

# plot(lift_info, auto.key = list(columns = 2))


####################################################
###  Question 2 (b): Numerical Summary of Plot.  ###
####################################################
# Here I am producing two tables that will be useful in answering 2b
library(tidyverse) # You will need to install the tidyverse library

# Clark Model
mod1_tab <- results[, c('make', 'predicted_prob_1')]

# Sort mod1_tab by descending order of probability, with makes coming first
mod1_tab <- mod1_tab %>% arrange(desc(predicted_prob_1), desc(make))

# Make perfect information lift, no information lift, and model lift, add them to table
no_info_lift <- c()
perf_info_lift <- c()
model_lift <- c()

num_obs <- nrow(mod1_tab)
num_makes <- sum(mod1_tab$make)
for(i in seq(num_obs)){
  no_info_lift <- append(no_info_lift, i / num_obs)
  perf_info_lift <- append(perf_info_lift, i / max(num_makes, i))
  model_lift <- append(model_lift, sum(mod1_tab[1:i,]$make) / num_makes)
}

mod1_tab <-  mod1_tab %>%
  mutate(no_info_lift = no_info_lift,
         perf_info_lift = perf_info_lift,
         model_lift = model_lift)

# Write the table to a csv file
write.csv(mod1_tab, 'clark_standard_model_plotting_table.csv')

####
# Clark Model + Additional 
####

mod2_tab <- results[, c('make', 'predicted_prob_2')]

# Sort mod2_tab by descending order of probability, with makes coming first
mod2_tab <- mod2_tab %>% arrange(desc(predicted_prob_2), desc(make))

no_info_lift <- c()
perf_info_lift <- c()
model_lift <- c()

num_obs <- nrow(mod2_tab)
num_makes <- sum(mod2_tab$make)
for(i in seq(num_obs)){
  no_info_lift <- append(no_info_lift, i / num_obs)
  perf_info_lift <- append(perf_info_lift, i / max(num_makes, i))
  model_lift <- append(model_lift, sum(mod2_tab[1:i,]$make) / num_makes)
}

mod2_tab <-  mod2_tab %>%
  mutate(no_info_lift = no_info_lift,
         perf_info_lift = perf_info_lift,
         model_lift = model_lift)

# Write the table to a csv file
write.csv(mod2_tab, 'clark_extended_model_plotting_table.csv')

