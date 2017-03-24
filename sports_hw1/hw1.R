###################################################################
###############   Sports Analytics Problem Set 1    ###############
###############           Spring 2017               ###############  
###################################################################

# This R code will estimate the logit models for problem set 1. 
# You should only need to change one line of code: you must set the working 
# directory to where you saved the field goal data.

####################################
#####  Import Field Goal Data  #####
####################################

# Set the working directory to where you saved the homework folder from chalk
setwd("~/playground/sports_hw1")

# Use this command to load the Field Goal data into R.  The data will be stored 
# in what R refers to as a data frame.  Here I've titled that data frame df_raw
df_raw <- read.csv("NFL FIeld Goals 2000-2011.csv")

####################################################
#####  Question 2 (a): Duplicate Clark et al.  #####
####################################################

# Duplicate the logistic regression results_make from the Clark et al. paper.
# The results_make will be stored in the logit_clark object.
# The syntax is as follows: MAKE is the response (dependent variable), the other
# variables DIST, GRASS, etc. are the independent (explanatory) variables, 
# family indicates you would like the model to be estimated using a binomial 
# logit model, and data indicates which data set/ data frame you would like to 
# use to estimate the model.
logit_clark <- glm(MAKE ~ DIST + GRASS + COLD49 + WINDY + ALTITUDE + PRECIP, 
               family = "binomial", data = df_raw)
summary.glm(logit_clark)
logLik(logit_clark)

#####################################################################
#####  Question 2 (a): Additional Logit Model and Lift Curves   #####
#####################################################################

# Create new distance variables
# You don't necessarily need to make new ones, you could also just enter in 
# DIST ^ 2 and DIST ^ 3 in the logit_clark_more formula.
df_raw$DIST2 <- df_raw$DIST ^ 2
df_raw$DIST3 <- df_raw$DIST ^ 3
 
# Create kicker experience variables
# Also don't have to make new ones here but we do anyway.
df_raw$KICKER.EXP <- df_raw$FG.OF.CAREER
df_raw$KICKER.EXP2 <- df_raw$KICKER.EXP ^ 2
df_raw$KICKER.EXP3 <- df_raw$KICKER.EXP ^ 3

# Estimate logit model with additional distance and season variables
logit_clark_more <- glm(MAKE ~ factor(SEASON) + DIST + DIST2 + DIST3 + KICKER.EXP + 
                 KICKER.EXP2 + KICKER.EXP3 +GRASS + COLD49 + WINDY + ALTITUDE + 
                 PRECIP, family = "binomial", data = df_raw)
summary.glm(logit_clark_more)
logLik(logit_clark_more)

#####################################################################
#####  Question 2 (a): Plot Some lift Curves  #######################
#####################################################################
# The code below creates lift curves of perfect information, no information, 
# and of the two logistic models that were created above.
library(caret) # You will need to install the 'caret' package first

results_make <- data.frame(make = df_raw$MAKE, 
                              clark_pred_prob_make = logit_clark$fitted.values, 
                      clark_more_pred_prob_make = logit_clark_more$fitted.values)

# Calculating lift info takes a while, be patient
lift_info <- caret::lift(factor(make) ~ clark_pred_prob_make + clark_more_pred_prob_make,
                        data = results_make, plot = 'lift', class = 1)

plot(lift_info, auto.key = list(columns = 2))

####################################################
###  Question 2 (b): Numerical Summary of Plot.  ###
####################################################
# Here I produce two tables that will be useful in answering 2b
# The information contained within these tables were actually calculated behind 
# the scenes to create the lift curve graph above.

library(tidyverse) # You will need to install the 'tidyverse' package

####
# Clark Model
####
clark_plot_dat <- results_make[, c('make', 'clark_pred_prob_make')]

# Sort clark_plot_dat by descending order of probability, with makes coming first
clark_plot_dat <- clark_plot_dat %>% arrange(desc(clark_pred_prob_make), desc(make))

# Make perfect information lift, no information lift, and model lift, add them 
# to table

# Let's make a function to do these things for us. (We'll use it again for the 
# extended model.)
add_plot_info <- function(make_prob_df, track = 'make'){
  no_info_lift <- c()
  perf_info_lift <- c()
  model_lift <- c()
  num_obs <- nrow(make_prob_df)
  num_events <- sum(make_prob_df[[track]])
  
  for (i in seq(num_obs)){
    no_info_lift <- append(no_info_lift, i / num_obs)
    perf_info_lift <- append(perf_info_lift, i / max(num_events, i))
    

    model_lift_i <- sum(make_prob_df[1:i,][[track]]) / num_events
    model_lift <- append(model_lift, model_lift_i)
  }
  
  make_prob_df <-  make_prob_df %>%
    mutate(no_info_lift = no_info_lift,
           perf_info_lift = perf_info_lift,
           model_lift = model_lift)
}

# Call function to add info
clark_plot_dat <- add_plot_info(clark_plot_dat)

# Write the table to a csv file
write.csv(clark_plot_dat, 'clark_standard_model_plotting_table.csv')

# Let's take a peek at what it looks like
head(clark_plot_dat)