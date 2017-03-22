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
setwd("~/playground/sports_hw1")

# Use this command to load the Field Goal data into R.  The data will be stored 
# in what R refers to as a data frame.  Here I've titled that data frame df
df <- read.csv("NFL FIeld Goals 2000-2011.csv")


####################################################
#####  Question 2 (a): Duplicate Clark et al.  #####
####################################################

# Duplicate the logistic regression results from the Clark et al. paper.
# The results will be stored in the logit.1 object.
# The syntax is as follows: MAKE is the response (dependent variable), the other
# variables DIST, GRASS, etc. are the independent (explanatory) variables, 
# family indicates you would like the model to be estimated using a binomial 
# logit model, and data indicates which data set/ data frame you would like to 
# use to estimate the model.
logit.1 <- glm(MAKE ~ DIST + GRASS + COLD49 + WINDY + ALTITUDE + PRECIP, family = "binomial", data = df)
summary.glm(logit.1)
logLik(logit.1)


#####################################################################
#####  Question 2 (a): Additional Logit Model and Lift Curves   #####
#####################################################################

# Create new distance variables
# You don't necessarily need to make new ones, you could also just enter in 
# DIST ^ 2 and DIST ^ 3 in the logit.2 formula.
df$DIST2 <- df$DIST ^ 2
df$DIST3 <- df$DIST ^ 3
 
# Create kicker experience variables
# Also don't have to make new ones here but we do anyway.
df$KICKER.EXP <- df$FG.OF.CAREER
df$KICKER.EXP2 <- df$KICKER.EXP ^ 2
df$KICKER.EXP3 <- df$KICKER.EXP ^ 3

# Estimate logit model with additional distance and season variables
logit.2 <- glm(MAKE ~ factor(SEASON) + DIST + DIST2 + DIST3 + KICKER.EXP + 
                 KICKER.EXP2 + KICKER.EXP3 +GRASS + COLD49 + WINDY + ALTITUDE + 
                 PRECIP, family = "binomial", data = df)
summary.glm(logit.2)
logLik(logit.2)


#####################################################################
#####  Question 2 (a): Plot Some lift Curves  #######################
#####################################################################
# The code below creates lift curves of perfect information, no information, 
# and of the two logistic models that were created above.
library(caret) # You will need to install the 'caret' package first

results <- data.frame(make = df$MAKE, predicted_prob_1 = logit.1$fitted.values, 
                      predicted_prob_2 = logit.2$fitted.values)

# Calculating lift info takes a while, be patient
# lift_info <- caret::lift(factor(make) ~ predicted_prob_1 + predicted_prob_2, 
#                         data = results, plot = 'lift', class = 1)
# 
# plot(lift_info, auto.key = list(columns = 2))


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
model1_tab <- results[, c('make', 'predicted_prob_1')]

# Sort model1_tab by descending order of probability, with makes coming first
model1_tab <- model1_tab %>% arrange(desc(predicted_prob_1), desc(make))

# Make perfect information lift, no information lift, and model lift, add them 
# to table

# First let's make a function to do these things for us 
# (we'll use it again for the extended model)
add_plot_info <- function(make_prob_df, track = 'make'){
  no_info_lift <- c()
  perf_info_lift <- c()
  model_lift <- c()
  num_obs <- nrow(make_prob_df)
  num_makes <- sum(make_prob_df$make)
  
  for (i in seq(num_obs)){
    no_info_lift <- append(no_info_lift, i / num_obs)
    perf_info_lift <- append(perf_info_lift, i / max(num_makes, i))
    
    if (track == 'make'){
      model_lift_i <- sum(make_prob_df[1:i,]$make) / num_makes
    } else if (track == 'miss'){
      model_lift_i <- (i - sum(make_prob_df[1:i,]$make)) / (num_obs - num_makes)
    }
    model_lift <- append(model_lift, model_lift_i)
  }
  
  make_prob_df <-  make_prob_df %>%
    mutate(no_info_lift = no_info_lift,
           perf_info_lift = perf_info_lift,
           model_lift = model_lift)
}

# Call function to add info
model1_tab <- add_plot_info(model1_tab)

# Write the table to a csv file
write.csv(model1_tab, 'clark_standard_model_plotting_table.csv')

# Let's take a peek at what it looks like
head(model1_tab)


####
# Clark Model + Additional 
####
model2_tab <- results[, c('make', 'predicted_prob_2')]

# Sort model2_tab by descending order of probability, with makes coming first
model2_tab <- model2_tab %>% arrange(desc(predicted_prob_2), desc(make))

# Make perfect information lift, no information lift, and model lift, add them 
# to the table
model2_tab <- add_plot_info(model2_tab)

# Write the table to a csv file
write.csv(model2_tab, 'clark_extended_model_plotting_table.csv')

# Let's take a peek and what it looks like
head(model2_tab)


#-------------------------------------------------------------------------------
# Follow-up Section
#-------------------------------------------------------------------------------

calc_relative_area <- function(plot_info_df){
  ra <- sum(plot_info_df$model_lift - plot_info_df$no_info_lift) / (
    sum(plot_info_df$perf_info_lift - plot_info_df$no_info_lift))
  ra
}

# Question 2b Answer: Relative Area Calculation (Clark Model)

(ra_clark <- calc_relative_area(model1_tab))

# ------------------------------------------------------------------------------
# Question 2b Answer: Relative Area Calculation (Clark Model - Extended )

(ra_clark_extended <- calc_relative_area(model2_tab))

# ------------------------------------------------------------------------------
# Question 2b Extension: RA calculations with "makes" last
# !todo: Write brief explanation explaining non-uniqueness of lift curves

# Clark Model
model1_tab_makel <- results[, c('make', 'predicted_prob_1')]
model1_tab_makel <- model1_tab_makel %>% arrange(desc(predicted_prob_1), make)
model1_tab_makel <- add_plot_info(model1_tab_makel)

(ra_clark_makel <- calc_relative_area(model1_tab_makel))

# Clark Model - Extended
model2_tab_makel <- results[, c('make', 'predicted_prob_1')]
model2_tab_makel <- model2_tab_makel %>% arrange(desc(predicted_prob_2), make)
model2_tab_makel <- add_plot_info(model1_tab_makel)

(ra_clark_ext_makel <- calc_relative_area(model1_tab_makel))

# ------------------------------------------------------------------------------
# Question 2b Extension: Plotting Lift Curves for Failure
# lift_info <- caret::lift(factor(make) ~ predicted_prob_1 + predicted_prob_2, 
#                         data = results, plot = 'lift', class = 0)
# 
# plot(lift_info, auto.key = list(columns = 2))

# ------------------------------------------------------------------------------
# Question 2b: Extension: Relative Area 








