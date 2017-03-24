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

###############################################################################
# Clark Model + Additional 
###############################################################################
clark_more_plot_dat <- results_make[, c('make', 'clark_more_pred_prob_make')]

# Sort clark_more_plot_dat by descending order of probability, with makes coming first
clark_more_plot_dat <- clark_more_plot_dat %>% arrange(desc(clark_more_pred_prob_make), desc(make))

# Make perfect information lift, no information lift, and model lift, add them 
# to the table
clark_more_plot_dat <- add_plot_info(clark_more_plot_dat)

# Write the table to a csv file
write.csv(clark_more_plot_dat, 'clark_moreended_model_plotting_table.csv')

# Let's take a peek and what it looks like
head(clark_more_plot_dat)


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Follow-up Section
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

# Define a function to calculate relative area given the tables that were 
# produced
calc_relative_area <- function(plot_info_df){
  model_pseudo_area <- sum(plot_info_df$model_lift - plot_info_df$no_info_lift)
  perf_info_pseudo_area <- sum(plot_info_df$perf_info_lift - plot_info_df$no_info_lift)
  relative_area <- model_pseudo_area / perf_info_pseudo_area
  relative_area
}

# You'll notice that within this function that for the model and perfect 
# information we do not exactly calculate the 'area' under the curve, but rather
# the sum of all the differences in height. If we wanted to calculate area we'd
# ahve to multiply each of these heights by the appropriate change in x. However
# since both the model and perfect information have the same changes in x, by
# dividing them we would effectively cancel them out, so we don't bother to 
# include them in the first place.

# Question 2b Answer: Relative Area Calculation (Clark Model)

(ra_clark_make <- calc_relative_area(clark_plot_dat))

# ------------------------------------------------------------------------------
# Question 2b Answer: Relative Area Calculation (Clark Model - Extended )

(ra_clark_more_make <- calc_relative_area(clark_more_plot_dat))

# ------------------------------------------------------------------------------
# Question 2b Extension: RA calculations with "makes" last
# When there are multiple observations with the same predicted
# probabilities and different outcomes, sorting the observations merely by the
# predicted probabilities does not uniquely define the Lift Curve. The Lift
# curve also depends on how the makes and misses are sorted within a set of
# observations that have the same predicted probability. If within this set of
# observations the makes occur first then the Lift Curve for success (for
# failure) is maximized (minimized).

# Clark Model
clark_plot_dat_makel <- results_make[, c('make', 'clark_pred_prob_make')]
clark_plot_dat_makel <- clark_plot_dat_makel %>% arrange(desc(clark_pred_prob_make), make)
clark_plot_dat_makel <- add_plot_info(clark_plot_dat_makel)

(ra_clark_makel <- calc_relative_area(clark_plot_dat_makel))

# Clark Model - Extended
# The 4 lines below take advantage of 'pipelines' (%>%). The code is written
# more efficiently then that above but does the exact same thing. With pipelines
# we don't have to retype arguments as much and the flow of work being done is
# easy to interpret. Here we say 'ra_calrk_more_makel' is assigned to have the 
# value of results_make[] after it is sent through to (%>%) the arrange() function
# and sent through the (%>%) add_plot_info() function, etc.
(ra_clark_more_makel <- results_make[, c('make', 'clark_more_pred_prob_make')] %>%
  arrange(desc(clark_more_pred_prob_make), make) %>%
  add_plot_info() %>%
  calc_relative_area())

print(sprintf('Range of Clark Model Relative Area: %.3f , %.3f', ra_clark_makel,
              ra_clark_make))
print(sprintf('Range of Clark Model + More Variables Relative Area: %.8f , %.8f',
              ra_clark_more_makel,
              ra_clark_more_make))

# ------------------------------------------------------------------------------
# Question 2b Extension: Plotting Lift Curves for Failure

# The predicted probability of failure is simply 1 - our predicted probabilities
# of success. We also set class = 0 to tell the function we are event of interest
# is 0 (the misses).
lift_info_miss <- caret::lift(factor(make) ~ (1 - clark_pred_prob_make) +
                           (1 - clark_more_pred_prob_make),
                         data = results_make,
                         plot = 'lift', class = 0)

plot(lift_info_miss, auto.key = list(columns = 2))

# ------------------------------------------------------------------------------
# Question 2b: Extension: Relative Area for Failure

# Let's make a dataframe 'results_miss' from our previous 'results_make'
results_miss <- results_make %>% transmute(miss = ifelse(make == 0, 1, 0),
                                clark_pred_prob_miss = 1 - clark_pred_prob_make,
                      clark_more_pred_prob_miss = 1 - clark_more_pred_prob_make)


# Now we use that dataframe to calculate the relsative areas for when misses
# come first on the clark model
(ra_clark_miss <- results_miss[, c('miss', 'clark_pred_prob_miss')] %>%
  arrange(desc(clark_pred_prob_miss), desc(miss)) %>%
  add_plot_info(track = 'miss') %>%
  calc_relative_area())

# And now when misses come last
(ra_clark_missl <- results_miss[, c('miss', 'clark_pred_prob_miss')] %>%
    arrange(desc(clark_pred_prob_miss), miss) %>%
    add_plot_info(track = 'miss') %>%
    calc_relative_area())

# To be complete we also use that dataframe to calculate the relative areas for 
# when misses comes first on the clark extended model
(ra_clark_more_miss <- results_miss[, c('miss', 'clark_more_pred_prob_miss')] %>%
    arrange(desc(clark_more_pred_prob_miss), desc(miss)) %>%
    add_plot_info(track = 'miss') %>%
    calc_relative_area())

# And now when misses come last
(ra_clark_more_missl <- results_miss[, c('miss', 'clark_more_pred_prob_miss')] %>%
    arrange(desc(clark_more_pred_prob_miss), miss) %>%
    add_plot_info(track = 'miss') %>%
    calc_relative_area())

# We see that it doesn't matter whether we calculate relative area for misses
# or makes, the area turns out to be the same.
(ra_clark_make == ra_clark_miss)
(ra_clark_makel == ra_clark_missl)
(ra_clark_more_make == ra_clark_more_miss)
(ra_clark_more_makel == ra_clark_more_missl)

# ------------------------------------------------------------------------------
# Question 2b (Extension): McFadden R^2 for model fit measure
# We can also use McFadden R^2 to measure the goodness of fit of our logistic
# models. Much like actual R^2 and relative area, this value ranges between 0 and
# 1.

library(pscl) # Install this package to easily get McFadden R ^ 2

(pR2(logit_clark)[4])
(pR2(logit_clark_more)[4])

# We see that the McFadden R^2 is higher for the clark model with more variables
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------