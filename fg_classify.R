# Using the caret package to create predictive models of field goal classification

library(caret)
library(doMC)

registerDoMC(4) # Easy multiprocessing

fg_data <- df <- read.csv("~/briefcase/sports/data/NFL FIeld Goals 2000-2011.csv")

set.seed(69)
train_index <- createDataPartition(fg_data$MAKE, p = .7, list = FALSE)

fg_data_train <- fg_data[train_index, ]
fg_data_test <- fg_data[-train_index, ]

# Modfieid from: https://topepo.github.io/caret/model-training-and-tuning.html#an-example
fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  ## repeated ten times
  repeats = 5)

param_grid <- expand.grid(interaction.depth = seq(1, 7, by = 2),
                          shrinkage = seq(.1, 1, by = .1),
                          n.trees = seq(100, 1000, by = 50),
                          n.minobsinnode = 10)

fg_gbm_fit <- train(factor(MAKE, labels = c('Make', 'Miss')) ~ DIST + GRASS + COLD49 + WINDY + ALTITUDE + PRECIP, 
                    data = fg_data_train, 
                    method = "gbm", 
                    trControl = fitControl,
                    metric = "ROC",
                    tuneGrid = param_grid, 
                    verbose = FALSE)
