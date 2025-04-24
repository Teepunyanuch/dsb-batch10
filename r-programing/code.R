## recap ML workflow (simple)
## 1. split data
# 2.trian model
# 3. score (predict test data)
# 4. evaluate model (train error vs test error)

## the biggest problem = overfitting
#optimiztion vs machine learning

library(tidyverse)
library(caret)
library(mlbench)

##make to function
##split
split_data <- function(data) {
  set.seed(42)
  n <- nrow(data)
  id <- sample(1:n, size=0.7*n)
  train_df <- data[id, ]
  test_df <- data[-id, ]
  return(list(train = train_df,
              test = test_df))
}

prep_df <- split_data(mtcars)

set.seed(42)

grid_k <- data.frame(k = c(5,9))

ctrl <- trainControl(method = "cv",
                     number = 5, #k
                     repeats = 5,
                     verboseIter = TRUE)  #k

##k-fold cross validation
knn <- train(mpg ~ .,
             data = prep_df$train,
             method = "knn",
             metric = "RMSE",
             trControl = ctrl,
             ##ask pro grame to random K
             tuneLength = 4)
knn
