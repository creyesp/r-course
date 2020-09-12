# https://cran.r-project.org/web/packages/randomForest/randomForest.pdf
# 
# install.packages('randomForest')
# install.packages("caret")
# install.packages("e1071")
# install.packages("doParallel")

library(caret)
library(e1071)
library(doParallel)
library(dplyr)
library(randomForest)
# library(doMC)  
# registerDoMC(cores=2)
set.seed(1234)

cl <- makeCluster(detectCores())
registerDoParallel(cl)

data_train <- read.csv("https://raw.githubusercontent.com/guru99-edu/R-Programming/master/train.csv") %>% na.omit()
data_test <- read.csv("https://raw.githubusercontent.com/guru99-edu/R-Programming/master/test.csv") %>% na.omit()

data_train <- data_train %>% mutate(Survived = factor(Survived))
data_test <-  data_test %>% mutate(Survived = factor(Survived))

glimpse(data_train)
glimpse(data_test)


##### Caret
# Default setting
# K-fold cross validation is controlled by the trainControl() function
# 
# trainControl(method = "cv", number = n, search ="grid")
# arguments
# - method = "cv": The method used to resample the dataset. 
# - number = n: Number of folders to create
# - search = "grid": Use the search grid method. For randomized method, use "grid"
# Note: You can refer to the vignette to see the other arguments of the function.

# Define the control
trControl <- trainControl(method = "cv",
                          number = 10,
                          search = "grid",
                          allowParallel=TRUE
                          )

## Caret
# train(formula, df, method = "rf", metric= "Accuracy", trControl = trainControl(), tuneGrid = NULL)
# argument
# - `formula`: Define the formula of the algorithm
# - `method`: Define which model to train. Note, at the end of the tutorial, there is a list of all the models that can be trained
# - `metric` = "Accuracy": Define how to select the optimal model
# - `trControl = trainControl()`: Define the control parameters
# - `tuneGrid = NULL`: Return a data frame with all the possible combination

# Run the model
# rf_default <- train(Survived~.,
#                     data = data_train,
#                     method = "rf",
#                     metric = "Accuracy",
#                     trControl = trControl)
# # Print the results
# print(rf_default)


##
# Search best mtry
# You can test the model with values of mtry from 1 to 10
tuneGrid <- expand.grid(.mtry = c(1:5))
rf_mtry <- train(Survived~.,
                 data = data_train,
                 method = "rf",
                 metric = "Accuracy",
                 tuneGrid = tuneGrid,
                 trControl = trControl,
                 importance = TRUE,
                 nodesize = 14,
                 ntree = 300)
print(rf_mtry)

prediction <-predict(fit_rf, data_test)
confusionMatrix(prediction, data_test$survived)
varImpPlot(fit_rf)

stopCluster(cl)

# names>(getModelInfo())
