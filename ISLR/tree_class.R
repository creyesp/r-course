install.packages("rpart.plot")	
library(dplyr)
library(rpart)
library(rpart.plot)


create_train_test <- function(data, size = 0.8, train = TRUE) {
  n_row = nrow(data)
  total_row = size * n_row
  train_sample <- 1:total_row
  
  if (train == TRUE) {
    return (data[train_sample, ])
  } else {
    return (data[-train_sample, ])
  }
}

accuracy <- function(y_true, y_pred) {
  table_mat <- table(y_true, y_pred)
  accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
  return(accuracy_Test)
}

set.seed(678)
path <- 'https://raw.githubusercontent.com/guru99-edu/R-Programming/master/titanic_data.csv'
titanic <-read.csv(path)
head(titanic)


titanic <- titanic[sample(1:nrow(titanic)), ]

##
# Drop variables
clean_titanic <- titanic %>%
  select(-c(home.dest, cabin, name, x, ticket)) %>% 
  mutate(pclass = factor(pclass, levels = c(1, 2, 3), labels = c('Upper', 'Middle', 'Lower')),
         survived = factor(survived, levels = c(0, 1), labels = c('No', 'Yes'))) %>%
  na.omit()
glimpse(clean_titanic)

##
data_train <- create_train_test(clean_titanic, 0.8, train = TRUE)
data_test <- create_train_test(clean_titanic, 0.8, train = FALSE)
dim(data_train)

##
prop.table(table(data_train$survived))
prop.table(table(data_test$survived))

##
fit <- rpart(survived~., data = data_train, method = 'class')
rpart.plot(fit, extra = 106)
##
predict_unseen <-predict(fit, data_test, type = 'class')
table_mat <- table(data_test$survived, predict_unseen)
table_mat
accuracy(data_test$survived, predict_unseen)
##
control <- rpart.control(minsplit = 4,
                         minbucket = round(5 / 3),
                         maxdepth = 3,
                         cp = 0)
tune_fit <- rpart(survived~., data = data_train, method = 'class', control = control)
y_pred = predict(tune_fit, data_test, type = 'class')
accuracy(data_test$survived, y_pred)


##
y_proba = predict(tune_fit, data_test, type = 'prob')
y_proba
