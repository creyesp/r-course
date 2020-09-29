#
# Curso Aprendizaje Automatico - 2014
#
#
# Ejemplo del ISLR- James,Witten,Hastie,Tibshirani
#

rm(list=ls())

#
# arbol de regresion
#

library(tree); library(MASS)
set.seed(1)

?Boston
# Conjunto de entrenamiento # toman una muestra de tamaño 253(nrow(Boston)/2)
train <- sample(1:nrow(Boston), nrow(Boston)/2)

# arbol de regresion (valor mediano de las propiedades)

tree.boston <- tree(medv~.,Boston,subset=train)
summary(tree.boston)
plot(tree.boston); text(tree.boston,pretty=0)

?cv.tree
#     Runs a K-fold cross-validation experiment to find the deviance or
#     number of misclassifications as a function of the cost-complexity
#     parameter k.

#     cv.tree(object, rand, FUN = prune.tree, K = 10, ...)

#  object: An object of class "tree".
#    rand: Optionally an integer vector of the length the number of
#          cases used to create object, assigning the cases to
#          different groups for cross-validation.
#     FUN: The function to do the pruning.
#       K: The number of folds of the cross-validation.

# 10-fold cross-validation si se poda el arbol
cv.boston <- cv.tree(tree.boston)
# como cae la deviance
plot(cv.boston$size,cv.boston$dev,type='b')

#se poda el arbol en con 5 nodos terminales
prune.boston <- prune.tree(tree.boston,best=5)
plot(prune.boston); text(prune.boston,pretty=0)

# grafiquemos el orignial y el podado
par(mfrow=c(1,2))
plot(tree.boston); text(tree.boston,pretty=0)
plot(prune.boston); text(prune.boston,pretty=0)
par(mfrow=c(1,1))

#se predice sobre el Conjunto de Testeo :## Boston[-train,]
yhat <- predict(tree.boston,newdata=Boston[-train,])

#valores de 'medv' en el el Test Set
boston.test <- Boston[-train,"medv"]

par(mfrow=c(1,1))
plot(boston.test,yhat);abline(0,1)
mean((yhat-boston.test)^2)   #25.04559

#
# Bagging 
#

# Hacen bagging utilizando la library(randomForest)
# utilizando TODOS los predictores en cada nodo: mtry=13 # dim(Boston)


library(randomForest)
set.seed(1)
bag.boston <- randomForest(medv~.,data=Boston,subset=train,mtry=13,importance=TRUE)
bag.boston
plot(bag.boston)

#se predice sobre el Conjunto de Testeo :## Boston[-train,]
yhat.bag <- predict(bag.boston,newdata=Boston[-train,])

#valores de 'medv' en el el Test Set
boston.test <- Boston[-train,"medv"]

plot(yhat.bag, boston.test); abline(0,1)

mean((yhat.bag-boston.test)^2) #13.47349

# lo mismo con ntree=25 
bag.boston <- randomForest(medv~.,data=Boston,subset=train,mtry=13,ntree=25)
yhat.bag <- predict(bag.boston,newdata=Boston[-train,])
mean((yhat.bag-boston.test)^2) #13.43068

#
MSE <- NULL
for (NT in c(seq(25),seq(25,500,25))){
    bag.boston <- randomForest(medv~.,data=Boston,subset=train,mtry=13,ntree=NT)
    yhat.bag <- predict(bag.boston,newdata=Boston[-train,])
    MSE <- c(MSE,mean((yhat.bag-boston.test)^2))}

plot(MSE)
