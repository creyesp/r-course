#
# Curso Aprendizaje Automatico - 2014
#
#
# Ejemplos del Lab-Chap 8 de ISLR: Decision Trees
#

# Arboles de Clasificacion:

# libraries que utiliza
library(tree)        # La de Ripley (...)
library(help=tree)

library(ISLR)        # La del libro. Tiene el set de datos   
attach(Carseats)
?Carseats
# Datos Simulados de ventas de asientos infantiles para autos en 400 tiendas.
#     'Sales'  Ventas unitarias en cada punto (en miles)
#     'CompPrice'  Precio de la cmpetencia en cada punto
#     'Income'  Nivel de Ingreso de la zona (en miles de $)
#     'Advertising'  Presupuesto de publicidad local de la compania en cada punto(en miles de $)
#     'Population'  Poblacin de la zona (en miles)
#     'Price'  Precio del asiento infantil en cada punto
#     'ShelveLoc' Calidad de la ubicacion de los asientos en cada punto('Bad', 'Good' and 'Medium')
#     'Age' Promedio de edad de la poblacion de la zona
#     'Education' Nivel de educación de la poblacion de la zona
#     'Urban'  La tienda esta en una ubucacion urbana o rural ('No', 'Yes')
#     'US'  La tienda esta en 'auer cantry' ('No', 'Yes')


# Minima Descripcion

summary(Carseats); plot(Carseats)
plot(density(Sales))

# Discretiza la variable de ventas para clasificar
High <- ifelse(Sales<=8,'No','Yes')
Carseats <- data.frame(Carseats,High)
head(Carseats)
#
# CART
#

?tree
?tree.control
#     tree.control(nobs, mincut = 5, minsize = 10, mindev = 0.01)
#    nobs: The number of observations in the training set.
#  mincut: The minimum number of observations to include in either child
#          node. This is a weighted quantity; the observational weights
#          are used to compute the 'number'. The default is 5.
# minsize: The smallest allowed node size: a weighted quantity. The
#          default is 10.
#  mindev: The within-node deviance must be at least this times that of
#          the root node for the node to be split.

#     This function produces default values of 'mincut' and 'minsize', and ensures that 'mincut' is at most half 'minsize'.
#     To produce a tree that fits the data perfectly, set 'mindev = 0'  and 'minsize = 2', if the limit on tree depth allows such a tree.


tree.carseats <- tree(High~.-Sales,Carseats)

table(Carseats$High)/400

# Veamos el objeto
class(tree.carseats)
attributes(tree.carseats)
tree.carseats$frame
tree.carseats$y
tree.carseats
summary(tree.carseats)

# Grafico
plot(tree.carseats)
text(tree.carseats,pretty=0)

#
# Ahora crea una muestra de aprendizaje/entrenamiento y otra de validacion/testeo/clasificacion
#

set.seed(2)

train <- sample(1:nrow(Carseats), 200) # Training set
Carseats.test <- Carseats[-train,]     # Excluye Test set

# Para tabular cuando predice sobre el Test set
High.test <- High[-train] 

# No hay porque hacer un nuevo DataFrame, basta 'subset'
tree.carseats <- tree(High~.-Sales,Carseats,subset=train)

tree.pred <- predict(tree.carseats,Carseats.test,type="class")
table(tree.pred,High.test)  # 71.5%
(86+57)/200

####
?cv.tree
#     cv.tree(object, rand, FUN = prune.tree, K = 10, ...)
#  object: An object of class ‘"tree"’.
#    rand: Optionally an integer vector of the length the number of cases used to create ‘object’, assigning the cases to  different groups for cross-validation.
#     FUN: The function to do the pruning.
#       K: The number of folds of the cross-validation.

?prune.tree
#     prune.tree(tree, k = NULL, best = NULL, newdata, nwts, method = c("deviance", "misclass"), loss, eps = 1e-3)
#      prune.misclass(tree, k = NULL, best = NULL, newdata, nwts, loss, eps = 1e-3)



set.seed(3)
cv.carseats <- cv.tree(tree.carseats,FUN=prune.misclass)
names(cv.carseats)

# k default es 10 en Cross-Validation
#size: number of terminal nodes in each tree in the cost-complexity  pruning sequence.
#deviance: total deviance of each tree in the cost-complexity pruning  sequence.
#k: the value of the cost-complexity pruning parameter of each tree in the sequence.

cv.carseats

par(mfrow=c(1,2))
plot(cv.carseats$size,cv.carseats$dev,type="b")
plot(cv.carseats$k,cv.carseats$dev,type="b")

# best es el tamaño del arbol que elegimos
prune.carseats <- prune.misclass(tree.carseats,best=9)


par(mfrow=c(1,1))
plot(prune.carseats); text(prune.carseats,pretty=0)

# prediccion con el podado en 9
tree.pred <- predict(prune.carseats,Carseats.test,type="class")
table(tree.pred,High.test) # 77%

prune.carseats <- prune.misclass(tree.carseats,best=15)
plot(prune.carseats); text(prune.carseats,pretty=0)

# prediccion con el podado en 9

tree.pred <- predict(prune.carseats,Carseats.test,type="class")
table(tree.pred,High.test) # 74#






# Fitting Regression Trees

library(MASS)
set.seed(1)
train  <-  sample(1:nrow(Boston), nrow(Boston)/2)
tree.boston <- tree(medv~.,Boston,subset=train)
summary(tree.boston)
plot(tree.boston)
text(tree.boston,pretty=0)
cv.boston <- cv.tree(tree.boston)
plot(cv.boston$size,cv.boston$dev,type='b')
prune.boston <- prune.tree(tree.boston,best=5)
plot(prune.boston)
text(prune.boston,pretty=0)
yhat <- predict(tree.boston,newdata=Boston[-train,])
boston.test <- Boston[-train,"medv"]
plot(yhat,boston.test)
abline(0,1)
mean((yhat-boston.test)^2)
