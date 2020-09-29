#
# Curso Aprendizaje Automatico - 2014
#
#
# Boosting
#
rm(list=ls())
library(adabag)
data(BreastCancer) # de la library(mlbench)


?BreastCancer
#A data frame with 699 observations on 11 variables
#       [,1]   Id               Sample code number          
#       [,2]   Cl.thickness     Clump Thickness             
#       [,3]   Cell.size        Uniformity of Cell Size     
#       [,4]   Cell.shape       Uniformity of Cell Shape    
#       [,5]   Marg.adhesion    Marginal Adhesion           
#       [,6]   Epith.c.size     Single Epithelial Cell Size 
#       [,7]   Bare.nuclei      Bare Nuclei                 
#       [,8]   Bl.cromatin      Bland Chromatin             
#       [,9]   Normal.nucleoli  Normal Nucleoli             
#       [,10]  Mitoses          Mitoses                     
#       [,11]  Class            Class                       

dim(BreastCancer)
str(BreastCancer)
head(BreastCancer)




# cosntruimos TRAINING y TEST set
BC <- BreastCancer[,-1]
train <- sample(1:nrow(BC),(2*nrow(BC))/3) #466
test <- seq(nrow(BC))[!(seq(nrow(BC))%in% train)]

BC.train <- BC[train,]
BC.test <- BC[test,]

#
# hagamos un arbolito simplon solo para comparar
#
library(rpart)
arbol <- rpart(Class~.,BC.train)
summary(arbol)
labels(arbol, pretty=T)
plot(arbol, uniform=T, branch=0.5, margin=0.05)
text(arbol, cex=0.7, pretty=0, fancy=T, use.n=T,fwidth=0.3, fheight=0.3)


# Error en TRAINING SET
aux <- (predict(arbol)>0.5); for (j in 1:2) aux[,j] <- aux[,j]*j ; A <- rowSums(aux)
# 'matriz de confusion'
table(BC.train$Class,A)
round(addmargins(prop.table(table(BC.train$Class,A))),4)*100

# Error en TEST SET
predCART <- predict(arbol,BC.test)
aux <- (predCART>0.5); for (j in 1:2) aux[,j] <- aux[,j]*j ; B <- rowSums(aux)
# 'matriz de confusion'
table(BC.test[,10],B)
tabCART <- round(addmargins(prop.table(table(BC.test[,10],B))),4)*100


#
# hagamos bagging para comparar
#
?bagging
m1.bagg <- bagging(Class~.,BC.train)
attributes(m1.bagg)

# Error en TRAINING SET
aux <- predict(m1.bagg,BC.train)
# 'matriz de confusion'
aux$confusion; round(addmargins(prop.table(aux$confusion)),4)*100

# Error en TEST SET
predBAGG <- predict(m1.bagg,BC.test)
# 'matriz de confusion'
predBAGG$confusion;
tabBAGG <- round(addmargins(prop.table(predBAGG$confusion)),4)*100


#
# AHORA SI boosting
#

?boosting
# boosting(formula, data, boos = TRUE, mfinal = 100, coeflearn = Breiman, control)
# formula:    data:
# boos: if TRUE (by default), a bootstrap sample of the training set is drawn using the weights for each observation on that iteration. If FALSE, every observation is used with its weights.
# mfinal: an integer, the number of iterations for which boosting is  run or the number of trees to use. Defaults to 'mfinal=100'  iterations.
# coeflearn: if Breiman(by default), alpha=1/2ln((1-err)/err) 
#            if Freund alpha=ln((1-err)/err)
#            if Zhu  SAMME with  alpha=ln((1-err)/err)+ln(nclasses-1)
# control:   rpart.control 


m1.boos <- boosting( Class~. , data=BC.train)

names(m1.boos)
#cada uno de los arboles contruidos
m1.boos$trees
#la clase predicha por el boosting
m1.boos$class
#voto (ponderado \alpha) de cada observacion a cada clase
m1.boos$votes
# prop de votos en en ensamble final
m1.boos$prob
#los pesos asignados a cada arbol
m1.boos$weights



# Error en TRAINING SET
table(BC.train$Class,m1.boos$class)

# Error en TEST SET
predBOOS <- predict.boosting(m1.boos,BC.test)
# 'matriz de confusion'
predBOOS$confusion
tabBOOS <- round(addmargins(prop.table(predBOOS$confusion)),4)*100




tabCART
tabBAGG
tabBOOS


## como evoluciona el error en el ensamble

?errorevol

# error adaboost  bagging
errBAGG=errorevol(m1.bagg,newdata=BC.test)
errBOOS=errorevol(m1.boos,newdata=BC.test)

#graficos

plot(errBAGG$error, type="l",xlab="Iterations", ylab="Error", col = "red")
lines(errBOOS$error, col = "blue")




#importancia de las variables
par(mfrow=c(2,1))       
barplot(sort(m1.bagg$importance, dec=T),col='red',horiz=TRUE)
barplot(sort(m1.boos$importance, dec=T),col='blue',horiz=TRUE)
