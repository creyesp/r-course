#
# Curso Aprendizaje Automatico - 2014
#
#
# CART - Ejemplito viejo con IRIS 
#

rm(list=ls())


data(iris)

library(rpart)         # libreria
library(help=rpart)    # ayuda de la la libreria

?rpart                 # ayuda de la funcion rpart


m.entrena <- c(1:35,51:85,101:135)       # filas que se incluiran en  muestra de ENTRENAMIENTO 
m.clasifi <- !(seq(150) %in% m.entrena)  # filas que se incluiran en  muestra de CLASIFICACION
L.iris <- iris[m.entrena,]               # Muestra de  ENTRENAMIENTO   
C.iris <- iris[m.clasifi,]               # Muestra de  CLASIFICACION


# CART sobre los datos de entrenamiento
arbol<-rpart(Species~.,L.iris)
summary(arbol)



# etiquetas
labels(arbol, pretty=T)

# SE GRAFICA SALIDA DE rpart
plot(arbol)
text(arbol)


# CAMBIANDO EL GRAFICO
plot(arbol, uniform=T, branch=0)
plot(arbol, uniform=T, branch=1)

# etiquetas
labels(arbol, pretty=T)

# SE GRAFICA SALIDA DE rpart
#postscript('./graph/ggCART.eps')
plot(arbol, uniform=T, branch=0.5, margin=0.05)
text(arbol, cex=0.7, pretty=0, fancy=T, use.n=T,fwidth=0.3, fheight=0.3)
#dev.off()


# que devuelve predict ?
predict(arbol)

# como clasific las que uso para entrenar # ya lo sabemos por el pretty tree que hicimos
aux <- (predict(arbol)>0.5)
for (j in 1:3) aux[,j] <- aux[,j]*j
A <- rowSums(aux)
A <- as.factor(A); levels(A) <- c('setosa', 'versicolor', 'virginica')

table(L.iris$Species,A) # 'matriz de confusion'


# como clasifica en la muestra de test/clasificacion
predicho <- predict(arbol,C.iris[,1:4])

aux <- (predicho>0.5)
for (j in 1:3) aux[,j] <- aux[,j]*j
A <- rowSums(aux)
A <- as.factor(A); levels(A) <- c('setosa', 'versicolor', 'virginica')

table(C.iris$Species,A) # 'matriz de confusion'


