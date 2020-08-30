data("Auto")
head(Auto)
# i
# "mpg", "cylinders", "displacement", "horsepower", "weight", "acceleration", "year", "origin", "name"
names(Auto)
# ii
range(Auto[,1])
range(Auto[,2])
range(Auto[,3])
summary(Auto,)

#
for (k in seq(1:4)){
  if (k >= 2){
    print(k)
  }
  
}