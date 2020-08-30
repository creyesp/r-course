library(ISLR)
data()

# ways to create a vector
x <- c(1,2,3,4)

x <- rnorm(50)
y <- x + rnorm(50, mean = 10, sd = 0.5)
plot(x, y, xlab = 'features', ylab = 'y values', main = 'title')

x = seq(0,10, 5)
x = 0:10
x = seq(-pi, pi, by=1)
x = seq(-pi, pi, length=10)

# information 
length(c(1,2,3,4))
dim(x)

# plotting
y = x 
f = outer(x ,y , function (x , y ) cos ( y ) /(1+ x ^2) )
contour(x, y, f, nlevels = 20, add = T)
fa = (f - t(f))/2
## countorn plots
facontour(x, y, fa, nlevels = 20, add = T)
## color plors
image(x,y,fa, theta=30, phi=20)
## 3d plots
persp(x,y,fa)

# Matrix 
m = matrix(1:12, 3, 4, byrow=T)
m = matrix(1:12, 3, byrow=T)
m = cbind(m, c(1,1,1))
m = rbind(m, c(2,2,2,2,2))
m
# indexing 
# [row, col]
x = matrix(1:9, 3, 3, byrow = TRUE)
x[3, 3]
x[, 3]
x[c(1,3), c(1,3)]
x[1:2, 2:3]
# negative values remove items
x[-c(1,2), c(1,3)]

# factors
# Create gender vector
gender_vector <- c("Male", "Female", "Female", "Male", "Male")
class(gender_vector)
# Convert gender_vector to a factor
factor_gender_vector <-factor(gender_vector)
class(factor_gender_vector)

# DAta Frame
df = data.frame(c(1,2), c('A', 'B'), c(1.1,3.3))
names(df) = c('col1', 'col2', 'col3')
df
df[1,]
str(df)
df$col4 = c('F', 'D')
df
subset(df, subset = col1 >= 2)
df[df$col1>=2,]
# Read data
# auto = read.table("/home/creyesp/Projects/repos/r_examples/auto", sep=',', header=T, na.strings ="?")
data(Auto)
auto <- Auto
fix(auto)

# lists
ll = list(1, '#', c(1,2,3), matrix(1:4,2))
ll
ll[c(1,2)]

# missing values
na.omit(auto)

# feature name
names(auto)

plot(auto$mpg, auto$cylinders)

#
attach(auto)
plot(cylinders, mpg, col='red', varwidth=T)

hist(mpg, col=2, breaks=15)

pairs(auto)

pairs(~mpg + displacement + horsepower + weight + acceleration , Auto)

identify(horsepower, mpg, name)

summary(auto)

# improve data frame
install.packages("dplyr")
library(dplyr)
set.seed(1234)
data_frame <- tibble(  
  c1 = rnorm(50, 5, 1.5),   
  c2 = rnorm(50, 5, 1.5),  
  c3 = rnorm(50, 5, 1.5),
  c4 = rnorm(50, 5, 1.5), 	
  c5 = rnorm(50, 5, 1.5)
)
# Sort by c1
df <-data_frame[order(data_frame$c1),]
head(df)

# Sort by c3 and c4
# sort(x, decreasing = FALSE, na.last = TRUE):
df <-data_frame[order(data_frame$c3, data_frame$c4),]
head(df)

