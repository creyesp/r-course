data(College)
head(College)
college <- College
fix(college)
#rownames(college) = college[,1]

# i
summary(college)
# ii
pairs(college[,2:10])
# iii
plot(college$Private, college$Outstate)
# iv
Elite = rep('No', nrow(college))
Elite[college$Top10perc>50] = 'Yes'
Elite = as.factor(Elite)
college = data.frame(college, Elite)
fix(college)
summary(college)
plot(college$Elite, college$Outstate)
# v
par(mfrow=c(2,2))
hist(college$Outstate, main="top10perc", breaks = 5)
hist(college$Outstate, main="top10perc", breaks = 10)
hist(college$Outstate, main="top10perc", breaks = 15)
hist(college$Outstate, main="top10perc", breaks = 30)
# vi
pairs(college)
par(mfrow=c(1,1))
plot(college$Apps, college$Accept, col=2)
plot(college$Accept, college$Enroll, col=2)
plot(college$Enroll, college$F.Undergrad, col=2)
plot(college$PhD, college$Grad.Rate, col=2)
plot(college$Personal, college$Grad.Rate, col=2)
plot(college$Personal, college$Grad.Rate, col=2) 

