rm(list=ls())

library(dplyr)
library(ggplot2)


# Administrative            
# Administrative_Duration   
# Informational             
# Informational_Duration    
# ProductRelated            
# ProductRelated_Duration   
# BounceRates               
# ExitRates                 
# PageValues                
# SpecialDay                
# Month                     
# OperatingSystems          
# Browser                   
# Region                    
# TrafficType               
# VisitorType               
# Weekend                   
# Revenue                   

path_data = '/home/creyesp/Projects/repos/r-course/data/online_shoppers_intention.csv'
data = read.csv(path_data)

glimpse(data)
summary(data$Administrative)

names(data)

ggplot(data, aes(x=Administrative, y=Administrative_Duration)) + 
  geom_point() + 
  geom_smooth()

ggplot(data, aes(x=factor(Administrative), y=Administrative_Duration)) + 
  geom_boxplot()
