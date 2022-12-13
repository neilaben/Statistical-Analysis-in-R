#Project 1: Regression Analysis on California Housing Data 
# Target variable/dependent variable == Median House Price

# install these packages and call on their libraries
library(archive) 
library(readr)
library(tidyverse)
library(ggplot2)

library(tidyr)
library(readr)

library(purrr)
library(dplyr)
library(stringr)

library(forcats)
library(corrplot)
library(GGally)
library(caret)

library(fmsb)
library(UsingR)

library(car)
library(mlbench)
library(reshape)
library(olsrr)
library(Hmisc)
library(vtable)
library(ggpubr)


housing = read.csv(file.choose())
housing

#Exploratory Data Analysis

dim(housing)
#20640    10

colnames(housing)
# "longitude"          "latitude"           "housing_median_age"
# "total_rooms"        "total_bedrooms"     "population"        
# "households"         "median_income"      "median_house_value"
# "ocean_proximity"   

st(housing)

#Check if there are any missing values
sum(is.null(housing))
housing[1==""]=NA
housing[2==""]=NA
housing[3==""]=NA
housing[4==""]=NA
housing[5==""]=NA
housing[6==""]=NA
housing[7==""]=NA
housing[8==""]=NA
housing[9==""]=NA
housing[10==""]=NA


sum(is.na(housing))
#207
mean(is.na(housing))*100
#0.1002907      Less than 1% of data is missing, we will omit these
housing = na.omit(housing)
dim(housing)  #new size of data 
#20433    10
sum(is.na(housing))
# 0       Nothing is missing now

summary(housing)
str(housing)
st(housing)


housing["ocean_proximity"][housing["ocean_proximity"] == "NEAR BAY"] = "NEAR WATER"
housing["ocean_proximity"][housing["ocean_proximity"] == "NEAR OCEAN"] = "NEAR WATER"
housing$ocean_proximity <- factor(housing$ocean_proximity, 
                                  levels = unique(housing$ocean_proximity), 
                                  order = TRUE, labels = 1:4)

housing$ocean_proximity <- as.numeric(housing$ocean_proximity)
summary(housing)

housing$median_house_value <- log(housing$median_house_value)
summary(housing)
st(housing)

str(housing)

summary(housing)

#we will drop latitude and longitude, we dont need it, its useless information
housing = housing[3:10]

colnames(housing)
st(housing)

#Plotting correlations, pairwise
#corrplot(cor(housing[1:7]), method = 'circle', type="upper")
corrplot(cor(housing[1:7]))
cor(housing[1:7])

#histogram all columns
ggplot(gather(housing), aes(value)) + 
  geom_histogram(bins = 10) + 
  facet_wrap(~key, scales = 'free_x')


#boxplot for all columns
ggplot(stack(housing), aes(x = ind, y = values)) +
  geom_boxplot()


#density plot for median housing
housing %>% 
  ggplot(aes(median_house_value)) +
  stat_density() + 
  theme_bw()



#Plot pairwise correlation for each 2 attributes

ggpairs(housing)

#Plot attribute distribution over data set for ocean proximity
ggplot(housing, aes(median_house_value, 
                    colour = ocean_proximity, 
                    fill = ocean_proximity)) + geom_density() #Plot showing how the data is distributed
#We can see that near ocean is nearly the same as near bay 

ggcorr(data = housing,
       label = TRUE,
       label_size = 3,
       color = "grey50")



ggplot(housing, aes(x = median_house_value) +
  geom_histogram(binwidth = 5000) +
  ggtitle("Figure 1 Histogram of Median House Price") +
  ylab("Count of Houses") +
  xlab("Median House Price") )
  theme_classic()
  theme(plot.title = element_text(hjust = 0.5))



#Building the Model
set.seed(1234)
#We will use 70% of the dataset for training, 20% for testing
nrow(housing)*0.70
nrow(housing)

TrainIndex = sample(1:nrow(housing), 0.7*nrow(housing))
training = housing[TrainIndex, ]
testing = housing[-TrainIndex, ]
 
dim(training)
#14447     8
dim(testing)
#6193    8

mod = lm(median_house_value~., data = training)
mod
ok <- ols_step_all_possible(mod)
k <- ols_step_best_subset(mod)
k
#plot(k)
summary(mod)
plot(mod)
RMSE(k, testing$median_house_value)
R2(k, testing$median_house_value)


modelall = lm(median_house_value ~ total_rooms + total_bedrooms +
                population + households + ocean_proximity,
              data = training)
all <- ols_step_best_subset(modelall)


#Recall our current variables:
#"housing_median_age" "total_rooms"        "total_bedrooms"    
# "population"         "households"         "c"     
# "median_house_value" "ocean_proximity" 

#model using all of them
#MODEL 1
modelall = lm(median_house_value ~ total_rooms + total_bedrooms +
                population + households + ocean_proximity,
              data = training)
#Test model for multicollinearity
#Greater than 5 for variable is bad, remove it, greater than 10 is very bad 
vif(modelall)
summary(modelall)
plot(modelall)

#This is a very bad model. Note: 
#Multiple R-squared:  0.3792,	Adjusted R-squared:  0.3789 

predictall = predict(modelall, testing)

#Test how the model performed
RMSE(predictall, testing$median_house_value)
#92167.19
R2(predictall, testing$median_house_value)
# 0.3769473

#Clearly, including all attributes leads to a bad model
#MODEL 2
modelIncome = lm(median_house_value ~ median_income, data = testing)

#Test model for multicollinearity NA since less than 2 variables 
predictIncome = predict(modelIncome, testing)



#Testing how the model perfomed
RMSE(predictIncome, testing$median_house_value)
#84658.32
R2(predictIncome, testing$median_house_value)
# 0.4741608

#Model with just the median income as an attribute perfomed better 
# than including all attributes but not by much 


#We know that:
#total_rooms, total_bedroom, Households and population area highly correlated
#MODEL 3
model3 = lm(median_house_value ~ total_rooms + total_bedrooms +
              population + households + ocean_proximity +
              median_income + housing_median_age +
              total_rooms:total_bedrooms:households:population, data = training)
#Test model for multicollinearity
vif(model3)
summary(model3)
predict3 = predict(model3, testing)
RMSE(predict3, testing$median_house_value)
#71846.46
R2(predict3, testing$median_house_value)
#0.6197898
plot(model3)

#So far model3 is our best model 
#lets try removing ocean_proximity
#MODEL 4
model4 = lm(median_house_value ~ total_rooms + total_bedrooms +
              population + households + median_income +
              total_rooms:total_bedrooms:households:population, data = training)
#Test model for multicollinearity
vif(model4)
predict4 = predict(model4, testing)


RMSE(predict4, testing$median_house_value)
#80464.41
R2(predict4, testing$median_house_value)
#0.5250211


#MODEL5
model5 = lm(median_house_value ~ total_rooms + total_bedrooms +
              population + households + ocean_proximity +
              median_income + housing_median_age +
              total_rooms:total_bedrooms:households:population, data = training)

#Test model for multicollinearity
vif(model5)
predict5 = predict(model5, testing)
RMSE(predict5, testing$median_house_value)
# 71418
R2(predict5, testing$median_house_value)
#  0.6258676

#Model3 = Model5, no difference in RMSE and R2 values

model55 = lm(median_house_value ~ + ocean_proximity +
              median_income + housing_median_age  data = training)
vif(model55)
predict55 = predict(model55, testing)
RMSE(predict55, testing$median_house_value)
R2(predict55, testing$median_house_value)

#MODEL 6
model6 = lm(median_house_value ~ 
              ocean_proximity + median_income + housing_median_age,
              data = training)
#Test model for multicollinearity
vif(model6)
predict6 = predict(model6, testing)
RMSE(predict6, testing$median_house_value)
# 71418
R2(predict6, testing$median_house_value)
#  0.6258676


#MODEL 7
model7 = lm(median_house_value ~median_income,data = training)
#Test model for multicollinearity
vif(model7)
predict7 = predict(model7, testing)
RMSE(predict7, testing$median_house_value)
# 71418
R2(predict7, testing$median_house_value)
#  0.6258676



