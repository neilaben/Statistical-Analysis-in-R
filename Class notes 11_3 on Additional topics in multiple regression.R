#Class Notes 11_3_2022
#Multiple Regression Interaction and ANOVA for Model Comparison 




data("PlantGrowth")
?PlantGrowth
head(PlantGrowth)
dim(PlantGrowth)
summary(PlantGrowth)
attach(PlantGrowth)


aov1 = aov(weight~group)
summary(aov1)
#               Df    Sum Sq    Mean Sq     F value     Pr(>F)  
#  group        2     3.766     1.8832      4.846       0.0159 *
#  Residuals   27     10.492    0.3886        
# All means are not equal we reject H0. 

plot(aov1) #to check assumptions
boxplot(weight~group)


#Try non-parametric, always better to go with parametric when assumptions are valid 
#Kruskal--Wallis Test
#   Nonparametric analogue to one-way ANOVA. 
#   Veiwed as ANOVA based on rank-transformed data
#   initial data are transformed to theri ranks before submitted to ANOVA 

kruskal.test(weight~group)
#data:  weight by group
#Kruskal-Wallis chi-squared = 7.9882, df = 2, p-value = 0.01842
#We can see that the p value gives evidence to reject H0. 
#note alpha = 0.05

#Additional Topics in Multiple Regression 

# An interaction effect occurs when the effect of one variable 
# depends on the value of anoather variable

#Ex. we want to see if annual income after graduating is dependent on gpa
#     but we want to add gender and we believe that gpa is affected by gender
#     i.e. its a multiple regression
#     when the affect of one variable can affect another variable that some other 
#     target variable is dependent on. 


#How do we include this in our model as we should 

#install package datarium
library(datarium)
data("marketing")
head(marketing)
?marketing
# youtube     facebook  newspaper   sales
#1  276.12    45.36     83.04       26.52
#2   53.40    47.16     54.12       12.48
#3   20.64    55.08     83.16       11.16
#4  181.80    49.56     70.20       22.20
#5  216.96    12.96     70.08       15.48
#6   10.44    58.68     90.00       8.64

#We want to find how much each marketing platform has an affect on sales (dependent v)
#  we can start with a pairwise chart to see 

model1 = lm(sales ~ youtube + facebook + newspaper, data = marketing)
summary(model1)
#Residuals:
#  Min       1Q   Median       3Q      Max 
#-10.5932  -1.0690   0.2902   1.4272   3.3951 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  3.526667   0.374290   9.422   <2e-16 ***
#  youtube      0.045765   0.001395  32.809   <2e-16 ***
#  facebook     0.188530   0.008611  21.893   <2e-16 ***
#  newspaper   -0.001037   0.005871  -0.177     0.86    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 2.023 on 196 degrees of freedom
#Multiple R-squared:  0.8972,	Adjusted R-squared:  0.8956 
#F-statistic: 570.3 on 3 and 196 DF,  p-value: < 2.2e-16

#we see that model1 includes newspapers which is very insignificant 

model2 = lm(sales ~ youtube + facebook , data = marketing)
summary(model2)
#Residuals:
#Min       1Q   Median       3Q      Max 
#-10.5572  -1.0502   0.2906   1.4049   3.3994 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  3.50532    0.35339   9.919   <2e-16 ***
#  youtube      0.04575    0.00139  32.909   <2e-16 ***
#  facebook     0.18799    0.00804  23.382   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 2.018 on 197 degrees of freedom
#Multiple R-squared:  0.8972,	Adjusted R-squared:  0.8962 
#F-statistic: 859.6 on 2 and 197 DF,  p-value: < 2.2e-16


#keep model 2 as both are significant, include interaction between independent
# variables 


model3 = lm(sales~ youtube + facebook + youtube*facebook, data = marketing)
summary(model3)
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-7.6039 -0.4833  0.2197  0.7137  1.8295 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)      8.100e+00  2.974e-01  27.233   <2e-16 ***
#  youtube          1.910e-02  1.504e-03  12.699   <2e-16 ***
#  facebook         2.886e-02  8.905e-03   3.241   0.0014 ** 
#  youtube:facebook 9.054e-04  4.368e-05  20.727   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 1.132 on 196 degrees of freedom
#Multiple R-squared:  0.9678,	Adjusted R-squared:  0.9673 
#F-statistic:  1963 on 3 and 196 DF,  p-value: < 2.2e-16

#NOTE IF THE INTERACTION IS SIGNIFICANT, MUST KEEP MAIN VARIABLES IN MODEL
# IE IF FACEBOOK*YOUTUBE IS SIGNIFICANT WHICH IT IS, 
# THEN WE MUST KEEP YOUTUBE AND FACEBOOK INDEPENDENTLY
# NOTE Adjusted R-squared:  0.9673 HAS SIGNIFICANTLY IMPROVED 

#To investigate how good the model is R^2 a good statistic 
# however we can also split into training and testing set 

#lets look at dimension 
dim(marketing)
#[1] 200   4

#split data into our training and testing sets 
#training data will be used to build the model 
#test data will be used to test the efficacy of the model 

#here we will do a 90% training 10% testing split 
#randomly sampled 

#from number of rows in marketing data randomly sample 90% of dataset 
nrow(marketing*0.90)
nrow(marketing)

# sample from rows 1:20, 8 observations 
#sample(1:20, 8)
#9 16  3 15 12  4 13 17


TrainIndex = sample(1:nrow(marketing), 0.9*nrow(marketing))
TrainIndex

train.data = marketing[TrainIndex, ]
test.data = marketing[-TrainIndex,]

dim(train.data)
# 180   4
dim(test.data)
#20  4

model1 = lm(sales ~ youtube + facebook, data = train.data)
summary(model1)

#Residuals:
#Min       1Q   Median       3Q      Max 
#-10.5419  -0.9958   0.2871   1.4018   3.3653 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 3.476025   0.375627   9.254   <2e-16 ***
#  youtube     0.046032   0.001479  31.123   <2e-16 ***
#  facebook    0.188283   0.008536  22.058   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 2.022 on 177 degrees of freedom
#Multiple R-squared:  0.8977,	Adjusted R-squared:  0.8966 
#F-statistic: 776.7 on 2 and 177 DF,  p-value: < 2.2e-16


#IF you find that overfitting is there check assumptions
# maybe multiple colinearity 
# youll see overfitting when adjusted r^2 is better for train data not test data 


model2 = lm(sales ~ youtube + facebook + youtube:facebook, data = train.data)
summary(model2)
#Residuals:
#Min      1Q  Median      3Q     Max 
#-7.6535 -0.4988  0.2576  0.7184  1.7640 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)      8.016e+00  3.195e-01  25.087  < 2e-16 ***
#  youtube          1.930e-02  1.631e-03  11.833  < 2e-16 ***
#  facebook         3.168e-02  9.514e-03   3.329  0.00106 ** 
#  youtube:facebook 9.011e-04  4.702e-05  19.163  < 2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 1.154 on 176 degrees of freedom
#Multiple R-squared:  0.9669,	Adjusted R-squared:  0.9663 
#F-statistic:  1712 on 3 and 176 DF,  p-value: < 2.2e-16

#WE COMPARED THE INTERACTION MODEL WITH MODEL WITHOUT INTERACTION

predict1 = predict(model1, test.data)
predict1
#2        13        39        55        57        97        99 
#14.813562 12.721184 11.889386 24.494171 10.228174 15.181885 29.035791 
#118       128       132       140       143       161       168 
#7.876976  7.906129 18.780424 23.608330 23.157254 17.094128 16.074174 
#169       170       171       174       178       197 
#20.706516 21.575211  8.858838 14.382312 14.639899  9.786569 

predict2 = predict(model2, test.data)
predict2
#2        13        39        55        57        97        99 
#12.809687 10.985355 11.522202 25.011308  9.519357 13.621909 32.233780 
#118       128       132       140       143       161       168 
#9.894733  9.872999 15.264927 24.499305 23.883290 16.749848 14.397545 
#169       170       171       174       178       197 
#20.497216 18.912471 10.367309 13.736697 13.976172 10.982406 


#How do we know which prediction model is better? 
# we look for the overall root mean squared error RMSE and R^2
#install caret package for rmse

library(caret)

RMSE(predict1, test.data$sales)
#1.98566

RMSE(predict2, test.data$sales)
#0.9269526

R2(predict1, test.data$sales)
#0.8932815

R2(predict2, test.data$sales)
#0.9778313


#model2 is better because it has lower RMSE compared to model1
#which is a measure of how close the predicted values are to actual valies
#and higher R^2 

#we can also see the correlation 
plot(predict1, test.data$sales)
#if they are close to each other they should be on a straight line
# here we can see they are 

plot(predict2, test.data$sales)
#we can see that this line is straighter 

#If the model is excellent then there will be a strong agreement 
# between observed and predicted values and the plotted values lies 
# closer to diagonal 


cor(predict2, test.data$sales)
 #0.9888535
cor(predict1, test.data$sales)
 #0.9451357 

#To get R^s for test data

cor(predict2, test.data$sales)^2
#0.9778313
cor(predict1, test.data$sales)^2
#0.8932815



#another way of comparing model is using ANOVA
# not same aov() function but similar concept, anova()

# if model1 y = b_0 + b_1 x_1 + b_2 X_2
# and model2 has different beta's but similar equation 
# if model2 is a subset of model2 then we can compare models 
# using anova()

#lets test using anova if model1 is equally good to model2 (hypothesis)
anova(model1, model2) 
#Analysis of Variance Table
#Model 1: sales ~ youtube + facebook
#Model 2: sales ~ youtube + facebook + youtube:facebook
#Res.Df    RSS Df Sum of Sq      F    Pr(>F)    
#1    177 723.42                                  
#2    176 234.38  1    489.04 367.23 < 2.2e-16 ***
  ---
#since p-value < 0.05 choose the biggr model (complex model)
  
# different anoval for if model1 had included newpaper 
  
  

  
  
  
  
  
  
  
  
  
  
  
  

