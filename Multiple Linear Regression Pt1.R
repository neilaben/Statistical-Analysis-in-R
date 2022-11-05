#Multiple Linear Regresion

library(UsingR)
attach(mtcars)

model1 = lm(mpg~wt + hp+disp, data = mtcars)
summary(model1)

#lm(formula = mpg ~ wt + hp + disp, data = mtcars)
#Residuals:
#  Min     1Q Median     3Q    Max 
#-3.891 -1.640 -0.172  1.061  5.861 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 37.105505   2.110815  17.579  < 2e-16 ***
#  wt          -3.800891   1.066191  -3.565  0.00133 ** 
#  hp          -0.031157   0.011436  -2.724  0.01097 *  
#  disp        -0.000937   0.010350  -0.091  0.92851    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 2.639 on 28 degrees of freedom
#Multiple R-squared:  0.8268,	Adjusted R-squared:  0.8083 
#F-statistic: 44.57 on 3 and 28 DF,  p-value: 8.65e-11

plot(model1)




#y = B_0 + B_1 * X_1 + B_2 *X_2 + ... + B_p *X_p + epsilon --> N(0, sigma^2) 
#project variable 'wt' and 'hp' are significant in the model
#sigmahat = 2.639
#sigmahat = 2.639^2

#The estimated Regression model is: 
#       mpg = 37.11 - 3.8 * wt - 0.31 * hp - 0.00937 disp
#       alpha = 0.5 

#1. H0: B_1 = 0
#   H1: B_1 != 0 
# p-value = 0.0013 < alpha so we reject H0
# The variable wt is significant controlling 'hp' and 'disp' 

#2. H0: B_2 = 0
#   H1: B_2 != 0 
#p - value  = .0109
# Reject H0, B_2 is significant (hp is significant in the model)

#3. H0: B_3 = 0
#   H1: B_3 != 0 
# p-value = 0.92851
#Fail to reject H0. 'disp' variable is not significant in the model 


#4. H0: B_1 = B_2 = B_3 = 0 (model is not significant)
#   H1:  At least one B_i != (H0 is false)
#p-value = 8.65e-11 = 0 = Reject  H0
#This means the model is significant, it does not mean the model is the best


#R^2 = 0.8268 and 
#   Adjusted R^2 = 0.8083 -> 80.83% of variation in y is explained by the model



#Adjusted R-square will penalize additional variables, does not like complex model.
#more meaningful than multiple R-squared.
#But will still increase if a complex model is good 
#range is up to 1, typically [0,1] sometimes will be [-1,1] when R^2 is 
#very close to 0, it means the model is not that good
#R^2 has range of [0,1]
#For both values, the higher the better
#In regression we report R^2 and adj R^2

#Example model with only 2 variables, disp removed since its insignificant 
model2 = lm(mpg~wt + hp, data = mtcars)
model2
summary(model2)
#Multiple R-squared:  0.8268,	Adjusted R-squared:  0.8148 
#Since we went to only 2 variables and removed the insignificant 3rd variable 
# form model2, R^2 is the same but Adjusted R^2 is better, higher. 


model3 = lm(mpg~wt, data = mtcars)
model3
summary(model3)
#Multiple R-squared:  0.7528,	Adjusted R-squared:  0.7446 
# Adj R^2 is worse even with one variable because theres a better model with the 
#   two signifi Cant variables. 


## BACK TO MODEL 1
summary(model1)
plot(mpg~wt)
plot(mpg-hp)
plot(mpg~disp)

pairs(mtcars)  #considers all variables
pairs(mpg~wt + hp +disp)


#if hp and disp, which are independentally correlated, its called multiple linearity 
#Always check for multiple linearity, we should not include them in the model
modelhd = lm(hp~disp, data = mtcars)
summary(modelhd)


## ASSUMPTIONS
plot(model1)
#Residuals Vs Fitted:
#Check if the points are near 0 line, if so then we can assume linearity

#Normal QQ
#Normality, if the majority of residuals follow straight dashed line
# then assumption of normality is allowed

#Scale-Location 
# If randomly distributed with no clear pattern
#we can assume  homoscedasticity (equal variance of residuals)

#Residuals vs Leverage: 
boxplot(residuals(model1))
#see if our analysis is being skewed by any extreme patterns. 



#More on R-squared: 
#R-squared is a statistical measure of how close the data are to the fitted regression
#line. It is also known as the coefficient of determination, or the coefficient of multiple
#determination for multiple regression.
#The definition of R-squared is straight-forward; it is the percentage of the response
#variable variation that is explained by a linear model. Or:
#  R-squared = Explained variation / Total variation

#Multicollinearity, when there is a linear correlation between 
# the independent variables: Find out by: 
#   a. look at scatter plot and see if you notice any
#   b. look at correlation between each pair of independent variables
#   c. compute variance inflation factor (VIF) an R function 

install.packages("car")
library(car)



#Vif finds inflation added to variance by adding a variable to the model
# uses sum of squares 
vif(model1)
#   wt       hp     disp 
#4.844618 2.736633 7.324517 
#Here disp should be removed
#Greater than 5 for variable is bad, remove it, greater than 10 is very bad 
# these are the correlated independent variables
#Which one is disp correlated with? 
cor(mpg, wt) #-0.8676594
cor(disp, wt) #0.8879799
cor(disp, hp) # 0.7909486
cor(wt, hp) #0.6587479

vif(model2)
#there will not be high variance inflation if two 
#variables are dependent. 




