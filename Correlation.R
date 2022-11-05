## Class on 10/20/22 on Correlation 

#Pearson Correlation Coefficient 
# Used to measure the strength of linear association between 
#   two numerical variables 

#population coeff is rho whose mean is mu 
# sample is gamma and mean is xbar 

#ranges from [-1 <= gamma <= 1]




#With the rmr data set in ISwR package, plot metabolic rate versus body weight.
library(ISwR)
attach(rmr)
head(rmr)
summary(rmr)
#a) Fit a linear regression model to the relation.
    plot(metabolic.rate~body.weight) 
      #looks like a positive association in scatter plot
    
#b) According to the fitted model, what is the predicted metabolic rate for 
#   a body weight of 70 kg?
#c) Give a 95% confidence interval for the regression coefficient.
#d) Compute Pearson’s correlation coefficient.
#e) Find 90% confidence interval for correlation coefficient and interpret.
#f) Test whether there is a significant linear correlation between metabolic rate and
#   body weight

library(ISwR)
attach(rmr)
head(rmr)
summary(rmr)

plot(metabolic.rate~body.weight) 
  #looks like a positive association in scatter plot

cor(metabolic.rate,body.weight) #0.7442379
  #sample correlation coefficient shows a positive correlation 

cor.test(metabolic.rate,body.weight) #

    #Pearson's product-moment correlation
    #data:  metabolic.rate and body.weight
    #t = 7.2213, df = 42, p-value = 7.025e-09
    #alternative hypothesis: true correlation is not equal to 0
    #95 percent confidence interval:
    #0.5742343 0.8527119
    #sample estimates:  cor   0.7442379 

    #p-value is approximately 0 so we reject H0. From the sample
    #we have enough evidence to conclude that there is a significant
    #association between the two variables.

dim(rmr) #[1] 44  2 the dimension of the object rmr



cor(metabolic.rate,body.weight)^2 #0.55389
  #R squared 

lm1 = lm(metabolic.rate~body.weight)
summary(lm1) #also gives the R^2

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-245.74 -113.99  -32.05  104.96  484.81 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 811.2267    76.9755  10.539 2.29e-13 ***
#  body.weight   7.0595     0.9776   7.221 7.03e-09 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 157.9 on 42 degrees of freedom
#Multiple R-squared:  0.5539,	Adjusted R-squared:  0.5433 
#F-statistic: 52.15 on 1 and 42 DF,  p-value: 7.025e-09










