install.packages("ISwR")
library(ISwR)
attach(thuesen)

names(thuesen)

lm1 = lm(short.velocity~blood.glucose)
summary(lm1)

#Scatter Plot
plot(short.velocity~blood.glucose, data=thuesen)
abline(lm1)
abline(lm1, col = 3)
plot(lm1)

boxplot(residuals(lm1))
    
shapiro.test(residuals(lm1))
  #data:  residuals(lm1)
  #W = 0.92413, p-value = 0.08173


#y = alpha +beta*x + epsilon 
#Regression coefficient is significant? 
# H0: beta = 0
# H1: Beta != 0 
# alpha = 0.5
# p-value  = 0.0479 < alpha
# Reject H0, theres enough evidence from the sample, 
# the regression coeff is signifianctly different from 0 

#simple linear regression : F-test
#   H0: beta = 0
#   H1: beta != 0

####      Checking Assumptions

# Linearity: In Fitted Vs. Residuals plot, 
#            points are randomly scattered 
#            around zero horizontal line 
#            without any pattern.
## So linearity assumpion seems to be valid

# QQ Plot and Boxplot of Residuals, shows that 
#     normality assumptions is some what valid. 

# Shapiro test for normality provides a p-value of 0.08
#     which also supportss normality assumption

#Equal Variance Assumption: 
#   In a residual plot, there is no clear pattern 
#   and the points are somewhat similarly distributed
#   across the zeri kube for any fitted value. 
## Equal variance assumption seems to be valid. 

#Independence Assumption:
#   Typically we assume independence as long as the data
#   was randomly collected. 





###     Confidence Interval


#The estimated regression line is: 
#     velocity = 1.0978 + 0.02196 * glucose
summary(blood.glucose)

# What is the expected velocity when glucose is 10? 
      #(mean(velocity when glucose = 10)
      # = 1.0978 + 0.02196 * 10

#What is the 95% confidence interval for expected velocity? 
# when glucose is 10? 

newdata = data.frame(blood.glucose = 10)

predict(lm1, newdata = newdata, interval = "confidence", level = 0.95) 
      #for specific CI
      # interval = "confidence" gives the interval for 95% confidence 
      # that the mean lies within
            #    fit      lwr      upr
            # 1 1.31744 1.223124 1.411757

predict(lm1, newdata = newdata, interval = "confidence") 
    #Default is 95%
       #    fit      lwr      upr
       # 1 1.31744 1.223124 1.411757
    

predict(lm1, newdata=newdata)
    #will give 1.31744 as the predicted value of the point
    #value estimation, the predicted mean of the velocity when 
    #glucose = 10


# We can be 95% confident that the expected velocity
# when glucose is 10 falls in the interval (1.22, 1.41)


#What is the prediction interval for velocity when
# glucose = 10
## NOTE: this is different form the previous question:
    #What is the prediction velocity for when glucose = 10

predict(lm1, newdata = newdata, interval = 'predict', level = .95)
      #     fit       lwr      upr
      # 1 1.31744 0.8570329 1.777847
  # In 95% of cases, the velocity, falls between in the range 
  # (0.857, 1.778) for a glucose level of 10. 


# Plot confidence interval 
plot(short.velocity~blood.glucose)

newdata = data.frame(blood.glucose = seq(5,20))
newdata

cis = predict(lm1, newdata = newdata, interval = "confidence")
cis

#plot(cis) is a simple scatter plot. 

matlines(newdata$blood.glucose, cis, col = c(2,3,3), lty=c(1,2,2))
    #matlines automatically draws the lines 
    # we first found cis for all data between 5 - 20
    #matlines will print the fitted line in red (abline)
      # and the lower interval bound and upper interval bounds
       # col = c(2, 3, 3) where 2 = red, and 3 = green
       # ity, is line type where 1 = solid and 2 = dashed


#   for prediction interval, just change interval = "predict" 
#   instead of "confidence
newdata = data.frame(blood.glucose = seq(5,20))
newdata
pis = predict(lm1, newdata = newdata, interval = "predict")
pis
matlines(newdata$blood.glucose, pis, col = c(2,3,3), lty=c(1,2,2))


