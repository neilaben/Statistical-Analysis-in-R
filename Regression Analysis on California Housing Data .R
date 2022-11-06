# Regression Analysis on California Housing Data 
# Target variable/dependent variable == Median House Price

# install bthese packages and call on their libraries
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
#longitude         latitude     housing_median_age  total_rooms   
#Min.   :-124.3   Min.   :32.54   Min.   : 1.00      Min.   :    2  
#1st Qu.:-121.8   1st Qu.:33.93   1st Qu.:18.00      1st Qu.: 1450  
#Median :-118.5   Median :34.26   Median :29.00      Median : 2127  
#Mean   :-119.6   Mean   :35.63   Mean   :28.63      Mean   : 2636  
#3rd Qu.:-118.0   3rd Qu.:37.72   3rd Qu.:37.00      3rd Qu.: 3143  
#Max.   :-114.3   Max.   :41.95   Max.   :52.00      Max.   :39320  
#total_bedrooms     population      households     median_income    
#Min.   :   1.0   Min.   :    3   Min.   :   1.0   Min.   : 0.4999  
#1st Qu.: 296.0   1st Qu.:  787   1st Qu.: 280.0   1st Qu.: 2.5637  
#Median : 435.0   Median : 1166   Median : 409.0   Median : 3.5365  
#Mean   : 537.9   Mean   : 1425   Mean   : 499.4   Mean   : 3.8712  
#3rd Qu.: 647.0   3rd Qu.: 1722   3rd Qu.: 604.0   3rd Qu.: 4.7440  
#Max.   :6445.0   Max.   :35682   Max.   :6082.0   Max.   :15.0001  
#median_house_value ocean_proximity   
#Min.   : 14999     Length:20433      
#1st Qu.:119500     Class :character  
#Median :179700     Mode  :character  
#Mean   :206864                       
#3rd Qu.:264700                       
#Max.   :500001  

str(housing)
#'data.frame':	20433 obs. of  10 variables:
#$ longitude         : num  -122 -122 -122 -122 -122 ...
#$ latitude          : num  37.9 37.9 37.9 37.9 37.9 ...
#$ housing_median_age: num  41 21 52 52 52 52 52 52 42 52 ...
#$ total_rooms       : num  880 7099 1467 1274 1627 ...
#$ total_bedrooms    : num  129 1106 190 235 280 ...
#$ population        : num  322 2401 496 558 565 ...
#$ households        : num  126 1138 177 219 259 ...
#$ median_income     : num  8.33 8.3 7.26 5.64 3.85 ...
#$ median_house_value: num  452600 358500 352100 341300 342200 ...
#$ ocean_proximity   : chr  "NEAR BAY" "NEAR BAY" "NEAR BAY" "NEAR BAY" ...
#- attr(*, "na.action")= 'omit' Named int [1:207] 291 342 539 564 697 739 1098 1351 1457 1494 ...
#..- attr(*, "names")= chr [1:207] "291" "342" "539" "564" ...

#we will drop latitude and longitude, we dont need it 
housing = housing[3:10]
colnames(housing)
#"housing_median_age" "total_rooms"        "total_bedrooms"    
# "population"         "households"         "median_income"     
# "median_house_value" "ocean_proximity" 

#Plotting correlations, pairwise
corrplot(cor(housing[1:7]), method = 'circle', type="upper")

cor(housing[1:7])
#                    housing_median_age  total_rooms   total_bedrooms
#housing_median_age          1.0000000  -0.3606283    -0.32045104
#total_rooms                -0.3606283   1.0000000     0.93037950
#total_bedrooms             -0.3204510   0.9303795     1.00000000
#population                 -0.2957873   0.8572813     0.87774674
#households                 -0.3027680   0.9189915     0.97972827
#median_income              -0.1182777   0.1978815    -0.00772285
#median_house_value          0.1064320   0.1332941     0.04968618

#                    population   households   median_income 
#housing_median_age -0.295787297 -0.30276797  -0.118277723
#total_rooms         0.857281251  0.91899153   0.197881519
#total_bedrooms      0.877746743  0.97972827  -0.007722850
#population          1.000000000  0.90718590   0.005086624
#households          0.907185900  1.00000000   0.013433892
#median_income       0.005086624  0.01343389   1.000000000
#median_house_value -0.025299732  0.06489355   0.688355475

#                        median_house_value
#housing_median_age         0.10643205
#total_rooms                0.13329413
#total_bedrooms             0.04968618
#population                -0.02529973
#households                 0.06489355
#median_income              0.68835548
#median_house_value         1.00000000


# Barlett Sphericity Test for checking the possibility of data dimension reduction 
print(cortest.bartlett(housing[1:7], nrow(median_house_price)),digits = 8)


housing %>% 
  ggplot(aes(median_house_value)) +
  stat_density() + 
  theme_bw()


#Principle Component Analysis: 
#total_rooms, total_bedroom, Households and population area highly correlated
#this will lead to the most amount of variability in the dataset


prcomp(housing[1:7])

#Plot pairwise correlation for each 2 attributes
ggpairs(housing)

#Plot attribute distribution over data set for ocean proximity
ggplot(housing, aes(median_house_value, 
                    colour = ocean_proximity, 
                    fill = ocean_proximity)) + geom_density() #Plot showing how the data is distributed
#We can see that near ocean is nearly the same as near bay 

ggplot(housing, aes(x = median_house_value) +
  geom_histogram(binwidth = 5000) +
  ggtitle("Figure 1 Histogram of Median House Price") +
  ylab("Count of Houses") +
  xlab("Median House Price") )
  #theme_classic())
  #theme(plot.title = element_text(hjust = 0.5))

#Building the Model


set.seed(1000)
#We will use 80% of the dataset for training, 20% for testing
nrow(housing*0.80)
nrow(housing)

TrainIndex = sample(1:nrow(housing), 0.8*nrow(housing))
training = housing[TrainIndex, ]
testing = housing[-TrainIndex, ]

dim(training)
#18389     8
dim(testing)
#2044    8

#Recall our current variables:
#"housing_median_age" "total_rooms"        "total_bedrooms"    
# "population"         "households"         "c"     
# "median_house_value" "ocean_proximity" 

#model using all of them
#MODEL 1
modelall = lm(median_house_value ~ total_rooms + total_bedrooms +
                population + households + households + ocean_proximity,
              data = training)
#Test model for multicollinearity
#Greater than 5 for variable is bad, remove it, greater than 10 is very bad 
vif(modelall)
summary(modelall)
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-379969  -55587  -17856   37789 1139573 
#Coefficients:
#                         Estimate Std.   Error    t value    Pr(>|t|)    
#(Intercept)                  2.424e+05  1.374e+03 176.427  < 2e-16 ***
#  total_rooms                4.920e+01  8.694e-01  56.587  < 2e-16 ***
#  total_bedrooms            -2.252e+02  8.695e+00 -25.897  < 2e-16 ***
#  population                -6.101e+01  1.454e+00 -41.965  < 2e-16 ***
#  households                 1.631e+02  1.003e+01  16.261  < 2e-16 ***
#  ocean_proximityINLAND     -1.241e+05  1.587e+03 -78.195  < 2e-16 ***
#  ocean_proximityISLAND      1.509e+05  4.066e+04   3.710    0.000208 ***
#  ocean_proximityNEAR BAY    5.899e+03  2.268e+03   2.602    0.009289 ** 
#  ocean_proximityNEAR OCEAN  1.184e+03  2.137e+03   0.554    0.579477    
#Residual standard error: 90860 on 18380 degrees of freedom
#Multiple R-squared:  0.3792,	Adjusted R-squared:  0.3789 
#F-statistic:  1403 on 8 and 18380 DF,  p-value: < 2.2e-16

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
predict3 = predict(model3, testing)
RMSE(predict3, testing$median_house_value)
#71418
R2(predict3, testing$median_house_value)
#0.6258676

#So far model3 is our best model 
#lets try removing ocean_proximity
#MODEL 4
model4 = lm(median_house_value ~ total_rooms + total_bedrooms +
              population + households + households + median_income +
              total_rooms:total_bedrooms:households:population, data = training)
#Test model for multicollinearity
vif(model4)
predict4 = predict(model4, testing)


RMSE(predict4, testing$median_house_value)
#80464.41
R2(predict4, testing$median_house_value)
#0.5250211

#ocean_proximity is too valuable and removing it leads to a worst model
#lets try making the values near bay and near ocean equal 

housing["ocean_proximity"][housing["ocean_proximity"] == "NEAR BAY"] = "NEAR WATER"
housing["ocean_proximity"][housing["ocean_proximity"] == "NEAR OCEAN"] = "NEAR WATER"

#Check that changing values worked 
unique(housing["ocean_proximity"])
#ocean_proximity
#1         NEAR WATER
#702        <1H OCEAN
#955           INLAND
#8315          ISLAND




#Now try model3 with new ocean_proximity values 



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
model7 = lm(median_house_value ~ 
                ,
            data = training)
#Test model for multicollinearity
vif(model7)
predict7 = predict(model7, testing)
RMSE(predict7, testing$median_house_value)
# 71418
R2(predict7, testing$median_house_value)
#  0.6258676



full_additive_model = lm(median_house_value ~ ., data = training)
full_additive_adjr2 = summary(full_additive_model)$adj.r.squared

full_twoway_model = lm(median_house_value ~ (.)^2, data = training)
full_twoway_adjr2 = summary(full_twoway_model)$adj.r.squared

full_threeway_model = lm(median_house_value ~ (.)^3, data = training)
full_threeway_adjr2 = summary(full_threeway_model)$adj.r.squared

library(knitr)

beginning_mods_results = data.frame(
  "Total Predictors" =
    c("Additive Model" = extractAIC(full_additive_model)[1],
      "Two-Way Int. Model" = extractAIC(full_twoway_model)[1],
      "Three-Way Int. Model" = extractAIC(full_threeway_model)[1]),
  "AIC" =
    c("Additive Model" = extractAIC(full_additive_model)[2],
      "Two-Way Int. Model" = extractAIC(full_twoway_model)[2],
      "Three-Way Int. Model" = extractAIC(full_threeway_model)[2]),
  "Adj R-Squared" =
    c("Additive Model" = full_additive_adjr2,
      "Two-Way Int. Model" = full_twoway_adjr2,
      "Three-Way Int. Model" = full_threeway_adjr2))

kable(beginning_mods_results, align = c("c", "r"))
#                        | Total.Predictors |      AIC| Adj.R.Squared |
#  |:--------------------|:----------------:|--------:|:-------------:|
#  |Additive Model       |        11        | 364686.7|   0.6325858   |
#  |Two-Way Int. Model   |        48        | 363021.9|   0.6689122   |
#  |Three-Way Int. Model |       113        | 362066.1|   0.6889500   |

#Three-way model seems to be the best right now. 69% accuracy 


back_additive_mod_finish_aic = step(full_additive_model, direction = "backward", trace = 0)
both_additive_mod_finish_aic = step(full_additive_model, direction = "both", trace = 0)

n = length(resid(full_additive_model))
back_additive_mod_finish_bic = step(full_additive_model, direction = "backward", k = log(n), trace = 0)
both_additive_mod_finish_bic = step(full_additive_model, direction = "both", k = log(n), trace = 0)

back_twoway_mod_finish_aic = step(full_twoway_model, direction = "backward", trace = 0)
both_twoway_mod_finish_aic = step(full_twoway_model, direction = "both", trace = 0)

n = length(resid(full_twoway_model))
back_twoway_mod_finish_bic = step(full_twoway_model, direction = "backward", k = log(n), trace = 0)
both_twoway_mod_finish_bic = step(full_twoway_model, direction = "both", k = log(n), trace = 0)

aic_and_bic_results = data.frame(
  "AIC" =
    c("Backward" =
        c("Additive" = extractAIC(back_additive_mod_finish_aic)[2],
          "Two-Way" = extractAIC(back_twoway_mod_finish_aic)[2],
          "Three-way" = extractAIC(full_threeway_model)[2]),
      "Both" =
        c("Additive" = extractAIC(both_additive_mod_finish_aic)[2],
          "Two-Way" = extractAIC(both_twoway_mod_finish_aic)[2],
          "Three-way" = extractAIC(full_threeway_model)[2])),
  "BIC" =
    c("Backward" =
        c("Additive" = extractAIC(back_additive_mod_finish_bic)[2],
          "Two-Way" = extractAIC(back_twoway_mod_finish_bic)[2],
          "Three-way" = extractAIC(full_threeway_model)[2]),
      "Both" =
        c("Additive" = extractAIC(both_additive_mod_finish_bic)[2],
          "Two-Way" = extractAIC(both_twoway_mod_finish_bic)[2],
          "Three-way" = extractAIC(full_threeway_model)[2])))

kable(aic_and_bic_results)


plotm = ggplot2(testing, 
              aes(x = longitude, y = latitude, 
                      color = test_predictions - test_actual, 
                      hma = housing_median_age, tr = total_rooms, tb = total_bedrooms,
                      hh = households, mi = median_income)) +
  geom_point(aes(size = abs(test_predictions - test_actual)), alpha = 0.4) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Predicted Price Over / Under Actual Price") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_distiller(palette = "Paired", labels = comma) +
  labs(color = "Predicted Price Over / Under (in $USD)", 
       size = "Magnitude of Price Difference")
plotm


















