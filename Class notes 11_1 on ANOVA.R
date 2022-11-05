# ANOVA = Analysis of Variance 
# We are considering a quantitative response variable as it relsats to 
#   one or more explanatory variables, usually categorical. 

#Sample Questions: 

#(i)Which academic department in the sciences gives out the lowest average grades?
#   Explanatory/Factor/Treatment variable: department.
#   Response /dependent variable: student GPA’s for individual courses
#(ii) Which kind of promotional campaign leads to greatest store income at Christmas time?
#     Explanatory/Factor/Treatment variable: promotion type.
#     Response /dependent variable: daily store income

#Hypothesis of one-way ANOVA: 

# H0: 𝜇1 = 𝜇2 = ⋯ . 𝜇𝑘 (
#   The population means of all groups are equal).
# Ha: Not all the population means are equal
#   (At least one is different from others).






#ANOVA TABLE   : 
# SOURCE        | SS = total sum of squares | df = degrees of freedom = # of observations | MS = mean of SS  | F = 
# Model/Group   | SSG                       |
#Residual/Error | SSE                       |
#Total          | SST                       |


# F = f statistic for ANOVA, if significant then reject H0. 
# Testing for mean not variance, but we use variation in test 





#Diet example 

# Medical researchers are interested to know if the calorie intake influences the
#average life expectancy of certain type of mice. For this purpose, assume that 80 mice were
#randomly assigned to four different groups (called treatment groups) with equal numbers and
#each group is restricted to one diet. Here the dependent or response variable is length of life of
#mice, which is a numerical variable. The independent or exploratory variable in this example is
#diet, which is categorical (which can also be considered as a factor with 4 levels). Let 𝜇1, 𝜇2, 𝜇3, 𝜇4
#represents the (population) average life expectancy of mice following the diets namely Diet 1,
#Diet 2, Diet 3 and Diet 4 respectively. Let 𝑛1, 𝑛2, 𝑛3, 𝑛4 represents sample sizes in each group. In
#this example we have 𝑛1 = 𝑛2 = 𝑛3 = 𝑛4 = 20. This is a balanced design, but ANOVA can be
#administered even for unbalanced design. The advantages of choosing a balanced design are
#higher power and robustness of the test to the departures of equal variance assumption. The
#data for the Diet example is displayed in the Table 1


# Table 1: Lifetime of Mice following different Diets
# 



diet1=scan()
28.7 34.1 32.6 35.5 32.5 35.8 36.0 32.0 37.4 37.3
35.1 36.1 38.5 37.2 39.2 31.1 33.5 26.9 35.5 42.1


diet2 = scan()
28.1 33.2 33.0 28.7 31.4 35.4 26.4 30.3 28.0 28.5
35.5 30.2 29.1 24.7 31.3 29.5 29.1 32.2 27.7 30.5

diet3 = scan()
28.3 34.4 30.9 31.7 31.7 29.9 33.6 28.3 30.4 32.8
28.0 31.3 28.7 31.6 29.0 29.5 30.1 34.5 30.4 37.3

diet4 = scan()
39.8 42.0 45.8 39.1 40.4 33.8 33.6 37.3 35.7 34.1
42.9 38.1 37.0 40.9 44.4 35.9 32.7 37.8 36.9 40.0

#lifetime is the groups 
lifetime = c(diet1, diet2, diet3, diet4)
lifetime

#we are grouping the values by diet here
group = c(rep(1,20), rep(2, 20), rep(3, 20), rep(4,20))
group
group = factor(group)
group

#now we know that the first 20 observations are part of group one 
# we can make an excel file

#if we have enough data > 20 points per group we can do a boxplot 
#continuous data so ~

boxplot(lifetime~group)
#we see one outlier in group 3 
#variance seems to be normal and equal per group 
# we will test it later using formal test with a residual plot variance  
# discuss what we see: group 4 have the highest life expentance.
#                      groups 2 and 3 have the lowest life expectancy 



#To do ANOVA 
#H0: mu1 = mu2 = mu3 = mu4
#H1: H0 is false 

#group is explanatory variable 
output1 = aov(lifetime~group)
output1
#``````````````````group Residuals
#Sum of Squares  856.5844  756.9275
#Deg. of Freedom        3        76

#Residual standard error: 3.155879
#Estimated effects may be unbalanced

#this is not enough we need p value: 
summary(output1)
#             Df  Sum Sq  Mean Sq   F value   Pr(>F)    
# group        3  856.6   285.53    28.67      .67e-12 ***
# Residuals   76  756.9    9.96                     
---

#group = diet, 4 diets -> 3 df 
# pvalue = P(F_{3,76} > 28.67)  = i.e. probability of getting this extreme value 
#f distribution is a right skew distribution  
  
#source of variation | df  | SS     | MSS     | F-Statistic | p-value 
# group              | 3   | 856.6  | 285.3   | 28.67       | 1.67 * 10^(-12)
# error              | 76  | 756.9  | 9.96    |             |
# total              | 79  |        |         |             |
  
# Now we have to make a decision 
# p value < alpha = 0.05
# reject H0

#Now we make a conclusion
# There is enough evidence from the sample to conclude that not all 
#   means are equal. 

#Check assumptions 
plot(output1)

# Linearity
# Covariance
# Normality -> large samples make it easier
#Independence -> not testing 

#Assumptions seems to be valid from residuals plot 
# If assumption fails we can do nonparametric or transformation 
# meaning will change (sqrt transformation)
# for ANOVA - only if you need to do a transformation 

#Now we know that ANOVA analysis shows that means are different 
# We would like to know which means are different, pairwise 
# This is called post hoc ANOVA aka multiple comparisons 
#      many ways to do this: 
#         pairwise t test with pentality 
#         Tukey HSD method (most popular) 
#         Bonferonni (second popular) 

#ONLY IF WE REJECT H0 WITH ANOVA DO WE TRY TO FIND THE DIFFERENCE BETWEEN MEANS

TukeyHSD(output1)
#Tukey multiple comparisons of means
#95% family-wise confidence level
#group       diff        lwr       upr         p adj
#2-1        -4.715     -7.3364816 -2.093518    0.0000604
#3-1        -3.735     -6.3564816 -1.113518    0.0019582
#4-1         3.555      0.9335184  6.176482    0.0035053
#3-2         0.980     -1.6414816  3.601482    0.7601005
#4-2         8.270      5.6485184 10.891482    0.0000000
#4-3         7.290      4.6685184  9.911482    0.0000000

#Small P value -> significantly different: 
  #2-1        -4.715     -7.3364816 -2.093518    0.0000604
  #3-1        -3.735     -6.3564816 -1.113518    0.0019582
  #4-1         3.555      0.9335184  6.176482    0.0035053
  #4-2         8.270      5.6485184 10.891482    0.0000000
  #4-3         7.290      4.6685184  9.911482    0.0000000

# All pairs are significantly different except: 
  #3-2         0.980     -1.6414816  3.601482    0.7601005

plot(TukeyHSD(output1), col = 3)
  #only 3-2 shows an interval that includes 0, showing they are similar
  # all others do not have 0 in their interval, signifying their dissimilarity

#Equal variance test

bartlett.test(lifetime~group)
#   bartlett test of homogeneity of variances
#   data:  lifetime by group
#   Bartlett's K-squared = 4.2124, df = 3, p-value = 0.2394

# p value > 0.05 so we can assume variance. 
# p value here is not based on f stat based on chi squared stat
