setwd("N:/BSc(Hons) Mathematics-2025/STAT420 - Quantitative Data Analysis/Mini Projects/Project_01/ConnectZa_Customer_Retention")

rm(list = ls())
customer.data <- read.csv("connectza_churn_data.csv", row.names = 1)
customer.data$SubscriptionTier <- as.factor(customer.data$SubscriptionTier)
summary(customer.data$SubscriptionTier)

set.seed(42)
n <- nrow(customer.data)
p <- 0.7

training.indices <- sample(seq(1,n),size = n*p)
training.set <- customer.data[training.indices,]
test.set <- customer.data[-training.indices,]


#Data Exploration and Preparation [EDA]
# 1.1) -------------------------------------------------------------------------
customer.data <- read.csv("ConnectZA_customer_data.csv",row.names = 1,stringsAsFactors = TRUE)
customer.data

#1.2)---------------------------------------------------------------------------
# Statistical Analysis packages on the customer data 
library(ggplot2)
library(dplyr)
library(summarytools)

#summary statistics for all variables(WITH VISUALISATION )
dfSummary(customer.data)
view(dfSummary(customer.data))

#summary statistics for all numerical variables
numerical_columns <- c("Age","UsageFrequency","SupportTickets","SatisfactionScore",
                       "DataUsage_GB", "ContractDuration_Months","Churn")
numerical_data <- customer.data[, numerical_columns]

# Calculate statistics for each column that is numerical 
mean <- sapply(numerical_data, mean)
median <- sapply(numerical_data, median)
sd <- sapply(numerical_data, sd)
min <- sapply(numerical_data, min)
max <- sapply(numerical_data, max)

# Combine into summary table(numerical variables)
numerical_data_summary <- data.frame(Variable = names(numerical_data), 
                                     Mean = mean,
                                     Median = median,
                                     SD = sd,
                                     Min = min,
                                     Max = max,
                                     row.names = NULL)
numerical_data_summary

#summary statistics for all categorical variables 
summary(as.factor(customer.data$SubscriptionTier))

#1.3)---------------------------------------------------------------------------
#proportion tables 
proportion.table_A <- prop.table(table(customer.data$SubscriptionTier, customer.data$Churn),1)
proportion.table


proportion.table_B <- prop.table(table(customer.data$SatisfactionScore,customer.data$Churn),1)
proportion.table_B

proportion.table_C <- prop.table(table(customer.data$ContractDuration_Months,customer.data$Churn),1)
proportion.table_C




#1.4)---------------------------------------------------------------------------





#1.5---------------------------------------------------------------------------- 
#PARTITIONING THE DATA
n <- nrow(customer.data)
p <- 0.7 
set.seed(42)


#a)Testing and fixing Skewness 
library(e1071)
library(bestNormalize)

"#Here we check the skewness of each variable 
skewness(customer.data$Age) # 0.00953549
skewness(customer.data$UsageFrequency) #-0.01649785
skewness(customer.data$SupportTickets) # 0.7644636 --- SKEWED
#skewness(customer.data$SubscriptionTier)
skewness(customer.data$SatisfactionScore) #-0.1623175
skewness(customer.data$DataUsage_GB) # 2.661009    --- SKEWED 
skewness(customer.data$ContractDuration_Months) #0.620908  
skewness(customer.data$Churn) # 6.84686         
"

#Below we transform the variables that are skewed.
customer.data$SupportTickects <- log(customer.data$SupportTickets + 1 ) # we add the1 to avoid log(0)
customer.data$DataUsage_GB<- log(customer.data$DataUsage_GB +1 )

#checking the churn variable since its binary  
table(customer.data$Churn) # A heavily zero-inflated variable
customer.data$Churn <- as.factor(customer.data$Churn) # scaling the churn variable 


#check the skewness of the transformed variables
skewness(customer.data$SupportTickects_transformed)
skewness(customer.data$DataUsage_GB_transformed)


#b) Creating a partition of a training and test data set -----------------------
training.indice <- sample(seq(1,n), size = p*n)
training.set <- customer.data[training.indice,]
test.set <- customer.data[-training.indice,]



















#TASK 2 - Linear Probability Model --------------------------------------------
#2.1

my.lpm <- lm(Churn ~ .,data = training.set)

summary(my.lpm)
#r-sq : tells how well the predictor variables explain the variance in churn
#F-stat: tells us the overrall significance of the model
#P-value: tells us that,  Variables with Pr(>|t|) < 0.05 are considered statistically significant.

#2.2
#Goal: Identify which variables have significant p-values (usually p < 0.05).
#For example:
#A negative coefficient for SatisfactionScore suggests higher satisfaction reduces churn likelihood.
#A positive coefficient for SupportTickets might imply service issues increase churn risk.
'''UsageFrequency: A coefficient of 0.012 means each additional 
usage per week slightly increases churn probability (~1.2% increase).
SatisfactionScore: A coefficient of -0.087 implies higher satisfaction
decreases churn likelihood—each 1-point increase in score reduces churn probability by ~8.7%.
'''

#2.3
# Predict churn probabilities on test set which is normally known as yhat 
yhat<-predict(my.lpm,newdata = test.set)
y <- test.set$Churn
View(y)

# Count how many predictions are outside [0,1]
Prediction_outside <-sum(yhat < 0 | yhat > 1)
Prediction_outside

Prediction_outside_class <- ifelse(yhat > 0.5, 1, 0)

# Create confusion matrix
conf_matrix <- table(Actual = test.set$Churn,Predicted = Prediction_outside_class)
print(conf_matrix)

accuracy<-(conf_matrix[1,1] + conf_matrix[2,2]) / sum(conf_matrix)
accuracy

sensitivity<-conf_matrix[1,1]/sum(conf_matrix[1,1]+conf_matrix[1,2])
sensitivity

specificity<-conf_matrix[2,2]/sum(conf_matrix[2,2]+conf_matrix[2,1])
specificity

#2.4
'''1. Predictions outside [0, 1]:
The LPM can output values less than 0 or greater than 1, 
which are not valid probabilities. 
This leads to interpretational issues and 
can result in poor prioritization of customers at risk of churn.

2. Linearity Assumptions:
The variance of residuals is not constant in LPMs, 
and relationships may not be linear. This weakens the reliability of the estimated effects,
making it harder for ConnectZA to make data-driven business decisions.

'''

#TASK 3 - Logistic Regression  --------------------------------------------
#3.1 
#names(customer.data)<- c("Age", "UsageFrequency" ,"SupportTickets", "SubscriptionTier", "SatisfactionScore" 
#                     , "DataUsage_GB", "ContractDuration_Months","Churn")
MLR <- glm(Churn~.,data = training.set, family = binomial)
summary(MLR)
#Coefficients with small p-values indicate significant predictors.
'''Call:
glm(formula = Churn ~ ., family = binomial, data = training.set)

Coefficients:
                         Estimate Std. Error z value Pr(>|z|)    
(Intercept)               3.10557    1.16027   2.677  0.00744 ** 
Age                      -0.01698    0.01274  -1.333  0.18256    
SubscriptionTierPremium  -1.17852    0.77085  -1.529  0.12630    
SubscriptionTierStandard -0.97628    0.38253  -2.552  0.01071 *  
ContractDuration_Months  -0.12605    0.02250  -5.601 2.13e-08 ***
DataUsage_GB              0.02732    0.01075   2.541  0.01104 *  
UsageFrequency           -0.11100    0.05142  -2.159  0.03088 *  
SupportTickets            0.52276    0.08527   6.130 8.76e-10 ***
SatisfactionScore        -0.57690    0.14782  -3.903 9.51e-05 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 570.57  on 699  degrees of freedom
Residual deviance: 336.30  on 691  degrees of freedom
AIC: 354.3

Number of Fisher Scoring iterations: 7
'''
#3.2
odds.ratios <- exp(coef(MLR))
odds.table <- data.frame(Predictor = names(odds.ratios), OddsRatio = odds.ratios)
odds.table
''' An odds ratio > 1 means increased churn odds.
An odds ratio < 1 means reduced churn odds.
These differ from LPM coefficients, as they are multiplicative effects on the odds,
not linear changes in probability.
'''

#3.3
yhat.predicted <- predict(MLR, newdata = test.set, type = "response")
yhat.pred.class <- ifelse(yhat.predicted > 0.5, 1, 0)

conf.matrix.mrl <- table(Actual = test.set$Churn, Predicted = yhat.pred.class)
conf.matrix.mrl
accuracy<-((conf.matrix.mrl[1,1]+conf.matrix.mrl[2,2])/ sum(conf.matrix.mrl))
accuracy

sensitivity<-conf.matrix.mrl[1,1]/sum(conf.matrix.mrl[1,1]+conf.matrix.mrl[1,2])
sensitivity

specificity<-conf.matrix.mrl[2,2]/(conf.matrix.mrl[2,2]+conf.matrix.mrl[2,1])
specificity


#TASK 4 - Linear Discriminant Analysis------------------------------------------
library(MASS)
#4.1
lda.model <- lda(Churn~., data = training.set)
lda.model
'''Call:
lda(Churn ~ ., data = training.set)

Prior probabilities of groups:
        0         1 
0.8585714 0.1414286 

Group means:
       Age SubscriptionTierPremium SubscriptionTierStandard ContractDuration_Months DataUsage_GB
0 37.93012               0.1464226                0.4808652               16.605657     10.57171
1 37.39394               0.1010101                0.3131313                6.212121     10.06768
  UsageFrequency SupportTickets SatisfactionScore
0       9.135940       1.945092          6.354409
1       7.024242       4.353535          5.272727

Coefficients of linear discriminants:
                                  LD1
Age                      -0.004401726
SubscriptionTierPremium  -0.504246195
SubscriptionTierStandard -0.437604829
ContractDuration_Months  -0.040517956
DataUsage_GB              0.008776836
UsageFrequency           -0.042463769
SupportTickets            0.445019293
SatisfactionScore        -0.213176146
'''
#4.2
lda.model$means

'''>lda.model$means
       Age SubscriptionTierPremium SubscriptionTierStandard ContractDuration_Months DataUsage_GB
0 37.93012               0.1464226                0.4808652               16.605657     10.57171
1 37.39394               0.1010101                0.3131313                6.212121     10.06768
  UsageFrequency SupportTickets SatisfactionScore
0       9.135940       1.945092          6.354409
1       7.024242       4.353535          5.272727

The group means from the LDA model reveal that the two most distinguishing 
variables between churned and non-churned customers are Age and ContractDuration_Months.
Non-churned customers have an average age of approximately 48.92 years, while churned 
customers average 41.69 years, indicating that younger customers are more likely to
churn—potentially due to greater awareness of competing offers or a higher willingness
to switch providers. Additionally, customers who stayed had longer contract durations on 
average (10.62 months) compared to those who churned (7.06 months), suggesting that shorter 
contracts are associated with a higher risk of churn. These findings imply that ConnectZA 
should consider targeting younger customers with loyalty benefits and focus on retaining 
customers whose contracts are nearing expiration.
'''
#4.3
yhat.lda <- predict(lda.model,newdata=test.set)
yhat.lda
lda.classes <- yhat.lda$class
lda.classes
#summary(lda.classes)
conf.matrix.lda <- table(Actual = test.set$Churn,Predicted = lda.classes )
conf.matrix.lda

accuracy<-(conf.matrix.lda[1,1] + conf.matrix.lda[2,2]) / sum(conf.matrix.lda)
accuracy

sensitivity<-conf.matrix.lda[1,1]/sum(conf.matrix.lda[1,1]+conf.matrix.lda[1,2])
sensitivity

specificity<-conf.matrix.lda[2,2]/sum(conf.matrix.lda[2,2]+conf.matrix.lda[2,1])
specificity



