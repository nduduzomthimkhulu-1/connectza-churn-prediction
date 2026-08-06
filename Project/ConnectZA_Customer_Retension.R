rm(list = ls() )
setwd("~/BSc(Hons) Mathematics/STAT 420 - Quantitative Data Analysis/2025/Project")

####################################  TASK ONE #################################


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
customer.data$SupportTickects_transformed <- log(customer.data$SupportTickets + 1 ) # we add the1 to avoid log(0)
customer.data$DataUsage_GB_transformed <- log(customer.data$DataUsage_GB +1 )

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








  
 

 

