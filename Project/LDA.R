#task 4 Linear Discriminant Analysis 

rm(list = ls() )
setwd("~/BSc(Hons) Mathematics/STAT 420 - Quantitative Data Analysis/2025/Project")


customer.data <- read.csv("ConnectZA_customer_data.csv",row.names = 1,stringsAsFactors = TRUE)
customer.data
summary(customer.data)

#Factoring our data into the 3 subscriptions that we want 
customer.data$SubscriptionTier <- factor(customer.data$SubscriptionTier,labels = c("Standard","Basic","Premium"),levels = c("Standard","Basic","Premium"))

#the data is large so we take a sample of 10 randomly choose entries and use this set as our new data set.
D <- customer.data[sample(seq(1,nrow(customer.data)),size = 10),]
D

#get the class mean 
library(tidyverse)
class.mean <- D |>group_by(SubscriptionTier) |> summarise(
  mean.Age = mean(Age),
  mean.UsageFrequency = mean(UsageFrequency),
  mean.SupportTickets = mean(SupportTickets),
  mean.SatisfactionScore= mean(SatisfactionScore),
  mean.DataUsage_GB = mean(DataUsage_GB),
  mean.ContractDuration_Months = mean(ContractDuration_Months), 
  mean.Churn = mean(Churn)
)
class.mean
View(class.mean)

#mean vector(this is the matrix for means) for the BASIC ,STANDARD and PREMIUM 
q <- 7 # number of predictor variables
mue.s <- matrix(as.numeric(class.mean[1,-1]),nrow = q,ncol = 1)
mue.s
mue.b <- matrix(as.numeric(class.mean[2,-1]),nrow = q,ncol = 1)
mue.b
mue.p <- matrix(as.numeric(class.mean[3,-1]),nrow = q,ncol = 1)
mue.p



 






