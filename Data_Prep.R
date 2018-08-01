install.packages("data.table")
library(data.table)
library(DataExplorer)
library(ggplot2)
library(e1071)


#Data Preparation Training Data
trans.data<-fread("train_non_zero.csv", header = TRUE)
training.data <- trans.data


#Standardize using scale(default z-score method)

training.data_scaled<-scale(training.data[,-1])
training.data_scaled<-data.frame(training.data_scaled)
training.data_scaled$ID<-training.data$ID 
training.data_scaled<-training.data_scaled[c(43,1:42)]
training.data_scaled$target<-training.data$target
?scale

#Binning Target Variable
boxplot(training.data_scaled$target)
set.seed(1)
bins<-5
minimumVal<-min(training.data_scaled$target)
maximumVal<-max(training.data_scaled$target)
width=(maximumVal-minimumVal)/bins;
training.data_scaled$bin_target<-cut(training.data_scaled$target, breaks=seq(minimumVal, maximumVal, width))

#plot frequencies in the bins
barplot(table(cut(training.data_scaled$target, breaks=seq(minimumVal, maximumVal, width))))


#Alternate method
training.data_scaled$bin_target2 <- as.factor( as.numeric( cut(training.data_scaled$target,3)))

#Plot correlation
plot_correlation(training.data_scaled,type="continuous")
pairs(training.data_scaled[2:10])

#Target transformation for Normality
ggplot(training.data_scaled,aes(x=target))+geom_histogram(fill="blue")
ggplot(training.data_scaled,aes(x=target))+geom_histogram(fill="blue",bins=50)+scale_x_log10()
summary(training.data_scaled$target)
log_target<-log(training.data_scaled$target)
skewness(log_target)

#Outlier check
library(dplyr)
library(tidyr)
library(purrr)
outliers <- function(dataframe){
  dataframe %>%
    select_if(is.numeric) %>% 
    map(~ boxplot.stats(.x)$out) 
  
  
}
outliers(training.data)
outliers(training.data_scaled)



#Data Preparation Test Data

test.data<-fread("test.data.csv", header = TRUE)
test.data<-test.data[,-1]


#Standardize using scale(default z-score method)

test.data_scaled<-scale(test.data[,-1])
test.data_scaled<-data.frame(test.data_scaled)
test.data_scaled$ID<-test.data$ID 
test.data_scaled<-test.data_scaled[c(41,1:40)]

?scale



#Plot correlation
plot_correlation(test.data_scaled,type="continuous")
pairs(test.data_scaled[2:10])


#Outlier check
library(dplyr)
library(tidyr)
library(purrr)
outliers <- function(dataframe){
  dataframe %>%
    select_if(is.numeric) %>% 
    map(~ boxplot.stats(.x)$out) 
  
  
}
outliers(test.data)
outliers(test.data_scaled)

#Test Data
test.data<-read.csv("test_non_zero.csv")


#Compare test and train
library(compare)
comparison <- compare(training.data,test.data,allowAll=TRUE)
comparison$tM
library(dplyr)
semi_join(training.data,test.data)
