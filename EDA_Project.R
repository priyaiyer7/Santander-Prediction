
#Install packages and load libraries;read csv file
train<-read.csv("train.csv",header=TRUE)
library(DataExplorer)
library(dplyr)
library(plotly)
install.packages("data.table")
library(data.table)

#Missing Values
plot_missing(train)
sapply(train, function(x) sum(is.na(x)))

#Structure of Training Data
plot_str(train)


#Target Output and Log Transformation
library(ggplot2)
ggplot(train,aes(x=target))+geom_histogram(fill="red",bins=50)
ggplot(train,aes(x=target))+geom_histogram(fill="blue",bins=50)+scale_x_log10()



#Dataframe with column names and count of non-zero items in each column
x<-colSums(train != 0)
y<-colnames(train)

#Dataframe Column names
x_name<-"Count"
y_name<-"Col_name"
head(y)
head(x)

#Create Dataframe
 Train_nz<- data.frame(x, y)
colnames(Train_nz) <- c(x_name, y_name)
head(Train_nz)
dim(Train_nz)

#Include columns with non_zero values greater than 1000
m<-Train_nz[Train_nz$Count>1000,]
head(m)
str(m$Col_name)
m$Col_name<-as.character(m$Col_name)
c<-m$Col_name
head(c)
train<-data.frame(train)
train_non_zero<-train[c]


#Compute rowmean for columns that have count of non-zero values less than 1000 and add mean value column to dataframe
n<-Train_nz[Train_nz$Count<=1000,]
d<-n$Col_name
train_1<-train[d]

head(train_1)
class(train_1)


mean_value<-rowMeans(train_1[sapply(train_1, is.numeric)]) 
train_non_zero$mean_Zero<-mean_value
mean_value
str(train_non_zero)
write.csv(train_non_zero,file="train_non_zero.csv")


w<-rowSums(train!= 0)
t<-rownames(train)
w_name<-"Count"
t_name<-"Row_name"
Train_nz2<- data.frame(w, t)
colnames(Train_nz2) <- c(w_name, t_name)
#head(Train_nz2)
#Include rows with non_zero values greater than 1000
Subset1a<-Train_nz2[Train_nz2$Count>1000,]
Subset1a$Row_name<-as.character(Subset1a$Row_name)
#head(Subset1a$Row_name)
#str(Subset1a$Row_name)
train_non_zero<-train_non_zero[Subset1a$Row_name,]
head(train_non_zero,3)


#test data
a<-rownames(train_non_zero)
b<-colnames(train_non_zero[,-c(2,43)])

test<-read.csv("test_data.csv")
test.data<-test[a,b]
colnames(test)
write.csv(test.data,file="test.data.csv")


#EDA
str(train_non_zero)
summary(train_non_zero)
plot_histogram(train_non_zero)
plot_correlation(train_non_zero,type="continuous")


#PCA
train2<-subset(train_non_zero,select=-c(target,ID))
pc<-prcomp(train2)
summary(pc)
plot(pc)
plot(pc,type="l")
biplot(pc)
attributes(pc)
pc$sdev


#Plot of zeros in dataset
train_table<-data.table(train)
n_zeros <- train_table[, lapply(.SD, function(x) sum(x == 0) / length(x))] %>% unlist
plot_ly(x = ~n_zeros, type = "histogram", 
        marker = list(color = "dodgerblue")) %>% 
  layout(title = "Histogram of zeros in dataset",
         margin = list(l = 100))




?nrow








