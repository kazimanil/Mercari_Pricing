library(stringr)
library(data.table)

path<-"C:/Users/fatih.dereli/Desktop/mercari/"

setwd(path)

train<-read.delim("train.tsv", header=TRUE, sep="\t")

test<-read.delim("test.tsv", header=TRUE, sep="\t")

sample_submission<-read.delim("sample_submission.csv", header=TRUE, sep=",")

#Separating category_name column
train[,9:11]<-data.frame(str_split_fixed(train$category_name,"/",3))
train$item_condition_id<-as.factor(train$item_condition_id)
train$shipping<-as.factor(train$shipping)

#Condition name
train[,12]<-ifelse(train$item_condition_id==1,"New","")
train[,12]<-ifelse(train$item_condition_id==2,"Good Condition",train$V12)
train[,12]<-ifelse(train$item_condition_id==3,"Used",train$V12)
train[,12]<-ifelse(train$item_condition_id==4,"Slightly Damaged",train$V12)
train[,12]<-ifelse(train$item_condition_id==5,"Highly Damaged",train$V12)


#No brand name
train[,13]<-ifelse(train$brand_name=="","NoName","Name")

#Brand name filling with decription

#Size variable?

#No description

#Authentic




#Goygoy
summary(train)
str(train)

a<-train[train$item_condition_id==2,]

plot(train$item_condition_id,train$price)


