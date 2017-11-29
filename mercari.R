library(stringr)
library(data.table)

path<-"C:/Users/fatih.dereli/Desktop/mercari/"

setwd(path)

train<-read.delim("train.tsv", header=TRUE, sep="\t")

test<-read.delim("test.tsv", header=TRUE, sep="\t")

sample_submission<-read.delim("sample_submission.csv", header=TRUE, sep=",")

#Separating category_name column
train[c('Cat1','Cat2','Cat3')]<-data.frame(str_split_fixed(train$category_name,"/",3))
train$item_condition_id<-as.factor(train$item_condition_id)
train$shipping<-as.factor(train$shipping)

#Condition name
train['Cond_Name']<-ifelse(train$item_condition_id==1,"New","")
train['Cond_Name']<-ifelse(train$item_condition_id==2,"Good Condition",train$V12)
train['Cond_Name']<-ifelse(train$item_condition_id==3,"Used",train$V12)
train['Cond_Name']<-ifelse(train$item_condition_id==4,"Slightly Damaged",train$V12)
train['Cond_Name']<-ifelse(train$item_condition_id==5,"Highly Damaged",train$V12)


#No brand name
train['Has_Brand']<-ifelse(train$brand_name=="","NoName","Name")

#Number of words in name
train['Nof_Name']<-str_count(train$name, '\\S+')

#Number of words in description
train['Nof_Desc']<-str_count(train$item_description, '\\S+')

#Length of name
train['Len_Name']<-str_length(train$name)

#Length of description
train['Len_Desc']<-str_length(train$item_description)

#Brand name filling with description

#Size variable?
any(sapply(c("XS","xsmall"), function(x) grepl(x, train$item_description)))
train['Size']<-ifelse(grepl(c("XS","xsmall"),train$item_description),"XS",train$V18)
train['Size']<-ifelse(grepl(c("S","small"),train$item_description),"S",train$V18)
train['Size']<-ifelse(grepl(c("M","medium"),train$item_description),"S",train$V18)
train['Size']<-ifelse(grepl(c("L","large"),train$item_description),"S",train$V18)
train['Size']<-ifelse(grepl(c("XL","xlarge"),train$item_description),"S",train$V18)

#Authentic
train['Is_Auth']<-ifelse(grepl("authentic",train$item_description),"auth","")
mean(train[train$V19=="auth",]$price)
mean(train[train$V19!="auth",]$price)

#No description
train['Has_Desc']<-ifelse(grepl("No description",train$item_description),"nodesc","")
mean(train[train$V20=="nodesc",]$price)
mean(train[train$V20!="nodesc",]$price)


#Log transformation?


#Goygoy
summary(train)
str(train)

a<-train[train$item_condition_id==2,]

plot(train$item_condition_id,train$price)


