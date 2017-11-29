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
train['Cond_Name']<-ifelse(train$item_condition_id==2,"Good Condition",train$Cond_Name)
train['Cond_Name']<-ifelse(train$item_condition_id==3,"Used",train$Cond_Name)
train['Cond_Name']<-ifelse(train$item_condition_id==4,"Slightly Damaged",train$Cond_Name)
train['Cond_Name']<-ifelse(train$item_condition_id==5,"Highly Damaged",train$Cond_Name)
# mean(train[train$Cond_Name=="New",]$price)
# mean(train[train$Cond_Name=="Good Condition",]$price)
# mean(train[train$Cond_Name=="Used",]$price)
# mean(train[train$Cond_Name=="Slightly Damaged",]$price)
# mean(train[train$Cond_Name=="Highly Damaged",]$price)


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

#Brand name filling with description/name
train[grepl(c("sephora","Sephora"),train$name)&train$Has_Brand=="NoName",]
train[grepl(c("sephora","Sephora"),train$item_description)&train$Has_Brand=="NoName",]

#Bundle separation
sum(grepl(c("bundle","Bundle"),train$name))

#Size variable? 
#any(sapply(c("XS","xsmall"), function(x) grepl(x, train$item_description)))
train['Size']<-ifelse(grepl(c("xsmall","Xsmall"),train$item_description),"XS","")
train['Size']<-ifelse(grepl(c("small","Small"),train$item_description),"S",train$Size)
train['Size']<-ifelse(grepl(c("medium","Medium"),train$item_description),"M",train$Size)
train['Size']<-ifelse(grepl(c("large","Large"),train$item_description),"L",train$Size)
train['Size']<-ifelse(grepl(c("xlarge","Xlarge","Xl"),train$item_description),"XL",train$Size)
# mean(train[train$Size=="XS",]$price)
# mean(train[train$Size=="S",]$price)
# mean(train[train$Size=="M",]$price)
# mean(train[train$Size=="L",]$price)
# mean(train[train$Size=="XL",]$price)
# mean(train[train$Size=="",]$price)

#Authentic
train['Is_Auth']<-ifelse(grepl("authentic",train$item_description),"auth","")
# mean(train[train$Is_Auth=="auth",]$price)
# mean(train[train$Is_Auth!="auth",]$price)

#No description
train['Has_Desc']<-ifelse(grepl("No description",train$item_description),"nodesc","")
# mean(train[train$Has_Desc=="nodesc",]$price)
# mean(train[train$Has_Desc!="nodesc",]$price)


#Log transformation?
train['Log_Price']<-log(train$price)

#Goygoy
summary(train)
str(train)

#Plot
plot(train$item_condition_id,train$price)
plot(train$item_condition_id,train$Log_Price)

