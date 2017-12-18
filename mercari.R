library(stringr)
library(data.table)
library(qdap)

path<-"C:/Users/fatih.dereli/Desktop/mercari/"

setwd(path)

train<-fread(paste0(path,"train.tsv"), stringsAsFactors = FALSE)

test <- fread(paste0(path,"test.tsv"), stringsAsFactors = FALSE)

sample_submission<-read.delim("sample_submission.csv", header=TRUE, sep=",")

train<-data.frame(train)

test<-data.table(test)

#Separating category_name column
train[c('Cat1','Cat2','Cat3')]<-data.frame(str_split_fixed(train$category_name,"/",3))
train$item_condition_id<-as.factor(train$item_condition_id)
train$shipping<-as.factor(train$shipping)

#Mean values
catmean<-data.table(train)[,mean(price),by=list(Cat1,Cat2,Cat3)]
brandmean<-data.table(train)[,mean(price),by=brand_name]

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
train['Size']<-ifelse(grepl(c(" small ","Small "),train$item_description),"S",train$Size)
train['Size']<-ifelse(grepl(c(" medium ","Medium "),train$item_description),"M",train$Size)
train['Size']<-ifelse(grepl(c(" large ","Large "),train$item_description),"L",train$Size)
train['Size']<-ifelse(grepl(c(" xlarge ","Xlarge ","Xl"),train$item_description),"XL",train$Size)
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

#Color ?transformation table?
train['Color']<-ifelse(grepl(" white ",train$item_description)|grepl(" white ",train$item_description),"white","")
train['Color']<-ifelse(grepl(" red ",train$item_description)|grepl(" red ",train$item_description),"red",train$Color)
train['Color']<-ifelse(grepl(" blue ",train$item_description)|grepl(" blue ",train$item_description),"blue",train$Color)
train['Color']<-ifelse(grepl(" green ",train$item_description)|grepl(" green ",train$item_description),"green",train$Color)
train['Color']<-ifelse(grepl(" brown ",train$item_description)|grepl(" brown ",train$item_description),"brown",train$Color)
train['Color']<-ifelse(grepl(" black ",train$item_description)|grepl(" black ",train$item_description),"black",train$Color)
train['Color']<-ifelse(grepl(" purple ",train$item_description)|grepl(" purple ",train$item_description),"purple",train$Color)
train['Color']<-ifelse(grepl(" pink ",train$item_description)|grepl(" pink ",train$item_description),"pink",train$Color)
train['Color']<-ifelse(grepl(" orange ",train$item_description)|grepl(" orange ",train$item_description),"orange",train$Color)
train['Color']<-ifelse(grepl(" gray ",train$item_description)|grepl(" gray ",train$item_description),"gray",train$Color)
train['Color']<-ifelse(grepl(" grey ",train$item_description)|grepl(" grey ",train$item_description),"gray",train$Color)
train['Color']<-ifelse(grepl(" beige ",train$item_description)|grepl(" beige ",train$item_description),"beige",train$Color)

data.table(train)[,.N,by=Color]

#Dark/Light
sum(grepl("light",train$item_description))
sum(grepl(" light ",train$item_description))

#bundle products needs to be separated or eliminated
train['IsBundle']<-ifelse(grepl("bundle",train$item_description)|grepl("Bundle",train$item_description)|grepl("bundle",train$name)|grepl("Bundle",train$name),1,0)

#Others ?eliminate/transform?
train[train$Cat1=="Others",]
train[train$Cat2=="Others",]
train[train$Cat3=="Others",]

#Descomposing categoricals


#Log transformation?
train['Log_Price']<-log(train$price)

#Outlier elimination/fill with mean
#0 price
a<-train[train$price==0,]
#"Blank category name"
a<-train[train$category_name=="",]

#bag of words
freq_terms(train$item_description,100)


#Goygoy
summary(train)
str(train)

#Plot
plot(train$item_condition_id,train$price)
plot(train$item_condition_id,train$Log_Price)

a<-train[train$IsBundle==1,]

