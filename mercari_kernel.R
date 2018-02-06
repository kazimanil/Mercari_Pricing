library(stringr)
library(data.table)
library(xgboost)
library(rpart)

train<-fread('../input/train.tsv')

test <- fread('../input/test.tsv')

data <- rbindlist(list(train, test), fill = TRUE, use.names = TRUE, idcol = NULL)

data<-data.frame(data)

#Separating category_name column
data[c('Cat1','Cat2','Cat3')]<-data.frame(str_split_fixed(data$category_name,"/",3))
data$item_condition_id<-as.factor(data$item_condition_id)
data$shipping<-as.factor(data$shipping)

#Condition name
data['Cond_Name']<-ifelse(data$item_condition_id==1,"New","")
data['Cond_Name']<-ifelse(data$item_condition_id==2,"Good Condition",data$Cond_Name)
data['Cond_Name']<-ifelse(data$item_condition_id==3,"Used",data$Cond_Name)
data['Cond_Name']<-ifelse(data$item_condition_id==4,"Slightly Damaged",data$Cond_Name)
data['Cond_Name']<-ifelse(data$item_condition_id==5,"Highly Damaged",data$Cond_Name)


#No brand name
data['Has_Brand']<-ifelse(data$brand_name=="","NoName","Name")

#Number of words in name
data['Nof_Name']<-str_count(data$name, '\\S+')

#Number of words in description
data['Nof_Desc']<-str_count(data$item_description, '\\S+')

#Length of name
data['Len_Name']<-str_length(data$name)

#Length of description
data['Len_Desc']<-str_length(data$item_description)


#Size variable? 
data['Size']<-ifelse(grepl(c("xsmall","Xsmall"),data$item_description),"XS","")
data['Size']<-ifelse(grepl(c(" small ","Small "),data$item_description),"S",data$Size)
data['Size']<-ifelse(grepl(c(" medium ","Medium "),data$item_description),"M",data$Size)
data['Size']<-ifelse(grepl(c(" large ","Large "),data$item_description),"L",data$Size)
data['Size']<-ifelse(grepl(c(" xlarge ","Xlarge ","Xl"),data$item_description),"XL",data$Size)


#Authentic
data['Is_Auth']<-ifelse(grepl("authentic",data$item_description),"auth","")


#No description
data['Has_Desc']<-ifelse(grepl("No description",data$item_description),"nodesc","")


#Color ?transformation table?
data['Color']<-ifelse(grepl(" white ",data$item_description)|grepl(" white ",data$item_description),"white","")
data['Color']<-ifelse(grepl(" red ",data$item_description)|grepl(" red ",data$item_description),"red",data$Color)
data['Color']<-ifelse(grepl(" blue ",data$item_description)|grepl(" blue ",data$item_description),"blue",data$Color)
data['Color']<-ifelse(grepl(" green ",data$item_description)|grepl(" green ",data$item_description),"green",data$Color)
data['Color']<-ifelse(grepl(" brown ",data$item_description)|grepl(" brown ",data$item_description),"brown",data$Color)
data['Color']<-ifelse(grepl(" black ",data$item_description)|grepl(" black ",data$item_description),"black",data$Color)
data['Color']<-ifelse(grepl(" purple ",data$item_description)|grepl(" purple ",data$item_description),"purple",data$Color)
data['Color']<-ifelse(grepl(" pink ",data$item_description)|grepl(" pink ",data$item_description),"pink",data$Color)
data['Color']<-ifelse(grepl(" orange ",data$item_description)|grepl(" orange ",data$item_description),"orange",data$Color)
data['Color']<-ifelse(grepl(" gray ",data$item_description)|grepl(" gray ",data$item_description),"gray",data$Color)
data['Color']<-ifelse(grepl(" grey ",data$item_description)|grepl(" grey ",data$item_description),"gray",data$Color)
data['Color']<-ifelse(grepl(" beige ",data$item_description)|grepl(" beige ",data$item_description),"beige",data$Color)


#bundle products needs to be separated or eliminated
data['IsBundle']<-ifelse(grepl("bundle",data$item_description)|grepl("Bundle",data$item_description)|grepl("bundle",data$name)|grepl("Bundle",data$name),1,0)

data[is.na(data$price),]$price<-mean(data[!is.na(data$price),]$price)

tr_data<-data[1:nrow(train),]
te_data<-data[(nrow(train)+1):nrow(data),]


trbrand<-data.table(brand=unique(tr_data$brand_name))
setkey(trbrand,brand)
tebrand<-data.table(brand=unique(te_data$brand_name))
setkey(tebrand,brand)


missingbrands<-tebrand[!tebrand$brand%in%trbrand$brand,]

te_data[te_data$brand_name%in%missingbrands$brand,]$brand_name<-""


control<-rpart.control(minsplit = 100,minbucket=100,cp = 0,xval = 10)

model<-rpart(price ~ item_condition_id + shipping + Cat1 + Cat2 +  Cat3 + Size + Color + brand_name + Has_Desc  + Is_Auth
                          # + Has_Brand + Nof_Name 
                          # + Nof_Desc + Len_Desc + Len_Name  + Is_Auth + Has_Desc  + IsBundle + brand_name + Has_Brand + Nof_Name
                            ,data=tr_data[,-1],control = control,method = 'anova')


cart_pruned<- prune(model,
                              cp=model$cptable[which.min(model$cptable[,"xerror"]),"CP"])

# cart_train<-predict(object = model,newdata=tr_data[,-1])

tr<-cbind(tr_data,cart_train)

rmlse <- function(model) { 
  y <- tr_data$price
  y.pred <- predict(object = cart_pruned,newdata=tr_data[,-1])
  return(sqrt(1/length(y)*sum((log(y.pred +1)-log(y +1))^2)))
}

rmlse(model)

cart_test<-predict(object = cart_pruned,newdata=te_data[,c(3,5,7,9:11,18,21)],se.fit=FALSE)

pred<-data.frame(test_id = te_data$test_id, price = cart_test)

write.csv(pred, file = 'mercari_Price_cart.csv', row.names = FALSE)


#####XGB
model<-xgboost(data=as.matrix(tr_data),label=tr_data$price,
nrounds = 200, objective = "reg:linear", eval_metric = "rmse", eta = 0.1, 
min_child_weight = 1.5, max_depth = 12, gamma = 1,subsample = 0.5, colsample_bytree = 0.5)

pred<-predict(model,as.matrix(te_data))

result <- fread(input = "../input/sample_submission.csv", header=TRUE, stringsAsFactors = FALSE)
result$price <- pred 
write.csv(result, file = "result.csv", row.names = FALSE)

#####Logit



model_logistic=glm(Survived~Pclass+Sex+Age+N_Family+individual_price+CabinCode,family=binomial(link="logit" ),data = tr_data)
summary(model_logistic)
mean(tr_data$Survived==round(predict(model_logistic,tr_data,type="response")))

te_data$Survived<-round(predict(model_logistic, te_data, type="response"))

write.csv(te_data[,1:2], file = 'Titanic.csv', row.names = F)


#NeuralNet Approach
library(neuralnet)

# Var1 <- rpois(100,0.5)
# Var2 <- rbinom(100,2,0.6)
# Var3 <- rbinom(100,1,0.5)
# SUM <- as.integer(abs(Var1+Var2+Var3+(rnorm(100))))
# sum.data <- data.frame(Var1+Var2+Var3, SUM)
m <- model.matrix( 
  ~ price + item_condition_id +shipping + Cat1 + Cat2 + Cat3 + Has_Brand + Nof_Name + Nof_Desc + Len_Name + Len_Desc + Size + Is_Auth + Has_Desc + Color + IsBundle , 
  data = tr_data 
)

net.sum <- neuralnet( Survived1 ~ Sexmale + Pclass2 + Pclass3 + Age + N_Family + Fare, m, hidden=6,
                      act.fct="tanh")
#print(net.sum)
#◙plot(net.sum)

# main <- glm(SUM~Var1+Var2+Var3, sum.data, family=poisson())
# full <- glm(SUM~Var1*Var2*Var3, sum.data, family=poisson())
# prediction(net.sum, te_data)

#temp_test <- subset(te_data, select = c("alcohol", "malic", "ash", "ash_alcalinity", "magnesium", "phenols", "flavanoids", "nonflavanoids", "proanthocyanins", "color_intensity", "hue", "od280", "proline"))

m2 <- model.matrix( 
  ~  Pclass + Sex + Age + N_Family + Fare, 
  data = te_data 
)

nn_pred<-compute(net.sum,m2[,2:ncol(m2)])

res<-cbind(te_data[,1],round(nn_pred$net.result))

names(res)<-c("PassengerId","Survived")

write.csv(res, file = 'Titanic.csv', row.names = F)



#SVM Model
library(e1071)


te_data[is.na(te_data$Fare),]$Fare<-mean(te_data$Fare,na.rm = TRUE)

SVMmodel<-svm(as.factor(Survived) ~ Age+SibSp+Parch+Sex+Fare, data = tr_data, cost = 100, gamma = 1)

prediction<-predict(SVMmodel, te_data[,c(5,6,7,8,10)])
plot(prediction)
# Pclass=table(prediction,te_data[,2])
# plot(Pclass)
# me=mean(Pclass)
# print(me)

output<-data.frame(te_data$PassengerId, data.frame(prediction))

colnames(output)=cbind("PassengerId","Survived")

write.csv(output, file = 'Titanic.csv', row.names = F)


#XGBoost
library(xgboost)

xgb <- xgb.cv(data = data.matrix(tr_data[,-1]), 
              label = tr_data[,2], 
              nfold = 3, nrounds = cv.nround
)

y_pred <- predict(xgb, data.matrix(X_test[,-1]))

#Random Forest


#Ctree
library(rpart)
library(rattle)
fit <- rpart(price ~  item_condition_id +shipping + Cat1 + Cat2 + Cat3 + Has_Brand + Nof_Name + Nof_Desc + Len_Name + Len_Desc + Size + Is_Auth + Has_Desc + Color + IsBundle , 
             data = tr_data ,
             method="class")

#fancyRpartPlot(fit)

Prediction <- predict(fit, te_data, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "Titanic.csv", row.names = FALSE)
