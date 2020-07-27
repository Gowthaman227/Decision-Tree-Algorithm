library(party)
library(caret)
library(tree)
Company <- read.csv(file.choose())
View(Company)
str(Company)
## Converting Sales into Categorical Data
## Consider if Sales value > 10 then Company has high sales 
Company$Sales <- ifelse(Company$Sales>10,"Yes","No")
Company$Sales <- as.factor(Company$Sales)
View(Company)
## Creating Data Partition for training and Testing
Company_parti <- createDataPartition(Company$Sales,p=0.50,list=F)
Company_train <- Company[Company_parti,]
View(Company_train)
Company_test <- Company[-Company_parti,]
View(Company_test)

## Buliding a Model 1 using C5.0 Function 
Company_Model2 <- C5.0(Sales~.,data=Company_train)
plot(Company_Model2)
pred1 <- predict.C5.0(Company_Model2,Company_test)
p <- table(Company_test$Sales,pred1)
p
## Accuracy
sum(diag(p))/(sum(p))## Accuracy is 82%
    
## Buliding a Model 2 using party function ##
Company_Model <- ctree(Sales~.,data=Company_train)
plot(Company_Model)
Company_pred <- table(predict(Company_Model),Company_train$Sales)
Company_pred                      
## Accuracy
sum(diag(Company_pred))/(sum(Company_pred))## Accuracy is 87%

##Buliding Model 3 using tree function
Company_Model1 <- tree(Sales~.,data=Company_train)
plot(Company_Model1)
text(Company_Model1,pretty = 0)
## Predicting the Model ##
pred_tree <- as.data.frame(predict(Company_Model1,data=Company_test))
pred_tree["final"] <- NULL
pred_test_df <- predict(Company_Model1,newdata=Company_test)
pred_tree$final <- colnames(pred_test_df)[apply(pred_test_df,1,which.max)]
View(pred_tree)
pred_tree$final <- as.factor(pred_tree$final)
summary(pred_tree$final)
summary(Company_test$Sales)
mean(pred_tree$final==Company_test$Sales)## Accuracy 75%


## Try Using Boosting Techniques to improve the Model
library(adabag)
library(caret)
Company$Sales <- ifelse(Company$Sales>10,"Yes","No")
Company$Sales <- as.factor(Company$Sales)
View(Company)
## Creating Data Partition for training and Testing
Company_parti <- createDataPartition(Company$Sales,p=0.70,list=F)
Company_train <- Company[Company_parti,]
View(Company_train)
Company_test <- Company[-Company_parti,]
View(Company_test)

## Bulid a Model
Model <- boosting(Sales~.,data=Company_train,boos=TRUE,mfinal=25)
print(names(Model))
print(Model$trees[2])

pred = predict(Model,Company_test)
pred
print(pred$confusion)
print(pred$error)

result = data.frame(Company_test$Sales, pred$prob, pred$class)
print(result)
