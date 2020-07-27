library(party)
library(C50)
library(caret)
Fra_Check <- read.csv(file.choose())
View(Fra_Check)
str(Fra_Check)
Fra_Check$Taxable.Income <- ifelse(Fra_Check$Taxable.Income<= 30000,"Risky","Good")
Fra_Check$Taxable.Income <- as.factor(Fra_Check$Taxable.Income)
View(Fra_Check)
## Creating partition for training and testing dataset
Fra_Check_parti <- createDataPartition(Fra_Check$Taxable.Income,p=0.75,list=F)
Fra_Check_train <- Fra_Check[Fra_Check_parti,]
View(Fra_Check_train)
Fra_Check_test <- Fra_Check[-Fra_Check_parti,]
View(Fra_Check_test)
## Buliding a Model
Fra_Check_Model <- C5.0(Fra_Check_train$Taxable.Income~Undergrad+Marital.Status+City.Population
                        +Work.Experience+Urban,data=Fra_Check_train)
plot(Fra_Check_Model)
summary(Fra_Check_Model)
pred_FC <- predict.C5.0(Fra_Check_Model,Fra_Check_test)
pred <- table(Fra_Check_test$Taxable.Income,pred_FC)
View(pred)
## Accuracy
sum(diag(pred))/(sum(pred))## Accuracy is 79.33%

## Buliding Model 2 using party Function
Fra_Check_Model1 <- ctree(Fra_Check$Taxable.Income~.,data=Fra_Check_train)
summary(Fra_Check_Model1)
plot(Fra_Check_Model1)
pred_tree <- as.data.frame(predict(Fra_Check_Model1,newdata=Fra_Check_test))
pred_tree["final"] <- NULL
pred_test_df <- predict(Fra_Check_Model1,newdata=Fra_Check_test)
mean(pred_test_df==Fra_Check_test$Taxable.Income)

## Buliding Model 3 using rpart Function
library(rpart)
library(rpart.plot)
tree <- rpart(Taxable.Income~.,data=Fra_Check_train,cp=0.02)
summary(tree)
rpart.plot(tree, box.palette="RdBu", shadow.col="gray", nn=TRUE)

