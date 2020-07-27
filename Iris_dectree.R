data("iris")
View(iris)
install.packages("party")
library(party)
library(caret)
# Data partition for training and testing dataset
training_parti <- createDataPartition(iris$Species,p=.75,list=F)
train_iris <- iris[training_parti,]
View(train_iris)
test_iris <- iris[-training_parti,]
View(test_iris)
# Model building
?ctree()
Iris_model <- ctree(Species~.,data = train_iris)
summary(Iris_model)
## Model Prediction
pred_train <- table(predict(Iris_model),train_iris$Species)
pred_train
## Accuracy of Model
sum(diag(pred)/sum(pred))## 97.36%
plot(Iris_model)
