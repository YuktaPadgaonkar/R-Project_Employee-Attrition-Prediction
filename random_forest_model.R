install.packages("tidyverse")
library(tidyverse)
install.packages("randomForest")
library(randomForest)
install.packages("caret")
library(caret)

# importing dataset
df<-read_csv("D:\\SEM8\\R Programming\\R Project\\RPROJECT\\try\\try\\empf.csv")

# to view dataset -> view(df)
# to get structure of dataset -> str(df)

# splitting dataset in the ratio 7:3
# 7 parts as training data and 3 parts as testing data
splitting <- sample(2,nrow(df), replace = TRUE, prob=c(0.7,0.3))
train <- df[splitting==1,]
test <- df[splitting==2,]

#Model building using Random Forest
rf_fit <- randomForest(Attrition ~ ., data=train)
#Model building using Decision Tree
c_fit<-ctree(Attrition ~ ., data=train)
#Model building using Logistic Regression
l_fit<-glm(Attrition ~ ., data=train)
#Model building using Naive Bayes
n_fit<-naiveBayes(Attrition~.,data = train)

#Predicting value  for test set for all the implemented Algorithms
result<-predict(rf_fit,newdata=test,type='response')
result1<-predict(c_fit,newdata=test,type='response')
resultf<-predict(l_fit,newdata=test,type='response')
resultn<-predict(n_fit,newdata=test)

t1<-ifelse(result > 0.5, 1, 0)
t2<-ifelse(result1 > 0.5, 1, 0)
tf<-ifelse(resultf > 0.5, 1, 0)

#Printing Confusion Matrix for all the models

c<-table(test$Attrition,t1)
print("The confusion Matrix of Random Forest is : ")
confusionMatrix(c)# Accuracy of Random Forest : 83.05%

d<-table(test$Attrition,t2)
print("The confusion Matrix of Decision Tree is : ")
confusionMatrix(d)# Accuracy of Decision Tree : 81.36%


f<-table(test$Attrition,tf)
print("The confusion Matrix of Logistic Regression is : ")
confusionMatrix(f)

n<-table(test$Attrition,resultn)
print("The confusion Matrix of Naive Bayes is : ")
confusionMatrix(n)# Accuracy of Naive Bayes  : 80.63%


#Saving Random Forest model 
#saveRDS(rf_fit,"C:/Users/lenovo/Desktop/try/modeft.rds")

