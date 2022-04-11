getwd()
setwd("D:\\SEM8\\R Programming\\R Project")
#Import Data
df=read.csv("EmployeeAttrition.csv",stringsAsFactors = T)

#Check for missing values
anyNA(df)

#CheckingOut the dataset and keep the required columns

str(df)
df$Age<-df$ï..Age
df$ï..Age<-NULL
table(df$Over18)
table(df$StandardHours)
str(df)

df$EmployeeCount<-NULL
df$EmployeeNumber<-NULL
df$Over18<-NULL
df$StandardHours<-NULL

str(df)
nrow(df)
ncol(df)
typeof(df$OverTime)


df$Attrition = factor(df$Attrition,
                           levels = c('No', 'Yes'),
                           labels = c(0, 1))
str(df)
table(df$BusinessTravel)

df$BusinessTravel <- factor(as.character(as.numeric(df$BusinessTravel)))
df$Department <- factor(as.character(as.numeric(df$Department)))
df$EducationField <- factor(as.character(as.numeric(df$EducationField)))
df$Gender <- factor(as.character(as.numeric(df$Gender)))
df$JobRole <- factor(as.character(as.numeric(df$JobRole)))
df$MaritalStatus <- factor(as.character(as.numeric(df$MaritalStatus)))
df$OverTime <- factor(as.character(as.numeric(df$OverTime)))

str(df)

# Splitting the dataset into the Training set and Test set
install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(df$Attrition, SplitRatio = 0.75)
training_set = subset(df, split == TRUE)
test_set = subset(df, split == FALSE)
str(training_set)

classifier = glm(formula = Attrition ~ .,
                 family = binomial,
                 data = training_set)

# Predicting the Test set results
prob_pred = predict(classifier, type = 'response', newdata = test_set[-1])
y_pred = ifelse(prob_pred > 0.5, 1, 0)

# Making the Confusion Matrix
cm = table(test_set[, 1], y_pred > 0.5)
cm

library(ggplot2)
install.packages('lattice')
library(lattice)
install.packages('caret')
library(caret)
xtab<-table(test_set[,1],y_pred)

confusionMatrix(xtab)

