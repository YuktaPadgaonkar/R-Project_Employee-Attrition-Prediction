df<-read.csv("D:\\SEM8\\R Programming\\R Project\\RPROJECT\\try\\try\\empf.csv")

splitting <- sample(2,nrow(df), replace = TRUE, prob=c(0.7,0.3))
training_set <- df[splitting==1,]
test_set <- df[splitting==2,]

#Model building using Logistic Regression
lg19 = glm(formula = Attrition ~ Age+ EnvironmentSatisfaction+ JobLevel+ MaritalStatus+ MonthlyIncome+ OverTime,StockOptionLevel+ TotalWorkingYears+ YearsAtCompany+ YearsInCurrentRole+ YearsWithCurrManager ,family = binomial,data =training_set)

#Saving the model
#saveRDS(lg19,"D:\\SEM8\\R Programming\\R Project\\RPROJECT\\try\\try\\model.rds")



