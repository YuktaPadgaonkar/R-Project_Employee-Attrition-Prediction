#R Project

#To get the working directory
getwd()
#To set the working directory
setwd("D:\\SEM8\\R Programming\\R Project")

#To get working directory to see if the working directory has been changed
getwd()
?read.csv()
#Get the csv file 
df=read.csv(file.choose(),stringsAsFactors = T)

#Get the top 5 rows/tuples
head(df)

#Get the summary to the data
summary(df)

#Get the structure of the data
str(df)
# --------------Data Cleaning and Data Transformation-----------------

#Removing unnecessary columns
df$EmployeeCount <-NULL
df$EmployeeNumber<-NULL
df$StandardHours<-NULL
df$Over18<-NULL

#Get the summary of the data
summary(df)

#Get the structure of the data
str(df)

summary(df$BusinessTravel)

head(df)

#Getting to know the data and changing it for visualization

#Renaming Age column
df$Age<-df$ï..Age
df$ï..Age<-NULL

summary(df$Age)

summary(df$BusinessTravel)

summary(df$Department)

summary(df$DistanceFromHome)

summary(df$Education)
table(df$Education)
#Since it indicates level we convert from string into factor
df$Education<-factor(df$Education)
str(df) #To check if it is converted or not

summary(df$EducationField)

summary(df$EnvironmentSatisfaction)
table(df$EnvironmentSatisfaction)
df$EnvironmentSatisfaction<-factor(df$EnvironmentSatisfaction)

summary(df)

table(df$JobLevel)
df$JobLevel<-factor(df$JobLevel)

table(df$JobSatisfaction)
df$JobSatisfaction<-factor(df$JobSatisfaction)

table(df$PerformanceRating)
df$PerformanceRating<-factor(df$PerformanceRating)


table(df$RelationshipSatisfaction)
df$RelationshipSatisfaction<-factor(df$RelationshipSatisfaction)

table(df$StockOptionLevel)
df$StockOptionLevel<-factor(df$StockOptionLevel)

table(df$TrainingTimesLastYear)
df$TrainingTimesLastYear<-factor(df$TrainingTimesLastYear)

table(df$WorkLifeBalance)
df$WorkLifeBalance<-df$WorkLifeBalance

summary(df)
str(df)

#Not Getting Factored
#1WorkLifeBalance
#2JobInvolvement


df$WorkLifeBalance<-as.factor(df$WorkLifeBalance)
summary(df$WorkLifeBalance)

df$JobInvolvement<-as.factor(df$JobInvolvement)
summary(df$JobInvolvement)

#Now all the data that was in int which had to be converted to levels/
#Categories has been converted

#------------------------Visualizing-----------------

install.packages("ggplot2")
library(ggplot2)

#Age vs Salary and Attrition >>>1
p<-ggplot(data=df)
p+geom_point(aes(x=MonthlyIncome,y=Age),color="#00AFBB")+
  ggtitle("Age vs Monthly Income")+
  xlab("Monthly Income")+
  ylab("Age")+
  theme(axis.title.x=element_text(color = "#214074", size = 25),
        axis.title.y=element_text(color = "#214074", size = 25),
        axis.text.x=element_text(size=15),
        axis.text.y=element_text(size=15),
        plot.title = element_text(color = "#213C3E",
                                  size=40, family="Courier"),
        legend.title = element_text(size=15),
        legend.text = element_text(size=12))

#Age vs Salary and Department>>>1
p<-ggplot(data=df)
p+geom_point(aes(x=MonthlyIncome,y=Age,color=Department))+
  ggtitle("Age vs Monthly Income by Department")+
  xlab("Monthly Income")+
  ylab("Age")+
  theme(axis.title.x=element_text(color = "#39A8B1", size = 25),
        axis.title.y=element_text(color = "#39A8B1", size = 25),
        axis.text.x=element_text(size=15),
        axis.text.y=element_text(size=15),
        plot.title = element_text(color = "#213C3E",
                                  size=40, family="Courier"),
        legend.title = element_text(size=15),
        legend.text = element_text(size=12))

#Age vs Salary and JobLevel>>>1
p<-ggplot(data=df)
p+geom_point(aes(x=MonthlyIncome,y=Age,color=JobLevel))+
  ggtitle("Age vs Salary by Job Level")+
  xlab("Monthly Income")+
  ylab("Age")+
  theme(axis.title.x=element_text(color = "#39A8B1", size = 25),
        axis.title.y=element_text(color = "#39A8B1", size = 25),
        axis.text.x=element_text(size=15),
        axis.text.y=element_text(size=15),
        plot.title = element_text(color = "#213C3E",
                                  size=40, family="Courier"),
        legend.title = element_text(size=15),
        legend.text = element_text(size=12),) 

#Age vs Salary and EducationField>>>1
p<-ggplot(data=df)
p+geom_point(aes(x=MonthlyIncome,y=Age,color=EducationField))+
  ggtitle("Age vs Monthly Income by Education field")+
  xlab("Monthly Income")+
  ylab("Age")+
  theme(axis.title.x=element_text(color = "#39A8B1", size = 25),
        axis.title.y=element_text(color = "#39A8B1", size = 25),
        axis.text.x=element_text(size=15),
        axis.text.y=element_text(size=15),
        plot.title = element_text(color = "#213C3E",
                                  size=40, family="Courier"),
        legend.title = element_text(size=15),
        legend.text = element_text(size=12),
        
        )

#Age vs Salary and EducationField>>>1
p<-ggplot(data=df)
p+geom_point(aes(x=MonthlyIncome,y=Age,color=EducationField,shape=Department))+
  ggtitle("Age vs Monthly Income by Education field and Department")+
  xlab("Monthly Income")+
  ylab("Age")+
  theme(axis.title.x=element_text(color = "#39A8B1", size = 25),
        axis.title.y=element_text(color = "#39A8B1", size = 25),
        axis.text.x=element_text(size=15),
        axis.text.y=element_text(size=15),
        plot.title = element_text(color = "#213C3E",
                                  size=40, family="Courier"),
        legend.title = element_text(size=15),
        legend.text = element_text(size=12),
        
  )


summary(df)


#Getting the attrition Rate
ggplot(df, aes(x = Attrition,fill=Attrition)) +
  geom_bar(position = position_dodge())+
  geom_text(stat='count', aes(label=..count..),
  vjust = -1, position = position_dodge(.9))+
  ggtitle("Attrition Count Plot  ")+
  xlab("Attrition")+
  ylab("Count")+
  theme(axis.title.x=element_text(color = "#456498", size = 25),
        axis.title.y=element_text(color = "#456498", size = 25),
        axis.text.x=element_text(size=15),
        axis.text.y=element_text(size=15),
        plot.title = element_text(color = "#213C3E",
                                  size=25, family="Courier"),
        legend.title = element_text(size=15),
        legend.text = element_text(size=12),
        
        legend.position = c(1,1),
        legend.justification = c(1,1),)


#Getting the relationship between attrition and Salary and Job Level 
u<-ggplot(data=df, aes(x=Attrition, y=MonthlyIncome,
                           color=JobLevel))
u+geom_boxplot()+ggtitle("Monthly Income vs Attrition by Job Level ")+
  xlab("Attrition")+
  ylab("Monthly Income")+
  theme(axis.title.x=element_text(color = "#456498", size = 25),
        axis.title.y=element_text(color = "#456498", size = 25),
        axis.text.x=element_text(size=15),
        axis.text.y=element_text(size=15),
        plot.title = element_text(color = "#213C3E",
                                  size=30),
        legend.title = element_text(size=20),
        legend.text = element_text(size=20),
        )

#Getting the relationship between attrition and Salary and Department
u<-ggplot(data=df, aes(x=Attrition, y=MonthlyIncome,
                       color=Department))+
  ggtitle("Monthly Income vs Attrition by Department ")+
  xlab("Attrition")+
  ylab("Monthly Income")+
  theme(axis.title.x=element_text(color = "#456498", size = 25),
        axis.title.y=element_text(color = "#456498", size = 25),
        axis.text.x=element_text(size=15),
        axis.text.y=element_text(size=15),
        plot.title = element_text(color = "#213C3E",
                                  size=30, family="Courier"),
        legend.title = element_text(size=15),
        legend.text = element_text(size=15),
  )
u+geom_boxplot()

#Getting the relationship between attrition and Salary and BusinessTravel 
u<-ggplot(data=df, aes(x=BusinessTravel, y=MonthlyIncome,
                       fill=BusinessTravel,color=BusinessTravel))+
  ggtitle("Monthly Income vs Attrition by Business Travel ")+
  xlab("Business Travel")+
  ylab("Monthly Income")+
  theme(axis.title.x=element_text(color = "#456498", size = 25),
        axis.title.y=element_text(color = "#456498", size = 25),
        axis.text.x=element_text(size=15),
        axis.text.y=element_text(size=15),
        plot.title = element_text(color = "#213C3E",
                                  size=30, family="Courier"),
        legend.title = element_text(size=15),
        legend.text = element_text(size=15),
  )
u+geom_boxplot(alpha=0.6)+geom_jitter()

#Getting the relationship between Salary and WorkLifeBalance 
u<-ggplot(data=df, aes(x= WorkLifeBalance, y=MonthlyIncome,
                      color=Attrition ))+
  ggtitle("Monthly Income vs Work Life Balance")+
  xlab("Work Life Balance")+
  ylab("Monthly Income")+
  theme(axis.title.x=element_text(color = "#456498", size = 25),
        axis.title.y=element_text(color = "#456498", size = 25),
        axis.text.x=element_text(size=15),
        axis.text.y=element_text(size=15),
        plot.title = element_text(color = "#213C3E",
                                  size=30)
  )
u+geom_boxplot()

#Getting the relationship between attrition and Salary and WorkLifeBalance 
u<-ggplot(data=df, aes(x= WorkLifeBalance, y=MonthlyIncome,
                       color=Attrition))
u+geom_boxplot()+
  ggtitle("Monthly Income vs Work Life Balance")+
  xlab("Work Life Balance")+
  ylab("Monthly Income")+
  theme(axis.title.x=element_text(color = "#456498", size = 25),
        axis.title.y=element_text(color = "#456498", size = 25),
        axis.text.x=element_text(size=15),
        axis.text.y=element_text(size=15),
        plot.title = element_text(color = "#213C3E",
                                  size=30, family="Courier"),
        legend.title = element_text(size=15),
        legend.text = element_text(size=15)  )

summary(df)
str(df)


#Getting the relationship between attrition and Salary and Job Involvement >>>7
u<-ggplot(data=df, aes(x=JobInvolvement, y=MonthlyIncome,
                       color= Attrition ))
u+geom_boxplot()+
  ggtitle("Monthly Income vs Job Involvement")+
  xlab("Job Involvement")+
  ylab("Monthly Income")+
  theme(axis.title.x=element_text(color = "#456498", size = 25),
        axis.title.y=element_text(color = "#456498", size = 25),
        axis.text.x=element_text(size=15),
        axis.text.y=element_text(size=15),
        plot.title = element_text(color = "#213C3E",
                                  size=30, family="Courier"),
        legend.title = element_text(size=15),
        legend.text = element_text(size=15)  )

#Relationship between Attrition vs Work Life Balance
ggplot(df, aes(x =WorkLifeBalance ,fill= Attrition)) +
  geom_bar(position = position_dodge())+
  ggtitle("Work Life balance Countplot by Attrition")+
  xlab("Work Life balance")+
  ylab("Count")+
  theme(axis.title.x=element_text(color = "#456498", size = 25),
        axis.title.y=element_text(color = "#456498", size = 25),
        axis.text.x=element_text(size=15),
        axis.text.y=element_text(size=15),
        plot.title = element_text(color = "#213C3E",
                                  size=30, family="Courier"),
        legend.title = element_text(size=15),
        legend.text = element_text(size=15)  )
 

#Getting the relationship between attrition and Salary and WorkLifeBalance 
u<-ggplot(data=df, aes(x=Attrition, y=MonthlyIncome,
                       color=WorkLifeBalance))
u+geom_boxplot() +
ggtitle("Attrition vs Monthly Income by Worklife Balance")+
  xlab("Attrition")+
  ylab("Monthly Income")+
  theme(axis.title.x=element_text(color = "#456498", size = 25),
        axis.title.y=element_text(color = "#456498", size = 25),
        axis.text.x=element_text(size=15),
        axis.text.y=element_text(size=15),
        plot.title = element_text(color = "#213C3E",
                                  size=30, family="Courier"),
        legend.title = element_text(size=15),
        legend.text = element_text(size=15)  )

#Relationship between Attrition vs  OverTime 
ggplot(df, aes(x = OverTime ,fill= Attrition)) +
  geom_bar(position = position_dodge())+geom_text(stat='count', aes(label=..count..),
  vjust = -1, position = position_dodge(.9))+
  scale_fill_manual(values=c( "#58C4C8","#05868B", "#56B4E9"))+
  ggtitle("Overtime Countplot by Attrition")+
  xlab("Overtime")+
  ylab("Count")+
  theme(axis.title.x=element_text(color = "#456498", size = 25),
        axis.title.y=element_text(color = "#456498", size = 25),
        axis.text.x=element_text(size=15),
        axis.text.y=element_text(size=15),
        plot.title = element_text(color = "#213C3E",
                                  size=30, family="Courier"),
        legend.title = element_text(size=15),
        legend.text = element_text(size=15)  )

#Relationship between Attrition vs  BusinessTravel 
ggplot(df, aes(x = OverTime ,fill= Attrition)) +
  geom_bar(position = position_dodge())+geom_text(stat='count',
                                                  aes(label=..count..),
  vjust = -1, position = position_dodge(.9))+
  scale_fill_manual(values=c( "#42C87C","#C84279", "#56B4E9"))+
  ggtitle("Business Travel and Attrition")+
  xlab("Business Travel")+
  ylab("Count")+
  theme(axis.title.x=element_text(color = "#456498", size = 25),
        axis.title.y=element_text(color = "#456498", size = 25),
        axis.text.x=element_text(size=15),
        axis.text.y=element_text(size=15),
        plot.title = element_text(color = "#213C3E",
                                  size=30, family="Courier"),
        legend.title = element_text(size=15),
        legend.text = element_text(size=15)  )


#Relationship between Attrition vs  Education 
ggplot(df, aes(x =Attrition  ,fill= Education )) +
  geom_bar(position = position_dodge())+geom_text(stat='count', aes(label=..count..),
  vjust = -1, position = position_dodge(.9))+
  scale_fill_manual(
    values=c( "#28D9E0","#DDE028", "#28E080","#E06B28","#E028A2","#E028A2"))+
  ggtitle("Education Level and Attrition")+
  xlab("Attrition")+
  ylab("Count")+
  theme(axis.title.x=element_text(color = "#456498", size = 25),
        axis.title.y=element_text(color = "#456498", size = 25),
        axis.text.x=element_text(size=15),
        axis.text.y=element_text(size=15),
        plot.title = element_text(color = "#213C3E",
                                  size=30, family="Courier"),
        legend.title = element_text(size=15),
        legend.text = element_text(size=15)  )

#Relationship between Attrition vs  Education Field
ggplot(df, aes(x =EducationField   ,fill= Attrition )) +
  geom_bar(position = position_dodge())+geom_text(stat='count', aes(label=..count..),
  vjust = -1, position = position_dodge(.9))+
  scale_fill_manual(
    values=c( "#28D9E0","#DDE028", "#28E080","#E06B28","#E028A2","#E028A2"))+
  ggtitle("Education Field and Attrition")+
  xlab("Education Field")+
  ylab("Count")+
  theme(axis.title.x=element_text(color = "#456498", size = 25),
        axis.title.y=element_text(color = "#456498", size = 25),
        axis.text.x=element_text(size=15),
        axis.text.y=element_text(size=15),
        plot.title = element_text(color = "#213C3E",
                                  size=30, family="Courier"),
        legend.title = element_text(size=15),
        legend.text = element_text(size=15)  )

#Getting the relationship between attrition and Salary and Education 
u<-ggplot(data=df, aes(x=Attrition, y=MonthlyIncome,
                       color=Education))
u+geom_boxplot()+ggtitle("Monthly Income vs Attrition by Education ")+
  xlab("Attrition")+
  ylab("Monthly Income")+
  theme(axis.title.x=element_text(color = "#456498", size = 25),
        axis.title.y=element_text(color = "#456498", size = 25),
        axis.text.x=element_text(size=15),
        axis.text.y=element_text(size=15),
        plot.title = element_text(color = "#213C3E",
                                  size=30),
        legend.title = element_text(size=20),
        legend.text = element_text(size=20),
  )

#Getting the relationship between attrition and Salary and Department 
u<-ggplot(data=df, aes(x=Attrition, y=MonthlyIncome,
                       color=Department))
u+geom_boxplot()+ggtitle("Monthly Income vs Attrition by Department ")+
  xlab("Attrition")+
  ylab("Monthly Income")+
  theme(axis.title.x=element_text(color = "#456498", size = 25),
        axis.title.y=element_text(color = "#456498", size = 25),
        axis.text.x=element_text(size=15),
        axis.text.y=element_text(size=15),
        plot.title = element_text(color = "#213C3E",
                                  size=30),
        legend.title = element_text(size=20),
        legend.text = element_text(size=20),
  )


#Getting the relationship between attrition and Salary and BusinessTravel
u<-ggplot(data=df, aes(x=Attrition, y=MonthlyIncome,
                       fill=BusinessTravel))
u+geom_boxplot()+ggtitle("Monthly Income vs Attrition by Business Travel ")+
  xlab("Attrition")+
  ylab("Monthly Income")+
  theme(axis.title.x=element_text(color = "#456498", size = 25),
        axis.title.y=element_text(color = "#456498", size = 25),
        axis.text.x=element_text(size=15),
        axis.text.y=element_text(size=15),
        plot.title = element_text(color = "#213C3E",
                                  size=30),
        legend.title = element_text(size=20),
        legend.text = element_text(size=20),
  )

#Relationship between Attrition vs  BusinessTravel 
ggplot(df, aes(x =BusinessTravel ,fill= Attrition)) +
  geom_bar(position = position_dodge())+geom_text(stat='count', aes(label=..count..),
  vjust = -1, position = position_dodge(.9))+
  scale_fill_manual(values=c( "#71A6D2","#FC8EAC","#E028A2"))+
  ggtitle(" BusinessTravel and Attrition")+
  xlab("Business Travel ")+
  ylab("Count")+
  theme(axis.title.x=element_text(color = "#456498", size = 25),
        axis.title.y=element_text(color = "#456498", size = 25),
        axis.text.x=element_text(size=15),
        axis.text.y=element_text(size=15),
        plot.title = element_text(color = "#213C3E", size=30),
        legend.title = element_text(size=20),
        legend.text = element_text(size=20),
  )

#Relationship between Attrition vs  BusinessTravel 
ggplot(df, aes(x =BusinessTravel ,fill= df$Gender)) +
  geom_bar(position = position_dodge())+geom_text(stat='count', aes(label=..count..),
  vjust = -1, position = position_dodge(.9))+
  scale_fill_manual(values=c( "#71A6D2","#FC8EAC","#E028A2"))+
  ggtitle(" BusinessTravel and Attrition")+
  xlab("Business Travel ")+
  ylab("Count")+
  theme(axis.title.x=element_text(color = "#456498", size = 25),
        axis.title.y=element_text(color = "#456498", size = 25),
        axis.text.x=element_text(size=15),
        axis.text.y=element_text(size=15),
        plot.title = element_text(color = "#213C3E", size=30),
        legend.title = element_text(size=20),
        legend.text = element_text(size=20),
  )

#Relationship between Attrition vs  EnvironmentSatisfaction 
ggplot(df, aes(x =EnvironmentSatisfaction ,fill= Attrition)) +
  geom_bar()+geom_text(stat='count', aes(label=..count..),
           vjust = -1, position = position_dodge(.9))+
  scale_fill_manual(
    values=c( "#28D9E0","#DDE028", "#28E080","#E06B28","#E028A2","#E028A2"))+
  ggtitle(" Environment Satisfaction and Attrition")+
  xlab("Environment Satisfaction")+
  ylab("Count")+
  theme(axis.title.x=element_text(color = "#456498", size = 25),
        axis.title.y=element_text(color = "#456498", size = 25),
        axis.text.x=element_text(size=15),
        axis.text.y=element_text(size=15),
        plot.title = element_text(color = "#213C3E", size=30),
        legend.title = element_text(size=20),
        legend.text = element_text(size=20),
  )


#Relationship between Attrition vs  EducationField 
ggplot(df, aes(x =EducationField ,fill= Attrition)) +
  geom_bar(position = position_dodge())+geom_text(stat='count', aes(label=..count..),
  vjust = -1, position = position_dodge(.9))+
  scale_fill_manual(values=c( "#456498","#FFC300"))+
  ggtitle(" Education Field and Attrition")+
  xlab(" Education Field")+
  ylab("Count")+
  theme(axis.title.x=element_text(color = "#456498", size = 25),
        axis.title.y=element_text(color = "#456498", size = 25),
        axis.text.x=element_text(size=15),
        axis.text.y=element_text(size=15),
        plot.title = element_text(color = "#213C3E", size=30),
        legend.title = element_text(size=20),
        legend.text = element_text(size=20),)

#Relationship between EnvironmentSatisfaction vs  EducationField 
ggplot(df, aes(x =EducationField ,fill= EnvironmentSatisfaction)) +
  geom_bar(position = position_dodge())+geom_text(stat='count', aes(label=..count..),
  vjust = -1, position = position_dodge(.9))+
  scale_fill_manual(values=c( "#456498","#FFC300","#FF5733","#900C3F"))+
  ggtitle(" Education Field and Environment Satisfaction")+
  xlab(" Education Field")+
  ylab("Count")+
  theme(axis.title.x=element_text(color = "#456498", size = 25),
        axis.title.y=element_text(color = "#456498", size = 25),
        axis.text.x=element_text(size=15),
        axis.text.y=element_text(size=15),
        plot.title = element_text(color = "#213C3E", size=30),
        legend.title = element_text(size=20),
        legend.text = element_text(size=20),
  )

#Relationship between EnvironmentSatisfaction vs  YearsWithCurrManager 
#Getting the relationship between EnvironmentSatisfaction and YearsWithCurrManager   
u<-ggplot(data=df, aes(x=EnvironmentSatisfaction, y=YearsWithCurrManager,
                       color=EnvironmentSatisfaction))
u+geom_boxplot()



#Getting the relationship between EnvironmentSatisfaction and JobSatisfaction and  
ggplot(df, aes(x =JobSatisfaction ,fill= EnvironmentSatisfaction)) +
  geom_bar(position = position_dodge())+geom_text(stat='count', aes(label=..count..),
  vjust = -1, position = position_dodge(.9))

#Getting the relationship between Attrition and JobSatisfaction and  
ggplot(df, aes(x =JobSatisfaction ,fill= Attrition)) +
  geom_bar(position = position_dodge())+geom_text(stat='count', aes(label=..count..),
  vjust = -1, position = position_dodge(.9))

#Getting the relationship between Attrition and RelationshipSatisfaction   
ggplot(df, aes(x =RelationshipSatisfaction ,fill= Attrition)) +
  geom_bar()+geom_text(stat='count', aes(label=..count..),
  vjust = -1, position = position_dodge(.9))+
  scale_fill_manual(values=c( "#FF5733","#900C3F"))+
  ggtitle(" Relationship Satisfaction and Attrition")+
  xlab(" Relationship Satisfaction")+
  ylab("Count")+
  theme(axis.title.x=element_text(color = "#456498", size = 25),
        axis.title.y=element_text(color = "#456498", size = 25),
        axis.text.x=element_text(size=15),
        axis.text.y=element_text(size=15),
        plot.title = element_text(color = "#213C3E", size=30),
        legend.title = element_text(size=20),
        legend.text = element_text(size=20),
  )


#Getting the relationship between JobSatisfaction and RelationshipSatisfaction   
ggplot(df, aes(x =RelationshipSatisfaction ,fill= JobSatisfaction)) +
  geom_bar()+
  scale_fill_manual(values=c("#ee4035" , "#fdf498" , "#7bc043" , "#0392cf"))+
  ggtitle(" Relationship Satisfaction and JobSatisfaction")+
  xlab(" Relationship Satisfaction")+
  ylab("Count")+
  theme(axis.title.x=element_text(color = "#456498", size = 25),
        axis.title.y=element_text(color = "#456498", size = 25),
        axis.text.x=element_text(size=15),
        axis.text.y=element_text(size=15),
        plot.title = element_text(color = "#213C3E", size=30),
        legend.title = element_text(size=20),
        legend.text = element_text(size=20),
  )


#Getting the relationship between Attrition and MaritalStatus   
ggplot(df, aes(x =Attrition ,fill= MaritalStatus)) +
  geom_bar(position = position_dodge())+geom_text(stat='count', aes(label=..count..),
                        vjust = -1, position = position_dodge(.9))+
  scale_fill_manual(values=c("#ee4035" , "#fdf498" , "#7bc043" , "#0392cf"))+
  ggtitle(" Marital Status and Attrition")+
  xlab(" Attrition")+
  ylab("Count")+
  theme(axis.title.x=element_text(color = "#456498", size = 25),
        axis.title.y=element_text(color = "#456498", size = 25),
        axis.text.x=element_text(size=15),
        axis.text.y=element_text(size=15),
        plot.title = element_text(color = "#213C3E", size=30),
        legend.title = element_text(size=20),
        legend.text = element_text(size=20),
  )

#Getting the relationship between Attrition and StockOptionLevel   
ggplot(df, aes(x =StockOptionLevel ,fill= Attrition)) +
  geom_bar(position = position_dodge())+geom_text(stat='count', aes(label=..count..),
  vjust = -1, position = position_dodge(.9))+
  scale_fill_manual(values=c("#ee4035" , "#fdf498" , "#7bc043" , "#0392cf"))+
  ggtitle(" Attrition and Stock Option Level")+
  xlab(" Stock Option Level")+
  ylab("Count")+
  theme(axis.title.x=element_text(color = "#456498", size = 25),
        axis.title.y=element_text(color = "#456498", size = 25),
        axis.text.x=element_text(size=15),
        axis.text.y=element_text(size=15),
        plot.title = element_text(color = "#213C3E", size=30),
        legend.title = element_text(size=20),
        legend.text = element_text(size=20),)

#Getting the relationship between Attrition and JobLevel   
ggplot(df, aes(x =JobLevel ,fill= Attrition)) +
  geom_bar(position = position_dodge())+geom_text(stat='count', aes(label=..count..),
  vjust = -1, position = position_dodge(.9))+
  scale_fill_manual(values=c("#E49B0F" , "#6082B6" , "#7bc043" , "#0392cf"))+
  ggtitle(" Job Level and Attrition")+
  xlab(" JobLevel")+
  ylab("Count")+
  theme(axis.title.x=element_text(color = "#456498", size = 25),
        axis.title.y=element_text(color = "#456498", size = 25),
        axis.text.x=element_text(size=15),
        axis.text.y=element_text(size=15),
        plot.title = element_text(color = "#213C3E", size=30),
        legend.title = element_text(size=20),
        legend.text = element_text(size=20),)

#Getting the relationship between PerformanceRating and Attrition    
ggplot(df, aes(x =PerformanceRating ,fill= Attrition)) +
  geom_bar(position = position_dodge())+geom_text(stat='count', aes(label=..count..),
  vjust = -1, position = position_dodge(.9))+
  scale_fill_manual(values=c("#E49B0F" , "#6082B6" , "#7bc043" , "#0392cf"))+
  ggtitle(" Performance Rating and Attrition")+
  xlab(" Performance Rating")+
  ylab("Count")+
  theme(axis.title.x=element_text(color = "#456498", size = 25),
        axis.title.y=element_text(color = "#456498", size = 25),
        axis.text.x=element_text(size=15),
        axis.text.y=element_text(size=15),
        plot.title = element_text(color = "#213C3E", size=30),
        legend.title = element_text(size=20),
        legend.text = element_text(size=20),)

#Getting the relationship between JobSatisfaction and Environment Satisfaction    
ggplot(df, aes(x =EnvironmentSatisfaction ,fill= JobSatisfaction)) +
  geom_bar(position = position_dodge())+geom_text(stat='count', aes(label=..count..),
  vjust = -1, position = position_dodge(.9))+
  scale_fill_manual(values=c("#ee4035" , "#fdf498" , "#7bc043" , "#0392cf"))+
  ggtitle(" Environment Satisfaction and JobSatisfaction")+
  xlab(" Environment Satisfaction")+
  ylab("Count")+
  theme(axis.title.x=element_text(color = "#456498", size = 25),
        axis.title.y=element_text(color = "#456498", size = 25),
        axis.text.x=element_text(size=15),
        axis.text.y=element_text(size=15),
        plot.title = element_text(color = "#213C3E", size=30),
        legend.title = element_text(size=20),
        legend.text = element_text(size=20),
  )


#Getting the relationship between Attrition and MaritalStatus 
ggplot(df, aes(x =MaritalStatus ,fill= Attrition)) +
  geom_bar(position = position_dodge())+
  geom_text(stat='count', aes(label=..count..),
  vjust = -1, position = position_dodge(.9))+
  ggtitle("Attrition and MaritalStatus by Gender ")+
  xlab("Marital Satus")+
  ylab("Count")+
  theme(axis.title.x=element_text(color = "#456498", size = 25),
        axis.title.y=element_text(color = "#456498", size = 25),
        axis.text.x=element_text(size=15),
        axis.text.y=element_text(size=15),
        plot.title = element_text(color = "#213C3E",
                                  size=25, family="Courier"),
        legend.title = element_text(size=15),
        legend.text = element_text(size=10),
        
        legend.position = c(1,1),
        legend.justification = c(1,1),)
  

#Getting the relationship between Attrition and MaritalStatus and  Gender
ggplot(df, aes(x =MaritalStatus ,fill= Attrition,color=Gender)) +
  geom_bar(position = position_dodge())+
  geom_text(stat='count', aes(label=..count..),
  vjust = -1, position = position_dodge(.9))+
  
  ggtitle("Attrition and MaritalStatus by Gender ")+
  xlab("Gender")+
  ylab("Count")+
  theme(axis.title.x=element_text(color = "#456498", size = 25),
        axis.title.y=element_text(color = "#456498", size = 25),
        axis.text.x=element_text(size=15),
        axis.text.y=element_text(size=15),
        plot.title = element_text(color = "#213C3E",
                                  size=25, family="Courier"),
        legend.title = element_text(size=15),
        legend.text = element_text(size=10),
        
        legend.position = c(1,1),
        legend.justification = c(1,1),)


#Getting the Total Number of Male and Female Employees >>>
ggplot(df, aes(x = Gender,fill=Gender)) +
  geom_bar(position = position_dodge())+
  geom_text(stat='count', aes(label=..count..),
            vjust = -1, position = position_dodge(.9))+
  ggtitle("Gender Count Plot  ")+
  xlab("Gender")+
  ylab("Count")+
  theme(axis.title.x=element_text(color = "#456498", size = 25),
        axis.title.y=element_text(color = "#456498", size = 25),
        axis.text.x=element_text(size=15),
        axis.text.y=element_text(size=15),
        plot.title = element_text(color = "#213C3E",
                                  size=25, family="Courier"),
        legend.title = element_text(size=15),
        legend.text = element_text(size=10),
        
        legend.position = c(1,1),
        legend.justification = c(1,1),)

#Getting the Total Number of Male and Female Employees >>>
ggplot(df, aes(x = Gender,fill=Attrition)) +
  geom_bar(position = position_dodge())+
  geom_text(stat='count', aes(label=..count..),
            vjust = -1, position = position_dodge(.9))+
  ggtitle("Gender And Attrition  ")+
  xlab("Gender")+
  ylab("Count")+
  theme(axis.title.x=element_text(color = "#456498", size = 25),
        axis.title.y=element_text(color = "#456498", size = 25),
        axis.text.x=element_text(size=15),
        axis.text.y=element_text(size=15),
        plot.title = element_text(color = "#213C3E",
                                  size=25, family="Courier"),
        legend.title = element_text(size=15),
        legend.text = element_text(size=10),
        
        legend.position = c(1,1),
        legend.justification = c(1,1),)


#Relationship between Gender vs  Job Involvement >>>
ggplot(df, aes(x = Gender ,fill=  JobInvolvement)) +
  geom_bar(position = position_dodge())+
  geom_text(stat='count', aes(label=..count..),
  vjust = -1, position = position_dodge(.9))+
  ggtitle("Gender Vs Job Involvement  ")+
  xlab("Gender")+
  ylab("Job Involvement")+
  theme(axis.title.x=element_text(color = "#456498", size = 25),
        axis.title.y=element_text(color = "#456498", size = 25),
        axis.text.x=element_text(size=15),
        axis.text.y=element_text(size=15),
        plot.title = element_text(color = "#213C3E",
                                  size=25, family="Courier"),
        legend.title = element_text(size=15),
        legend.text = element_text(size=10),
        
        legend.position = c(1,1),
        legend.justification = c(1,1),)

install.packages('plotrix')
library(plotrix)
pie_3d <- function(dfCol,main) {
  tdf=data.frame(table(dfCol))
  tdf$percentage<-round(tdf$Freq/sum(tdf$Freq)*100)
  tdf
  labels_new<-paste(tdf$dfCol,tdf$percentage,sep = " ")
  
  print(tdf)
  print(labels_new)
  final_labels<-paste(labels_new,'%'," (",tdf$Freq,")")
  final_labels
  
  #plots the 3-D pie chart
  
  pie3D(tdf$percentage,labels = final_labels,explode = .2,main=main,cex.main="2",
        labelcex = 1.5,shade = 0.9,
        col=c( "#444e86","#955196","#A7C7E7", "#dd5182","#ff6e54","#ffa600")) 
  legend("topright",legend =   tdf$dfCol,cex =1.2,  
         fill = c( "#444e86","#955196","#A7C7E7", "#dd5182","#ff6e54","#ffa600"))
  
}

pie_3d(df$Attrition,"Employee Attrition Pie chart") #==>1
pie_3d(df$JobLevel,"Job Level Count Pie chart") #==>2
pie_3d(df$MaritalStatus,"Marital Status Pie chart") #==>3
pie_3d(df$Department,"Count of Employees in each Department") #===>4
pie_3d(df$EducationField,"Count of Employees in Education Field") #===>5
pie_3d(df$OverTime," Count of Employees working Overtime") #===>6
pie_3d(df$Attrition,"Attrition Count Pie chart")


histogram<-function(df,xdata,xtitle,ytitle,title, color=NULL,legend=NULL){
  u<-ggplot(data=df, aes(x=xdata,
                         fill=color))
  u+geom_histogram(stat = "count")+ggtitle(title)+
    xlab(xtitle)+
    ylab(ytitle)+
    theme(axis.title.x=element_text(color = "#456498", size = 25),
          axis.title.y=element_text(color = "#456498", size = 25),
          axis.text.x=element_text(size=15),
          axis.text.y=element_text(size=15),
          plot.title = element_text(color = "#213C3E",
                                    size=30),
          legend.title = element_text(size=20),
          legend.text = element_text(size=20),
    )+labs(fill = legend)
}

histogram(df, df$TotalWorkingYears,"Total Working Years",
          "Count","Working years and Attrition",df$Attrition,"Attrition")

histogram(df, df$YearsAtCompany,"Years At Company","Count",
          "Years At Company and Attrition",df$Attrition,"Attrition")

histogram(df, df$TrainingTimesLastYear,"Training Times Last Year","Count",
          "Training Times Last Year and Attrition",df$Attrition,"Attrition")

histogram(df, df$YearsInCurrentRole,"Years In CurrentRole","Count",
          "Years In Current Role and Attrition",df$Attrition,"Attrition")

histogram(df, df$YearsWithCurrManager,"Years With Current Manager","Count",
          "Years With Current Manager and Attrition",df$Attrition,"Attrition")

histogram(df, df$YearsWithCurrManager,"Years With Current Manager","Count",
          "Years With Current Manager and PerformanceRating",
          df$PerformanceRating,"PerformanceRating")

histogram(df, df$YearsWithCurrManager,"Years With Current Manager",
          "Count","Years With Current Manager and EnvironmentSatisfaction",
          df$EnvironmentSatisfaction,"EnvironmentSatisfaction")

histogram(df, df$YearsSinceLastPromotion,"Years Since Last Promotion","Count",
          "Years Since Last Promotion and Attrition",df$Attrition,"Attrition")

histogram(df, df$PercentSalaryHike,"Percent Salary Hike","Count",
          "Percent Salary Hike and Attrition",df$Attrition,"Attrition")

histogram(df, df$PercentSalaryHike,"Percent Salary Hike","Count",
          "Percent Salary Hike and Performance Rating",df$PerformanceRating,
          "Performance Rating")

histogram(df, df$PercentSalaryHike,"Percent Salary Hike","Count",
          "Percent Salary Hike and Job Level",df$JobLevel,"JobLevel")

histogram(df, df$DistanceFromHome,"Distance From Home","Count",
          "Distance From Home and Attrition",df$Attrition,"Attrition")

histogram(df, df$NumCompaniesWorked,"Number of Companies Worked","Count",
          "Number of Companies Worke and Attrition",df$Attrition,"Attrition")


