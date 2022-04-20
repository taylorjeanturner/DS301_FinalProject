library(dplyr)
library(tidyverse)

df <- readr::read_csv('./DS301_FinalProject/heart_2020_cleaned.csv')
str(df)
summary(df)
any(is.na(df))
ggplot(df, aes(x=BMI, y=MentalHealth)) + geom_point()
ggplot(df, aes(x=BMI, y=PhysicalHealth)) + geom_point()
ggplot(df, aes(x=MentalHealth, y=PhysicalHealth)) + geom_point()

df$HeartDisease = factor(df$HeartDisease)
df$Smoking = factor(df$Smoking)
df$AlcoholDrinking = factor(df$AlcoholDrinking)
df$Stroke = factor(df$Stroke)
df$DiffWalking = factor(df$DiffWalking)
df$Sex = factor(df$Sex)
df$AgeCategory = factor(df$AgeCategory)
df$Race = factor(df$Race)
df$Diabetic = factor(df$Diabetic)
df$PhysicalActivity = factor(df$PhysicalActivity)
df$GenHealth = factor(df$GenHealth)
df$Asthma = factor(df$Asthma)
df$KidneyDisease = factor(df$KidneyDisease)
df$SkinCancer = factor(df$SkinCancer)
str(df)

model = lm(BMI~., data=df)
set.seed(100)
n = nrow(df)
train_index = sample(1:n,n/2,rep=FALSE)

train = df[train_index,]
test = df[-train_index,]

model_train = lm(BMI~., data=train)
MSE_train = mean((train$BMI - model_train$fitted.values)^2) 
MSE_train

predicted_values = predict(model_train,test)
MSE_test = mean((test$BMI - predicted_values)^2)
MSE_test

ggplot(df, aes(x=HeartDisease, fill=Smoking)) + geom_bar(position="dodge")+ ggtitle("Heart Disease and Smoking")
ggplot(df, aes(x=HeartDisease, y=BMI)) + geom_boxplot() + ggtitle("Heart Disease and BMI")
ggplot(df, aes(x=HeartDisease, y=MentalHealth)) + geom_boxplot()
ggplot(df, aes(x=HeartDisease, fill=Smoking)) + geom_bar(position="dodge")
ggplot(df, aes(x=HeartDisease, fill=AgeCategory)) + geom_bar(position="dodge")+ ggtitle("Heart Disease and Age")

