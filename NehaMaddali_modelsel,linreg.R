
########### Linear Regression ########### 
###########    Neha Maddali   ###########
library(leaps)
setwd("/Users/neham/Desktop/DS301/FinalProject")

df <- readr::read_csv('/Users/neham/Desktop/DS301/FinalProject/heart_2020_cleaned.csv')
df$HeartDisease = ifelse(df$HeartDisease == 'Yes', 1, 0)
df$Smoking = factor(df$Smoking)
df$AlcoholDrinking = factor(df$AlcoholDrinking)
df$Stroke = factor(df$Stroke)
df$DiffWalking = factor(df$DiffWalking)
df$Sex = factor(df$Sex)
df$Race = factor(df$Race)
df$Diabetic = factor(df$Diabetic)
df$PhysicalActivity = factor(df$PhysicalActivity)
df$GenHealth = factor(df$GenHealth)
df$Asthma = factor(df$Asthma)
df$KidneyDisease = factor(df$KidneyDisease)
df$SkinCancer = factor(df$SkinCancer)

df$AgeCategory = ifelse(df$AgeCategory == '18-24', 'Young',
                        ifelse(df$AgeCategory == '25-29', 'Young',
                               ifelse(df$AgeCategory == '30-34', 'Young',
                                      ifelse(df$AgeCategory == '35-39', 'Young',
                                             ifelse(df$AgeCategory == '40-44', 'Young',
                                                    ifelse(df$AgeCategory == '45-49', 'Old',
                                                           ifelse(df$AgeCategory == '50-54', 'Old',
                                                                  ifelse(df$AgeCategory == '55-59', 'Old',
                                                                         ifelse(df$AgeCategory == '60-64', 'Old',
                                                                                ifelse(df$AgeCategory == '65-69', 'Old',
                                                                                       ifelse(df$AgeCategory == '70-74', 'Old',
                                                                                              ifelse(df$AgeCategory == '75-79', 'Old',
                                                                                                     ifelse(df$AgeCategory == '80 or older', 'Old', 0)))))))))))))

df$AgeCategory = factor(df$AgeCategory)

#check for multicollinearity -- there is no multicollinearity
model = lm(BMI ~., data=df)
library(car)
vif(model)

#multiple linear regression model on all predictors
fit = lm(BMI~., data=df)
n = nrow(df)
train_index = sample(1:n,n/2,rep=FALSE)
train = df[train_index,]
test = df[-train_index,]
model_train = lm(BMI~.,data=train)
MSE_train = mean((train$BMI - model_train$fitted.values)^2) 
predicted_values = predict(model_train,test)
MSE_test = mean((test$BMI - predicted_values)^2)
MSE_test
sum(fit$residual^2)/(319795-(18+1))
#having kidney disease did not seem to affect BMI

#best subset selection on all predictors
regfit = regsubsets(BMI~.,data=df,nbest=1,nvmax=NULL)
regfit.sum = summary(regfit)
n = dim(df)[1]
p = rowSums(regfit.sum$which)
adjr2 = regfit.sum$adjr2
cp = regfit.sum$cp
rss = regfit.sum$rss
AIC = n*log(rss/n) + 2*(p)
BIC = n*log(rss/n) + (p)*log(n)
cbind(p,adjr2,cp,AIC,BIC)
which.min(BIC)
which.min(AIC)
which.max(adjr2)
regfit.sum$which[25,]

#best subset selection - find the best test mse
train_index = sort(sample(nrow(df),nrow(df)/2))
train = df[train_index,]
test = df[-train_index,]
regfit.train = regsubsets(BMI~., data = train, nvmax=NULL)
val.errors = rep(NA, 26)
for(i in 1:26){
  test.mat = model.matrix(BMI~., data=test)
  coef.m = coef(regfit.train,id=i)
  pred = test.mat[,names(coef.m)]%*%coef.m
  val.errors[i] = mean((test$BMI-pred)^2)
}
which.min(val.errors)
coef(regfit.train,25)
