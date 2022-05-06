library(MASS)
library(dplyr)
library(ROCR)

##### Logistic Regression #####
df = readr::read_csv('./DS301_FinalProject/heart_2020_cleaned.csv')
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
set.seed(100)
n = nrow(df)
train = sample(1:nrow(df),nrow(df)/2, replace=FALSE)
test = (-train)

# full model
glm.fit = glm(HeartDisease~., data=df, subset=train, family='binomial')
glm.prob = predict(glm.fit, df[test,], type='response') 
glm.pred = rep(0,dim(df[test,])[1])
glm.pred[glm.prob > 0.5] = 1
table(glm.pred, df[test,]$HeartDisease)
1-mean(glm.pred == df[test,]$HeartDisease)

ROCRpred <- prediction(glm.prob,df[test,]$HeartDisease)
plot(performance(ROCRpred,'tnr','fnr'),colorize=TRUE,
     print.cutoffs.at=seq(0,1,by=0.05), text.adj=c(-0.2,1.7))
auc_ROCR <- performance(ROCRpred, measure = "auc")
auc_ROCR@y.values[[1]] 
12396 / (12396 + 1282)
1022 / (1022+145198)

glm.prob = predict(glm.fit, df[test,], type='response') 
glm.pred = rep(0,dim(df[test,])[1])
glm.pred[glm.prob > 0.1] = 1
table(glm.pred, df[test,]$HeartDisease)
1-mean(glm.pred == df[test,]$HeartDisease)
3918 / (3918 + 9760)
33355 / (33355+112865)


##### Linear Regression #####
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
sum(fit$residual^2)/(319795-(18+1)) #having kidney disease did not seem to affect BMI


##### Model Selection #####
#best subset selection on all predictors
regfit = regsubsets(BMI~.,data=df,nbest=1,nvmax=17)
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
regfit.sum$which[17,]

#best subset selection - find the best test mse
train_index = sort(sample(nrow(df),nrow(df)/2))
train = df[train_index,]
test = df[-train_index,]
regfit.train = regsubsets(BMI~., data = train, nvmax=17)
val.errors = rep(NA, 17)
for(i in 1:17){
  test.mat = model.matrix(BMI~., data=test)
  coef.m = coef(regfit.train,id=i)
  pred = test.mat[,names(coef.m)]%*%coef.m
  val.errors[i] = mean((test$BMI-pred)^2)
}
which.min(val.errors)
coef(regfit.train,17)
