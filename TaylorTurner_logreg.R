library(MASS)
library(dplyr)
library(ROCR)

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


library(corrplot)
library(caret)
dm = data.matrix(df[,names(df) != 'HeartDisease'])
cor(dm)
                 