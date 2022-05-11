## Bagging and Random Forests

#install.packages("randomForest")
library(randomForest)
heart = read.csv('C:/Users/wjddj/Desktop/DS301/final/heart_2020_cleaned.csv')

factorcols <-c("HeartDisease", "Smoking","AlcoholDrinking","Stroke","DiffWalking","Sex",
               "Race","Diabetic","PhysicalActivity","GenHealth",
               "Asthma","KidneyDisease","SkinCancer")
heart[factorcols]<- lapply(heart[factorcols],factor)

## Even though I set seed, your results may differ from mine depending on your version of R and your version of randomForest package. 
set.seed(1)

## Bagging is a special case of a random forest with m = p. 
## Therefore, randomForest() function can implement both random forests and bagging. 

## Bagging

train = sample(1:nrow(heart),nrow(heart)/2)
dim(heart)

bag.heart = randomForest(HeartDisease~.,data=heart, subset = train, mtry = 17, importance = TRUE, ntree=10)

importance(bag.heart)
# two measures of variable importance are report
# (1) %IncMSE: A larger value means a predictor is more important. 

# (2): IncNodePurity: for classification trees, this is the total decrease in node impurity (or increase in node purity) from splitting on a predictor, averaged over all trees. Measured by Gini index. 
#For regression trees, the node impurity is measured by the training RSS. 

?importance

varImpPlot(bag.heart)

## test MSE
yhat.bag =  predict(bag.heart, newdata = heart[-train, ]) 
heart.test = heart[-train,"HeartDisease"]
mean(yhat.bag==heart.test)
conf_table = table(yhat.bag,heart.test)
conf_table
#plot(yhat.bag, heart.test)
#abline(0, 1)

#mean((yhat.bag - heart.test)^2)

#### Random Forest ####
## By default, randomForest() uses p/3 variables when building a random forest of regression trees, and sqrt(p) variables when building a random forest of classification trees. 

set.seed(1)
rf.heart = randomForest(HeartDisease ~., data = heart, subset = train, importance = TRUE, ntree = 100)

yhat.rf = predict(rf.heart, newdata = heart[-train, ]) 
mean(yhat.rf==heart.test)
conf_table = table(yhat.rf,heart.test)
conf_table
#mean((yhat.rf - heart.test)^2)

importance(rf.heart)
varImpPlot(rf.heart)