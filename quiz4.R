library(ElemStatLearn)

data(vowel.train)
data(vowel.test)

vowel.train$y <- factor(vowel.train$y)
vowel.test$y <- factor(vowel.test$y)

str(vowel.train)
set.seed(33833)

library(caret)
fit.rf <- train(y ~ ., method="rf", data=vowel.train)

library(gbm)
fit.gbm <- train(y ~ ., method="gbm", data= vowel.train)

test.rf <- predict(fit.rf, newdata = vowel.test)
test.gbm <- predict(fit.gbm, newdata = vowel.test)

confusionMatrix(test.rf, vowel.test$y)
# Accuracy: 59.96%
confusionMatrix(test.gbm, vowel.test$y)
# Accuracy: 52.38%
confusionMatrix(test.rf, test.gbm)
# 69.7%

# RF: 60.82%
# GBM: 51.52%
# Combined: 63.61%

#== Q.2
library(caret)
library(gbm)

set.seed(3433)

library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)

inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

set.seed(62433)

model1 <- train(diagnosis ~ ., method="rf", data= training)
model2 <- train(diagnosis ~ ., method="gbm", data= training)
model3 <- train(diagnosis ~ ., method="lda", data= training)

result.rf <- predict(model1, training)
result.gbm <- predict(model2, training)
result.lda <- predict(model3, training)

comb.data <- data.frame(diagnosis= training$diagnosis, rf = result.rf, gbm= result.gbm, lda= result.lda)
model4 <- train(diagnosis ~ ., method= "rf", data= comb.data)

pred.rf <- predict(model1, newdata = testing)
pred.gbm <- predict(model2, newdata = testing)
pred.lda <- predict(model3, newdata = testing)

confusionMatrix(pred.rf, testing$diagnosis)
#76.83%
confusionMatrix(pred.gbm, testing$diagnosis)
#79.27%
confusionMatrix(pred.lda, testing$diagnosis)
#76.83%

test.data <- data.frame(diagnosis= testing$diagnosis, rf= pred.rf, gbm= pred.gbm, lda= pred.lda)
pred.comb <- predict(model4, newdata = test.data)

confusionMatrix(pred.comb, testing$diagnosis)
# 79.27%
#80% Same as boosting

#== Q. 3
set.seed(3523)

library(AppliedPredictiveModeling)
data(concrete)

inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

set.seed(233)
model <- train(CompressiveStrength ~ . , method="lasso", data=concrete)
plot.enet(model$finalModel, xvar="penalty", use.color = T)
#Cement

#== Q. 4
library(lubridate) # For year() function below

dat= read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/gaData.csv")
#dat = read.csv("gaData.csv")

training = dat[year(dat$date) < 2012,]
testing = dat[(year(dat$date)) > 2011,]

tstrain = ts(training$visitsTumblr)

head(training)
tail(training)
head(testing)
tail(testing)

library(forecast)
fit <- bats(tstrain)
fcast <- forecast(fit, h= length(testing$visitsTumblr), level = c(95))
fcast
sum(testing$visitsTumblr > fcast$lower[,1] & testing$visitsTumblr < fcast$upper[,1]) / length(testing$visitsTumblr)

#96%

#== Q. 5
set.seed(3523)

library(AppliedPredictiveModeling)
data(concrete)

inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

set.seed(325)
library(e1071)
model <- svm(CompressiveStrength ~ ., data= training)
print(model)
summary(model)
pred <- predict(model, testing)
table(pred, training$CompressiveStrength)
#sqrt(sum((pred - testing$CompressiveStrength)^2)/length(testing$CompressiveStrength))
accuracy(pred, testing$CompressiveStrength)

#6.72