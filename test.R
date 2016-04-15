data(iris)
pairs(iris)
cor(iris[,1:4])
str(iris)

set.seed(123)

inTrain <- createDataPartition(iris$Species, p= .7, list = F)
training <- iris[inTrain,]
testing <- iris[-inTrain,]

trCtrl <- trainControl(method = "cv", number= 10) # do k-fold, k= 10
fit <- train(Species ~ ., data=training, trControl= trCtrl, method= "nb")
fit

trCtrl2 <- trainControl(method = "repeatedcv", repeats = 5, number= 10)
fit2 <- train(Species ~ ., data=training, trControl= trCtrl2, method= "nb")
fit2

test.out <- predict(fit, newdata = testing)
confusionMatrix(test.out, testing$Species)

test.out2<- predict(fit2, newdata = testing)
confusionMatrix(test.out2, testing$Species)

#== Decision Tree
fit.part <- train(Species ~ ., data=training, method= "rpart")
fit.part

test.out.part <- predict(fit.part, testing)
confusionMatrix(test.out.part, testing$Species)
confusionMatrix(test.out2, testing$Species)

#== Random Forest
fit.rf <-  train(Species ~ ., data=training, method= "rf")
fit.rf

test.out.rf <- predict(fit.rf, testing)
confusionMatrix(test.out.rf, testing$Species)
confusionMatrix(test.out2, testing$Species)
