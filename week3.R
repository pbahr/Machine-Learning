data(iris)
library(ggplot2)
library(caret)

names(iris)
table(iris$Species)

model.data <- iris
outcome <- iris$Species
train.proportion <- .7

inTrain <- createDataPartition(y=outcome, p=train.proportion, list = FALSE)
training <- model.data[inTrain,]
testing <- model.data[-inTrain,]

# EDA
# ===
ggplot(model.data, aes(Petal.Width, Sepal.Width, color= Species)) +
    geom_point()

ggplot(model.data, aes(Petal.Length, Sepal.Length, color= Species)) +
    geom_point()
# Modeling

library(caret)
model <- train(Species ~ ., method= "rpart", data= training)
print(model$finalModel)

# Fancy plot
library(rattle)
library(rpart.plot)
fancyRpartPlot(model$finalModel)

# Predict
results <- predict(model, newdata= testing)
model$finalModel
confusionMatrix(results, testing$Species)

