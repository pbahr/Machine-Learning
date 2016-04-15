library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)

model.data <- segmentationOriginal
outcome <- model.data$Class
#train.proportion <- .7
set.seed(125)

inTrain <- model.data$Case == "Train"
training <- model.data[inTrain,]
testing <- model.data[!inTrain,]

str(model.data)
model <- train(Class ~ ., method= "rpart", data=training)
#    rpart(Case ~ ., training)

library(rattle)
library(rpart.plot)
fancyRpartPlot(model)

model$finalModel

table(model.data$Case)
table(model.data$Class)

names(model.data)

# predict(model, newdata = data.frame(TotalIntench2 = 23000, FiberWidthCh1 = 10, PerimStatusCh1=2))
# PS
# predict(model, newdata = data.frame(TotalIntench2 = 50000, FiberWidthCh1 = 10, VarIntenCh4=100))
# WS
# predict(model, newdata = data.frame(TotalIntench2 = 57000, FiberWidthCh1 = 8, VarIntenCh4=2))
# PS
# predict(model, newdata = data.frame(FiberWidthCh1 = 8, VarIntenCh4 = 100, PerimStatusCh1=2))
# Not able to predict

library(rattle)
library(rpart.plot)
fancyRpartPlot(model$finalModel)

# Q 3

library(pgmm)
data(olive)
olive = olive[,-1]
str(olive)
table(olive$Area)

model.data <- olive
outcome <- model.data$Area

sapply(olive, table, useNA= "ifany")
model <- train(Area ~ ., method="rpart", data=olive)
predict(model, newdata = as.data.frame(t(colMeans(olive))))
# 2.783, Strange b/c we need categorical not quant.

# Q 4

library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

set.seed(13234)
model <- train(trainSA$chd ~ age + alcohol + obesity + tobacco + typea + ldl, method="glm", family="binomial", data = trainSA)
str(SAheart)
missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}
missClass(trainSA$chd, predict(model))
missClass(testSA$chd, predict(model, newdata = testSA))

# train: .27, test: .31

# Q 5

library(ElemStatLearn)
data(vowel.train)
data(vowel.test)
str(vowel.train)

vowel.train$y <- factor(vowel.train$y)
vowel.test$y <- factor(vowel.test$y)

set.seed(33833)

model <- train(y ~ ., data=vowel.train, method="rf")
varImp(model)
