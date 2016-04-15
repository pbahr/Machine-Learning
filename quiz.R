library(AppliedPredictiveModeling)
data(AlzheimerDisease)

adData = data.frame(diagnosis, predictors)
trainIndex = createDataPartition(diagnosis, p=.5, list=F)
train = adData[trainIndex,]
test = adData[-trainIndex,]
# .50, train, -train

#== Q 2
data("concrete")
library(caret)
set.seed(1000)
library(ggplot2)

inTrain = createDataPartition(mixtures$CompressiveStrength, p= 3/4)[[1]]
train = mixtures[inTrain,]
test = mixtures[-inTrain,]

mixtures$Cement.group <- cut(mixtures$Cement, breaks=4)
mixtures$Age.group <- cut(mixtures$Age, breaks=4)
mixtures$FlyAsh.group <- cut(mixtures$FlyAsh, breaks=4)
ggplot(mixtures, aes(y=CompressiveStrength, x=1:nrow(mixtures), color= FlyAsh.group)) +
    geom_point() +
    geom_smooth(method= "lm")

ggplot(mixtures, aes(x= Superplasticizer)) +
    geom_histogram()
# non-random pattern, not perfectly explained

# Q 3
# Large number the same

#== Q 4
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis, predictors)
trainIndex = createDataPartition(diagnosis, p=3/4)[[1]]
train = adData[trainIndex,]
test = adData[-trainIndex,]

index <- grep("^IL", names(adData))
names(adData)[index]

prep = preProcess(adData[,index], method = "pca", thresh = .9)
prep = preProcess(adData[,index], method = "pca", thresh = .8)
length(prep)
dim(prep$rotation)[2]
#9

#== Q 5
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
mydata <- data.frame(diagnosis, adData[,index])
trainIndex = createDataPartition(diagnosis, p=3/4)[[1]]
str(mydata)
train = mydata[trainIndex,]
test = mydata[-trainIndex,]

model1 <- train(diagnosis ~ ., data= train, method="glm")
model1.test <- predict(model1, test[,-1])
model1.test
confusionMatrix(model1.test, test$diagnosis)

prep = preProcess(train, method=c("center","scale", "pca"), thresh = .8)
pc = predict(prep, train)
pc

model2 <- train(diagnosis ~ ., data= pc, method= "glm")
model2
testPC <- predict(prep, test)
testPC
model2.test <- predict(model2, testPC[,-1])
model2.test
confusionMatrix(model2.test, testPC$diagnosis)

# .65, .72