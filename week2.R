library(ISLR)
library(ggplot2)
library(caret)

data("Wage")
summary(Wage)

#=== Partition

inTrain <- createDataPartition(y=Wage$wage, p=.7, list = FALSE)
training <- Wage[inTrain,]
test <- Wage[-inTrain,]
dim(training)
dim(test)

featurePlot(x= training[,c("age","education", "jobclass")],
            y= training$wage,
            plot= "pairs")

library(kernlab)
data("spam")
inTrain <- createDataPartition(y=spam$type, p=.7, list = FALSE)
training <- spam[inTrain,]
test <- spam[-inTrain,]

set.seed(13343)
training$capAve <- training$capitalAve
selectNA <- rbinom(dim(training)[1], size= 1, prob = .05) == 1
#sum(selectNA)/dim(training)[1]

training$capAve[selectNA] <- NA
names(training)
preObj <- preProcess(training[,-58], method = "knnImpute")
capAve <- predict(preObj, training[,-58])$capAve

quantile(capAve - training$capitalAve)[!selectNA]

str(Wage)
