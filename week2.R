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

# add capAve column with capitalAve values
training$capAve <- training$capitalAve

# turn some random values of capAve to NA
selectNA <- rbinom(dim(training)[1], size= 1, prob = .05) == 1
training$capAve[selectNA] <- NA

str(training)
names(training)
# 59: capAve
# 58: type
# 55: capitalAve

preObj <- preProcess(training[,-58], method = "knnImpute")

#mine
pre.df <- predict(preObj, training[,-58]) # works fine
summary(pre.df$capAve)

capAve <- pre.df$capAve
# end of mine

#capAve <- predict(preObj, training[,-58])$capAve

quantile(capAve - training$capitalAve)
length(capAve - training$capitalAve)

length((capAve - training$capitalAve)[!selectNA])
quantile((capAve - training$capitalAve)[!selectNA])

quantile((capAve - training$capitalAve)[selectNA])

str(Wage)
