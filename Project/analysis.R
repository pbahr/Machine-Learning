training <- read.csv("./Project/data/pml-training.csv", na.strings = c("","#DIV/0!", "NA"))
testing <- read.csv("./Project/data/pml-testing.csv", na.strings = c("","#DIV/0!", "NA"))

str(training)
head(training)

# Classe: A Right way
# Others Wrong way

# Citation: Velloso, E.; Bulling, A.; Gellersen, H.; Ugulino, W.; Fuks, H. Qualitative Activity Recognition of Weight Lifting Exercises. Proceedings of 4th International Conference in Cooperation with SIGCHI (Augmented Human '13) . Stuttgart, Germany: ACM SIGCHI, 2013.

# Finding PCAs
outcome <- 160
irrelevant <- c(1:7)

str(training[,-c(irrelevant,outcome)])
str(training[,90:160])

predictors <- training[,-c(irrelevant,outcome)] 
M <- abs(cor(predictors))
diag(M) <- 0
which(M>.7, arr.ind=T)

# Array containing highly-correlated vars
arr <- which(M>.7, arr.ind=T)
# Data frame containing correlated vars
cor.df <- data.frame(row.no= arr[,1], row= names(predictors)[arr[,1]], 
                     col.no= arr[,2], col= names(predictors)[arr[,2]])
cor.df$corr <- mapply(function(i,j) { M[i,j] }, cor.df$row.no, cor.df$col.no)
    #M[cor.df$row.no, cor.df$col.no]

# Keep one of the instances of each pair
library(dplyr)
cor.df <- filter(cor.df, row.no > col.no)

# visualize correlation btw correlated variables                     
library(ggplot2)
ggplot(cor.df, aes(row.no, col.no, color= corr)) +
    geom_point()

ggplot(cor.df, aes(row, col, color= corr)) +
    geom_point() +
    theme(axis.text.x= element_text(angle= 45, vjust=1, hjust= 1))

grep("amplitude_yaw", names(predictors))
summary(predictors[,c(19,94,132)])

# Find columns with high % of NAs
percent.nas <- sapply(predictors, function(x){round(sum(is.na(x)) * 100/length(x), 2)})
which(percent.nas > 0)
percent.nas[percent.nas > 0]

# Filter highly-NA from predictors
predictors <- predictors[, -which(percent.nas > 0)]
#grep("kurtosis", names(predictors), value = T)

# Came to 52 from 152
# We can reduce them further by PCA
pcs.pr <- prcomp(predictors, scale. = T)
plot(pcs.pr, type="l")
# Result: 7 components

library(caret)
PC.model <- preProcess(predictors, method = "pca", thresh = .9)
# Calculate training Principle Components
pcs <- predict(PC.model, predictors)
#length(pcs)
#pcs

pc.data <- cbind(pcs, training$classe)
names(pc.data)[ncol(pc.data)] <- "classe"
#str(pc.data)

tr <- trainControl(method= "cv", number = 10)
model <- train(classe ~ ., method= "rpart", data= pc.data, trControl = tr)
model

model2 <- train(classe ~ ., method= "rf", data= pc.data, trControl = tr)
model2
test.pc <- predict(PC.model, newdata = testing)
str(test.pc)
names(test.pc)
test.pc <- test.pc[,c(109:127)]

test.classe <- predict(model, newdata = test.pc)

test.classe
