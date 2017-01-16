# Creates a simple random forest benchmark

library(randomForest)
library(readr)

set.seed(0)

numTrain <- 10000
numTrees <- 25

train <- read_csv("C:/Users/hp/Desktop/Kaggle/DigitRecognizer/train.csv")
test <- read_csv("C:/Users/hp/Desktop/Kaggle/DigitRecognizer/test.csv")

rows <- sample(1:nrow(train), numTrain)
labels <- as.matrix(train[rows,1])
require(reshape2)
labels$Id <-rownames(labels)
melt(labels)

train <- train[rows,-1]

rf <- randomForest(train, labels, xtest=test, ntree=numTrees)
predictions <- data.frame(ImageId=1:nrow(test), Label=levels(labels)[rf$test$predicted])
head(predictions)

write_csv(predictions, "rf_benchmark.csv") 
