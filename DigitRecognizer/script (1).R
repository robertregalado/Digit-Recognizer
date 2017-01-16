library(plyr)
library(ggplot2)

## This script creates a NB classifier for hand written digit recog
## In this context, each class is a digit, thus we have totally 10 classes for 10 digits

# Load training set -------------------------------------------------------
train <- data.frame(read.csv("C:/Users/hp/Desktop/Kaggle/DigitRecognizer/train.csv"))
num.obs <- nrow(train)

labels   <- train[ ,1]
features <- train[ ,-1]

# Compute priors p(C_k) by simple counting --------------------------------
counts <- aggregate(labels, list(labels), length)
counts
sum(counts) == num.obs
# counts <- count(train, "label")
priors <- counts/num.obs



# Compute the means and standard deviations of pixels by digit --------
means <- aggregate(features, list(labels), mean)
means[is.na(means)] <- 0.0

stds <- aggregate(features, list(labels), sd)
stds[is.na(stds)] <- 0.0

# Use the means and standard deviations to make predictions