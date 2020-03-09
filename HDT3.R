setwd("C:/Users/Gustavo/Desktop/SEPTIMO SEMESTRE/MINERIA/HDT3/HDT3-Mineria")
library(rpart)
library(caret)
library(tree)
library(rpart.plot)
library(randomForest)

test <- read.csv("test.csv", stringsAsFactors = FALSE)
train <- read.csv("train.csv", stringsAsFactors = FALSE)
