setwd("C:/Users/alber/Documents/UVG/Septimo semestre/Mineria de Datos/Hoja_Trabajo_3/HDT3-Mineria")
library(rpart)
library(caret)
library(tree)
library(rpart.plot)
library(randomForest)

library(cluster) #Para calcular la silueta
library(e1071)#para cmeans
library(mclust) #mixtures of gaussians
library(fpc) #para hacer el plotcluster
library(NbClust) #Para determinar el número de clusters óptimo
library(factoextra) #Para hacer gráficos bonitos de clustering
library(tidyr)
library(splitstackshape)

test <- read.csv("test.csv", stringsAsFactors = FALSE)
train <- read.csv("train.csv", stringsAsFactors = FALSE)
View(head(train))
summary(train)

trainImportantes <- train[c("MSSubClass","LotFrontage","LotArea","OverallCond","YearBuilt","YearRemodAdd","X2ndFlrSF","FullBath","TotRmsAbvGrd","KitchenAbvGr","GarageCars","PoolArea","SalePrice")]
trainImportantes[is.na(trainImportantes)]<-0
#Para saber cual es el mejor numero de clusters
wss <- (nrow(trainImportantes)-1)*sum(apply(trainImportantes,2,var))
for (i in 2:10) 
  wss[i] <- sum(kmeans(trainImportantes, centers=i)$withinss)

# Se plotea la grafica de codo
plot(1:10, wss, type="b", xlab="Number of Clusters",  ylab="Within groups sum of squares")

