#setwd("C:/Users/alber/Documents/UVG/Septimo semestre/Mineria de Datos/Hoja_Trabajo_3/HDT3-Mineria")
setwd("C:/Users/Gustavo/Desktop/SEPTIMO SEMESTRE/MINERIA/HDT3/HDT3-Mineria")
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

trainImportantes <- train[c("MSSubClass","LotFrontage","LotArea","OverallCond","YearBuilt","YearRemodAdd","X2ndFlrSF","FullBath","TotRmsAbvGrd","KitchenAbvGr","GarageCars","PoolArea","SalePrice")]
trainImportantes[is.na(trainImportantes)]<-0

#Para saber cual es el mejor numero de clusters
wss <- (nrow(trainImportantes)-1)*sum(apply(trainImportantes,2,var))
for (i in 2:10) 
  wss[i] <- sum(kmeans(trainImportantes, centers=i)$withinss)

# Se plotea la grafica de codo
plot(1:10, wss, type="b", xlab="Number of Clusters",  ylab="Within groups sum of squares")

km<-kmeans(trainImportantes,3)
train$grupo<-km$cluster

plotcluster(trainImportantes,km$cluster)
#Visualización de las k-medias
fviz_cluster(km, data = trainImportantes,geom = "point", ellipse.type = "norm")
#-----------------------------------------------------------------------------------------------
#Silueta de que tan bien hizo el cluster
silkm<-silhouette(km$cluster,dist(trainImportantes))
mean(silkm[,3])

g1<- trainImportantes[trainImportantes$grupo==1,]
g2<- trainImportantes[trainImportantes$grupo==2,]
g3<- trainImportantes[trainImportantes$grupo==3,]


trainTree <- train[c("LotArea","YearBuilt","YearRemodAdd","X2ndFlrSF","FullBath","KitchenAbvGr","GarageCars","grupo")]
#-----------------------------------------------------------------------------------------------
porciento <- 70/100
set.seed(2)
trainRowsNumber<-sample(1:nrow(trainTree),porciento*nrow(trainTree))
train1<-trainTree[trainRowsNumber,]
test1<-trainTree[-trainRowsNumber,]
dt_model<-rpart(grupo~.,train1,method = "class")
plot(dt_model);text(dt_model)
prp(dt_model)
#rpart.plot(dt_model)
prediccion <- predict(dt_model, newdata = test1[,1:7])

columnaMasAlta<-apply(prediccion, 1, function(x) colnames(prediccion)[which.max(x)])
test1$prediccion<-columnaMasAlta #Se le aÃ±ade al grupo de prueba el valor de la predicciÃ³n
#View(test1)
cfm<-confusionMatrix(table(test1$prediccion, test1$grupo))
cfm

#---------------------------------------------------------------------
porciento <- 70/100
set.seed(18)
trainRowsNumber<-sample(1:nrow(trainTree),porciento*nrow(trainTree))
train1<-trainTree[trainRowsNumber,]
test1<-trainTree[-trainRowsNumber,]
dt_modelR<-rpart(grupo~.,train1,method = "anova")
plot(dt_modelR);text(dt_modelR)
prp(dt_modelR)
rpart.plot(dt_modelR)
prediccionR <- predict(dt_modelR, newdata = test1[,1:6])

columnaMasAlta<-apply(prediccionR, 1, function(x) colnames(prediccionR)[which.max(x)])
#test1$prediccion<-columnaMasAlta #Se le aÃ±ade al grupo de prueba el valor de la predicciÃ³n

#cfm<-confusionMatrix(table(test1$prediccion, test1$grupo))
#cfm
#------------------------------------------------------------------ con random forest
modeloRF1<-randomForest(grupo~.,data=train1)
prediccionRF1<-predict(modeloRF1,newdata = test1[,1:7])
testCompleto<-test1
testCompleto$predRF<-round(prediccionRF1)
cfmRandomForest <- confusionMatrix(table(testCompleto$predRF, testCompleto$grupo))
cfmRandomForest
#########################################################################################




