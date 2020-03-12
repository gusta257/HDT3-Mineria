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
#View(head(train))
#summary(train)

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


g1<- train[train$grupo==1,]
prop11 <- prop.table(table(g1$categoria1))*100
prop12 <- prop.table(table(g1$categoria2))*100
prop13 <- prop.table(table(g1$categoria3))*100

g2<- train[train$grupo==2,]
prop21 <-prop.table(table(g2$categoria1))*100
prop22 <-prop.table(table(g2$categoria2))*100
prop23 <-prop.table(table(g2$categoria3))*100

g3<- train[train$grupo==3,]
prop31 <-prop.table(table(g3$categoria1))*100
prop32 <-prop.table(table(g3$categoria2))*100
prop33 <-prop.table(table(g3$categoria3))*100
#-----------------------------------------------------------------------------------------------
porciento <- 70/100
set.seed(546)
trainRowsNumber<-sample(1:nrow(train),porciento*nrow(train))
train1<-train[trainRowsNumber,]
test1<-train[-trainRowsNumber,]
dt_model<-rpart(grupo~.,train1,method = "class")
plot(dt_model);text(dt_model)
prp(dt_model)
rpart.plot(dt_model)
prediccion <- predict(dt_model, newdata = test1[1:81])






