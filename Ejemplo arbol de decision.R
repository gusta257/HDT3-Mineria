# instalación de paquetes -------------------------------------------------

# install.packages("tree")

library(rpart)
library(caret)
library(tree)
library(rpart.plot)
library(randomForest)

datos <- iris

# variable respuesta la clase de la flor
porciento <- 70/100

set.seed(123)

trainRowsNumber<-sample(1:nrow(datos),porciento*nrow(datos))
train<-datos[trainRowsNumber,]
test<-datos[-trainRowsNumber,]
test0<-test
dt_model<-rpart(Species~.,train,method = "class")
plot(dt_model);text(dt_model)
prp(dt_model)
rpart.plot(dt_model)

head(test0)
prediccion <- predict(dt_model, newdata = test0[1:4])

#Apply: Para cada fila, determina el nombre de la columna del valor máximo entre los tres valores de una fila
columnaMasAlta<-apply(prediccion, 1, function(x) colnames(prediccion)[which.max(x)])
test0$prediccion<-columnaMasAlta #Se le añade al grupo de prueba el valor de la predicción

cfm<-confusionMatrix(table(test0$prediccion, test0$Species))
cfm

#con caret
ct<-trainControl(method = "cv",train[,1:4],number=3, verboseIter=T)
modelorf<-train(Species~.,data=train,method="rf",trControl = ct)
prediccionRF<-predict(modelorf,newdata = test[,1:4])
testCompleto1<-test
testCompleto1$predRFCaret<-prediccionRF
cfmCaret <- confusionMatrix(testCompleto1$predRFCaret,test$Species)
cfmCaret

#con random forest
modeloRF1<-randomForest(Species~.,data=train)
prediccionRF1<-predict(modeloRF1,newdata = test)
testCompleto<-test
testCompleto$predRF<-prediccionRF1
cfmRandomForest <- confusionMatrix(testCompleto$predRF, testCompleto$Species)
View(test0)
View(testCompleto)
View(testCompleto1)
cfm
cfmCaret
cfmRandomForest
