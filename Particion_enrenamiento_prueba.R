datos<-iris
porciento <- 70/100 #Porciento en el que se partirán los datos
muestra<-sample(1:nrow(datos),porciento*nrow(datos))#Muestra aleatoria de numeros de un vector

trainSet<-datos[muestra,] #Obtengo las filas de los elementos que estan en el sector de muestra
testSet<-datos[-muestra,] #Obtengo las filas de los elementos que no están en el vector de muestra
