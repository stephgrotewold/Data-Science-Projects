setwd("/Users/stephgrotewold/Desktop/UFM/VERANO 2023/DATA SCIENCE FOR FINANCE/casos/caso4")
library(ISLR)
library(pROC)
library(ggplot2)
library(dplyr)
library(caret)
library(randomForest)

perdida = read.csv('perdida_clientes.csv')
head(perdida)
str(perdida)

#Distribución de variables numéricas: Permanencia
ggplot(perdida, aes(x = permanencia)) + geom_histogram(binwidth = 5, fill = "lightblue", col='black') +
  labs(title = "Distribución de la Permanencia") + xlab("Permanencia (meses)") + ylab("Frecuencia")
summary(perdida$permanencia)
boxplot(perdida$permanencia, main = "Diagrama de Caja de la Permanencia")
e.permanencia = roc(perdida$perdida~perdida$permanencia)
plot(e.permanencia)
ci.auc(e.permanencia)

#Distribución de variables numéricas: Costo Mensual
ggplot(perdida, aes(x = costo_mensual)) + geom_histogram(binwidth = 10, fill = "pink", col='black') +
  labs(title = "Distribución del Costo Mensual") + xlab("Costo Mensual") + ylab("Frecuencia")
boxplot(perdida$costo_mensual, main = "Diagrama de Caja del Costo Mensual")
summary(perdida$costo_mensual)
e.cmensual = roc(perdida$perdida~perdida$costo_mensual)
plot(e.cmensual)
ci.auc(e.cmensual)

#Distribución de variables numéricas: Total Cobrado
ggplot(perdida, aes(x = total_cobrado)) + geom_histogram(binwidth = 100, fill = "purple", col='black') +
  labs(title = "Distribución del Total Cobrado") + xlab("Total Cobrado") + ylab("Frecuencia")
boxplot(perdida$total_cobrado, main = "Diagrama de Caja del Total Cobrado")
summary(perdida$total_cobrado)
e.tcobro = roc(perdida$perdida~perdida$total_cobrado)
plot(e.tcobro)
ci.auc(e.tcobro)

#Variables cualitativas
#Perdida Por Genero
table(perdida$genero)
e.genero = table(perdida$genero, perdida$perdida)
e.genero
ggplot(perdida, aes(genero)) + geom_bar(fill = c('pink', 'lightblue'))
prop.table(e.genero, 1)
summary(e.genero)

#Perdida Por Estado de Jubilacion
table(perdida$jubilado)
e.jubilado=table(perdida$jubilado, perdida$perdida)
e.jubilado
barplot(table(perdida$jubilado), main = "Distribución de jubilados", xlab = "Jubilado", ylab = "Frecuencia")
prop.table(e.jubilado, 1)
summary(e.jubilado)

#Perdida por pareja
table(perdida$pareja)
e.pareja=table(perdida$pareja, perdida$perdida)
e.pareja
prop.table(e.pareja, 1)
summary(e.pareja)

#Perdida tipo de contrato
e.contrato= table(perdida$contrato, perdida$perdida)
e.contrato
prop.table(e.contrato, 1)
summary(e.contrato)

#Perdida por si tiene proteccion o no
e.proteccion = table(perdida$proteccion, perdida$perdida)
e.proteccion
prop.table(e.proteccion,1)
summary(e.proteccion)

#Perdida por tipo de servicio de internet
e.internet = table(perdida$internet, perdida$perdida)
e.internet
prop.table(e.internet,1)
summary(e.internet)

#Perdida por metodo de pago
e.mpago = table(perdida$metodo_pago, perdida$perdida)
e.mpago
prop.table(e.mpago,1)
summary(e.mpago)

#Perdida por si cuenta con servicio de backup
e.backup = table(perdida$backup_online, perdida$perdida)
e.backup
prop.table(e.backup,1)
summary(e.backup)

#Perdida por si cuenta con soporte o no
e.soporte = table(perdida$soporte, perdida$perdida)
e.soporte
prop.table(e.soporte,1)
summary(e.soporte)

#Perdida por si cuenta con servicio de streaming
e.streamingTV = table(perdida$streaming_TV, perdida$perdida)
e.streamingTV
prop.table(e.streamingTV,1)
summary(e.streamingTV)

#Perdida por si cuenta con el servicio de streaming de peliculas
e.streamingpelis = table(perdida$streaming_peliculas, perdida$perdida)
e.streamingpelis
prop.table(e.streamingpelis,1)
summary(e.streamingpelis)

#Perdida por si cuenta servicio de celular
e.servtel = table(perdida$servicio_tel, perdida$perdida)
e.servtel
prop.table(e.servtel,1)
summary(e.servtel)

#Perdida Por si cuenta con multiples lineas telefonicas
e.mult = table(perdida$multiples_lineas, perdida$perdida)
e.mult
prop.table(e.mult,1)
summary(e.mult)

#Perdida por si cuenta con seguridad online
e.seguridad = table(perdida$seguridad_online, perdida$perdida)
e.seguridad
prop.table(e.seguridad,1)
summary(e.seguridad)


#ajustar formato a factores para variables categoricas
perdida2=transform(perdida, 
                      genero = as.factor(genero),
                      jubilado = as.factor(jubilado),
                      pareja = as.factor(pareja),
                      dependientes = as.factor(dependientes),
                      servicio_tel = as.factor(servicio_tel),
                      multiples_lineas = as.factor(multiples_lineas),
                      internet = as.factor(internet),
                      seguridad_online = as.factor(seguridad_online),
                      backup_online = as.factor(backup_online),
                      proteccion = as.factor(proteccion),
                      soporte = as.factor(soporte),
                      streaming_TV = as.factor(streaming_TV),
                      streaming_peliculas = as.factor(streaming_peliculas),
                      contrato = as.factor(contrato),
                      factura_electronica = as.factor(factura_electronica),
                      metodo_pago = as.factor(metodo_pago),
                      perdida = as.factor(perdida))
str(perdida2)

#Dividir el dataset en train y test
index = sort(sample(nrow(perdida2),nrow(perdida2)*0.7))
train = perdida2[index,]
test = perdida2[-index,]

cols=c("permanencia", "costo_mensual", "total_cobrado")
cols

#preproesamiento
escaladas = preProcess(train[,cols],method=c('center','scale'))
escaladas

#escalar datos de train con predict
train[,cols]=predict(escaladas, train[,cols])
summary(train)

#aplicar a test
test[,cols]=predict(escaladas, test[,cols])
summary(test)

#quitar NA
train = na.omit(train)

#construir modelo de random forest
rf = randomForest(perdida ~ . -ID, data = train, importance=TRUE, ntree = 70, 
                  mtry = sqrt(ncol(train)),
                  nodesize = 10, type = 'regression', predicted = TRUE)
rf

rf2 = randomForest(perdida ~. -ID - servicio_tel - genero, data = train, importance=TRUE, ntree = 30, 
                  mtry = sqrt(ncol(train)),
                  nodesize = 4, type = 'regression', predicted = TRUE)
rf2

rf3 = randomForest(perdida ~. -ID - servicio_tel - genero - dependientes - pareja -multiples_lineas
                   - proteccion - metodo_pago, data = train, importance=TRUE, ntree = 50, 
                   mtry = sqrt(ncol(train)),
                   nodesize = 10, type = 'regression', predicted = TRUE)
rf3

rf4 = randomForest(perdida ~. -ID - servicio_tel -genero, data = train, importance=TRUE, ntree = 60, 
                   mtry = sqrt(ncol(train)),
                   nodesize = 6, type = 'regression', predicted = TRUE)
rf4


#ver la importancia de las variables
varImp(rf)
varImpPlot(rf) # cuanto aumenta el % de erro medio estandar del modelo


# Definición de función de medición de precision, recall, acurracy and f1
medicion=function(cmatrix)
{
  TN = cmatrix[1,1]
  TP =cmatrix[2,2]
  FN=cmatrix[1,2]
  FP=cmatrix[2,1]
  
  recall=TP/(TP+FN)
  accuracy= (TP+TN)/(TP+TN+FP+FN)
  precision=TP/(TP+FP)
  f1=2*precision*recall/(precision+recall)
  print(paste("Recall: ", round(recall,2)))
  print(paste("precision: ", round(precision,2)))
  print(paste("accuracy: ", round(accuracy,2)))
  print(paste("f1: ", round(f1,2)))
} 

#predicciones del modelo en datos de prueba
predicciones = predict(rf, newdata = test)
predicciones2 = predict(rf2, newdata = test)
predicciones3 = predict(rf3, newdata = test)
predicciones4 = predict(rf3, newdata = test)

#crear matriz de confusión
cmatrix = table(test$perdida, predicciones)
cmatrix2 = table(test$perdida, predicciones2)
cmatrix3 = table(test$perdida, predicciones3)
cmatrix4 = table(test$perdida, predicciones4)


#Aplicar la función de medición a la matriz de confusión
medicion(cmatrix)
medicion(cmatrix2)
medicion(cmatrix3)
medicion(cmatrix4)

