setwd("/Users/stephgrotewold/Desktop/UFM/VERANO 2023/DATA SCIENCE FOR FINANCE/casos/caso_final")
library(forecast)
library(tseries)
library(pROC)
library(ggplot2)
library(tree)
library(caret)
library(randomForest)
library(vcd)
library(fastDummies)

tarjetas = read.csv('tarjetas.csv')
str(tarjetas)

#Convertir los datos en una serie de tiempo
ventas=ts(tarjetas$sales, start = c(2014, 1), frequency = 12)

#Ajustar un modelo ARIMA a los datos
modelo_arima = auto.arima(ventas, trace = TRUE)
modelo_arima$aic

#Realizar pronóstico con el modelo ARIMA
pronostico_arima = forecast(modelo_arima, 12, level = 95)
plot(pronostico_arima)
pronostico_arima

#Crear una matriz con los resultados del pronóstico
matriz.pronostico = data.frame(pronostico_arima$mean, pronostico_arima$lower, pronostico_arima$upper)
matriz.pronostico

#Ajustar un modelo ARIMA con tsclean
modelo_arima2 = auto.arima(tsclean(ventas), trace = TRUE)
modelo_arima2$aic #best

#Realizar pronóstico con el modelo ARIMA de los datos limpios
pronostico_arima2 = forecast(modelo_arima2, 12, level = 95)
plot(pronostico_arima2)
pronostico_arima2
#Crear una matriz con los resultados del pronóstico de los datos limpios
matriz.pronostico2 = data.frame(pronostico_arima2$mean, pronostico_arima2$lower, pronostico_arima2$upper)
matriz.pronostico2


sum(pronostico_arima2$mean)
num_tarjetas=sum(pronostico_arima2$mean)
num_tarjetas
#no llegan
min(tarjetas$sales)
mean(pronostico_arima2$lower)
mean(pronostico_arima2$upper)
max(pronostico_arima2$upper)
min(pronostico_arima2$upper)
max(pronostico_arima2$lower)
min(pronostico_arima2$lower)
sum(pronostico_arima2$lower)
sum(pronostico_arima2$upper)

#-----* PARTE 3 *-----

historica = read.csv('cartera_historica.csv')
str(historica)

actual = read.csv('cartera_actual.csv')
str(actual)
historica2=transform(historica, 
                     Cancelo = as.factor(Cancelo),
                     NumProductos = as.factor(NumProductos))
actual=transform(actual, NumProductos = as.factor(NumProductos))

#exploracion de datos:

#Variables Numericas:
e.CreditScore = roc(historica$Cancelo~historica$CreditScore)
plot(e.CreditScore)
ci.auc(e.CreditScore)
#barely there

e.edad = roc(historica$Cancelo~historica$Edad)
plot(e.edad)
ci.auc(e.edad)
#buenisima
#Crear variable de rango de edad
historica5 = historica
historica5$RangoEdad = cut(historica$Edad, breaks = seq(10, 90, by = 10), right = FALSE)
historica5=historica5[!is.na(historica5$RangoEdad), ]

ggplot(data = historica5, aes(x = RangoEdad, fill = Cancelo)) +
  geom_bar() +
  labs(title = "Cancelación por Rango de Edad",
       x = "Rango de Edad", y = "Cantidad") +
  scale_fill_manual(values = c("lightblue", "orange")) +
  theme_minimal()

e.permanencia = roc(historica$Cancelo~historica$Permanencia)
plot(e.permanencia)
ci.auc(e.permanencia)
#not good at all

e.balance = roc(historica$Cancelo~historica$Balance)
plot(e.balance)
ci.auc(e.balance)
#kinda good
ggplot(data = historica2, aes(x = Balance, fill = Cancelo)) +
  geom_density(alpha = 0.5) +
  labs(title = "Distribución del Saldo por Cancelación de Tarjetas",
       x = "Saldo", y = "Densidad") +
  scale_fill_manual(values = c("darkgreen", "red")) +
  theme_minimal()

e.numProducts = roc(historica$Cancelo~historica$NumProductos)
plot(e.numProducts)
ci.auc(e.numProducts)
table(historica$NumProductos)
#so so but good
ggplot(data = historica2, aes(x = NumProductos, fill = Cancelo)) +
  geom_bar(position = "fill") +
  labs(title = "Cancelación de Tarjetas por Número de Productos",
       x = "Número de Productos", y = "Proporción de Clientes") +
  scale_fill_manual(values = c("darkgreen", "orange")) +
  theme_minimal()

ggplot(data = historica2, aes(x = NumProductos, fill = Cancelo)) +
  geom_bar() +
  labs(title = "Cancelación de Tarjetas por Número de Productos",
       x = "Número de Productos", y = "Clientes") +
  scale_fill_manual(values = c("darkgreen", "orange")) +
  theme_minimal()

e.ttarjerta = roc(historica$Cancelo~historica$TieneTarjeta)
plot(e.ttarjerta)
ci.auc(e.ttarjerta)
#just dont

e.activo = roc(historica$Cancelo~historica$Activo)
plot(e.activo)
ci.auc(e.activo)
#not bad, could be

e.salario = roc(historica$Cancelo~historica$SalarioEstimado)
plot(e.salario)
ci.auc(e.salario)
#pls dont

#variables categoricas
e.sexo = table(historica$Sexo, historica$Cancelo)
e.sexo
prop.table(e.sexo,1)
summary(e.sexo)
#good
ggplot(historica2, aes(x = Cancelo, fill = Sexo)) +
  geom_bar(position = "fill") +
  labs(title = "Distribución de Cancelo por Sexo") +
  scale_fill_manual(values = c("pink",'lightblue')) +
  scale_x_discrete(labels =c('No','Si'))+
  theme_minimal()

e.pais = table(historica$Pais, historica$Cancelo)
e.pais
prop.table(e.pais,1)
summary(e.pais)
#i mean it works but does it make sense?
ggplot(historica2, aes(x = Cancelo, fill = Pais)) +
  geom_bar(position = "fill") +
  labs(title = "Distribución de Cancelo por País") +
  scale_fill_manual(values = c("lightblue", "yellow", "red")) +
  scale_x_discrete(labels = c("No", "Sí")) +
  theme_minimal()


historica3= dummy_cols(historica2,select_columns = c('Sexo','Pais','NumProductos'))
actual= dummy_cols(actual,select_columns = c('Sexo','Pais','NumProductos'))
historica4=historica3[,4:23]
str(historica4)
class(historica4)

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

#test y train
index=sort(sample(nrow(historica4),nrow(historica4)*0.7))
train=historica4[index,]
test=historica4[-index,]


#modelos
#regresion
train_regression=train
test_regression=test

modelo_regresion = glm(Cancelo~., data = train_regression, family = 'binomial')
summary(modelo_regresion)

modelo_regresion2 = glm(Cancelo ~ Sexo + Pais + Activo + NumProductos + Balance +
                          Edad + CreditScore, data = train_regression, family = 'binomial')
summary(modelo_regresion2)

modelo_regresion3 = glm(Cancelo ~ Activo + NumProductos + Balance + Edad
                        + CreditScore, data = train_regression, family = 'binomial')
summary(modelo_regresion3)

modelo_regresion4 = glm(Cancelo ~ Sexo_Female + Pais_Germany+ Activo + NumProductos_1+NumProductos_2 +
                          Edad + CreditScore, data = train_regression, family = 'binomial')
summary(modelo_regresion4)

modelo_regresion5 = glm(Cancelo ~ Sexo_Female + Pais_Germany+ Activo + NumProductos + Balance +
                          Edad + CreditScore, data = train_regression, family = 'binomial')
summary(modelo_regresion5)

modelo_regresion6 = glm(Cancelo ~ Sexo + Pais +Activo+ NumProductos + Balance +
                          Edad, data = train_regression, family = 'binomial')
summary(modelo_regresion6)

modelo_regresion$aic #best one? 
modelo_regresion2$aic #could be
modelo_regresion3$aic
modelo_regresion4$aic 
modelo_regresion5$aic #slightly better than 2
modelo_regresion6$aic


#sacar predicted values
probs=predict(modelo_regresion5,type = 'response')
predicted_regresion=ifelse(probs>=0.5,1,0)

train_regression$predicted_regresion=predicted_regresion

#matirz de confusion
cmatrix_regresion=table(predicted_regresion=train_regression$predicted_regresion,
                        train_regression=train_regression$Cancelo)
cmatrix_regresion

probs_regresion=predict(modelo_regresion5,test_regression,typ1='response')
predicted2=ifelse(probs_regresion>=0.5,1,0)
test_regression$predicted2=predicted2

cmatrix_regresion2=table(predicted = test_regression$predicted2, actuals = test_regression$Cancelo)
cmatrix_regresion2

medicion(cmatrix_regresion)#train
medicion(cmatrix_regresion2)#test

mosaic(cmatrix_regresion,shade = T,colorize=T,main="Matriz de confusion")
mosaic(cmatrix_regresion2,shade = T,colorize=T,main="Matriz de confusion")





#arbol de decision
train_arbol = train
test_arbol = test

arbol_decision= tree(Cancelo ~ ., data= train_arbol)
summary(arbol_decision)
plot(arbol_decision)
text(arbol_decision,pretty = 0)

tree.predc=predict(arbol_decision,train_arbol,type="class")

#matriz de confusion
train_arbol$predicted = tree.predc

matrix_decision = table(predicted = train_arbol$predicted,reales=train_arbol$Cancelo)
medicion(matrix_decision)

#si nos gusta el modelo, entonces lo aplicamos a test
tree.predt = predict(arbol_decision,train_arbol,type = "class")
train_arbol$predicted = tree.predt #pegar resultados a dataset de test

#matriz de confusion para predicciones de test
matrix_arbol2 = table(predicted=train_arbol$predicted,reales=train_arbol$Cancelo)


cv.cf = cv.tree(arbol_decision,FUN = prune.misclass)
cv.cf
plot(cv.cf) #el misclass no disminuye mucho

#podar arbol de acuerdo a analisis de misclass:
prune.cf = prune.misclass(arbol_decision,best = 3)
plot(prune.cf)
text(prune.cf,pretty = 0)

summary(prune.cf)
summary(arbol_decision)

#predicted values con modelo podado prune.cf
predprune = predict(prune.cf,test,type = "class")
test_arbol$predicted2 = predprune #pegar resultados a dataset de test

#matriz de confusion de modelo con pruning
matrix_arbol3 = table(predicted = test_arbol$predicted2, reales=test_arbol$Cancelo)

medicion(matrix_arbol2) #test
medicion(matrix_arbol3)#pruned



#RAndom foreeeeessstttt
train_forest = train
test_forest = test
#prerparar variables continuas con montos muy grandes escalandolas
#vector con columnas a transformar:
cols = c('Balance', 'SalarioEstimado')

#Preprocesamiento - escalar variables
escaladas = preProcess(train_forest[, cols], method = c('center', 'scale'))
train_forest[, cols] = predict(escaladas, train_forest[, cols])
test_forest[, cols] = predict(escaladas, test_forest[, cols])

#quitar NA
train_forest2 = na.omit(train_forest)

#construir modelo de random forest
rf = randomForest(Cancelo ~. , data = train_forest2, importance=TRUE, ntree = 50, 
                  mtry = sqrt(ncol(train_forest)),
                  nodesize = 10, type = 'regression', predicted = TRUE)
rf

#ver importancia de las variables
varImpPlot(rf)


#matriz de confusion
rf$confusion

#pegar predicted values calculados en rf al dataset de train
rf.pred= rf$predicted
train_forest$predicted=rf.pred

#predecir datos para test
rf.predt = predict(rf, test_forest)
test_forest$predicted = rf.predt

#matriz de confusion para test
cmatrix_forest = table(predicted=test_forest$predicted,reales=test_forest$Cancelo)

#train:
medicion(rf$confusion)

cmatrix_forest2 = table(predicted = train_forest$predicted,actuals = train_forest$Cancelo)
cmatrix_forest2

medicion(cmatrix_forest) #test
medicion(cmatrix_forest2)#train

cmatrix_forest
mosaic(cmatrix_forest,shade = T,colorize=T,main="Matriz de confusion del Random Forest")

nrow(test)

#comparar los tres modelos finales
medicion(cmatrix_regresion2)
medicion(matrix_arbol2)
medicion(cmatrix_forest)

#forest es el mejor entonces se aplica para los actuales
#Random Forest
rf.pred_actual=predict(rf, actual)
actual$Cancelo=rf.pred_actual

#a. Porcentaje de clientes actuales que cancelarán
porcentaje_cancelados=sum(actual$Cancelo == 1) / nrow(actual) * 100
porcentaje_cancelados

cancel_plot=as.data.frame(table(actual$Cancelo))
cancel_plot$Cancelo=as.factor(cancel_plot$Var1)

ggplot(cancel_plot, aes(x = Cancelo, y = Freq, fill = Cancelo)) +
  geom_bar(stat = "identity") +
  labs(title = "Comparación de Clientes Cancelados vs No Cancelados",
       x = "Cancelo", y = "Cantidad de Clientes") +
  scale_fill_manual(values = c("darkblue", "orange")) +
  theme_minimal() +
  geom_text(aes(label = Freq), vjust = -0.5)



#b. Perfil de los clientes que cancelarán
#edad
actual$RangoEdad = cut(actual$Edad, breaks = seq(10, 90, by = 10), right = FALSE)
actual=actual[!is.na(actual$RangoEdad), ]

table(actual$RangoEdad,actual$Cancelo)

ggplot(data = actual, aes(x = RangoEdad, fill = Cancelo)) +
  geom_bar() +
  labs(title = "Cancelación por Rango de Edad",
       x = "Rango de Edad", y = "Cantidad") +
  scale_fill_manual(values = c("pink", "darkblue")) +
  theme_minimal()

484/1000
36/1000
83/183
#sexo
table(actual$Sexo,actual$Cancelo)
ggplot(actual, aes(x = Cancelo, fill = Sexo)) +
  geom_bar(position = "fill") +
  labs(title = "Distribución de Cancelo por Sexo") +
  scale_fill_manual(values = c("pink",'lightblue')) +
  scale_x_discrete(labels =c('No','Si'))+
  theme_minimal()

#pais
table(actual$Pais,actual$Cancelo)
ggplot(actual, aes(x = Cancelo, fill = Pais)) +
  geom_bar(position = "fill") +
  labs(title = "Distribución de Cancelo por País") +
  scale_fill_manual(values = c("darkblue", "yellow", "red")) +
  scale_x_discrete(labels = c("No", "Sí")) +
  theme_minimal()


#balance 
options(scipen=10)
ggplot(data = actual, aes(x = Balance, fill = Cancelo)) +
  geom_density(alpha = 0.5) +
  labs(title = "Distribución del Saldo por Cancelación",
       x = "Saldo", y = "Densidad") +
  scale_fill_manual(values = c("darkgreen", "red")) +
  theme_minimal()

#num productos
table(actual$NumProductos, actual$Cancelo)
ggplot(data = actual, aes(x = NumProductos, fill = Cancelo)) +
  geom_bar() +
  labs(title = "Cancelación por Número de Productos",
       x = "Número de Productos", y = "Clientes") +
  scale_fill_manual(values = c("darkgreen", "orange")) +
  theme_minimal()

#----*RECOMENDACIONES*----
#Se recomienda que se mejore las segmentacion, es decir que aquellos que tienen
#saldos más altos son los que tienen mas riesgo de irse por lo cual se debe encontrar
# algun producto o servcio personalizado que satisfaga sus nuevas necesidades financieras.

#Programas de Recompensas, Implementar descuentos exclusivos o programas de recompensas
#que motive a tener más productos en el banco porque eso baja el riesgo de que cancelen el servicio.

#ofrecer incentivos para las personas que estan en francia, con algun tipo de acumulacion
#de puntos canjeables en tiendas en francias. (Aunque se puede usar para cada pais)

#Asi como tambien algun tipo de cashback para las mujeres en fechas importantes
#ya que son las que tienden a irse. Algo que una mujer le gusta son promociones, 
# y un cashback seria un gran motivo de retencion para ese segmento.


# d. Cálculo de saldo perdido por los clientes que probablemente cancelarán
saldo_perdido=sum(actual$Balance[actual$Cancelo == 1])
saldo_perdido

sum(actual$Balance[actual$Cancelo == 0])
sum(actual$Balance)

sum(historica4$Balance[historica4$Cancelo==1])

