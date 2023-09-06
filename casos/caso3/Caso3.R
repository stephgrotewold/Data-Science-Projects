setwd("/Users/stephgrotewold/Desktop/UFM/VERANO 2023/DATA SCIENCE FOR FINANCE/casos/caso3")
library(nnet)
library(pROC)
library(dplyr)
library(ggplot2)
library(fastDummies)


bank1=read.csv("bank1.csv")
head(bank1)

#cambiar el nombre de la variable para que sean iguales
bank1 = bank1 %>%
  rename(Calificacion_riesgo = Credit_Score)

#Exploración de la estructura y características de los datos
str(bank1)
summary(bank1) #tambien sirve para ver un resumen estaditico de algunas variables num

#Exploración de variables categóricas
table(bank1$Pago_minimo)
table(bank1$Tipo_pago)

#preparar variable a clasificar(respuesta)
bank1$Ocupacion=as.factor(bank1$Ocupacion)
multiclass.roc(bank1$Ocupacion, bank1$Calificacion_riesgo)
#explorar cuantos clientes hay por ocupacion
ocup = bank1 %>% 
  group_by(Ocupacion) %>% 
  summarise(clientes=n())
ocup
ggplot(ocup, aes(x=reorder(Ocupacion,-clientes),y=clientes))+
  geom_bar(stat = 'identity', aes(fill=Ocupacion))+
  geom_text(aes(label = clientes), vjust = -0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Ocupación", y = "Cantidad de clientes", title = "Distribución de Ocupación")

#preparar variable a clasificar(respuesta)
bank1$Pago_minimo=as.factor(bank1$Pago_minimo)
multiclass.roc(bank1$Pago_minimo, bank1$Calificacion_riesgo)
#explorar cuantos clientes hay por pago minimo
pag_min = bank1 %>% 
  group_by(Pago_minimo) %>% 
  summarise(clientes=n())
pag_min
ggplot(pag_min, aes(x = reorder(Pago_minimo, -clientes), y = clientes)) +
  geom_bar(stat = "identity", aes(fill = Pago_minimo)) +
  geom_text(aes(label = clientes), vjust = -0.5) +
  labs(x = "Pago mínimo", y = "Cantidad de clientes", title = "Distribución de Pago mínimo")


#preparar variable a clasificar(respuesta)
head(bank1$Tipo_pago)
bank1$Tipo_pago=as.factor(bank1$Tipo_pago)
multiclass.roc(bank1$Tipo_pago, bank1$Calificacion_riesgo)
str(bank1)
#explorar cuantos clientes hay por tipo de pago 
tip_pago = bank1 %>% 
  group_by(Tipo_pago) %>% 
  summarise(clientes=n())
tip_pago
ggplot(tip_pago, aes(x = reorder(Tipo_pago, -clientes), y = clientes)) +
  geom_bar(stat = "identity", aes(fill = Tipo_pago)) +
  geom_text(aes(label = clientes), vjust = -0.5) +
  scale_x_discrete(labels = c("Pocos pagos pequeños", "Pocos pagos medianos", "Pocos pagos grandes",
                              "Muchos pagos pequeños", "Muchos pagos medianos", "Muchos pagos grandes")) +
  labs(x = "Tipo de pago", y = "Cantidad de clientes", title = "Distribución del Tipo de pago")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



hist(bank1$Edad)

options(scipen = 10)
#Boxplot
boxplot(bank1$Ingreso_anual, main = "Boxplot de Ingresos Anuales", ylab = "Ingreso Anual")
summary(bank1$Ingreso_anual)

hist(bank1$Ingreso_mensual)

#Gráfico de barras
ggplot(data = bank1, aes(x = Cuentas_bancarias)) +
  geom_bar(fill = "pink", color = "black") +
  labs(title = "Cantidad de Cuentas Bancarias", x = "Cuentas Bancarias", y = "Frecuencia")

#quitar outliers
box=boxplot(bank1$Cuentas_bancarias)
box$out
hist(bank1$Cuentas_bancarias, col='green', main = 'Cuentas Bancarias con outliers')
outliers=box$out
bank1_v2=subset(bank1,!(Cuentas_bancarias%in%outliers))
nrow(bank1)
nrow(bank1_v2)
hist(bank1_v2$Cuentas_bancarias, col='blue',main = 'Cuentas Bancarias sin outliers')

hist(bank1$Tasa_Interes)
hist(bank1$Meses_atraso)
hist(bank1$Cambio_limite)

#Gráfico de barras
ggplot(data = bank1, aes(x = Consulta_credito)) +
  geom_bar(fill = "darkblue", color = "black") +
  labs(title = "Cantidad de Consulta credito", x = "Consulta_credito", y = "Frecuencia")
#quitar outliers
box=boxplot(bank1_v2$Consulta_credito)
box$out
hist(bank1_v2$Consulta_credito, col='lightgreen', main = 'Consulta_credito con outliers')
outliers=box$out
bank1_v3=subset(bank1_v2,!(Consulta_credito%in%outliers))
nrow(bank1)
nrow(bank1_v3)
hist(bank1_v3$Consulta_credito, col='lightblue',main = 'Consulta_credito sin outliers')


hist(bank1$Inversion_mensual)
#quitar outliers
box=boxplot(bank1_v3$Inversion_mensual)
box$out
hist(bank1_v3$Inversion_mensual, col='darkgreen', main = 'Inversion_mensual con outliers')
outliers=box$out
bank1_v4=subset(bank1_v3,!(Inversion_mensual%in%outliers))
nrow(bank1)
nrow(bank1_v4)
hist(bank1_v4$Inversion_mensual, col='darkblue',main = 'Inversion_mensual sin outliers')


hist(bank1$Deuda)
hist(bank1$Utilizacion_credito)

hist(bank1$Balance_mensual)
#Gráfico de barras
ggplot(data = bank1, aes(x = Historial_crediticio)) +
  geom_histogram(fill = "orange", color = "black") +
  labs(title = "Cantidad de Historial_crediticio ", x = "Historial_crediticio", y = "Frecuencia")


#convertir en dummy la variable categorias
bankito= dummy_cols(bank1_v4,select_columns = c('Ocupacion','Tipo_pago','Pago_minimo'))
#Convertir la variable de respuesta en un factor
bankito$Calificacion_riesgo = as.factor(bankito$Calificacion_riesgo)

#dividir set de datos en training y test
index= sort(sample(nrow(bankito),nrow(bankito)*0.7))
train=bankito[index,]
test=bankito[-index,]

str(bankito)
#Modelo 1: Todas las variables predictoras
model1 = multinom(Calificacion_riesgo ~ ., data = train)
summary(model1)

model2 = multinom(Calificacion_riesgo ~ Edad + Cuentas_bancarias + Tasa_Interes 
                   + Meses_atraso + Cambio_limite + Consulta_credito + Deuda +
                     Utilizacion_credito + Balance_mensual + Historial_crediticio 
                   + Ocupacion_Doctor + Ocupacion_Teacher +Ocupacion_Lawyer+ Ocupacion_Architect+Tipo_pago_1
                   + Tipo_pago_2, data = train)
summary(model2)

model3 = multinom(Calificacion_riesgo ~ Edad  + Ingreso_anual + Ingreso_mensual + Cuentas_bancarias +
                              Tasa_Interes + Meses_atraso + Cambio_limite + Consulta_credito + Deuda +
                              Utilizacion_credito + Pago_minimo_1 + Inversion_mensual + Balance_mensual +
                              Historial_crediticio + Ocupacion_Accountant + Ocupacion_Writer+
                              Ocupacion_Architect + Ocupacion_Developer + Ocupacion_Doctor +
                              Ocupacion_Engineer + Ocupacion_Entrepreneur + Ocupacion_Journalist +
                              Ocupacion_Lawyer + Ocupacion_Manager + Ocupacion_Mechanic +
                              Ocupacion_Media_Manager + Ocupacion_Musician + Ocupacion_Scientist +
                              Ocupacion_Teacher + Tipo_pago_1 + Tipo_pago_2 + Tipo_pago_4 +
                              Tipo_pago_5 + Tipo_pago_6, data = train)

summary(model3)

model4 = multinom(Calificacion_riesgo ~ Edad   +Utilizacion_credito+ Ingreso_mensual + Cuentas_bancarias +
                    Tasa_Interes + Meses_atraso  + Deuda +Pago_minimo_1 + Inversion_mensual + Balance_mensual +
                    Historial_crediticio + Ocupacion_Accountant + Ocupacion_Writer+
                    Ocupacion_Architect + Ocupacion_Developer + Ocupacion_Doctor +
                    Ocupacion_Engineer + Ocupacion_Entrepreneur + Ocupacion_Journalist +
                    Ocupacion_Lawyer + Ocupacion_Manager + Ocupacion_Mechanic +
                    Ocupacion_Media_Manager + Ocupacion_Musician + Ocupacion_Scientist +
                    Ocupacion_Teacher + Tipo_pago_1 + Tipo_pago_2 + Tipo_pago_4 +
                    Tipo_pago_5 + Tipo_pago_6, data = train)
summary(model4)
model5 = multinom(Calificacion_riesgo ~ Edad   +Utilizacion_credito+ Ingreso_mensual + Cuentas_bancarias +
                    Tasa_Interes + Meses_atraso  + Deuda  + Inversion_mensual + Balance_mensual +
                    Historial_crediticio + Ocupacion_Accountant + Ocupacion_Writer+
                    Ocupacion_Architect + Ocupacion_Developer + Ocupacion_Doctor +
                    Ocupacion_Engineer + Ocupacion_Entrepreneur + Ocupacion_Journalist +
                    Ocupacion_Lawyer + Ocupacion_Manager + Ocupacion_Mechanic +
                    Ocupacion_Media_Manager + Ocupacion_Musician + Ocupacion_Scientist +
                    Ocupacion_Teacher + Tipo_pago_1 + Tipo_pago_2 + Tipo_pago_4 +
                    Tipo_pago_5 + Tipo_pago_6+ Pago_minimo_0, data = train)
summary(model5)
model6 = multinom(Calificacion_riesgo ~ Edad   +Utilizacion_credito+ Ingreso_mensual + Cuentas_bancarias +
                    Tasa_Interes + Meses_atraso  + Deuda  + Inversion_mensual + Balance_mensual +
                    Historial_crediticio + Ocupacion_Accountant + Ocupacion_Writer+
                    Ocupacion_Architect + Ocupacion_Developer + Ocupacion_Doctor +
                    Ocupacion_Lawyer + Ocupacion_Manager + Ocupacion_Mechanic +
                    Ocupacion_Media_Manager + Ocupacion_Musician + Ocupacion_Scientist + Tipo_pago_2 + Pago_minimo_1, data = train)
summary(model6)
model7 = multinom(Calificacion_riesgo ~ Edad  + Ingreso_anual + Ingreso_mensual + Cuentas_bancarias +
                         Tasa_Interes + Meses_atraso + Cambio_limite + Consulta_credito + Deuda +
                         Utilizacion_credito + Pago_minimo_1 + Inversion_mensual + Balance_mensual +
                         Historial_crediticio + Ocupacion_Accountant + Ocupacion_Writer+
                         Ocupacion_Architect + Ocupacion_Developer + Ocupacion_Doctor +
                         Ocupacion_Engineer + Ocupacion_Entrepreneur + Ocupacion_Journalist +
                         Ocupacion_Lawyer + Ocupacion_Manager + Ocupacion_Mechanic +
                         Ocupacion_Media_Manager + Ocupacion_Musician + Ocupacion_Scientist +
                         Ocupacion_Teacher, data = train)
summary(model7)

model8 = multinom(Calificacion_riesgo ~ Edad + Ocupacion + Ingreso_anual + Ingreso_mensual + Cuentas_bancarias +
                         Tasa_Interes + Meses_atraso + Cambio_limite + Consulta_credito + Deuda +
                         Utilizacion_credito + Pago_minimo_1 + Inversion_mensual + Balance_mensual +
                         Historial_crediticio, data = train)
summary(model8)

model_final = multinom(Calificacion_riesgo ~ Edad  + Ingreso_anual + Ingreso_mensual + Cuentas_bancarias +
                         Tasa_Interes + Meses_atraso + Cambio_limite + Consulta_credito + Deuda +
                         Utilizacion_credito + Pago_minimo_1 + Inversion_mensual + Balance_mensual +
                         Historial_crediticio + Ocupacion + Tipo_pago_1 + Tipo_pago_2 + Tipo_pago_4 +
                         Tipo_pago_5 + Tipo_pago_6, data = train)
summary(model_final)

#fijo no
model2$AIC
model4$AIC
model5$AIC
model6$AIC

#pueden ser
model1$AIC
model7$AIC
model8$AIC
model_final$AIC
model3$AIC

head(fitted(model_final))
#predicted values como probabilidad de ocurrencia de las categorias
train$predictedp = predict(model_final, newdata = train, 'probs')
#predited values como clasificacion(0y 1)
train$predictedc =predict(model_final, newdata = train, 'class')

train$original = train$Calificacion_riesgo
train$nuevo = train$predictedc

comparacion = table(original = train$original, nuevo = train$nuevo)
comparacion

accuracy =sum(diag(comparacion))/sum(comparacion)*100
accuracy

test$predicted = predict(model_final, newdata = test, 'class')
comparaciontest= table(original = test$Calificacion_riesgo, preddiccion = test$predicted)
comparaciontest

accuracy_test = sum(diag(comparaciontest))/sum(comparaciontest)*100
accuracy_test


#Cargar el conjunto de datos bank2
bank2 = read.csv("bank2.csv")
#convertir en dummy la variable categorias
bankito2= dummy_cols(bank2,select_columns = c('Ocupacion','Tipo_pago','Pago_minimo'))
str(bankito2)
#Realizar la predicción utilizando tu mejor modelo
pred = predict(model_final, newdata = bankito2, type = "class")
#Crear un nuevo dataframe con el ID de los clientes y las predicciones
predictions = data.frame(ID = bankito2$ID, Calificacion_riesgo_pred = pred)
str(predictions)
#Guardar el dataframe en un archivo CSV
write.csv(predictions, "bank3.csv", row.names = FALSE)










