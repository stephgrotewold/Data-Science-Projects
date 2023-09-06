setwd("/Users/stephgrotewold/Desktop/UFM/VERANO 2023/DATA SCIENCE FOR FINANCE/casos/caso2")
library(dplyr)
library(ggplot2)
library(forecast)
library(tseries)
library(quantmod)
library(PortfolioAnalytics)

#Obtener los datos históricos de la acción MBG.DE
MAMG = get.hist.quote("MBG.DE", as.Date('2012-01-01'), as.Date('2022-05-31'),
                      quote = 'AdjClose', compression = 'm')
MAMG
plot(MAMG)

#Convertir los datos en una serie de tiempo
MAMG = ts(MAMG, start = c(2012, 1), frequency = 12)

#Realizar descomposición de la serie de tiempo
plot(decompose(MAMG[, 1]))

#Ajustar un modelo ARIMA a los datos
modelo = auto.arima(MAMG[, 1], trace = TRUE)
modelo$aic

#Realizar pronóstico con el modelo ARIMA
pronostico = forecast(modelo, 12, level = 95)
plot(pronostico)
pronostico

#Crear una matriz con los resultados del pronóstico
matriz.pronostico = data.frame(pronostico$mean, pronostico$lower, pronostico$upper)
matriz.pronostico

#Ajustar un modelo ARIMA con tsclean
modelo2 = auto.arima(tsclean(MAMG[, 1]), trace = TRUE)
modelo2$aic

#Realizar pronóstico con el modelo ARIMA de los datos limpios
pronostico2 = forecast(modelo2, 12, level = 95)
plot(pronostico2)

#Crear una matriz con los resultados del pronóstico de los datos limpios
matriz.pronostico2 = data.frame(pronostico2$mean, pronostico2$lower, pronostico2$upper)

matriz.pronostico2

#Obtener los datos históricos del índice S&P 500 (^GSPC)
SYP = get.hist.quote("^GSPC", as.Date('2012-01-01'), as.Date('2022-05-31'),
                     quote = 'AdjClose', compression = 'm')
plot(SYP)

#Crear un DataFrame con los datos
precios = data.frame(MAMG, SYP)

#Crear un modelo de regresión
model = lm(MAMG ~ SYP, data = precios)
#model = lm(MAMG ~ SYP)
summary(model)

# Split the data into training and test sets
train_rows=round(0.7 * nrow(precios))
training_index=sample(1:nrow(precios), train_rows)
training_data=precios[training_index, ]
testdata=precios[-training_index, ]

# Perform predictions using the regression model
predictions=predict(model, newdata = testdata)
predictions

#Obtener los datos históricos del índice S&P 500 para el período de pronóstico
SYP2 = get.hist.quote("^GSPC", as.Date('2022-06-01'), as.Date('2023-05-31'),
                      quote = 'AdjClose', compression = 'm')
SYP2

#Crear nuevos datos para el período de pronóstico
newdata = data.frame(Date = seq(as.Date("2022-06-01"), as.Date("2023-05-31"), by = "month"))
newdata

#Realizar predicciones utilizando el modelo de regresión para el período de pronóstico
predictions = predict(model, newdata=newdata)
predictions

#Obtener los datos históricos de la acción MBG.DE para el período de pronóstico
MAMG2 = get.hist.quote("MBG.DE", as.Date("2022-06-01"), as.Date("2023-05-31"),
                       quote = "AdjClose", compression = "m")
MAMG2

#Crear un DataFrame con los resultados reales y las predicciones del modelo ARIMA
resultados_arima = data.frame(Real = MAMG2, Prediccion = pronostico$mean)
resultados_arima

#Crear un DataFrame con los resultados reales y las predicciones del modelo ARIMA de los datos limpios
resultados_arima_tsclean = data.frame(Real = MAMG2, Prediccion = pronostico2$mean)
resultados_arima_tsclean

#Seleccionar las ultimas 12 predicciones del modelo de regresión
predictions = predictions[114:125]

#Crear un DataFrame con los resultados reales y las predicciones del modelo de regresión
resultados_regresion = data.frame(Real = MAMG2, Prediccion = predictions)
resultados_regresion

#Crear un DataFrame con los resultados para visualización
resultados = data.frame(
  Fecha = index(MAMG2),
  ARIMA = resultados_arima$Prediccion,
  ARIMA_tsclean = resultados_arima_tsclean$Prediccion,
  Regresion = resultados_regresion$Prediccion
)

#grfica para que se vea bonito
ggplot(resultados, aes(x = Fecha)) +
  geom_line(aes(y = coredata(MAMG2), color = "Real"), linewidth = 1) +
  geom_line(aes(y = ARIMA, color = "ARIMA"), linewidth = 1.5) +
  geom_line(aes(y = ARIMA_tsclean, color = "ARIMA_tsclean"), linewidth = 1) +
  geom_line(aes(y = Regresion, color = "Regresion"), linewidth = 1) +
  labs(x = "Fecha", y = "Precio", color = "Modelo") +
  scale_color_manual(values = c("Real" = "black", "ARIMA" = "blue", "ARIMA_tsclean" = "green", "Regresion" = "red")) +
  theme_minimal()
