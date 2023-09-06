setwd("/Users/stephgrotewold/Desktop/UFM/VERANO 2023/DATA SCIENCE FOR FINANCE/casos/caso1")
library(dplyr)
library(ggplot2)
library(fitdistrplus)
library(MASS)
library(fBasics)
library(bestNormalize)
#library(readxl)
library(scales)

autos=read.csv('autos.csv')
head(autos)

hist(autos$propios)  # Histograma de los precios de daños propios
hist(autos$terceros)  # Histograma de los precios de terceros
hist(autos$diferencia)

descdist(autos$propios) #se acerca a la gamma
descdist(autos$terceros) #esta lejitos pero a la gamma
descdist(autos$diferencia)# lognormal


#Fittear los datos a la distribución que escogieron
fit_propios_gamma = fitdist(autos$propios,'gamma')
fit_diferencia_lnorm = fitdist(autos$diferencia,'lnorm')

#se comento la parte de box cox hasta abajo porque quedaba peor entonces mejor se dejo solo asi
fit_terceros_gamma=fitdist(autos$terceros, "gamma")
fit_terceros_gamma

fit_propios_gamma$aic
fit_diferencia_lnorm$aic
fit_terceros_gamma$aic

#Hacer la gráfica comparativa entre la distribución teórica y la empírica (la función cdfcomp)
cdfcomp(fit_propios_gamma)
denscomp(fit_propios_gamma)
cdfcomp(fit_diferencia_lnorm)
denscomp(fit_diferencia_lnorm)
cdfcomp(fit_terceros_gamma)
denscomp(fit_terceros_gamma)

#Simular 10,000 datos con la distribución que fittearon
sim_propios=rgamma(10000,shape = fit_propios_gamma$estimate["shape"] , 
                   rate = fit_propios_gamma$estimate["rate"])

sim_diferencia=rlnorm(10000, meanlog = fit_diferencia_lnorm$estimate["meanlog"],
                      sdlog = fit_diferencia_lnorm$estimate["sdlog"])

sim_terceros=rgamma(10000,shape = fit_terceros_gamma$estimate["shape"] , 
                       rate = fit_terceros_gamma$estimate["rate"])

# Calcular VAR al 95% de confianza
var_propios = quantile(sim_propios, 0.95)
var_diferencia = quantile(sim_diferencia, 0.95)
var_terceros = quantile(sim_terceros, 0.95)

var_propios
var_terceros

# Crear un data frame con los resultados de la matriz de precios simulados
matriz_df=data.frame(precios_simulados = c(sim_propios, sim_terceros, sim_diferencia))

# Generar la gráfica
ggplot(matriz_df, aes(x = precios_simulados)) +
  geom_histogram(fill = "lightblue", col = "black") +
  geom_vline(xintercept = c(quantile(matriz_df$precios_simulados, 0.99),
                            quantile(matriz_df$precios_simulados, 0.95),
                            quantile(matriz_df$precios_simulados, 0.05)),
             linetype = 2, color = c("red", "orange", "darkgreen"))+
  scale_x_continuous(label=comma)+scale_y_continuous(label=comma)



#--------------------------*PARTE II*--------------------------------
# Cargar el archivo ventas.csv
ventas = read.csv("ventas.csv")
#1) Generar la distribución de precios a los que la agencia venderá sus seguros
precio_promedio = 2200 
desviacion_estandar =350

#datos historicos
ventas_mensuales= ventas$ventas
hist(ventas_mensuales, col = "pink", border = "black")
max(ventas_mensuales)
min(ventas_mensuales)
mean(ventas_mensuales)

sim_precios_seguros=rnorm(10000, mean = precio_promedio, sd = desviacion_estandar)
# Ajustar la distribución de ventas
fit_ventas=fitdist(ventas_mensuales, "norm")
parametro_ventas=fit_ventas$estimate
#2) Generar las ventas mensuales en unidades
ventas_mensuales =sample(ventas$ventas, 10000, replace = TRUE)
ventas_mensuales
# Generar datos simulados de ventas mensuales
ventas_mensuales_simuladas=round(rnorm(10000, mean = parametro_ventas['mean'], sd = parametro_ventas['sd']))
# Calcular la media
mean(ventas_mensuales_simuladas)

# 3) Calcular los ingresos mensuales
ingresos_mensuales=sim_precios_seguros * ventas_mensuales
ingresos_mensuales
sum(sample(ingresos_mensuales,12))
# Calcular los escenarios de ingreso para 1 año
peor_escenario=quantile(ingresos_mensuales, 0.05)
escenario_esperado=mean(ingresos_mensuales)
mejor_escenario=quantile(ingresos_mensuales, 0.95)

# Imprimir los resultados de ingresos
#peor escenario:
peor_escenario
#el esperado
escenario_esperado
#mejor ecenario
mejor_escenario

# 4) Calcular la probabilidad de vender 65,60 y encontrar meta
probabilidad_65_autos= sum(ventas_mensuales_simuladas >= 65) / length(ventas_mensuales_simuladas)
probabilidad_65_autos

probabilidad_60_autos=sum(ventas_mensuales_simuladas >= 60) / length(ventas_mensuales_simuladas)
probabilidad_60_autos

probabilidad_39_autos=sum(ventas_mensuales_simuladas >= 39) / length(ventas_mensuales_simuladas)
probabilidad_39_autos
