## Cargamos librerias a ser usadas 

library(tidyverse)
library(MASS)
library(GGally)
library(fBasics)


# limpiamos ambiente en R

rm(list=ls())


## Colocamos la semilla

set.seed(3234)

## Leemos la base de datos completa

raw_df <- read.csv("scrap price.csv")



##Se inicia análisis exploratorio de datos##

## Para poder hacer un análisis en donde se entiendan los datos y podamos saber que variables podemos usar 
## vamos a dividir la información en un intervalo de 6 (incluyendo siempre el precio) para ver la relación
## con la fn de ggpairs 

## Como es un modelo de regresión lineal simple, usamos una variable que sea numérica en vez de una variable
## caracter, ya que [Poner explicación]

#### 1.- hacemos la limpieza de datos necesaria para que sólo queden variables numéricas ####

numeric_df <- raw_df %>% 
  dplyr::select(wheelbase, carlength, carwidth, carheight, curbweight, enginesize, boreratio, stroke, compressionratio,
                horsepower, peakrpm, citympg, highwaympg,carbody, price)

#### 2.- Dividimos la info en intervalos ####

first_set <- numeric_df %>% 
  ggpairs(columns = c(1:4, 15), aes(color = carbody, alpha = .5)) + 
  ggtitle("First set")

second_set <- numeric_df %>% 
  ggpairs(columns = c(5:9, 15), aes(color = carbody, alpha = .5)) +
  ggtitle("Second set")

third_set <- numeric_df %>% 
  ggpairs(columns = c(10:13, 15), aes(color = carbody, alpha = .5)) +
  ggtitle("Third set")

## Vamos a utilizar la variable X = enginesize ya que lógicamente se puede entender que haya una relación
## positiva respecto al tamaño del motor y el precio
## Aunado a esto pudimos observar que tienen una correlación de .874, la más alta de los intervalos analizados

## 3.- Dividimos nuestra base de datos en la que se va a entrenar y en la que se va a probar el modelo

train.base <- raw_df %>% 
  sample_frac(.70)

val.base <- raw_df %>% 
  setdiff(train.base)

## 4.- Hacemos el modelo de regresión lineal con la fn lm(), aplicada a la base de entrenamiento

modelo_1 <- lm(price~enginesize, data = train.base) 


# La fn lm nos da B0 y B1 de nuestra muestra 


modelo_1

# Checamos los parámetros de nuestro modelo para obtener el T value y así saber si nuestras Bs 

(modelo_1)

summary(modelo_1)

## Podemos observar que para B0 y B1 el P value < |t value | ∴ B0 y B1 son significativas con un 
## nivel de confianza de 1

## Nuestra R^2 es .7788 y ajustada es .7772 ∴ mi modelo explica el .7788 de Y 

anova(modelo_1)



## 4.- Iniciamos con el análisis de residuales 

# 1) la fn de regresión es lineal?
# eso lo podemos obtener con R^2, en este caso los errores se acercan un 77% a nuestra recta de regresión
# lo que nos dice que sí hay linealidad en ella, lo comprobamos sacando la R^2 

suma <- summary(modelo_1)
suma$r.squared

## nos da el .7787968 de R^2

# 2) Comprobamos heterocedisticidad (la varianza de los errores es constante)

# Eso se comprueba con un gráfico comparando los residuales con las Y observadas (ŷ)
# para esto tenemos que hacer un DF con ambos vectores obtenidos de nuestro modelo

prueba_heter <- as.data.frame(cbind(modelo_1$fitted.values, modelo_1$residuals))

# cambiamos el nombre de nuestras columnas para una mayor facilidad de lectura 

colnames(prueba_heter) <- c("Y_observada", "Residuales")

# Graficamos para observar si hay un patrón o no respecto ambas variables

grafico_heter <- prueba_heter %>% 
  ggplot() + 
  geom_point(aes(Y_observada, Residuales)) + 
  theme_classic()
  

plotly::ggplotly(grafico_heter) # (Sólo se hace para hacer el gráfico interactivo)

## Se puede observar que no hay un patrón en sí en el gráfico, como una recta, con esto podemos asumir 
## Que hay heterocedisticidad

# 3) Independencia en los errores 

# No es una serie de tiempo - no aplica

# 4) Presencia de errores atípicos  

## Esto se hace calculando la raíz de el cuadrado medio de la suma de cuadrados de los errores (MSE)
## El cual se obtiene de la tabala ANOVA de nuestros residuales el mean 

anova_1 <- anova(modelo_1)
sqrt_mse <- sqrt(anova_1[2,3])

## Ya con MSE^(1/2), podemos sacar la división de los residuales entre la raíz de MSE y compararlos con xi
## Esas variables las metemos en un DF 

df_ea <- as.data.frame(cbind(train.base$enginesize, modelo_1$residuals/sqrt_mse))

colnames(df_ea) <- c("X", "Errores")

# Graficamos esas diferencias

grafico_ea <- df_ea %>% 
  ggplot() + 
  geom_line(aes(x = X, y = -4),col = "Red", alpha=.5) +
  geom_line(aes(x = X, y = 4), col = "Red", alpha=.5) +
  geom_point(aes(x = X, y = Errores)) + 
  ggtitle("x vs ui / raiz(mse)") +
  ylab("ui / raiz(mse)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

plotly::ggplotly(grafico_ea) # lo hacemos interactivo para ver que datos están cerca de ser aberrantes 
# el error más alto es 3.58, no hay niguno lejos de 4 ∴ no tenemos que hacer cap y floor para los datos 

# 5) Verificar la normalidad en los errores 

# Prueba 1 - la QQ plot - esa se hace con los residuales [ checar teoría de inferencia]
# Sacamos los residuales de nustro modelo

ui <- as.data.frame(modelo_1$residuals)
colnames(ui) <- c("Ui")

ui %>% 
  ggplot(aes(sample = Ui)) + 
  stat_qq() +
  stat_qq_line() + 
  ggtitle("QQ Plot Residuales") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Rechazamos el supuesto de normalidad de los errores debido a las dos colas que muestra el gráfico de QQ plot
# no obstante, ya que el modelo de regresión lineal simple ajustado es robusto ante el supuesto de normalidad
# podemos continuar usando esta variable

##### [ hacer Jarquebera y ks norm] #####


# Sacamos el intervalo de Confianza de E[Y], esto lo hacemos para ver el intervalo en donde nuestras 
# rectas de E[Y / X] estan, eso lo vamos a usar para comprobar en el 30% de los datos las siguientes 
# En R usamos la fn Predict.lm la alimentamos con el modelo_1 con nuestra data de entrenamiento
# aplicando el intervalo de confianza a el nivel requerido, en este caso .95 [Explicar porque ]

confianza <- as.data.frame(predict.lm(modelo_1, train.base, interval = "confidence", level = .95))

colnames(confianza) <- c("Y_Observada", "lwr", "upr")

# también hacemos la predicción para poder obtener ambos intervalos, los de confianza y los de predicción

prediccion <- as.data.frame(predict.lm(modelo_1, train.base, interval = "prediction", level = .95))

colnames(prediccion) <- c("Y_Observada_predecida", "lwr_pred", "upr_pred")

# Ya nos da un Df con la info de nuestra
# confianza y la predicción 

train.base.confianza.prediccion <- as.data.frame(c(train.base, confianza, prediccion))


grafico_confianza_prediccion <- train.base.confianza.prediccion %>% 
  ggplot() + 
  geom_point(aes(x = enginesize, y = price)) + 
  # Agregamos los intervalos de confianza y de predicción 
  geom_line(aes(x = enginesize, y = lwr), color = "red", linetype = "dashed") + 
  geom_line(aes(x = enginesize, y = upr), color = "red", linetype = "dashed") +
  geom_line(aes(x = enginesize, y = lwr_pred), color = "purple", linetype = "dashed") +
  geom_line(aes(x = enginesize, y = upr_pred), color = "purple", linetype = "dashed") +
  # Agregamos las rectas de E[Y / X]
  geom_line(aes(x = enginesize, y = Y_Observada), color = "blue") +
  #Agregamos la recta de Y predecida
  geom_line(aes(x = enginesize, y = Y_Observada_predecida), color = "black", linetype = "dashed") +
  ggtitle("Intervalo de predicción y confianza al 95%")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))
  
  
  
grafico_confianza_prediccion <- plotly::ggplotly(grafico_confianza_prediccion)

grafico_confianza_prediccion


#-------------------- validación ---------------------#
## Aquí termina nuestro entrenamiento del modelo ∴ vamos a usar nuestras betas entrenadas en la población 
## del 30%, y las diferentes muestras deben entrar en nuestros intervalos o sea validar nuestro modelo 


confianza.val <- as.data.frame(predict.lm(modelo_1, val.base, interval = "confidence", level = .95))

colnames(confianza.val) <- c("y_gorro", "lwr", "upr") 

prediccion.val <- as.data.frame(predict.lm(modelo_1, val.base, interval = "prediction", level = .95))

colnames(prediccion.val) <- c("y_gorro_pred", "lwr_pred", "upr_pred")


validacion.base.confianza.prediccion <- as.data.frame(c(val.base, confianza.val, prediccion.val))


grafico_confianza_prediccion_validacion <- validacion.base.confianza.prediccion %>% 
  ggplot() + 
  geom_point(aes(x = enginesize, y = price)) + 
  # Agregamos los intervalos de confianza y de predicción 
  geom_line(aes(x = enginesize, y = lwr), color = "red", linetype = "dashed") + 
  geom_line(aes(x = enginesize, y = upr), color = "red", linetype = "dashed") +
  geom_line(aes(x = enginesize, y = lwr_pred), color = "purple", linetype = "dashed") +
  geom_line(aes(x = enginesize, y = upr_pred), color = "purple", linetype = "dashed") +
  # Agregamos las rectas de E[Y / X]
  geom_line(aes(x = enginesize, y = y_gorro), color = "blue") +
  #Agregamos la recta de Y predecida
  geom_line(aes(x = enginesize, y = y_gorro_pred), color = "black", linetype = "dashed") +
  ggtitle("Intervalo de predicción y confianza al 95%")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

grafico_confianza_prediccion_validacion <- plotly::ggplotly(grafico_confianza_prediccion_validacion)

grafico_confianza_prediccion_validacion

