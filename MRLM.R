# Cargamos las librerías del modelo simple

library(tidyverse)
library(MASS)
library(GGally)
library(fBasics)

# Agregamos las librerías necesarias del modelo múltiple

library(caTools)
library(car)

rm(list=ls())

set.seed(3234)

raw_df <- read.csv("scrap price.csv")

# 1. Limpieza de la base de datos ####

# Por el momento usaremos la base con variables independientes numéricas

numeric_df <- raw_df %>% 
  dplyr::select(wheelbase, carlength, carwidth, carheight, curbweight, enginesize, boreratio, stroke, compressionratio,
                horsepower, peakrpm, citympg, highwaympg,carbody, price)


# Dividimos nuestra base de datos en la que se va a entrenar y en la que se va a probar el modelo

train.base_m <- numeric_df %>% 
  sample_frac(.70)

val.base_m <- numeric_df %>% 
  setdiff(train.base_m)

# 2. MRLM #### 

# Ya tenemos el análisis de las variables independientes de nuestro modelo así que en este punto vamos a ir
# Eligiendo cuales son las que pueden ir en el modelo sín que tengan un VIF mayor a 10 

modelo_todo <- lm(price~ ., data = train.base_m)

summary(modelo_todo)

# 2.1 Limpieza de variables con correlación alta (VIF) ####

# Ya tenemos el análisis de las variables independientes de nuestro modelo así que en este punto vamos a ir
# Eligiendo cuales son las que pueden ir en el modelo sín que tengan un VIF mayor a 10 
# Generalized Variation Inflation Factor (GVIF)


vif(modelo_todo)

# Vamos a ir corriendo la Fn VIF cada vez que quitemos una variable ya que van cambiando conforme vas quitando 
# Variables independientes e iremos llamando a los modelos como modelo 1, 2, 3... hasta que no haya variables con 
# Correlaciones altas 

modelo.1 <- lm(price~ . -citympg, data = train.base_m)

vif(modelo.1)

# Eliminamos Curb weight ahora

modelo.2 <- lm(price~ . -citympg -curbweight, data = train.base_m)

vif(modelo.2)

# Eliminamos Carlenght

modelo.3 <- lm(price~ . -citympg -curbweight -carlength, data = train.base_m)

vif(modelo.3)

# Eliminamos HorsePower ya que aunque tiene un VIF menor a 10, pero tiene una correlación con enginesize de .8 lo 
# Cual es alta y preferimos la variable de enginesize

modelo.4 <- lm(price~ . -citympg -curbweight -carlength -horsepower, data = train.base_m)

vif(modelo.4)

# Ya con las variables de VIF alto fuera de nuestro modelo, podemos empezar a limpiarlas con la prueba global de 
# la regresión, la F* en donde si el pvalue > .05, No rechazamos H0 y eso hace que la B no sea significativa para
# nuestro modelo y podamos eliminarla.

summary(modelo.4)

# Podemos observar que la variable Boreratio tiene un Pvalue > .05 lo que hace que aceptemos la hipótesis de que 
# Bboreratio no es significativa para el modelo ya que = 0
# No podemos quitar todas las variables con el summary del modelo 4 porque los pvalues van modificandose cada que 
# Alteramos las variables

modelo.5 <- lm(price~ . -citympg -curbweight -carlength -horsepower -boreratio, data = train.base_m)

summary(modelo.5)

# Eliminamos highwaympg ya que es .064

modelo.6 <- lm(price~ . -citympg -curbweight -carlength -horsepower -boreratio -highwaympg, data = train.base_m)

summary(modelo.6)

# Eliminamos carheight ya que es .21

modelo.7 <- lm(price~ . -citympg -curbweight -carlength -horsepower -boreratio -highwaympg -carheight, data = train.base_m)

summary(modelo.7)

# Eliminamos wheelbase ya que es 0.055316 

modelo.7 <- lm(price~ . -citympg -curbweight -carlength -horsepower -boreratio -highwaympg -carheight -wheelbase, 
               data = train.base_m)

summary(modelo.7)

# Eliminamos carbody porque tiene 0.087241

modelo.8 <- lm(price~ . -citympg -curbweight -carlength -horsepower -boreratio -highwaympg -carheight -wheelbase
               -carbody, data = train.base_m)

summary(modelo.8)

# Ya todos los pvalues que quedan son menores a .05
# Checamos un VIF final para confirmar que ya no hay correlación en nuestras variables independientes

vif(modelo.8)

# Todos son menores a 10 y están dentro de nuestro pvalue para rechazar H0, estas son las variables que vamos a usar
# lo siguiente que vamos a hacer es checar que se cumplan los supuestos de los residuales

# 3. Análisis de residuales ####

# 3.1 Comprobar si la regresión es lineal respecto los parámetros ####

# Eso lo hacemos con la R^2 ajustada

suma_m <- summary(modelo.8)
suma_m$r.squared

#Tenemos una R^2 de .8352, lo cual nos dice que es una gran bondad de ajuste de nuestro modelo respecto Y

# 3.2 Comprobamos heterocedisticidad (la varianza de los errores es constante) ####

prueba_heter_m <- as.data.frame(cbind(modelo.8$fitted.values, modelo.8$residuals))

colnames(prueba_heter_m) <- c("Y_estimada", "Residuales")

grafico_heter_m <- prueba_heter_m %>% 
  ggplot() + 
  geom_point(aes(Y_estimada, Residuales)) + 
  theme_classic()

plotly::ggplotly(grafico_heter_m)

# No se aprecia un patrón por lo que podemos asumir que no hay Heterocedasticidad

# 3.3 Independencia en los errores ####

# No es una serie de tiempo - no aplica 

# 3.4 Presencia de errores atípicos ####

# Esto se hace calculando la raíz de el cuadrado medio de la suma de cuadrados de los errores (MSE)
# el cual se obtiene de la tabala ANOVA de nuestros residuales el mean 

anova_m <- anova(modelo.8)
sqrt_mse_m <- sqrt(anova_m[6,3])

# Ya con MSE^(1/2), podemos sacar la división de los residuales entre la raíz de MSE y compararlos con xi
# Esas variables las metemos en un DF 

df_ea_m <- as.data.frame(cbind(train.base_m$carwidth, train.base_m$enginesize, train.base_m$stroke,
                                 train.base_m$compressionratio, train.base_m$peakrpm, modelo.8$residuals/sqrt_mse_m))

colnames(df_ea_m) <- c("X1", "X2", "X3", "X4", "X5", "Errores")

# Graficamos esas diferencias


grafico_ea_m <- df_ea_m %>% 
  ggplot() + 
  geom_hline(yintercept = -4, colour = "red") + 
  geom_hline(yintercept = 4, colour = "red")  +
  geom_point(aes(x = X1, y = Errores, color = "Carwidth")) + 
  geom_point(aes(x = X2, y = Errores, color = "Enginesize")) +
  geom_point(aes(x = X3, y = Errores, color = "Stroke")) +
  geom_point(aes(x = X4, y = Errores, color = "Compressionratio")) +
  geom_point(aes(x = log(X5), y = Errores, color = "Peakrpm")) + # le hice una transformación lineal a los datos para poder comparar mejor
  ggtitle("x vs ui / raiz(mse)") +
  ylab("ui / raiz(mse)") +
  labs(title = "Datos atípicos") + 
  scale_color_manual(values = c("Carwidth" = "blue", "Enginesize" = "green", "Stroke" = "black", 
                                "Compressionratio" = "red", "Peakrpm" = "purple")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

grafico_ea_m

plotly::ggplotly(grafico_ea_m) 

# Sí hay datos atípicos para las 5 variables, en este caso podemos eliminarlos ya que no es 
# una serie de tiempo, por lo que vamos a eliminar de nuestro DF el renglón 28 ya que es el que tiene 
# los datos atípicos arriba del 4

df_sin_ea_m <- df_ea_m %>% 
  slice(-28)

# lo graficamos para confirmar que ya no hay datos atípicos

grafico_sin_ea_m <- df_sin_ea_m %>% 
  ggplot() + 
  geom_hline(yintercept = -4, colour = "red") + 
  geom_hline(yintercept = 4, colour = "red")  +
  geom_point(aes(x = X1, y = Errores, color = "Carwidth")) + 
  geom_point(aes(x = X2, y = Errores, color = "Enginesize")) +
  geom_point(aes(x = X3, y = Errores, color = "Stroke")) +
  geom_point(aes(x = X4, y = Errores, color = "Compressionratio")) +
  geom_point(aes(x = log(X5), y = Errores, color = "Peakrpm")) + 
  ggtitle("x vs ui / raiz(mse)") +
  ylab("ui / raiz(mse)") +
  labs(title = "Sin datos atípicos") + 
  scale_color_manual(values = c("Carwidth" = "blue", "Enginesize" = "green", "Stroke" = "black", 
                                "Compressionratio" = "red", "Peakrpm" = "purple")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

grafico_sin_ea_m

plotly::ggplotly(grafico_sin_ea_m) 

# 3.5 Verificar la normalidad en los errores ####

# 3.5.1 QQ plot ####

# Obtenemos residuales

ui_m <- as.data.frame(modelo.8$residuals)
colnames(ui_m) <- c("Ui")

ui_m %>% 
  ggplot(aes(sample = Ui)) + 
  stat_qq() +
  stat_qq_line() + 
  ggtitle("QQ Plot Residuales") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Rechazamos el supuesto de normalidad de los errores debido a las dos colas que muestra el gráfico de QQ plot
# no obstante, ya que el modelo de regresión lineal simple ajustado es robusto ante el supuesto de normalidad
# podemos continuar usando estas variable

# 4. Intervalos de Confianza ( E[Y/X´s] ) y de predicción (Ys) ####

# Intervalo confianza

confianza_m <- as.data.frame(predict.lm(modelo.8, train.base_m, interval = "confidence", level = .95))

colnames(confianza_m) <- c("Y_Estimada", "lwr", "upr")

# Intervalo Predicción 

prediccion_m <- as.data.frame(predict.lm(modelo.8, train.base_m, interval = "prediction", level = .95))

colnames(prediccion_m) <- c("Y_Observada_predecida", "lwr_pred", "upr_pred")

# Unimos ambos

train.base.confianza.prediccion_m <- as.data.frame(c(train.base_m, confianza_m, prediccion_m))

# Graficamos - Preguntar a profesor esta parte ya que no me sale el gráfico con todas las variables en un mismo lugar

grafico_confianza_prediccion_m <- train.base.confianza.prediccion_m %>% 
  ggplot() + 
  geom_point(aes(x = carwidth, y = price)) +
  geom_point(aes(x = enginesize, y = price)) + 
  geom_point(aes(x = stroke, y = price)) + 
  geom_point(aes(x = compressionratio, y = price)) + 
  geom_point(aes(x = log(peakrpm), y = price)) + 
  
  geom_line(aes(x = carwidth, y = lwr), color = "red", linetype = "dashed") + 
  geom_line(aes(x = carwidth, y = upr), color = "red", linetype = "dashed") +
  geom_line(aes(x = enginesize, y = lwr), color = "blue", linetype = "dashed") + 
  geom_line(aes(x = enginesize, y = upr), color = "blue", linetype = "dashed") +
  geom_line(aes(x = stroke, y = lwr), color = "purple", linetype = "dashed") + 
  geom_line(aes(x = stroke, y = upr), color = "purple", linetype = "dashed") 
  


grafico_confianza_prediccion_m
