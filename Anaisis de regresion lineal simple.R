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

## 1.- hacemos la limpieza de datos necesaria para que sólo queden variables numéricas

numeric_df <- raw_df %>% 
  dplyr::select(wheelbase, carlength, carwidth, carheight, curbweight, enginesize, boreratio, stroke, compressionratio,
                horsepower, peakrpm, citympg, highwaympg,carbody, price)

## 2.- Dividimos la info en intervalos

first_set <- numeric_df %>% 
  ggpairs(columns = c(1:4, 15), aes(color = carbody, alpha = .5)) + 
  ggtitle("First set")

second_set <- numeric_df %>% 
  ggpairs(columns = c(5:9, 15), aes(color = carbody, alpha = .5)) +
  ggtitle("Second set")

third_set <- numeric_df %>% 
  ggpairs(columns = c(10:13, 15), aes(color = carbody, alpha = .5)) +
  ggtitle("Third set")


