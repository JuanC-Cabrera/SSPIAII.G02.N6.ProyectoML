#################### Parte 1: Preprocesamiento #################### 
options(scipen = 999)
set.seed(1991)
getwd()

library(ggplot2)
library(plyr)

#Importar nuestro dataset
df.Datos <- read.csv("Vancouver_Wings.csv",   #Cambiar ruta
                     header =  T,
                     stringsAsFactors = T)

# De una vez eliminamos la comumna Id 
df.Datos$ID <- NULL

# Verificamos si hay valores NA
colSums(is.na(df.Datos))         

# Ver que tipo de dato son las caracteristcas 
class(df.Datos$Dia)
class(df.Datos$Turno)
class(df.Datos$Num_Personas)
class(df.Datos$Tipo_Grupo)
class(df.Datos$Metodo_Pago)
class(df.Datos$Consumo)
class(df.Datos$Propina)

#Convertimos a factor  
df.Datos$Tipo_Grupo <- factor(df.Datos$Tipo_Grupo,
                              levels = c("Familia", "Amigos", "Pareja", "Solo"),
                              labels = c(1,2,3,4))   #Converir a factores

# Calculamos la frecuencia de cada uno de los factores
tabla <- table(df.Datos$Tipo_Grupo, exclude = NULL)
summary(df.Datos)
# Obtener la moda a partir de los valores más frecuentes
moda <- names(tabla)[which.max(tabla)]
# Cambiamos los NA por la moda 
df.Datos$Tipo_Grupo <- factor(ifelse(is.na(df.Datos$Tipo_Grupo), moda, df.Datos$Tipo_Grupo))

#Convertimos a factor los Dias de la semana (Martes se descansa)
df.Datos$Dia <- factor(df.Datos$Dia,
                       levels = c("Lunes", "Miercoles", "Jueves", "Viernes", "Sábado", "Domingo"),
                       labels = c(1,2,3,4,5,6))   #Converir a factores

#Convertimos a factor los metodos de pago (Efectivo y tarjeta)
df.Datos$Metodo_Pago <- factor(df.Datos$Metodo_Pago,
                               levels = c("Efectivo", "Tarjeta"),
                               labels = c(0,1))   #Converir a factores

#Convertimos a factor los turnos en los que consumio (Tarde y Noche)
df.Datos$Turno <- factor(df.Datos$Turno,
                         levels = c("Noche", "Tarde "),
                         labels = c(0,1))   #Converir a factores
