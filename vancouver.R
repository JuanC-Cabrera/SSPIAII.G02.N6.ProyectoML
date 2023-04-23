options(scipen = 999)
set.seed(1991)
getwd() 


library(DescTools)
library(ggplot2)
library(plyr)

#install.packages("DescTools")

#Importar nuestro dataset
df.Datos <- read.csv("IA/SSPIAII.G02.N6.ProyectoML/Vancouver_Wings.csv",   #Cambiar ruta
                     header =  T,
                     stringsAsFactors = T)

############### Preprocesamiento de los datos

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


############### Analisis descriptivo 

library(ggplot2)
# Histrograma de los grupos de personas que visitaron el local 
ggplot(df.Datos, aes(x = Tipo_Grupo)) +
  geom_histogram(stat = "count", fill = "azure1", color = "blue", binwidth = 0.5) +
  labs(title = "Distribucion de los grupos ",
       x = "Grupo",
       y = "Frecuencia") +
  scale_x_discrete(labels = c("Familia", "Amigos", "Pareja", "Solo")) +
  theme(plot.title = element_text(hjust = 0.5))


count(df.Datos$Dia)

# Histrograma de los grupos de personas que visitaron el local 
ggplot(df.Datos, aes(x = Dia)) +
  geom_histogram(stat = "count", fill = "azure1", color = "blue", binwidth = 0.5) +
  labs(title = "Distribucion de los días de la semana",
       x = "Día",
       y = "Frecuencia") +
  scale_x_discrete(labels = c("Lunes", "Miercoles", "Jueves", "Viernes", "Sábado", "Domingo")) +
  theme(plot.title = element_text(hjust = 0.5))


# Histograma para comparar el metodo de pago (Efectivo o tarjeta )
ggplot(df.Datos, aes(x = Metodo_Pago, fill = Metodo_Pago)) +
  geom_histogram(stat = "count") +
  scale_fill_manual(values = c("steelblue", "orange"), labels = c("Efectivo", "Tarjeta")) +
  labs(title = "Histograma del metodo de pago",
       x = "Metodo de pago",
       y = "Frecuencia") +
  theme(plot.title = element_text(hjust = 0.5))


############### Analisis descriptivo 

 