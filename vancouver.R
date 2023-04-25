

options(scipen = 999)
set.seed(1991)
getwd() 

library(ggplot2)
library(plyr)

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

#Convertimos a factor los turnos en los que consumio (Tarde y Noche)
df.Datos$Turno <- factor(df.Datos$Turno,
                               levels = c("Noche", "Tarde "),
                               labels = c(0,1))   #Converir a factores


############### Analisis descriptivo 

library(ggplot2)
summary(df.Datos)

# Histrograma de los grupos de personas que visitaron el local 
Hist_grupos <- ggplot(df.Datos, aes(x = Tipo_Grupo)) +
  geom_histogram(stat = "count", fill = "azure1", color = "blue", binwidth = 0.5) +
  labs(title = "Distribucion de los grupos ",
       x = "Grupo",
       y = "Frecuencia") +
  scale_x_discrete(labels = c("Familia", "Amigos", "Pareja", "Solo")) +
  theme(plot.title = element_text(hjust = 0.5))

Hist_grupos

# Histrograma de la distribucion de los días en los que se visitó el local
hist_dias <- ggplot(df.Datos, aes(x = Dia)) +
  geom_histogram(stat = "count", fill = "cadetblue4", color = "cadetblue", binwidth = 0.5) +
  labs(title = "Distribucion de los días de la semana",
       x = "Día",
       y = "Frecuencia") +
  scale_x_discrete(labels = c("Lunes", "Miercoles", "Jueves", "Viernes", "Sábado", "Domingo")) +
  theme(plot.title = element_text(hjust = 0.5))

hist_dias 

# Histograma para comparar el metodo de pago (Efectivo o tarjeta )
hist_pago <- ggplot(df.Datos, aes(x = Metodo_Pago, fill = Metodo_Pago)) +
  geom_histogram(stat = "count") +
  scale_fill_manual(values = c("steelblue", "orange"), labels = c("Efectivo", "Tarjeta")) +
  labs(title = "Método de pago",
       x = "Metodo de pago",
       y = "Frecuencia") +
  theme(plot.title = element_text(hjust = 0.5))

hist_pago

# Histograma para comparar el turno en el que se consumio (Noche o Tarde)
hist_turno <- ggplot(df.Datos, aes(x = Turno, fill = Turno)) +
  geom_histogram(stat = "count") +
  scale_fill_manual(values = c("deepskyblue3", "darkslategray3"), labels = c("Noche", "Tarde")) +
  labs(title = "Turno en el que se consumio",
       x = "Turno",
       y = "Frecuencia") +
  theme(plot.title = element_text(hjust = 0.5))

hist_turno

# Histrograma del numero de personas que visitan el local 
hist_personas <- ggplot(df.Datos, aes(x = Num_Personas)) +
  geom_histogram(stat = "count", fill = "lightskyblue2", color = "lightblue4", binwidth = 0.5) +
  labs(title = "Día de la visita",
       x = "Número de personas",
       y = "Frecuencia") +
  scale_x_continuous(breaks = seq(min(df.Datos$Num_Personas), max(df.Datos$Num_Personas+1), by = 1)) +
  theme(plot.title = element_text(hjust = 0.5))

hist_personas

# Histrograma de la distribución del cosnumo en $
hist_consumo <-ggplot(df.Datos, aes(x = Consumo)) +
  geom_histogram(fill = "cyan4", color = "darkslategray") +
  labs(title = "Histograma del consumo en $",
       x = "Dinero en $",
         y = "Frecuencia") +
  theme(plot.title = element_text(hjust = 0.5))

hist_consumo


# Histrograma de la distribución de la propina que se dejo 
hist_propina <-ggplot(df.Datos, aes(x = Propina)) +
  geom_histogram(fill = "cyan4", color = "darkslategray") +
  labs(title = "Histograma de las propinas",
       x = "Propina en $",
       y = "Frecuencia") +
  theme(plot.title = element_text(hjust = 0.5))

hist_propina

##### Intento de regresion lineal 

library(caTools)   # Para dividir el dataset 

#Train/Test
split <- sample.split(df.Datos$Consumo, SplitRatio = 0.8)    
df.Train <- subset(df.Datos, split == T)                     # 80% Entrenamiento 
df.Test <- subset(df.Datos,split == F)                       # 20% Test

# Para la selección de variables usaremos la selección hacia adelante 
# Es decir, agregaremos todas y eliminaremos una por una para ver su impacto.


# Crear el modelo de regresión lineal con todas las variables 
modelo <- lm(Consumo ~ ., data = df.Train)
summary(modelo)
# Adjusted R-squared sea lo mas pegado a 1 es mejor --> 0.7025 

#Primer intento -->  Todas las varibles 
#   Adjusted R-squared:  0.7025 
#   Multiple R-squared:  0.7052
# VARIABLES COM MAYOR IMPACTO 
#     Propina y Num_personas



# Intento 2  Eliminamos Dia
modelo <- lm(Consumo ~ Propina + Num_Personas + Metodo_Pago + Tipo_Grupo + Turno , data = df.Train)
summary(modelo)

# Segundo intento -->  Eliminamos Dia  
#   Adjusted R-squared:  0.7085 
#   Multiple R-squared:  0.7 
 

# Intento 3
modelo <- lm(Consumo ~ Propina + Num_Personas + Metodo_Pago  + Turno , data = df.Train)
summary(modelo)

# Tercer intento -->   Eliminamos Dia y Tipo_Grupo
#   Adjusted R-squared:  0.0.7072 
#   Multiple R-squared:  0.7 

# Hacer los intentos que se requieran





# Intento 3
modelo <- lm(Consumo ~ Propina + Num_Personas  , data = df.Train)
summary(modelo)


# Hacer predicciones con el modelo en los datos de prueba
predicciones <- predict(modelo, newdata = df.Test)
# Cremos un dataframe con los valores REALES y PREDICCION
comparar <- data.frame(Observado = df.Test$Consumo, Predicho = predicciones)
View(comparar)
# Crear un data frame con los datos observados y predichos
df_pred <- data.frame(Consumo_obs = df.Test$Consumo, Consumo_pred = predicciones)

# Definir colores personalizados
df_pred$colores <- ifelse(df_pred$Consumo_obs > df_pred$Consumo_pred, "Predicción baja", "Predicción alta")

# Crear el gráfico 
ggplot(df_pred, aes(x = Consumo_obs, y = Consumo_pred, color = colores)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  labs(title = "Observación vs. Predicción", 
       x = "Consumo Observado", y = "Consumo Predicho", 
       color = "Comparación") +
  scale_color_manual(values = c("Predicción alta" = "blue", "Predicción baja" = "red")) +
  theme_bw()


# En este caso, la función ifelse() se utiliza para asignar el color "rojo" a los puntos donde el consumo 
# observado es mayor que el consumo predicho, y el color "azul" para los puntos donde el consumo observado 
# es menor o igual que el consumo predicho


