

options(scipen = 999)
set.seed(1991)
getwd() 
library(ggplot2)
library(plyr)
library(caTools)
library(caret)
library(rpart)


#Importar nuestro dataset
df.Datos <- read.csv("Vancouver_Wings.csv",   #Cambiar ruta
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
# Histrograma de los grupos de personas que visitaron el local. 
ggplot(df.Datos, aes(x = Tipo_Grupo)) +
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
count(df.Datos$Dia)

# Histrograma de los la cantidad de gente que iba por dia. 
ggplot(df.Datos, aes(x = Dia)) +
  geom_histogram(stat = "count", fill = "azure1", color = "blue", binwidth = 0.5) +
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

<<<<<<< HEAD
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


############################################################################################################

# Separación de los datos que se utilizaran.
split <- sample.split(df.Datos$Tipo_Grupo, SplitRatio = 0.7) 
trainig <- subset(df.Datos, split == T) 
test <- subset(df.Datos, split == F)

# Seleccionar solo las variables numéricas.
# Aquí, sapply(df.Datos, is.numeric) devuelve un vector lógico que indica qué columnas de df.Datos
# son numéricas.
df.num <- df.Datos[, sapply(df.Datos, is.numeric)]

# Calcular la matriz de correlación.
cor_mat <- cor(df.num)

# Random forest
mdl.Forest <- randomForest(formula = as.factor(Tipo_Grupo) ~ ., data = trainig)

predictData <- predict(mdl.Forest, type = 'class', newdata = test)

# Matríz de confusión
matriz <- table(test$Tipo_Grupo,predictData)
matrizRandom <- matriz
matrizRandom
#      1   2   3   4
#   1  22  8   7   0
#   2  12  8   7   0
#   3  0   0   25  0
#   4  0   0   0   4

# Generar la matriz de confusión para calcular F1 score.
matrizLibrary <- confusionMatrix(matriz,positive = "1", mode = "everything")
F1 <- matrizLibrary$byClass['F1']
matrizLibrary

#                      Class: 1  Class: 2  Class: 3  Class: 4
#Sensitivity            0.6471   0.50000    0.6410   1.00000
#Specificity            0.7458   0.75325    1.0000   1.00000
#Pos Pred Value         0.5946   0.29630    1.0000   1.00000
#Neg Pred Value         0.7857   0.87879    0.7941   1.00000
#Precision              0.5946   0.29630    1.0000   1.00000
#Recall                 0.6471   0.50000    0.6410   1.00000
#F1                     0.6197   0.37209    0.7813   1.00000
#Prevalence             0.3656   0.17204    0.4194   0.04301
#Detection Rate         0.2366   0.08602    0.2688   0.04301
#Detection Prevalence   0.3978   0.29032    0.2688   0.04301
#Balanced Accuracy      0.6964   0.62662    0.8205   1.00000

# Random forest aplicado al método de pago.
mdl.Forest <- randomForest(formula = as.factor(Metodo_Pago) ~ ., data = trainig)

predictData <- predict(mdl.Forest, type = 'class', newdata = test)

# Matríz de confusión
matriz <- table(test$Metodo_Pago,predictData)
matrizRandom <- matriz
matrizRandom
#      0  1
#  0  75  3
#  1  14  1

# Generar la matriz de confusión para calcular F1 score.
matrizLibrary <- confusionMatrix(matriz,positive = "1", mode = "everything")
F1 <- matrizLibrary$byClass['F1']
matrizLibrary

# Accuracy : 0.8172          
# 95% CI : (0.7235, 0.8898)
# No Information Rate : 0.957           
# P-Value [Acc > NIR] : 1.00000         

# Kappa : 0.0401          

# Mcnemar's Test P-Value : 0.01529         
                                          
#            Sensitivity : 0.25000         
#            Specificity : 0.84270         
#         Pos Pred Value : 0.06667         
#         Neg Pred Value : 0.96154         
#              Precision : 0.06667         
#                 Recall : 0.25000         
#                     F1 : 0.10526         
#             Prevalence : 0.04301         
#         Detection Rate : 0.01075         
#   Detection Prevalence : 0.16129         
#      Balanced Accuracy : 0.54635         
                                          
#       'Positive' Class : 1               
                              
trainig$Turno <- as.factor(trainig$Turno)
test$Turno <- as.factor(test$Turno)

# El código utiliza la función "rpart" para ajustar un modelo de árbol de decisión a los datos de "entrenamiento". El argumento "rpart.control" 
# especifica algunos parámetros de control para el proceso de ajuste del modelo, incluido el parámetro "minsplit" que determina el número mínimo 
# de observaciones necesarias para dividir un nodo.

mdl.TurnoRpart <- rpart(Turno ~ . ,data = trainig,control=rpart.control(minsplit=2))
summary(mdl.TurnoRpart)

mdl.Turno.Predict <- predict(mdl.TurnoRpart, newdata =  test)
mdl.Turno.Predict
summary(mdl.Turno.Predict)

#     Noche             Tarde       
# Min.   :0.00000   Min.   :0.0000  
# 1st Qu.:0.03846   1st Qu.:0.0000  
# Median :0.09091   Median :0.9091  
# Mean   :0.40471   Mean   :0.5953  
# 3rd Qu.:1.00000   3rd Qu.:0.9615  
# Max.   :1.00000   Max.   :1.0000 

accuracy <- sum(diag(matriz))/sum(matriz)
accuracy
#[1] 0.8172043
>>>>>>> Raul
