
getwd()
setwd("/Users/jack9/Downloads/SSPIAII.G02.N6.ProyectoML")   # Cambiar la ruta

library(caret)

# Llamamos al archivo con los datos anteriores
source("Preprocesamiento.R")

# Validacion cruzada -> permite obtener la exactitud promedio del modelo

###############################################################
#                        K-fold   
###############################################################

# 1 -> Variable k del número de folds a utilizar. Por ejemplo, k <- 5
# 2 -> Dividir los datos en k partes iguales. 
#   La función createFolds de caret para crear los índices de los folds de manera aleatoria. 
#   Por ejemplo, folds <- createFolds(df.Datos$Consumo, k = k).
# 3 --> Utiliza un bucle for para iterar sobre los k folds. 


# especificar el número de k-folds
k <- 3
library(caret)
set.seed(123) # para reproducibilidad
folds <- createFolds(df.Datos$Consumo, k = k)

# inicializar vector para guardar los valores de R^2
r_squared <- numeric(k)
# crear vector para almacenar los RMSE de cada fold
rmse_vec <- numeric(k)


for (i in 1:k) {
  # Obtener los índices del fold i
  fold_indices <- folds[[i]]
  
  # Crear los conjuntos de entrenamiento y validación
  train_data <- df.Datos[-fold_indices, ]
  valid_data <- df.Datos[fold_indices, ]
  
  # Entrenar el modelo en el conjunto de entrenamiento
  modelo_consumo <- lm(Consumo ~ ., data = train_data)
  
  # Hacer predicciones en el conjunto de validación
  predicciones <- predict(modelo_consumo, newdata = valid_data)
  
  # Calcular la métrica de evaluación (por ejemplo, el RMSE)
  rmse <- sqrt(mean((predicciones - valid_data$Consumo)^2))
  
  # almacenar el RMSE en el vector
  rmse_vec[i] <- rmse
  
  # Calcular el R^2 y guardarlo en el vector correspondiente
  r_squared[i] <- summary(modelo_consumo)$r.squared
  
  # Imprimir el resultado del fold i
  cat("Fold", i, "- RMSE:", rmse, "R^2:", r_squared[i], "\n")
}

# Calcular el promedio y la desviación estándar de los RMSE y R^2
mean_rmse <- mean(rmse)
sd_rmse <- sd(rmse_vec)
mean_r_squared <- mean(r_squared)
sd_r_squared <- sd(r_squared)

# Imprimir el resultado
cat("Mean RMSE:", mean_rmse, "\n")        # valor promedio del error cuadrático medio (RMSE) 
cat("SD RMSE:", sd_rmse, "\n")            # desviación estándar del RMSE
cat("Mean R^2:", mean_r_squared, "\n")    # promedio del coeficiente de determinación (R^2) 
cat("SD R^2:", sd_r_squared, "\n")        # desviación estándar del R^2 en los k-folds

# ¿Por qué K = 3? 

# k = 4 --> R^2: 0.7194404  
# K = 6 --> R^2: 0.7176395
# k = 5 --> R^2: 0.7182101 
# k = 3 --> R^2: 0.7226803       Nos arroja el mejor resultado
# k = 2 --> R^2: 0.7161765        


###############################################################
#                   Validación del modelo  
###############################################################

####### Preprocesar los nuevos datos 

#Importar nuestro dataset
df.validation <- read.csv("IA/SSPIAII.G02.N6.ProyectoML/Vancouver_Wings_Validation.csv",   #Cambiar ruta
                     header =  T,
                     stringsAsFactors = T)

# De una vez eliminamos la comumna Id 
df.validation$ID <- NULL

# Verificamos si hay valores NA
colSums(is.na(df.validation))   

#Convertimos a factor  
df.validation$Tipo_Grupo <- factor(df.validation$Tipo_Grupo,
                              levels = c("Familia", "Amigos", "Pareja", "Solo"),
                              labels = c(1,2,3,4))   #Converir a factores

# Calculamos la frecuencia de cada uno de los factores
tabla <- table(df.validation$Tipo_Grupo, exclude = NULL)
summary(df.validation)

# Obtener la moda a partir de los valores más frecuentes
moda <- names(tabla)[which.max(tabla)]

# Cambiamos los NA por la moda 
df.validation$Tipo_Grupo <- factor(ifelse(is.na(df.validation$Tipo_Grupo), moda, df.validation$Tipo_Grupo))

# Verificamos nuavamente si hay NA
colSums(is.na(df.validation)) 

#Convertimos a factor los metodos de pago (Efectivo y tarjeta)
df.validation$Metodo_Pago <- factor(df.validation$Metodo_Pago,
                               levels = c("Efectivo", "Tarjeta"),
                               labels = c(0,1))   #Converir a factores

#Convertimos a factor los turnos en los que consumio (Tarde y Noche)
df.validation$Turno <- factor(df.validation$Turno,
                         levels = c("Noche","Tarde"),
                         labels = c(0,1))   #Converir a factores

#Convertimos a factor los Dias de la semana (Martes se descansa)
df.validation$Dia <- factor(df.validation$Dia,
                            levels = c("Lunes", "Miércoles ", "Jueves", "Viernes", "Sábado", "Domingo"),
                            labels = c(1,2,3,4,5,6))   #Converir a factores


# Recortar el dataset a 226 filas 
df.recortado <- head(df.Datos, 226)
#El dataset se debe recortar ya que el de validacion tiene 226 observaciones,
# si ni lo recortamos nos dara un error 

#Aqui se entrena el modelo con 226 datos de los que ya teniamos de df.data
modelo<- lm(Consumo ~ ., data = df.recortado)

# Hacer predicciones en los datos nuevos
predicciones_nuevas <- predict(modelo, newdata = df.validation)

# Crear un data frame con los datos observados y predichos en los nuevos datos
df_pred_nuevos <- data.frame(Consumo_obs = df.validation$Consumo, Consumo_pred = predicciones_nuevas)
 
# Definir colores personalizados Con el dataframe creado 
df_pred_nuevos$colores <- ifelse(df_pred_nuevos$Consumo_obs > df_pred_nuevos$Consumo_pred, "Predicción baja", "Predicción alta")

library(ggplot2)

# Crear el gráfico 
ggplot(df_pred_nuevos, aes(x = Consumo_obs, y = Consumo_pred, color = colores)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  labs(title = "Observación vs. Predicción en nuevos datos", 
       x = "Consumo Observado", y = "Consumo Predicho", 
       color = "Comparación") +
  scale_color_manual(values = c("Predicción alta" = "blue", "Predicción baja" = "red")) +
  theme_bw()

#########################################################################################################################################################
                                    # K-FOLD CLASIFICACIÓN #
#########################################################################################################################################################

library(caret)

# Especificar el número de k-folds
k <- 5

set.seed(123) # para reproducibilidad
foldsc <- createFolds(df.Datos$Tipo_Grupo, k = k)

# Inicializar vector para guardar los valores de precisión
accuracy <- numeric(k)

for (i in 1:k) {
  # Obtener los índices del fold i
  fold_indicesi <- foldsc[[i]]
  
  # Crear los conjuntos de entrenamiento y validación
  training_data <- df.Datos[-fold_indicesi, ]
  validi_data <- df.Datos[fold_indicesi, ]
  
  # Entrenar el modelo de clasificación en el conjunto de entrenamiento
  modelo_clasificacion <- train(Tipo_Grupo ~ ., data = training_data, method = "rf")
  
  # Hacer predicciones en el conjunto de validación
  predicciones <- predict(modelo_clasificacion, newdata = validi_data)
  
  # Calcular la precisión del modelo
  acc <- confusionMatrix(predicciones, validi_data$Tipo_Grupo)$overall["Accuracy"]
  
  # Almacenar la precisión en el vector
  accuracy[i] <- acc
  
  # Imprimir el resultado del fold i
  cat("Fold", i, "- Accuracy:", acc, "\n")
}

# Calcular el promedio y la desviación estándar de la precisión
mean_accuracy <- mean(accuracy)
sd_accuracy <- sd(accuracy)

# Imprimir el resultado
cat("Mean Accuracy:", mean_accuracy, "\n")        # valor promedio de la precisión
cat("SD Accuracy:", sd_accuracy, "\n")            # desviación estándar de la precisión en los k-folds
