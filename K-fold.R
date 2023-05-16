
getwd()
setwd("/Users/jack9/Downloads/SSPIAII.G02.N6.ProyectoML")   # Cambiar la ruta

library(caret)

source("Preprocesamiento.R")

# Valodacion cruzada -> permite obtener la exactitud promedio del modelo

########          K-fold       #######

# 1 -> Variable k del número de folds a utilizar. Por ejemplo, k <- 5
# 2 -> Dividir los datos en k partes iguales. 
#   La función createFolds de caret para crear los índices de los folds de manera aleatoria. 
#   Por ejemplo, folds <- createFolds(df.Datos$Consumo, k = k).
# 3 --> Utiliza un bucle for para iterar sobre los k folds. 

library(caret)

# especificar el número de k-folds
k <- 5
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


####### Me quede aqui... nuevos datos para ver si el modelo funciona con las nuevas entradas 
# Hacer de nuevo el preprocesamiento 





# Entrenar el modelo en todo el conjunto de datos de entrenamiento
modelo_consumo <- lm(Consumo ~ ., data = df.Datos)

# Hacer predicciones en los datos nuevos
predicciones_nuevas <- predict(modelo_consumo, newdata = df.Datos)

# Crear un data frame con los datos observados y predichos en los nuevos datos
df_pred_nuevos <- data.frame(Consumo_obs = df.NuevosDatos$Consumo, Consumo_pred = predicciones_nuevas)

# Definir colores personalizados
df_pred_nuevos$colores <- ifelse(df_pred_nuevos$Consumo_obs > df_pred_nuevos$Consumo_pred, "Predicción baja", "Predicción alta")

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
