#################### Parte 4.1: Análisis predictivo ####################

###Regresion lineal 

library(caTools)   # Para dividir el dataset 

#Train/Test
split <- sample.split(df.Datos$Consumo, SplitRatio = 0.8)    
df.Train <- subset(df.Datos, split == T)                     # 80% Entrenamiento 
df.Test <- subset(df.Datos,split == F)                       # 20% Test

# Para la selección de variables usaremos la selección hacia adelante 
# Es decir, agregaremos todas y eliminaremos una por una para ver su impacto.

# Crear el modelo de regresión lineal con todas las variables 
modelo_consumo <- lm(Consumo ~ ., data = df.Train)
summary(modelo_consumo)

#Primer intento -->  Todas las varibles 
#   Adjusted R-squared:  0.7025 
#   Multiple R-squared:  0.7052
# VARIABLES COM MAYOR IMPACTO 
#     Propina y Num_personas



# Intento 2  Eliminamos Dia
modelo_consumo <- lm(Consumo ~ Propina + Num_Personas + Metodo_Pago + Tipo_Grupo + Turno , data = df.Train)
summary(modelo_consumo)

# Segundo intento -->  Eliminamos Dia  
#   Adjusted R-squared:  0.7085 
#   Multiple R-squared:  0.7 


# Intento 3
modelo_consumo <- lm(Consumo ~ Propina + Num_Personas + Metodo_Pago  + Turno , data = df.Train)
summary(modelo_consumo)

# Tercer intento -->   Eliminamos Dia y Tipo_Grupo
#   Adjusted R-squared:  0.7072 
#   Multiple R-squared:  0.7 


# Intento 4
modelo_consumo <- lm(Consumo ~ Propina + Num_Personas  , data = df.Train)
summary(modelo_consumo)


# Hacer predicciones con el modelo en los datos de prueba
predicciones <- predict(modelo_consumo, newdata = df.Test)
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

###### Clasificación
install.packages("randomForest")
library(randomForest)

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

