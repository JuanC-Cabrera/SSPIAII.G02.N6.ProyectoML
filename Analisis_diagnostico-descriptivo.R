#################### Parte 2: Análisis descriptivo ####################
source("Preprocesamiento.R")

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
  labs(title = "Número de Personas por mesas",
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

# Instalar y cargar el paquete GGally para ver toda la informacion
install.packages("GGally")
library(GGally)

# Hacer el scatterplot con el scatterplot podemos ver las relaciones de entre las variables de la mejor forma posible
Datos_visual<-ggpairs(data = df.Datos)

Datos_visual

# Grafica de dispersion entre consumo y propina
dispersion<-ggplot(data = df.Datos, aes(x = Consumo, y = Propina)) + 
  geom_point(color = "#69b3a2") + 
  labs(x = "Consumo", y = "Propina", title = "Gráfica de dispersión de altura y consumo") +
  theme_minimal()

dispersion

#################### Parte 3: Análisis diagnóstico ####################
# Después de observar las gráficas con los datos decidimos crear 1 modelo 
# de regresion lineal tomando como cvariable dependiente el consumo ya que
# observamos con las graficas que existen variables con una gran correlacion, 
# lo que puede como resultado un modelo. se puede apreciar una tendencia lineal
# clara entre la variable dependiente y la variable independiente de propina.
# Esto significa que la relación entre ambas variables puede ser descrita por 
# una línea recta. Esto proporciona una buena estimación de la relación entre
# las variables y permitiendo hacer predicciones precisas sobre los valores futuros
# de la variable dependiente
