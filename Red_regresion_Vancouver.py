# Jazmin Alejandra Lomelí Zermeño 
# 20-04-23

#import tensorflow as tf
#print(tf.__version__)            #version de tensorflow


#Librerias
import pandas as pd                            # control y lectura de archivos 
from sklearn.preprocessing import LabelEncoder, OneHotEncoder, StandardScaler 
from sklearn.compose import ColumnTransformer 
from sklearn.model_selection import train_test_split 
import matplotlib.pyplot as plt
import numpy as np                             # se usa en la matriz  
from scipy.stats import mode



#dataset 
Data = pd.read_csv("Vancouver_Wings.csv")
#X = Data.iloc[:, [0, 1, 2, 3, 4, 6]].values

#   PREPROCESAMIENTOO
# Ver si hay valores NA
print(Data.isna().sum())

# Obtener la moda de la columna que tiene los vacios
moda = mode(Data['Tipo_Grupo'].dropna())[0][0]

#Imprimir la moda
print("La moda es: ", moda, "\n")   

# Reemplazar los valores NA con la moda
Data['Tipo_Grupo'].fillna(moda, inplace=True)

# Verificar que ya no haya NA
print(Data.isna().sum())

X = Data.iloc[:,[1, 2, 3, 4, 5, 7]].values    # Extreamos las columnas 
Y = Data.iloc[:,6].values      # Variable objetivo (Consumo)
 

# crear una instancia de LabelEncoder para cada variable categórica 
lblEncoder_Dia = LabelEncoder()
lblEncoder_Turno = LabelEncoder()
lblEncoder_TipoGrupo = LabelEncoder()
lblEncoder_MetodoPago = LabelEncoder()

# Convertimos a factor 
X[:,0] = lblEncoder_Dia.fit_transform(X[:, 0])         # Convierte a factor los dias de la semana
X[:,1] = lblEncoder_Turno.fit_transform(X[:,1])        # Convierte a factor  la variable Turno
X[:,3] = lblEncoder_TipoGrupo.fit_transform(X[:,3])    # Convierte a factor la variable tipo_grupo
X[:,4] = lblEncoder_MetodoPago.fit_transform(X[:,4])   # Convierte a factor la variable Metodo_Pago
 
# El dataset ya son solo números

#One hot encoder 
one =  ColumnTransformer(
    [('one_hot_encoder' , OneHotEncoder(categories='auto'), [0])],  #Columna de los DIAS DE LA SEMANA
    remainder='passthrough'   
    )
# Preparar un objeto transformador --> la columna que transforma es la columna 0 

# Hasta aqui el dataset son 6 columnas

# Convertir ahora si en 0 y 1 
X = one.fit_transform(X)

# Ahora el dataset son 10 columnas

# Como lo marca Dummy, eliminar una columna 
X = X[:, 1:]    # Quitamos la columna 1 

# En en dataset las columnas de 1-4 son para los dias 
# La columna 5 es para el Turno
# La columna 6 es para el numero de personas
# La columna 7 es para el tipo de amigos --> Hacer One hot Encoding

# Hasta aqui el dataset son 0-9 columnas 

#One hot encoder 
one_grupo =  ColumnTransformer(
    [('one_hot_encoder' , OneHotEncoder(categories='auto'), [7])],  #Columna de los DIAS DE LA SEMANA
    remainder='passthrough'   
    )

X = one_grupo.fit_transform(X)
# El dataset aumento a 0-12 columnas, lo que quiere decir que se agregaron 4 columnas en el OHE (Al inicio)
# Solo, Familia, Amigos y pareja
 
# Como lo marca Dummy, eliminar una columna 
X = X[:, 1:]    # Quitamos la columna 1 

# Finalmente el Dataset queda con 10 columnas 
# 3 Tipo_amigos
# 5 Dia
# 1 Turno
# 1 Num_personas
# 1 Metodo de pago
# 1 Propina

# Escalamos todos los datos
X = StandardScaler().fit_transform(X)

#Split
X_train, X_test, Y_train, Y_test = train_test_split(X, Y,
                                                    test_size= 0.2,
                                                    random_state=10)
#  Modelo
from keras.models import Sequential
from keras.layers import Dense

# Crear el modelo de la red neuronal
red_neuronal = Sequential()

red_neuronal.add(
    Dense(units=6, 
          input_dim=X_train.shape[1], 
          activation='softplus')
          )

red_neuronal.add(
    Dense(units= 5, 
          activation='relu')
          )

red_neuronal.add(
    Dense(units= 5, 
          activation='relu')
          )

red_neuronal.add(
    Dense(units=1, 
    activation='linear')
    )
# Se comenzo con 3 capas y 4 neuronas en cada capa (relu)--> R^2 = 0.7076
# 3 capas y 5 y 4 neuronas --> R^2 = 0.7796
# 3 capas y 5 neuronas en cada capa --> R^2 = 0.7945

# Se agrego la nueva capa con softmax --> R^2 = 0.8099


# Compilar el modelo    
red_neuronal.compile(loss='mean_squared_error', optimizer='adam' )

# Entrenar el modelo
red_neuronal.fit(X_train, Y_train, epochs=100, batch_size=10, verbose=1)

from sklearn.metrics import r2_score

# Hacer predicciones 
Y_pred = red_neuronal.predict(X_test)

# Calcular el valor de R^2
r2 = r2_score(Y_test, Y_pred)
print(f"Coeficiente de determinación R^2: {r2:.4f}") # 4 Digitos despues del punto decimal

# calcular el Mean Squared Error
mse = red_neuronal.evaluate(X_test, Y_test, verbose=0)
print('MSE: %.2f' % (mse))
 
# Crear la gráfica de dispersión con los datos de prueba y predicción
fig, ax = plt.subplots()

plt.scatter(Y_test, Y_pred)
# Añadir una línea diagonal para comparar los valores reales y predichos
plt.plot([Y_test.min(), Y_test.max()], [Y_test.min(), Y_test.max()], 'r--')
# Añadir etiquetas a los ejes x e y y título
plt.xlabel('Valores reales')
plt.ylabel('Valores predichos')
plt.title('Observación Vs Prediccion')

# Mostrar la gráfica
# Mostrar la matriz 
fig.tight_layout()
plt.savefig('modelo_regresion.png')       # Guardar en png la matriz
plt.show()                                # Mostrar matriz 
 
# Si los puntos de datos se agrupan cerca de la línea diagonal, esto indica que la red neuronal ha 
# hecho predicciones precisas. 
