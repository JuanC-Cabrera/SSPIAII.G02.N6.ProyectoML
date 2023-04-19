options(scipen = 999)
set.seed(1991)
getwd() 

#Dtaset 
df.Datos <- read.csv("IA/SSPIAII.G02.N6.ProyectoML/Vancouver_Wings.csv",   #Cambiar ruta
                     header =  T,
                     stringsAsFactors = T)

df.Datos$ID <- NULL

# ver que tipo de dato son las caracteristcas 
class(df.Datos$Dia)
class(df.Datos$Turno)
class(df.Datos$Num_Personas)
class(df.Datos$Tipo_Grupo)
class(df.Datos$Metodo_Pago)
class(df.Datos$Consumo)
class(df.Datos$Propina)
