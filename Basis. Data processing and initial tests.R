### Areas de aplicacion

## Introduccion. Pasos a seguir

# 1.Lectura correcta de los datos

coches<- read.table('car.txt', header = T, sep = ",", na.strings = "?" , dec = ".", strip.white = T)

head(coches)


precios<- read.table('precios.txt', header = T, sep = ",", na.strings = "?" , dec = ".", strip.white = T)

head(precios)

    # para unir ficheros
install.packages("dplyr")
library(dplyr)


coches2<- left_join (coches, precios, by="ID")

head(coches2)


# las variables que no se hayan cargado correctamente( se carga como una va, siendo factor)

origen2<- c("EEUU","Europa","Japon")
coches2$origen<-factor(coches2$origen,labels=origen2)

# convertiremos la variable fecha en un factor
coches2$fecha<-as.factor(coches2$fecha)
# el resto de variables las convertimos en numéricas
coches2$cilindros<-as.numeric(coches2$cilindros)
coches2$CV<-as.numeric(coches2$CV)
coches2$peso<-as.numeric(coches2$peso)
coches2$aceleracion<-as.numeric(coches2$aceleracion)
coches2$precio<-as.numeric(coches2$precio)
str(coches2)


# 1.1 Valores perdidos (IMPORTANTE EN ANALISIS MULTIVARIANTE)

which(is.na(coches2))

colSums(is.na(coches2))  #Se determina donde estan los valores perdidos

apply(is.na(coches2), 2, which) # para averiguar que posiciones/individuos son las perdidas

# para solucionarlo, eliminamos los valores perdidos o le imputamos algun valor

# si decidimos eliminar los individuos la muestra se reduciría en 6 unidades
coches3<-na.omit(coches2)
dim(coches2)


dim(coches3)

------------------------------------------------------------------------------------------------------------------------------------

# 2.Estadistica descriptiva

install.packages("modeest")
library(modeest)

c(mean(coches3$CV),sd(coches3$CV),IQR(coches3$CV),mfv(coches3$CV),
  quantile(coches3$CV,c(0.25,0.5,0.75),na.rm = TRUE))


c(range(coches3$CV),min(coches3$CV),max(coches3$CV),var(coches3$CV))



## Para componentes cualitativas se trabaja con la regresion logistica
## Para componentes cuantitativas se trabaja con analisis factorial, etc


table(coches3$origen) #para obtener la tabla de frecuencias (Base)


# porcentajes
prop.table(table(coches3$origen))


#porcentajes en tanto por ciento
prop.table(table(coches3$origen))*100


#frecuencias acumuladas
cumsum(table(coches3$origen))


cumsum(prop.table(table(coches3$origen)))

------------------------------------------------------------------------------------------------------------------

# 3.Graficos


install.packages("ggplot2")
library(ggplot2)

  #Histograma (hacer para todas las variables)

  #vamos a realizar un histograma del consumo de combustible

qplot(coches3$consumo, xlab = 'consumo', ylab = 'recuento', binwidth = 2,
      main='histograma del consumo')



  # otra opción de hacerlo es la orden ggplot donde se van añadiendo características
  # con la orden +

ggplot(coches3, aes(consumo)) +
  geom_histogram(binwidth = 2) +
  labs(title = "Histograma consumo ", y = "recuento") +
  theme_classic()

qplot(coches3$cilindros, xlab = 'consumo', ylab = 'recuento',
      main='histograma del cilindros')


#La representacion mas comun para encontrar datos anomalos, etc, se hace con DIAGRAMA DE CAJAS


# En primer lugar activamos una salida donde podamos crear dos gráficos
# dentro de la misma imagen
par(mfrow = c(1, 2))


# en este caso tendremos el diagrama de caja del consumo

boxplot(coches3$consumo, main="Diagrama de caja", xlab="", ylab="caja")
legend("bottomleft", legend = "consumo", # Posición y título
       fill = rgb(1, 0, 0, alpha = 0.4), # Color
       inset = c(0.03, 0.05), # Cambiamos los márgenes
       bg = "white") # Color de fondo de la leyenda


# si queremos hacerlo según una variable de FACTOR y añadir colores

boxplot(coches3$consumo~coches3$origen ,main="Diagrama de caja",
        xlab="", ylab="caja", col=c("orange","blue","red"))



#Otra forma

ggplot(data = coches3, aes(x = origen, y = consumo,col=origen)) +
  geom_boxplot() +
  xlab('regiones') +
  ylab('peso') +
  ggtitle('Pesos según procedencia')



# Si quisiéramos añadir alguna variable de factor como el año de procedencia

ggplot(data = coches3, aes(x = origen, y = consumo,col=origen)) +
  geom_boxplot() +
  xlab('region') +
  facet_wrap(~ fecha)+
  ylab('pesos') +
  ggtitle('diagrama de caja según el origen y año')


# Se va a intentar hacer una correlacion k-dimensional entre las variables
# se asume que si la correlacion parcial entre varias variables es significativa, se puede observar cierta correlacion global


#DIAGRAMA DE PUNTOS  Si se quiere estudiar entre varias variables, se modifican los ejes x,y
ggplot(coches3, aes(x = consumo, y = precio)) +
  geom_point() +
  xlab("consumo") +
  ylab("Precio") +
  ggtitle("precio vs consumo")



# Añadiendo la regresion

ggplot(coches3, aes(x = consumo, y = precio)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + #Suavizado geometrico mediante modelo lineal
  xlab("Mileage") +
  ylab("Price") +
  ggtitle("Price of Car by Mileage")

---------------------------------------------------------------------------------------------------------------------

# CORRELACIONES

vehiculos <- coches2[complete.cases(coches2),] # quitamos los valores perdidos

vehiculos <- vehiculos[,-1] # quitamos la variable identificadora (YA NO ES EL MISMO FICHERO QUE COCHES3)


vehiculos2 <- sapply(vehiculos, is.numeric) # determinamos cuales son numéricas
vehiculos2
correlaciones<-cor(vehiculos[,vehiculos2])


# calculamos las correlaciones entre vehículos para las variables que son numéricas
install.packages("psych")
library(psych) #Para representar correlaciones

corPlot(correlaciones, pval=T) #obtenemos un gráfico de calor con las p-valores


-----------------------------------------------------------------------------------------------------------------------------
# 4.Supuestos de Analisis Multivariante (antes del analisis de datos)


## Supuestos de NORMALIDAD K-dimensional

  #En primer lugar se ven los coeficientes de asimetria y curtosis (indicio de NORMALIDAD)

install.packages("pastecs")
library(pastecs)
#lo hacemos con la variable vehículos que hemos seleccionado antes,
# es decir solo con las variables numéricas
round(stat.desc(vehiculos[,vehiculos2],basic=FALSE,norm=TRUE),digits=3)

#nos interesa sobre todo asimetria, curtosis, tanto tipificado como normal. No parece haber indicis de normalidad
# Los indices tipificados tienen que estar entre (-1,1) y no tipificados entre (-2,2)

install.packages("car")
library(car)


qqPlot(coches2$peso)  #se sale un poco de los datos

qqPlot(coches2$peso, groups = coches2$origen) #Hipotesis normalidad por grupos


#Otra forma de comprobarlo
install.packages("ggplot2")
library(ggplot2)
ggplot(coches2, aes(consumo)) +geom_density()

#Por grupos
ggplot(coches2, aes(consumo,color=origen)) +geom_density()

---------------------------------------------------------------------------------------------
## CONTRASTE NORMALIDAD (Kolmogorov N>50, Shaphiro N<50)
install.packages("nortest")
library(nortest)
#vamos a volver a crear una variable solo con las variables numéricas
vehiculosnumerico<-coches2[,c("consumo","cilindros","desplazamiento","CV",
                              "peso","aceleracion","precio")]

# es equivalente usar esta nueva variable o vehiculos[,vehiculos2]

# solo lo hace para una variable
lapply(vehiculosnumerico, lillie.test) # con esta orden hacemos el test de Kolmogorov corregido normalmente

#p-valor< 0.05 => Se rechaza la hipotesis de normalidad para cada una de las variables (normalidad marginal)

shapiro.test(coches2$cilindros)  
lapply(vehiculosnumerico, shapiro.test) # para hacer para una matriz


#NORMALMENTE A PESAR DE CONTRASTAR LA NORMALIDAD MARGINAL, HAY QUE HACERLO EN CONJUNTO
install.packages("MVN")
install.packages("MSQC")

library(MVN)
library(MSQC)

mvn(vehiculosnumerico,univariateTest = "Lillie")# con este expresión realiza el test de Henze-Zirkler para normalidad K-dimensional

#COMANDO MVN del test (Normalidad Multivariante) nos indica que NO
# Este comando tambien incluye un resumen descripitivo, y test de normalidad marginales


mvn(vehiculosnumerico,mvnTest = "mardia") # con esta con el test de mardia

mvn(vehiculosnumerico,mvnTest = "royston") # test de royston


mvn(vehiculosnumerico[,c("desplazamiento","CV")],mvnTest = "royston",univariateTest = "Lillie",univariatePlot="qq",multivariatePlot = "persp")  
# test de royston


#Algunas de las herramientas para corregir normalidad es la transformacion de los datos

mvn(vehiculosnumerico,univariateTest = "Lillie",transform = "sqrt")


------------------------------------------------------------------------------------------------------------
### HIPOTESIS DE HOMOCEDASTICIDAD (Igualdad de las varianzas)

library(car)

lapply(vehiculosnumerico,leveneTest,coches2$origen) # se ve si para cada grupo es significativa (p-valor<0.05)

install.packages("biotools")
library(biotools)
boxM(coches3[,c("cilindros","desplazamiento","CV","peso","aceleracion","precio")],coches3$origen)

## Los grupos no tienen igualdad de varianzas p-value<0.05

-----------------------------------------------------------------

## CONTRASTAMOS LINEALIDAD
install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")
plot(vehiculosnumerico) ##Nube de puntos de todas las variables

chart.Correlation(vehiculosnumerico)  #Grafico de correlaciones conjunto


