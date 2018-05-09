# Data ----

# Index.1 = datos no estacionados y no ajustados al numero de dias trabajados
# Index.2 = datos estacionales, pero no ajustados al numero de dias trabajados
# Index.3 = datos no estacionados, pero ajustados al numero de dias trabajados
source("R/library.R")

data <- read_excel("Data/data.xlsx", sheet = "Hoja1")

data <- ts(data[,3:9],start=c(2006,1),freq=4)

plot(data[,c(1,4,7)],
     xlab = "Meses",
     main = "Index")

abline(v=2006:2017,lwd=.7,lty=2,col="Red")

# Veamos los datos de forma anual
data.anual <- aggregate(data[,c(1,4,7)],FUN=sum)

plot(data.anual,
     xlab = "Años",
     main = "Serie temporal agregada")
# Para ver los años con puntos (pero solo de uno en uno)
# datos.anyos<-window(data.anual,start=2006,freq=1)
# points(datos.anyos,col=4)

#Se ve una importante caida en tre los años 2010 y 2012.

#Veamos como se comporta la estacionalidad a lo largo de los cuartos de año.
par(mfrow=1,3)
boxplot((data[,1]-mean(data[,1]))~cycle(data[,1]),
        main="diagramas de cajas mensuales",
        xlab="Mes",
        ylab="Index 1")

boxplot((data[,4]-mean(data[,4]))~cycle(data[,4]),
        main="diagramas de cajas mensuales",
        xlab="Mes",
        ylab="Index 2")

boxplot((data[,7]-mean(data[,7]))~cycle(data[,7]),
        main="diagramas de cajas mensuales",
        xlab="Mes",
        ylab="Index 3")

# En los datos no estacionarios se ve una tendencia creciente, en el index 2 , son similares (lo cual es logico). Se ve que una tendencia creciente a los largo de un año, alcanzando en el ultimo cuarto los valores maximos

# Ahora veremos como evoluciona la media en lugar de la mediana

data.1.q <- tapply(data[,1], cycle(data[,1]), mean)
data.2.q <- tapply(data[,4], cycle(data[,4]), mean)
data.3.q <- tapply(data[,7], cycle(data[,7]), mean)

plot(data.1.q,
     type = 'l',
     ylab = "Index 1",
     xlab = "Cuarto",
     main = "Index 1 medio")

plot(data.2.q,
     type = 'l',
     ylab = "Index 2",
     xlab = "Cuarto",
     main = "Index 2 medio")

plot(data.3.q,
     type = 'l',
     ylab = "Index 3",
     xlab = "Cuarto",
     main = "Index 3 medio")

# Veremos si se trata de una serie multiplicativa o no

data.anual <- as.numeric(aggregate(data[,1],FUN=sum))
data.sd <- as.numeric(aggregate(data[,1],FUN=sd))

plot(data.anual,data.sd,
     col = "Red",
     pch = 20)

abline(lm(data.sd~data.anual),
       lty  = 2,
       ylab = "Desv. Tipica",
       xlab = "Años")

# Creo que sera multiplicativa porque son indices, pero....  ¯\_(ツ)_/¯

datos.descom=decompose(data[,1], type="multiplicative")
plot(datos.descom)

hist(datos.descom$random,main="Histograma de los residuos")


# vemos que los residuos estan centrados en el 1 (buena señal), pero si cambiamos el modelo a aditivo estan centrados en el 0, de modo que no se que modelo corresponde.



error <- data[,1]-datos.descom$seasonal*datos.descom$trend
se    <- sd(error,na.rm=TRUE)

plot(error,
     main = "Error de la descomposición",
     xlab = "Periodo",
     ylab = "Casos")

abline(h = c(-3*se,-2*se,2*se,3*se),lty=2,lwd=2,
       col = c("Grey","Black","Black","Grey"))


layout(matrix(c(1,2), 1, 2, byrow = TRUE))
plot(lag(as.vector(error)),error,pch=20,
     xlab=expression(e[t]),ylab=expression(e[t+1]),
     main="Analisis incorrelación")
plot(aggregate(error,FUN=sd),type="p",pch=20,
     xlab='Periodo',ylab="Desviación estándar",
     main="Analisis homocedasticidad")


# Si ejecutas esto de nuevo con un modelo aditivo fliparas
