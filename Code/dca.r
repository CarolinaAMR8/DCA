rm(list=ls(all=TRUE)) #Este es el primer comando para trabajar con al memoria desocupada

#generando estructuras de carpetas 
file.path(".", "graficos") #esto es una funcion de R para crear carpeta sin problemas de rutas
dir.create("./graficos", showWarnings = FALSE, recursive = TRUE) #para crear carpetas 

dir.create(file.path(".","salidas","graficos"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(".","salidas","excel"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(".","salidas","pdf"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(".","salidas","html"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(".","script"), showWarnings = FALSE, recursive = TRUE)

dir.create(file.path(".","rmd"), showWarnings = FALSE, recursive = TRUE) #esta extension mezcla codigo con letra
dir.create(file.path(".","qmd"), showWarnings = FALSE, recursive = TRUE) #esta extension mezcla codigo con letra
dir.create(file.path(".","BD"), showWarnings = FALSE, recursive = TRUE)

#Datos 
#esta es un a forma de hacerlo
datos <- data.frame(
  tratamiento = factor(rep(c("3D0K1", "3D0K5", "3D0K4", "3D0K7", "3D0K13", "Compuesto"), each = 5)),
  nitrogeno = c(19.4, 32.6, 27.0, 32.1, 33.0,
  17.7, 24.8, 27.9, 25.2, 24.3,
17.0, 19.4, 9.1, 11.9, 15.8,
20.7, 21.0, 20.5, 18.8, 18.6,
14.3, 14.4, 11.8, 11.6, 14.2,
17.3, 19.4, 19.1, 16.9, 20.8)
)

datos$tratamiento_c<-as.character(datos$tratamiento) #se crearon caracteres
str(datos)

#otra forma de hacerlo es importandolos 
library(readxl) #esta es la libreria de excel
datosexcel<-read_excel(file.path(".", "BD", "dca.xlsx"))

str(datosexcel) #me muestra que los datos estan en tibble y esto no se puede leer
#en todos los computadores asi que se debe hacer un paso extra
datosexcel<-as.data.frame(datosexcel) #pasar los datos a un formato estandar
# como es importante que los reconosca como caracteres esntonces:
datosexcel$tratamiento_f<-as.factor(datosexcel$tratamiento) #se convirtio a factor
str(datosexcel) #despues de hacer lo de arriba aparace como data.frame 
#en este caso se dejaron como caracteres y como factor para efectos practicos 
#las ventajas del factor es que se le pueden preguntar cosas
levels(datosexcel$tratamiento_f) #para preguntar niveles
nlevels(datosexcel$tratamiento_f) #para preguntar numero de niveles

#Estadistica descriptiva 
#funciones previas

descriptiva<-aggregate(nitrogeno~tratamiento,data=datos,mean)
descriptiva

#personalizando aggregate forma 1
descriptivasd<-aggregate(nitrogeno~tratamiento,data=datos,sd)
descriptivasd
#para no hacer esto uno por uno. puedo utilizar una funcion parfa que sea 
#mas profesional
descriptivasd<-aggregate(nitrogeno~tratamiento,data=datos,function(x){
  media<-mean(x)
  sd<-sd(x)
  n<-length(x)
  # ese intervalo de confianza de t(alpha/2,gl)*s/raiz(n)
  delta<-qt(0.025,n-1, lower.tail = FALSE)*sd/sqrt(n)
  return(c(Media=media,Ds=sd,N=n, Delta=delta,inferior=media-delta,
  Superior=media+delta,Maximo=max(x),Minimo=min(x),Mediana=median(x))) #el comando para que devuelva "devuelva un vector(c) con nombre media"
})
descriptivasd
#una funcion simpre devuelve la ultima linea. hay que decirle de formaexplicita
#lo que tiene que hacer (return)

#tambien podemos hacer la funcion por fuera para que no quede tan lleno
#personalizando a aggregate forma 2
resumen<-function(x){
  media<-mean(x)
  sd<-sd(x)
  n<-length(x)
  # ese intervalo de confianza de t(alpha/2,gl)*s/raiz(n)
  delta<-qt(0.025,n-1, lower.tail = FALSE)*sd/sqrt(n)
  return(c(Media=media,Ds=sd,N=n, Delta=delta,inferior=media-delta,
  Superior=media+delta,Maximo=max(x),Minimo=min(x),Mediana=median(x))) 
}
#para usar la funcion de esta forma tenermos que:
descriptiva<-aggregate(nitrogeno~tratamiento,data=datos,resumen)
descriptivasd

#otra forma es con tapply aunque se traba para bases de datos grandes 
descriptiva_tapply<-tapply(datos$nitrogeno, datos$tratamiento,resumen)
descriptiva_tapply
descriptiva2<-do.call(rbind,descriptiva_tapply)

#esta forma con tapply devuelve una lista 

#Formas de acceder a una lista 
#asi podemos ver como quedan los datos

str(descriptivasd)
str(descriptiva_tapply)

#para ver la lista

descriptiva_tapply$`3D0K1`
#o tambien se accede asi
descriptiva_tapply[[1]]
names(descriptiva_tapply) #para ver los nombres
descriptiva_tapply[["3D0K1"]]
str(descriptiva2)


#graficos
boxplot(nitrogeno~tratamiento,data = datos)

#el numero de oro o la proporcion aurea
num_oro<-(1+sqrt(5))/2
num_oro
#para exportar graficas en jpg, organizar tamaños, etc..
jpeg(file.path(".","salidas","graficos","barras.jpg"),
units="px",height = 5*1200,width =num_oro*5*1200,res=1200,quality=100) 

#Paleta de colores
color_barras <-"#397F9F"
color_datos <-"#6485BC"
color_mediana <-"#D55E00"
color_media <-"#111827"

#Estadisticos
media <-descriptiva$nitrogeno[, "Media"]
mediana <-descriptiva$nitrogeno[, "Mediana"]
inferior <-descriptiva$nitrogeno[, "inferior"]
superior <-descriptiva$nitrogeno[, "Superior"]
names_trt <-descriptiva$tratamiento

bp<-barplot(media,xlab=expression
("Ceptas de " * italic("Rhizobium") *  "sp."),
ylab="Nitrogeno (mg)",ylim=c(0,40),names.arg = names_trt,
cex.axis = 0.7,
col=adjustcolor(color_barras,alpha.f = 0.3)) #para poner color y transparencia
str(bp)
#se hace con las medias
#se le cambia el color de las barras, 
# se ponen los nombres en y y en x
#poner nombres en cursiva
#poner barras de error

colnames(descriptivasd$nitrogeno) #para verificar como se llaman las columnas
arrows(bp,inferior,bp,superior,length = 0.05, lwd = 1.5,code=3)
#para poner flecha, organizar grozor de la grafica
#organizar tamaño flechas

#si no se quiere con flechas sino con otra cosa
arrows(bp,inferior,bp,superior,length = 0.05, lwd = 1.5,code=3, angle=45)
#order(datos$tratamiento)#ordenar los datos por tratamiento
points(x=rep(bp,each=5),y=datos$nitrogeno[order(datos$tratamiento)],pch=24,cex=0.5, col=color_datos) 
#tambien puedo hacer puntos que solo resalten la mediana, media
points(x=bp, y=mediana,pch=18,cex=0.9,col=color_mediana)
points(x=bp, y=media,pch=16,cex=0.9,col=color_media)

#Leyenda
legend("topright",legend = c("Observaciones", "Media", "Mediana"),
pch = c(24, 16, 18),pt.bg = c(color_datos, NA, NA),
col = c(color_datos, color_media, color_mediana),
pt.cex = c(0.8, 1.0, 1,2),bty = "n",cex = 0.85)

#Esto es un epígrafe:
#Fig 1. Grafico descriptivo para el contenido de nitrogebo en 6 tratamientos de cepas de Rhizobium. 
#.      Las barras indican la media del tratamiento la cual tambien esta resaltada en negro. La mediana 
#       se resalta en color naranja. Las observaciones originales estan en color azul y las barras 
# .     de error indican los limites del intervalo de confianza de 95% 

#colours() para ver todos los colores, si no quiero ver todos los colores
#entonces rgb permite definir cualquier color
#help(points) para obtener ayuda con los puntos
#hay que cerrar el jpg
dev.off()

help(barplot) #para ver el funcionamiento de todas las opciones

#barplot(descriptiva2[,"Media"]) #se hace con las medias
