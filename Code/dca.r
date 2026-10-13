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
