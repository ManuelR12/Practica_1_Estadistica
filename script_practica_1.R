# Libreria especial para hacer carga automática de librerías
install.packages("easypackages")
library("easypackages")

# Listado de librerías requeridas
lib_req <- c("ggplot2", "dplyr", "readxl", "utils")

# Verificación, instalación y carga de librerías
easypackages::packages(lib_req)

# Leer el archivo Excel
base <- read_excel("salarios_maestros.xlsx")

# Ver las primeras filas para confirmar
head(base)

# Resumen de variables cuantitativas
resumen_general <- summary(base)
resumen_general
print(resumen_general)

# Calcular medidas adicionales
media_salario <- mean(base$Salarios)
mediana_salario <- median(base$Salarios)
varianza_salario <- var(base$Salarios)
desviacion_salario <- sd(base$Salarios)
minimo_salario <- min(base$Salarios)
maximo_salario <- max(base$Salarios)

# Imprimirlas
media_salario
mediana_salario
varianza_salario
desviacion_salario
minimo_salario
maximo_salario

# Crear intervalos para los salarios (5 intervalos, como 'hp')
intervalos_salarios <- cut(base$Salarios, breaks = 5, include.lowest = TRUE, right = FALSE)

# Calcular la tabla de frecuencias completa
tabla_frec <- as.data.frame(table(intervalos_salarios)) %>%
  rename(Intervalo = intervalos_salarios, Frec_Abs = Freq) %>%
  mutate(Frec_Rel = Frec_Abs / sum(Frec_Abs),
         Frec_Acum = cumsum(Frec_Abs),
         Frec_Rel_Acum = cumsum(Frec_Rel))

# Ver la tabla final
tabla_frec


# Punto 1:
# a) Histograma
hist(base$Salarios, col = "lightblue", main = "Histograma de Salarios", xlab = "Salarios Minimos", ylab = "Frecuencia")

# Graficar histograma a partir de la tabla de frecuencias
grafica_histograma = barplot(tabla_frec$Frec_Abs, 
        names.arg = tabla_frec$Intervalo,
        space = 0,
        col = "lightblue", 
        main = "Histograma de Salarios basado en la Tabla de Frecuencias", 
        xlab = "Salarios Minimos", 
        ylab = "Frecuencia", 
        border = "black")




