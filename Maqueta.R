library(dplyr)
library(ggplot2)
library(nortest)
library(psych)
library(readxl)
library(gridExtra)
library(rstudioapi)


getwd()
# Para encontrar donde esta guardado este archivo
script_directory <- dirname(rstudioapi::getSourceEditorContext()$path)
script_directory
# Se toma esta dirección como el nuevo working directory
setwd(script_directory)
# Se carga el archivo, es importante que tanto Maqueta.R como 3 Dimensiones.xlsx estén guardados en el mismo lugar
indicadoresPobreza <- read_excel("3 Dimensiones.xlsx")

#Separamos los datos por variable
datosSalud <- indicadoresPobreza$Salud
datosEstandarVida <- indicadoresPobreza$`Estándares de Vida`
datosEducacion <- indicadoresPobreza$Educación

################################################################################


# Pruebas Shapiro-Wilk

shapiro_test_salud <- shapiro.test(datosSalud)
shapiro_test_educacion <- shapiro.test(datosEducacion)
shapiro_test_vida <- shapiro.test(datosEstandarVida)
#Resultados
cat("Variable Salud: p-value =", shapiro_test_salud$p.value, "\n",
"Variable Educación: p-value =", shapiro_test_educacion$p.value, "\n",
"Variable Estándar de Vida: p-value =", shapiro_test_vida$p.value, "\n")


# Pruebas de Normalidad D'Angostino

dangostino_test_salud <- lillie.test(datosSalud)
dangostino_test_educacion <- lillie.test(datosEducacion)
dangostino_test_vida <- lillie.test(datosEstandarVida)
#Resultados
cat("Variable Salud: Estadístico =", dangostino_test_salud$statistic, ", p-value =", dangostino_test_salud$p.value, "\n",
"Variable Educación: Estadístico =", dangostino_test_educacion$statistic, ", p-value =", dangostino_test_educacion$p.value, "\n",
"Variable Estándar de Vida: Estadístico =", dangostino_test_vida$statistic, ", p-value =", dangostino_test_vida$p.value, "\n")


# Pruebas Kolmorogov

kolmogorov_test_salud <- ks.test(datosSalud, "pnorm")
kolmogorov_test_educacion <- ks.test(datosEducacion, "pnorm")
kolmogorov_test_estandar_vida <- ks.test(datosEstandarVida, "pnorm")
#Resultados
cat("Variable Salud: Estadistico =", kolmogorov_test_salud$statistic,", p-value = ", kolmogorov_test_salud$p.value, "\n",
"Variable Educación: Estadistico =", kolmogorov_test_educacion$statistic,", p-value = ", kolmogorov_test_educacion$p.value, "\n",
"Variable EstandarVida: Estadistico =", kolmogorov_test_estandar_vida$statistic,", p-value = ", kolmogorov_test_estandar_vida$p.value, "\n")

#Todas las pruebas indican que los daos no siguen una distribución normal. 
#Por lo que se procede a calcular de la correlación de Spearman, el cual se adapta mejor para datos no normales.

##################################################################################
# Correlacion de Spearman
cor_Salud_Educacion <- corr.test(datosSalud, datosEducacion, method = "spearman")
cor_EstandarVida_Educacion <- corr.test(datosEstandarVida, datosEducacion, method = "spearman")
cor_Salud_EstandarVida <- corr.test(datosSalud, datosEstandarVida, method = "spearman")

# Resultados
cat("Correlación Salud - Educación: ", cor_Salud_Educacion$r, "\n",
    "Correlación Estándar de Vida - Educación: " ,cor_EstandarVida_Educacion$r, "\n",
    "Correlación Salud - Estándar de Vida: ",cor_Salud_EstandarVida$r, "\n")

#Estos valores cercanos a 1 indican que todos los indicadores están fuertemente correlaciones.

################################################################################
#Bootstrap

set.seed(1)
num_simulaciones <- 1000

#-------------------------------------------------------------------------------
# Salud - Educación

# Vector para almacenar los coeficientes de correlacion simulados
coefs_Salud_Educacion <- numeric(num_simulaciones)

# Simulaciones
for (i in 1:num_simulaciones) {
  muestra_Salud <- sample(datosSalud, length(datosSalud), replace = FALSE)
  muestra_Educacion <- sample(datosEducacion, length(datosEducacion), replace = FALSE)
  
  coefs_Salud_Educacion[i] <- cor(muestra_Salud, muestra_Educacion, method = "spearman")
}


# Histograma de los coeficientes de correlacion simulados
hist(coefs_Salud_Educacion, breaks = 30, col = "#efa7a7", main = "Dist. Coef. Correlación Método Bootstrap", 
     xlab =  "Coeficiente de correlación Salud - Educación", ylab = "Frecuencia")
abline(h = 0, col = "black", lty = 1)
 
#-------------------------------------------------------------------------------
# Educación - Estándar de Vida

# Vector para almacenar los coeficientes de correlacion simulados
coefs_Educacion_EstandarVida <- numeric(num_simulaciones)
tamano_muestra <- length(datosEducacion)

# Simulaciones
for (i in 1:num_simulaciones) {
  muestra_Educacion <- sample(datosEducacion, tamano_muestra, replace = FALSE)
  muestra_EstandarVida <- sample(datosEstandarVida, length(datosEstandarVida), replace = FALSE)
   
  coefs_Educacion_EstandarVida[i] <- cor(muestra_Educacion, muestra_EstandarVida, method = "spearman")
}

# Histograma de los coeficientes de correlacion simulados
hist(coefs_Educacion_EstandarVida, breaks = 30, col = "#C2D7A7", main = "Dist. Coef. Correlación Método Bootstrap", 
     xlab = "Coeficiente de correlación Educación - Estándar de Vida", ylab = "Frecuencia")
abline(h = 0, col = "black", lty = 1)


#-------------------------------------------------------------------------------
#Estándar de Vida - Salud

# Vector para almacenar los coeficientes de correlacion simulados
coefs_EstandarVida_Salud <- numeric(num_simulaciones)

# Simulaciones
for (i in 1:num_simulaciones) {
  muestra_EstandarVida <- sample(datosEstandarVida, length(datosEstandarVida), replace = FALSE)
  muestra_Salud <- sample(datosSalud, length(datosSalud), replace = FALSE)
  
  coefs_EstandarVida_Salud[i] <- cor(muestra_EstandarVida, muestra_Salud, method = "spearman")
}

# Histograma de los coeficientes de correlacion simulados
hist(coefs_EstandarVida_Salud, breaks = 30, col = "#623397", main = "Dist. Coef. Correlación Método Bootstrap", 
     xlab = "Coeficiente de correlación Estándar de Vida - Salud", ylab = "Frecuencia")
abline(h = 0, col = "black", lty = 1)

#Resultados
coefs <- data.frame(Salud_Educacion = coefs_Salud_Educacion, 
                    Educacion_EstandarVida = coefs_Educacion_EstandarVida,
                    EstandarVida_Salud = coefs_EstandarVida_Salud)


#write_xlsx(coefs, path = "Distribucion_Correlacion_Bootstrap.xlsx")
#-------------------------------------------------------------------------------



