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
#Método Bootstrap para simular la distribución de los coeficientes de correlación

set.seed(123)
num_simulaciones <- 1000

#-------------------------------------------------------------------------------
# Salud - Educación

# Vector para almacenar los coeficientes de correlacion simulados
coefs_Salud_Educacion <- numeric(num_simulaciones)
tamano_muestra <- length(datosSalud)

# Simulaciones
for (i in 1:num_simulaciones) {
  muestra_salud <- sample(datosSalud, tamano_muestra, replace = FALSE)
  coef_Salud_Educacion  <- cor(muestra_salud, datosEducacion, method = "spearman")
  coefs_Salud_Educacion[i] <- coef_Salud_Educacion 
}

# Histograma de los coeficientes de correlacion simulados
hist(coefs_Salud_Educacion, breaks = 30, col = "#3182bd", main = "Dist. Coef. Correlación Método Bootstrap", xlab = "Salud - Educación", ylab = "Valor")
abline(h = 0, col = "black", lty = 1)

#-------------------------------------------------------------------------------
# Educación - Estándar de Vida

# Vector para almacenar los coeficientes de correlacion simulados
coefs_Educacion_EstandarVida <- numeric(num_simulaciones)
tamano_muestra <- length(datosEducacion)

# Simulaciones
for (i in 1:num_simulaciones) {
  muestra_Educacion <- sample(datosEducacion, tamano_muestra, replace = FALSE)
  coef_Educacion_EstandarVida  <- cor(muestra_Educacion, datosEstandarVida, method = "spearman")
  coefs_Educacion_EstandarVida[i] <- coef_Educacion_EstandarVida
}

# Histograma de los coeficientes de correlacion simulados
hist(coefs_Educacion_EstandarVida, breaks = 30, col = "#319b1d", main = "Dist. Coef. Correlación Método Bootstrap", xlab = "Educación - Estándar de Vida", ylab = "Valor")
abline(h = 0, col = "black", lty = 1)


#-------------------------------------------------------------------------------
#Estándar de Vida - Salud
# Vector para almacenar los coeficientes de correlacion simulados
coefs_EstandarVida_Salud <- numeric(num_simulaciones)

tamano_muestra <- length(datosEstandarVida)

# Simulaciones
for (i in 1:num_simulaciones) {
  muestra_EstandarVida <- sample(datosEstandarVida, tamano_muestra, replace = FALSE)
  coef_EstandarVida_Salud  <- cor(muestra_EstandarVida, datosSalud, method = "spearman")
  coefs_EstandarVida_Salud[i] <- coef_EstandarVida_Salud
}

# Histograma de los coeficientes de correlacion simulados
hist(coefs_EstandarVida_Salud, breaks = 30, col = "#623397", main = "Dist. Coef. Correlación Método Bootstrap", xlab = "Estándar de Vida - Salud", ylab = "Valor")
abline(h = 0, col = "black", lty = 1)


