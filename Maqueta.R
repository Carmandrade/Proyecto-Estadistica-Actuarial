library(dplyr)
library(ggplot2)
library(nortest)
library(psych)
library(readxl)
library(ggplot2)


indicadoresPobreza <- read_excel("3 Dimensiones.xlsx")

#Separamos los datos por variable
datosSalud <- indicadoresPobreza$Salud
datosEstandarVida <- indicadoresPobreza$`Estandares de Vida`
datosEducacion <- indicadoresPobreza$Educacion

# Scatter plot: Salud-Educacion
plot_Salud_Educacion <- ggplot(data = indicadoresPobreza, aes(x = datosSalud, y = datosEducacion)) +
  geom_point(colour="#3182bd", alpha = 0.8) +
  labs(x = "Salud", y = "Educacion")
options(repr.plot.width = 6, repr.plot.height = 4)

print(plot_Salud_Educacion)

# Scatter plot: Educacion - Estandar de Vida
plot_Educacion_EstandarVida <- ggplot(data = indicadoresPobreza, aes(x = datosEduacion, y = datosEstandarVida)) +
  geom_point(colour="#319b1d",alpha = 0.8) +
  labs(x = "Educacion", y = "Estadar Vida")
options(repr.plot.width = 6, repr.plot.height = 4)

print(plot_EstandarVida_Educacion)

# Scatter plot: Estandares de Vida-Salud
plot_EstandarVida_Educacion <- ggplot(data = indicadoresPobreza, aes(x = datosEstandarVida, y = datosSalud)) +
  geom_point(colour="#623397",alpha = 0.8) +
  labs(x = "Estandar de Vida", y = "Salud")
options(repr.plot.width = 6, repr.plot.height = 4)

print(plot_EstandarVida_Educacion)


# Visualizacion del plot 1x3
layout(matrix(1:3, nrow = 1))
par(mar = c(5, 4, 4, 2))

# Plot 1: Distribucion Salud
hist(datosSalud, breaks = 20, col = "#3182bd", main = "Distribucion", xlab = "Salud", ylab = "Valor")
abline(h = -0.01, col = "black", lty = 1)

# Plot 2: Distribucion Educacion
hist(datosEducacion, breaks = 20, col = "#319b1d", main = "Distribucion", xlab = "Educacion", ylab = "Valor")
abline(h = -0.01, col = "black", lty = 1)

# Plot 3: Distribucion Estandar Vida
hist(datosEstandarVida, breaks = 20, col = "#623397", main = "Distribucion", xlab = "Estandar Vida", ylab = "Valor")
abline(h = -0.01, col = "black", lty = 1)

# Pruebas Shapiro-Wilk

shapiro_test_salud <- shapiro.test(datosSalud)
shapiro_test_educacion <- shapiro.test(datosEducacion)
shapiro_test_vida <- shapiro.test(datosEstandarVida)
cat("Variable Salud: p-value =", shapiro_test_salud$p.value, "\n",
"Variable Educacion: p-value =", shapiro_test_educacion$p.value, "\n",
"Variable Estandar de Vida: p-value =", shapiro_test_vida$p.value, "\n")



# Pruebas de Normalidad
kolmogorov_test_salud <- lillie.test(datosSalud)
cat("Variable Salud: Estadistico =", kolmogorov_test_salud$statistic, ", p-value =", kolmogorov_test_salud$p.value, "\n")

kolmogorov_test_educacion <- lillie.test(datosEducacion)
cat("Variable Educacion: Estadistico =", kolmogorov_test_educacion$statistic, ", p-value =", kolmogorov_test_educacion$p.value, "\n")

kolmogorov_test_vida <- lillie.test(datosEstandarVida)
cat("Variable Estandar de Vida: Estadistico =", kolmogorov_test_vida$statistic, ", p-value =", kolmogorov_test_vida$p.value, "\n")

# Correlacion de Spearman
cor_Salud_Educacion <- corr.test(datosSalud, datosEducacion, method = "spearman")
cor_EstandarVida_Educacion <- corr.test(datosEstandarVida, datosEducacion, method = "spearman")
cor_Salud_EstandarVida <- corr.test(datosSalud, datosEstandarVida, method = "spearman")

# Resultados
print(cor_EstandarVida_Educacion)


#Bootstrap

set.seed(123)
num_simulaciones <- 1000

#-------------------------------------------------------------------------------
# Salud - Educacion

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
hist(coefs_Salud_Educacion, breaks = 30, col = "#3182bd", main = "Dist. Coef. Correlacion Metodo Bootstrap", xlab = "Salud - Educacion", ylab = "Valor")
abline(h = 0, col = "black", lty = 1)

#-------------------------------------------------------------------------------
# Educacion - Estandar de Vida

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
hist(coefs_Educacion_EstandarVida, breaks = 30, col = "#319b1d", main = "Dist. Coef. Correlacion Metodo Bootstrap", xlab = "Educacion - Estandar de Vida", ylab = "Valor")
abline(h = 0, col = "black", lty = 1)


#-------------------------------------------------------------------------------
#Estandar de Vida - Salud
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
hist(coefs_EstandarVida_Salud, breaks = 30, col = "#623397", main = "Dist. Coef. Correlacion Metodo Bootstrap", xlab = "Estandar de Vida - Salud", ylab = "Valor")
abline(h = 0, col = "black", lty = 1)



