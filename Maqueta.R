library(dplyr)
library(ggplot2)
library(nortest)
library(psych)
library(readxl)
library(gridExtra)


indicadoresPobreza <- read_excel("C:/Users/Maricruz Vásquez/Documents/UCR/CA0303 Estadistica/Proyecto/3 Indicadores.xlsx")

#Separamos los datos por variable
datosSalud <- indicadoresPobreza$Salud
datosEstandarVida <- indicadoresPobreza$`Estándares de Vida`
datosEducacion <- indicadoresPobreza$Educación



# Grafica Dispersion: Salud-Educacion
plot_Salud_Educacion <- ggplot(data = indicadoresPobreza, aes(x = datosSalud, y = datosEducacion)) +
  geom_point(colour="#3182bd", alpha = 0.8) +
  labs(x = "Salud", y = "Educación")
options(repr.plot.width = 6, repr.plot.height = 4)

print(plot_Salud_Educacion)

# Grafica Dispersion: Educacion - Estandar de Vida
plot_Educacion_EstandarVida <- ggplot(data = indicadoresPobreza, aes(x = datosEducacion, y = datosEstandarVida)) +
  geom_point(colour="#319b1d",alpha = 0.8) +
  labs(x = "Educación", y = "Estádar Vida")
options(repr.plot.width = 6, repr.plot.height = 4)

print(plot_EstandarVida_Educacion)

# Grafica Dispersion: Estandares de Vida-Salud
plot_EstandarVida_Educacion <- ggplot(data = indicadoresPobreza, aes(x = datosEstandarVida, y = datosSalud)) +
  geom_point(colour="#623397",alpha = 0.8) +
  labs(x = "Estándar de Vida", y = "Salud")
options(repr.plot.width = 6, repr.plot.height = 4)

print(plot_EstandarVida_Educacion)


dispersion <- grid.arrange(plot_Salud_Educacion, plot_Educacion_EstandarVida, plot_EstandarVida_Educacion, ncol = 3)

ggsave(filename = "Dispersion3Dimensiones.pdf", dispersion)

# Visualización del plot 1x3
layout(matrix(1:3, nrow = 1))
par(mar = c(5, 4, 4, 2))

# Plot 1: Distribución Salud
hist(datosSalud, breaks = 20, col = "#3182bd", main = "Distribución", xlab = "Salud", ylab = "Valor")
abline(h = -0.01, col = "black", lty = 1)

# Plot 2: Distribución Educación
hist(datosEducacion, breaks = 20, col = "#319b1d", main = "Distribución", xlab = "Educación", ylab = "Valor")
abline(h = -0.01, col = "black", lty = 1)

# Plot 3: Distribución Estándar Vida
hist(datosEstandarVida, breaks = 20, col = "#623397", main = "Distribución", xlab = "Estándar Vida", ylab = "Valor")
abline(h = -0.01, col = "black", lty = 1)

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



