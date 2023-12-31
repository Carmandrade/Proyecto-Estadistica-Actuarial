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
# Pruebas de Normalidad
# Las siguientes 3 pruebas utlizan para comprobar la normalidad de los datos, contribuyendo a responder el tercer objetivo, que es estimar la distribución de la correlación entre los indicadores.

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
# El objetivo de este codigo es conseguir el coeficiente de correlación, contribuyendo a responder el segundo objetivo, que es cuantificar el coeficiente de correlación entre las diferentes dimensiones.
cor_Salud_Educacion <- corr.test(datosSalud, datosEducacion, method = "spearman")
cor_EstandarVida_Educacion <- corr.test(datosEstandarVida, datosEducacion, method = "spearman")
cor_Salud_EstandarVida <- corr.test(datosSalud, datosEstandarVida, method = "spearman")

# Resultados
cat("Correlación Salud - Educación: ", cor_Salud_Educacion$r, "\n",
    "Correlación Estándar de Vida - Educación: " ,cor_EstandarVida_Educacion$r, "\n",
    "Correlación Salud - Estándar de Vida: ",cor_Salud_EstandarVida$r, "\n")

#Estos valores cercanos a 1 indican que todos los indicadores están fuertemente correlaciones.
################################################################################

#Promedios de los coeficientes de correlación
promSalud <- (cor_Salud_Educacion$r + cor_Salud_EstandarVida$r)/2
promEducacion <- (cor_EstandarVida_Educacion$r + cor_Salud_Educacion$r)/2
promEstandarvida <- (cor_EstandarVida_Educacion$r + cor_Salud_EstandarVida$r)/2

#Resultados
cat("Promedio correlación Salud: ", promSalud, "\n",
    "Promedio correlación Educación: " ,promEducacion, "\n",
    "Promedio correlación Estándar de Vida: ",promEstandarvida, "\n")

################################################################################
#Intervalos de confianza con estabilización de la varianza
#El siguiente codigo contribuye a responder la pregunta central. 
'''
Calcula el intercalo de confianza de una correlación.
r:= Coeficiente de correlación.
nivelSignificancia:= Nivel de significancia (alpha). Como decimales, no en porcentaje.
cantidadMuestra:= Cantidad de datos en la muestra.
'''
calculaIntervaloConfianza <- function(r,nivelSignificancia,cantiadMuestra) {
  return(data.frame(limInf=tanh(atanh(r)-qnorm(1-(nivelSignificancia)/2)/sqrt(cantiadMuestra)),
                    limSup=tanh(atanh(r)+qnorm(1-(nivelSignificancia)/2)/sqrt(cantiadMuestra))))
}

alpha <- 0.05 
n <- length(datosEducacion)
intervalo_Salud_Educacion<- calculaIntervaloConfianza(cor_Salud_Educacion$r,alpha,n)
intervalo_Salud_Educacion
intervalo_EstandarVida_Educacion<- calculaIntervaloConfianza(cor_EstandarVida_Educacion$r,alpha,n)
intervalo_EstandarVida_Educacion
intervalo_Salud_EstandarVida<- calculaIntervaloConfianza(cor_Salud_EstandarVida$r,alpha,n)
intervalo_Salud_EstandarVida

################################################################################
#Bootstrap
#El siguiente método tienen como objetivo encontrar la distribución de las correlaciones entre los indicadores, respondiendo al objetivo 3.

library("boot")
set.seed(0)

datos_Salud_Educacion <- data.frame(datosSalud,datosEducacion)
datos_Educacion_EstandarVida <- data.frame(datosEducacion, datosEstandarVida)
datos_EstandarVida_Salud <- data.frame(datosEstandarVida, datosSalud)

b_Salud_Educacion <- boot(datos_Salud_Educacion, R = 1000,
           statistic = function(datos_Salud_Educacion, i) {
             cor(datos_Salud_Educacion[i, "datosSalud"], 
                 datos_Salud_Educacion[i, "datosEducacion"], method='spearman')
           }
)
b_Salud_Educacion
boot.ci(b_Salud_Educacion, type = c("norm", "basic", "perc", "bca")) 



g1<- ggplot(as.data.frame(b_Salud_Educacion$t), aes(x = V1)) +
  geom_density( fill = "#efa7a7") +
    labs(x = "Correlación Salud - Educación",
       y = "Densidad") + theme(plot.margin = unit(c(5, 5, 5, 5), "mm"))+
  theme_minimal() 
         
print(g1)
         
#------------------------------------------------------------------------------
b_Educacion_EstandarVida <- boot(datos_Educacion_EstandarVida, R = 1000,
                          statistic = function(datos_Educacion_EstandarVida, i) {
                            cor(datos_Educacion_EstandarVida[i, "datosEducacion"], 
                                datos_Educacion_EstandarVida[i, "datosEstandarVida"], method='spearman')
                          }
)
b_Educacion_EstandarVida$
boot.ci(b_Educacion_EstandarVida, type = c("norm", "basic", "perc", "bca")) 

g2<- ggplot(as.data.frame(b_Educacion_EstandarVida$t), aes(x = V1)) +
  geom_density( fill = "#C2D7A7") +
  labs(x = "Correlación: Educación - Estándar de Vida",
       y = "Densidad") + theme(plot.margin = unit(c(5, 5, 5, 5), "mm"))+
  theme_minimal() 

print(g2)
#-------------------------------------------------------------------------------
b_EstandarVida_Salud <- boot(datos_EstandarVida_Salud, R = 1000,
                          statistic = function(datos_EstandarVida_Salud, i) {
                            cor(datos_EstandarVida_Salud[i, "datosEstandarVida"], 
                                datos_EstandarVida_Salud[i, "datosSalud"], method='spearman')
                          }
)
b_EstandarVida_Salud
boot.ci(b_EstandarVida_Salud, type = c("norm", "basic", "perc", "bca")) 
g3<- ggplot(as.data.frame(b_Educacion_EstandarVida$t), aes(x = V1)) +
  geom_density( fill = "#9cadce") +
  labs( x = "Correlación: Educación - Estándar de Vida",
       y = "Densidad") + theme(plot.margin = unit(c(5, 5, 5, 5), "mm"))+
  theme_minimal() 

print(g3)

#-------------------------------------------------------------------------------
#Resultados del Bootstrap:

ci_Salud_Educacion <- boot.ci(b_Salud_Educacion, type = c("norm", "basic", "perc", "bca"))
ci_Educacion_EstandarVida <- boot.ci(b_Educacion_EstandarVida, type = c("norm", "basic", "perc", "bca"))
ci_EstandarVida_Salud <- boot.ci(b_EstandarVida_Salud, type = c("norm", "basic", "perc", "bca"))
# Df de los intervalos de confianza
ci <- data.frame(
  Indicador = c("Salud-Educación", "Educación-EstándarVida", "EstándarVida-Salud"),
  Tipo_intervalo = c("Normal", "Basico", "Percentil", "BCa"),
  Lower = c(
    ci_Salud_Educacion$normal[1], ci_Educacion_EstandarVida$normal[1], ci_EstandarVida_Salud$normal[1],
    ci_Salud_Educacion$basic[1], ci_Educacion_EstandarVida$basic[1], ci_EstandarVida_Salud$basic[1],
    ci_Salud_Educacion$percent[1], ci_Educacion_EstandarVida$percent[1], ci_EstandarVida_Salud$percent[1],
    ci_Salud_Educacion$bca[1], ci_Educacion_EstandarVida$bca[1], ci_EstandarVida_Salud$bca[1]
  ),
  Upper = c(
    ci_Salud_Educacion$normal[2], ci_Educacion_EstandarVida$normal[2], ci_EstandarVida_Salud$normal[2],
    ci_Salud_Educacion$basic[2], ci_Educacion_EstandarVida$basic[2], ci_EstandarVida_Salud$basic[2],
    ci_Salud_Educacion$percent[2], ci_Educacion_EstandarVida$percent[2], ci_EstandarVida_Salud$percent[2],
    ci_Salud_Educacion$bca[2], ci_Educacion_EstandarVida$bca[2], ci_EstandarVida_Salud$bca[2]
  )
)

ci_invertido <- pivot_wider(ci, names_from = Indicador, values_from = Lower:Upper)

#write_xlsx(ci_invertido, path = "Intervalos Bootstrap.xlsx")

#se obtienen intervalos de confianza muy cercanos a los obtenidos por el metodo de estabilizacion de la varianza.

