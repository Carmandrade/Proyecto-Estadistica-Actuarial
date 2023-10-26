library(tidyverse)
library(ggplot2)
library(readxl)
library(rstudioapi)
library(dplyr)
library(tibble)
library(stringr)
library(gridExtra)

getwd()
# Para encontrar donde esta guardado este archivo
script_directory <- dirname(rstudioapi::getSourceEditorContext()$path)
# Se toma esta dirección como el nuevo working directory
setwd(script_directory)
# Se carga el archivo, es importante que tanto Graficos.R como los archivos .xlsx estén guardados en el mismo lugar
indicadoresPobreza <- read_excel("Indicadores.xlsx")

#Se calcula el promedio de los indicadores y se convierten en una nueva tabla.
mean_df <- colMeans(indicadoresPobreza, na.rm = TRUE) %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "Columna") %>% 
  rename(Promedio = ".")

#-------------------------------------------------------------------------------
#Grafico 1


mean_df$Indicador <- c("Salud", "Salud", "Educación", "Educación", "Estándar de Vida","Estándar de Vida","Estándar de Vida","Estándar de Vida","Estándar de Vida","Estándar de Vida")

grafico1 <- ggplot(mean_df, aes(x = reorder(Columna, Promedio), y = Promedio, fill = Indicador)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Promedio, 1)), hjust = 1.1, vjust = 0.4, size = 4, colour = "black") +
  coord_flip() +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.y = element_text(angle = 0, hjust = 1, size = 10, colour = "black")) +
  labs(x = "Subindicadores", y = "Porcentaje global", title = "") +
  scale_y_continuous(labels = function(x) str_wrap(x, width = 11), breaks = seq(ceiling(min(mean_df$Promedio)), ceiling(max(mean_df$Promedio)), by = 4)) +
  scale_fill_manual(values = c("Salud" = "#efa7a7", "Educación" = "#C2D7A7", "Estándar de Vida"= "#9cadce"))


print(grafico1)

ggsave("Grafico1.pdf", grafico1, bg="white")
#-------------------------------------------------------------------------------
#Grafico 2
tabla_limpia <- read_excel("Tabla Limpia.xlsx")

# Se crea un vector con las traducciones de los nombres d ecada region
traducciones <- c("Europe and Central Asia" = "Europa y Asia Central",
                  "Arab States" = "Estados Árabes",
                  "Latin America and the Caribbean" = "América Latina y el Caribe",
                  "East Asia and the Pacific" = "Asia Oriental y el Pacífico",
                  "South Asia" = "Asia del Sur",
                  "Sub-Saharan Africa" = "África Subsahariana"
)

# Reemplaza los nombres con su traduccion respectiva en la tabla
tabla_limpia$Región <- traducciones[tabla_limpia$Región]

# Gráfico de cajas
grafico2 <- ggplot(tabla_limpia, aes(x = `Región`, y = `IPM (0 a 1)`)) +
  geom_boxplot(fill = "#ECAC7C") + 
  labs(title = "",
       x = "Región",
       y = "IPM 0-1") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.05)) + # Ajusta las etiquetas del eje y
  theme_bw() + #Cambia el fondo a blanco
  theme(plot.background = element_rect(fill = "white")) 

print(grafico2)
# Guarda los gráficos combinados como una imagen.
ggsave("Grafico2.pdf", grafico2, bg="white", w=5*1.6, h=5)

#-------------------------------------------------------------------------------
#Grafico 3

df <- read_excel("3 Dimensiones.xlsx")
p1 <- ggplot(df, aes(x=Salud, y=Educación)) +
  geom_point(color = "#efa7a7") +
  labs(x="Salud", y="Educación", 
       title="") +
  theme_minimal()

p2 <- ggplot(df, aes(x=Educación, y=`Estándares de Vida`)) +
  geom_point(color = "#C2D7A7") +
  labs(x="Educación", y="Estándar de Vida", 
       title="") +
  theme_minimal()
p3 <- ggplot(df, aes( x=`Estándares de Vida`, y=Salud)) +
  geom_point(color = "#9cadce") +
  labs(y="Salud", x="Estándar de Vida", 
       title="") +
  theme_minimal()

# Se combinan los gráficos:
grafico3<-grid.arrange(p1, p2, p3, nrow = 1)


ggsave("Grafico3.pdf", grafico3, h=5, w=5*1.8)

#-------------------------------------------------------------------------------
#Grafico 4
plot_salud <- ggplot(df, aes( x = Salud)) +
  geom_histogram(bins = 20, fill = "#efa7a7") +
  labs(title = "Distribución Salud", x = "Salud", y = "Valor Porcentual") +
  theme_minimal() +
  theme(plot.margin = unit(c(5, 5, 5, 5), "mm"))


plot_educacion <- ggplot(df, aes(x = Educación)) +
  geom_histogram(bins = 20, fill = "#C2D7A7") +
  labs(title = "Distribución Educación", x = "Educación", y = "Valor Porcentual") +
  theme_minimal() +
  theme(plot.margin = unit(c(5, 5, 5, 5), "mm"))

plot_estandar_vida <- ggplot(df, aes(x = `Estándares de Vida`)) +
  geom_histogram(bins = 20, fill = "#9cadce") +
  labs(title = "Distribución Estándar de Vida", x = "Estándar de Vida", y = "Valor Porcentual") +
  theme_minimal() +
  theme(plot.margin = unit(c(5, 5, 5, 5), "mm"))


histogramas<- grid.arrange(plot_salud, plot_educacion, plot_estandar_vida, ncol = 3)

ggsave(filename = "Grafico4.pdf", histogramas, h=2.5, w=5*1.8)







