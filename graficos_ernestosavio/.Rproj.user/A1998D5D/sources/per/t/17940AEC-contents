# Instalo los paquetes necesarios (si aún no los tengo instalados)
# install.packages("googledrive")
# install.packages("readxl")
# install.packages("tidyverse")

# Cargo los paquetes que voy a usar
library(magrittr)  # Para usar %>%
library(dplyr)     # Para usar %>% y funciones de manipulación de datos
library(tidyverse)
library(ggplot2)
library(janitor)

datos <- readxl::read_excel("Datos_LP.xlsx", 
                            col_names = FALSE, 
                            skip = 3)

# Veo la estructura del dataset
str(datos)


# Fijo el dataset
attach(datos)

# ---------------------- tabla de frecuencia provincias -------------------
# Renombrar la columna de interés
datos <- datos %>%
  rename(provincias = ...2)

# Crear la tabla de distribución de frecuencias usando tabyl
tabla <- datos %>%
  tabyl(provincias) %>%
  arrange(desc(n))  # Ordenar por frecuencia de manera descendente

# Adornar la tabla de frecuencias
tabla <- tabla %>%
  adorn_totals("row") %>%  # Agregar fila de totales
  adorn_pct_formatting(digits = 1) %>%  # Formatear los porcentajes con 1 decimal
  rename(
    "Provincias" = provincias,
    "Cantidad de relevaciones" = n,
    "Porcentaje de relevaciones" = percent
  )

print(tabla)

# ---------------------- tabla de frecuencia barrios ----------------------
# Renombrar la columna de interés
datos <- datos %>%
  rename(barrios = ...3)

# Crear una nueva columna que combine provincias y barrios
datos <- datos %>%
  mutate(barrio_provincia = paste(barrios, "-", provincias))

# Crear la tabla de distribución de frecuencias usando tabyl y ordenar por frecuencia
tabla2 <- datos %>%
  tabyl(barrio_provincia) %>%
  arrange(desc(n))  # Ordenar por frecuencia de manera descendente

# Adornar la tabla de frecuencias
tabla2 <- tabla2 %>%
  adorn_totals("row") %>%  # Agregar fila de totales
  adorn_pct_formatting(digits = 1) %>%  # Formatear los porcentajes con 1 decimal
  rename(
    "Barrio popular - Provincia " = barrio_provincia,
    "Cantidad de relevaciones" = n,
    "Porcentaje de relevaciones" = percent
  )

print(tabla2)

# -------------------- Formas de obtener el agua en las viviendas --------------------
# Usar rename() para cambiar el nombre de la columna
datos <- datos %>%
  rename(formaObtieneAgua = ...24)

datos %>%
  mutate( formaObtieneAguaFactor = factor(formaObtieneAgua,
                                          labels = c("Camion Cisterna", "Pozo", "Red publica", "Conexion informal", "Tanque comunitario", "No posee", "No sabe")
  )) %>%
  ggplot() + 
  
  #aes(x = formaObtieneAguaFactor) + # Frecuencias absolutas
  aes(x = reorder(formaObtieneAguaFactor, formaObtieneAguaFactor, function(x) -length(x))) + # Ordenar según frecuencia
  #aes(x = tiempo, y = ..count.. / sum(..count..)) + # Porcentajes
  # aes(x = reorder(tiempo, tiempo, function(x) -length(x)), 
  #		y = ..count.. / sum(..count..)) +  # Porcentajes ordenados según frecuencia
  #scale_y_continuous(labels = scales::percent) +    # Eje para porcentajes
  
  geom_bar(width = 0.75,   # Ancho de barras
           fill = 'lightblue',  # Color de relleno 
           col = "black",  # Color de línea
           alpha = 0.6) +  # Transparencia
  
  labs(y = "Cantidad de viviendas", x = "Obtencion del agua") + # Nombres de ejes
  
  ggtitle("Distribución de las Formas de Obtención de Agua\nBarrios populares de Argentina, 2022") +
  
  coord_flip() + # Barras horizontales o verticales
  
  theme_classic() + # Temas preconfigurados de R https://r-charts.com/ggplot2/themes/

  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20), # Centrar y poner en negrita el título
    plot.caption = element_text(hjust = 0, face = "italic", size = 12), # Alinear la fuente a la izquierda y en cursiva
    axis.title.x = element_text(size = 15), # Aumentar tamaño del título del eje X
    axis.title.y = element_text(size = 15), # Aumentar tamaño del título del eje Y
    axis.text.x = element_text(size = 12),  # Aumentar tamaño de las etiquetas del eje X
    axis.text.y = element_text(size = 12)   # Aumentar tamaño de las etiquetas del eje Y
  ) +
  labs(caption = "Fuente: Observatorio Villero - La Poderosa 2020 ~ 2022") # Añadir la fuente

# ---------------------------- presion del agua ----------------------------
# Usar rename() para cambiar el nombre de la columna
datos <- datos %>%
  rename(presionAgua = ...26)

datos %>%
  mutate( presionAguaFactor = factor(presionAgua)) %>%
  ggplot() + 
  
  aes(x = reorder(presionAguaFactor, presionAguaFactor, function(x) -length(x))) + # Ordenar según frecuencia
  
  geom_bar(width = 0.75,   # Ancho de barras
           fill = 'lightblue',  # Color de relleno 
           col = "black",  # Color de línea
           alpha = 0.6) +  # Transparencia
  
  labs(y = "Cantidad de viviendas", x = "Tipo de Presion") + # Nombres de ejes
  
  ggtitle("Presion del Agua\nBarrios populares de Argentina, 2022") +
  
  coord_flip() + # Barras horizontales o verticales
  
  theme_classic() +  # Temas preconfigurados de R https://r-charts.com/ggplot2/themes/
  
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20), # Centrar y poner en negrita el título
    plot.caption = element_text(hjust = 0, face = "italic", size = 12), # Alinear la fuente a la izquierda y en cursiva
    axis.title.x = element_text(size = 15), # Aumentar tamaño del título del eje X
    axis.title.y = element_text(size = 15), # Aumentar tamaño del título del eje Y
    axis.text.x = element_text(size = 12),  # Aumentar tamaño de las etiquetas del eje X
    axis.text.y = element_text(size = 12)   # Aumentar tamaño de las etiquetas del eje Y
  ) +
  labs(caption = "Fuente: Observatorio Villero - La Poderosa 2020 ~ 2022") # Añadir la fuente


# -------------------------- Cantidad de integrantes por viviendas --------------------------
# Renombrar la columna
datos <- datos %>%
  rename(cantHabiViv = ...6)

# Asegurarse de que no haya valores NA en la columna
datos <- datos %>%
  filter(!is.na(cantHabiViv))

# Crear el gráfico
grafico <- ggplot(datos) +
  aes(x = cantHabiViv) + 
  geom_bar(width = 0.10, fill = '#7ed021', col = "black", alpha = 0.6) +
  scale_x_continuous(breaks = seq(1, max(datos$cantHabiViv, na.rm = TRUE), by = 1)) + # Definir los límites y los saltos del eje X
  labs(y = "Cantidad de viviendas", 
       x = "Numero de personas por vivienda") +
  ggtitle("Distribución de Personas por Viviendas\nBarrios populares de Argentina, 2022") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20), # Centrar y poner en negrita el título
    plot.caption = element_text(hjust = 0, face = "italic", size = 12), # Alinear la fuente a la izquierda y en cursiva
    axis.title.x = element_text(size = 15), # Aumentar tamaño del título del eje X
    axis.title.y = element_text(size = 15), # Aumentar tamaño del título del eje Y
    axis.text.x = element_text(size = 12),  # Aumentar tamaño de las etiquetas del eje X
    axis.text.y = element_text(size = 12)   # Aumentar tamaño de las etiquetas del eje Y
  ) +
  labs(caption = "Fuente: Observatorio Villero - La Poderosa 2020 ~ 2022") # Añadir la fuente

# Mostrar el gráfico
print(grafico)

# --------------------------- tendido electrico dentro ---------------------------
datos <- datos %>%
  rename(tendidoElec = ...51) %>%
  mutate(tendidoElecFactor = factor(tendidoElec))

# Agrupar los datos y calcular los porcentajes
datos_porcentaje <- datos %>%
  group_by(tendidoElecFactor) %>%
  summarize(count = n()) %>%
  mutate(percentage = count / sum(count) * 100,
         label = paste0(round(percentage, 1), "%"))

# Crear el gráfico de torta
ggplot(datos_porcentaje, aes(x = "", y = count, fill = tendidoElecFactor)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 5) + # Aumentar tamaño de las etiquetas
  labs(title = "Tipo de Tendido Electrico dentro de la Vivienda\nBarrios populares de Argentina, 2022",
       fill = "Tipo de Tendido Electrico dentro de la Vivienda") +
  theme_void() +
  scale_fill_brewer(palette = "Set3") + # Utiliza una paleta de colores predefinida
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20), # Centrar y poner en negrita el título y aumentar tamaño
    plot.caption = element_text(hjust = 0, face = "italic", size = 14), # Alinear la fuente a la izquierda, en cursiva y aumentar tamaño
    legend.title = element_text(size = 14), # Aumentar tamaño del título de la leyenda
    legend.text = element_text(size = 12) # Aumentar tamaño del texto de la leyenda
  ) +
  labs(caption = "Fuente: Observatorio Villero - La Poderosa 2020 ~ 2022") # Añadir la fuente

# --------------------------- Tipo de conexion electrica en la vivienda ---------------------------
datos <- datos %>%
  rename(tipoConexionElectrica = ...50) %>%
  mutate(tipoConexionElectrica = factor(tipoConexionElectrica))

# Agrupar los datos y calcular los porcentajes
datos_porcentaje <- datos %>%
  group_by(tipoConexionElectrica) %>%
  summarize(count = n()) %>%
  mutate(percentage = count / sum(count) * 100,
         label = paste0(round(percentage, 1), "%"))

# Crear el gráfico de torta
ggplot(datos_porcentaje, aes(x = "", y = count, fill = tipoConexionElectrica)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 4.5) +
  labs(title = "Tipo de Conexion a la Electricidad por Vivienda\nBarrios populares de Argentina, 2022.",
       fill = "Tipo de conexion",
       caption = "Fuente: Observatorio Villero - La Poderosa 2020 ~ 2022") +
  theme_void() +
  scale_fill_brewer(palette = "Set3") +  # Utiliza una paleta de colores predefinida
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20), # Centrar y poner en negrita el título y aumentar tamaño
    plot.caption = element_text(hjust = 0, face = "italic", size = 14), # Alinear la fuente a la izquierda, en cursiva y aumentar tamaño
    legend.title = element_text(size = 14), # Aumentar tamaño del título de la leyenda
    legend.text = element_text(size = 12) # Aumentar tamaño del texto de la leyenda
  )

# -------------------------- Quemaduras Electricas --------------------------
datos <- datos %>%
  rename(sufrioQuemaduraElectrodomestico = ...52)

datos %>% 
  ggplot() + 
  coord_flip() + 
  aes(x = tipoConexionElectrica, fill = sufrioQuemaduraElectrodomestico) +
  labs(x = "Tipo de Conexion", 
       y = "Quemaduras Electricas", 
       fill = "Tipo de Follaje") +
  geom_bar(position = "fill") + # position = fill, dodge, stack
  
  ggtitle("Cantidad de Quemaduras Electricas asociadas al Tipo de Conexion\nBarrios populares de Argentina, 2022") +
  
  theme_classic() +  # Temas preconfigurados de R https://r-charts.com/ggplot2/themes/
  
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20), # Centrar y poner en negrita el título
    plot.caption = element_text(hjust = 0, face = "italic", size = 12), # Alinear la fuente a la izquierda y en cursiva
    legend.title = element_text(size = 15), # Aumentar tamaño del título de la leyenda
    legend.text = element_text(size = 12), # Aumentar tamaño del texto de la leyenda
    axis.title.x = element_text(size = 15), # Aumentar tamaño del título del eje X
    axis.title.y = element_text(size = 15), # Aumentar tamaño del título del eje Y
    axis.text.x = element_text(size = 12),  # Aumentar tamaño de las etiquetas del eje X
    axis.text.y = element_text(size = 12)   # Aumentar tamaño de las etiquetas del eje Y
  ) +
  labs(caption = "Fuente: Observatorio Villero - La Poderosa 2020 ~ 2022") # Añadir la fuente

# ----------------- SI - NO se le quemo algo depende de la red electrica -----------------
attach(datos)

quemadura_limpio = respuestas_limpio <- gsub("^S[ií].*", "Si", sufrioQuemaduraElectrodomestico)

datos %>% 
  ggplot() + 
  aes(y = tipoConexionElectrica, fill = quemadura_limpio) +
  labs(y = "Sufrio quemaduras electricas", 
       x = "Tipo de conexion", 
       fill = "Sufrio quemaduras") +
  geom_bar(position = "fill", stat = "count", width = 0.7) +
  
  ggtitle("Quemaduras Electricas asociadas al Tipo de Conexion\nBarrios populares de Argentina, 2022") +
  
  theme_minimal() + 
  
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20), # Centrar y poner en negrita el título
    plot.caption = element_text(hjust = 0, face = "italic", size = 12), # Alinear la fuente a la izquierda y en cursiva
    legend.title = element_text(size = 15), # Aumentar tamaño del título de la leyenda
    legend.text = element_text(size = 12), # Aumentar tamaño del texto de la leyenda
    axis.title.x = element_text(size = 15), # Aumentar tamaño del título del eje X
    axis.title.y = element_text(size = 15), # Aumentar tamaño del título del eje Y
    axis.text.x = element_text(size = 12),  # Aumentar tamaño de las etiquetas del eje X
    axis.text.y = element_text(size = 12)   # Aumentar tamaño de las etiquetas del eje Y
  ) +
  labs(caption = "Fuente: Observatorio Villero - La Poderosa 2020 ~ 2022") # Añadir la fuente


# -------------------- Tipo de Conexion Internet --------------------
datos <- datos %>%
  rename(conexionInternet = ...56) %>%
  mutate(conexionInternet = factor(conexionInternet))

# Agrupar los datos y calcular los porcentajes
datos_porcentaje <- datos %>%
  group_by(conexionInternet) %>%
  summarize(count = n()) %>%
  mutate(percentage = count / sum(count) * 100,
         label = paste0(round(percentage, 1), "%"))

# Crear el gráfico de torta
ggplot(datos_porcentaje, aes(x = "", y = count, fill = conexionInternet)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 4.5) +
  labs(title = "Tipo de Conexion a Internet\nBarrios populares de Argentina, 2022.",
       fill = "Tipo de conexion",
       caption = "Fuente: Observatorio Villero - La Poderosa 2020 ~ 2022") +
  theme_void() +
  scale_fill_brewer(palette = "Set3") +  # Utiliza una paleta de colores predefinida
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20), # Centrar y poner en negrita el título y aumentar tamaño
    plot.caption = element_text(hjust = 0, face = "italic", size = 14), # Alinear la fuente a la izquierda, en cursiva y aumentar tamaño
    legend.title = element_text(size = 14), # Aumentar tamaño del título de la leyenda
    legend.text = element_text(size = 12) # Aumentar tamaño del texto de la leyenda
  )

# --------------- Distribución de la cantidad de celulares segun el acceso a internet ---------------
datos <- datos %>%
  rename(poseeConexionInternet = conexionInternet)

datos <- datos %>%
  rename(cantidadCelulares = ...60)

cantCel = as.integer(datos$cantidadCelulares)

respuestas_limpio <- gsub("^S[ií].*", "Si", poseeConexionInternet)
respuestas_limpio <- gsub("^No.*", "No", respuestas_limpio)


ggplot(datos) +
  aes(y = respuestas_limpio, x = cantCel) +
  geom_boxplot(show.legend = F, fill = "lightpink") +
  labs(x = "Cantidad de Celulares por Vivienda", y = "Acceso a Internet de banda ancha") +
  coord_flip() +
  ggtitle("Distribucion de la Cantidad de Celulares segun el acceso a Internet\nBarrios populares de Argentina, 2022") +
  
  theme_light() + 
  
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20), # Centrar y poner en negrita el título
    plot.caption = element_text(hjust = 0, face = "italic", size = 12), # Alinear la fuente a la izquierda y en cursiva
    legend.title = element_text(size = 15), # Aumentar tamaño del título de la leyenda
    legend.text = element_text(size = 12), # Aumentar tamaño del texto de la leyenda
    axis.title.x = element_text(size = 15), # Aumentar tamaño del título del eje X
    axis.title.y = element_text(size = 15), # Aumentar tamaño del título del eje Y
    axis.text.x = element_text(size = 12),  # Aumentar tamaño de las etiquetas del eje X
    axis.text.y = element_text(size = 12)   # Aumentar tamaño de las etiquetas del eje Y
  ) +
  labs(caption = "Fuente: Observatorio Villero - La Poderosa 2020 ~ 2022") # Añadir la fuente

  

