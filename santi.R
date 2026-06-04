library(readr)
library(tidyverse)
library(igraph)
library(ggraph)
install.packages("ggraph")

#voy a trarme los datos de la base de datos del MLER:
#mler <- read_csv("dataset/MLER.csv")
#mler


# Prototipo de grafo ------------------------------------------------------


# 1 Preparar la Tabla de Aristas (Flujos)
# Imaginemos que estos son los datos del MLER.
aristas <- tibble(
    origen = c("Comercio", "Comercio", "Industria", "Industria", "Servicios"),
    destino = c("Servicios", "Industria", "Servicios", "Comercio", "Tecnología"),
    peso = c(500, 150, 300, 100, 50) # Cantidad de personas que saltaron de un rubro a otro
)


# 2 Preparar la Tabla de Nodos (Atributos)
nodos <- tibble(
    nombre = c("Comercio", "Industria", "Servicios", "Tecnología"),
    trabajadores = c(5000, 3000, 4500, 1000), # Definirá el tamaño del círculo
    ingreso_promedio = c(300000, 450000, 350000, 800000) # Definirá el color (calor)
)


# 3 Construir la Red (El objeto grafo)
# directed = TRUE porque importa la dirección (no es lo mismo ir de Comercio a Industria que al revés)
mi_grafo <- graph_from_data_frame(d = aristas, vertices = nodos, directed = TRUE)


# 4 Dibujar el Grafo (El resultado visual)
ggraph(mi_grafo, layout = 'fr') +  # 'fr' (Fruchterman-Reingold) es un algoritmo para que los nodos no se superpongan

    # Dibujamos las flechas (aristas) con grosor dependiente del 'peso'
    geom_edge_link(aes(width = peso), alpha = 0.4, arrow = arrow(length = unit(3, 'mm'))) +

    # Dibujamos los nodos con tamaño por 'trabajadores' y color por 'ingreso'
    geom_node_point(aes(size = trabajadores, color = ingreso_promedio)) +

    # Agregamos los nombres de los rubros
    geom_node_text(aes(label = name), vjust = 1.5, fontface = "bold") +

    # Ajustamos las escalas visuales
    scale_edge_width(range = c(0.5, 3), name = "Volumen de paso") +
    scale_color_viridis_c(option = "plasma", name = "Ingreso Promedio") + # Escala de calor
    scale_size_continuous(range = c(5, 15), name = "Total Trabajadores") +
    theme_void()

