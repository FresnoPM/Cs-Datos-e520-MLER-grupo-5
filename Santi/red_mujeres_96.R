# Cargar librerías
library(tidyverse)
install.packages("arrow")
library(arrow)
library(igraph)
library(ggraph)


# =======================================================
# PASO PREVIO: Conexión al dataset en disco
# =======================================================

# Abrir el CSV en disco sin cargarlo a la RAM
#dataset_csv <- open_dataset("dataset/MLER.csv", format = "csv")

# Escribir directamente a Parquet de forma ultra rápida
#write_parquet(dataset_csv, "dataset/MLER.parquet")

ds <- open_dataset("./dataset/MLER.parquet")

# =======================================================
# PASO 0: Filtrar la base (Ejemplo: Mujeres, Año 1996)
# =======================================================
df_mujeres_96 <- ds %>%
    filter(
        floor(tiempo / 100) == 1996,   # Extrae el año
        sexo == 1
    ) %>%
    collect() # ¡ESTA ES LA CLAVE!
# collect() ejecuta la consulta en C++ y solo trae a la RAM las mujeres de 1996.
# Como la base ya está achicada, tus 15GB de RAM van a procesar el resto sin problemas.

# =======================================================
# PASO 1: Armar los NODOS (Atributos estáticos) DEL R32!!
# =======================================================
nodos <- df_mujeres_96 %>%
    group_by(r32) %>%
    summarise(
        trabajadores = n_distinct(id_trabajador),
        ingreso_promedio = mean(rem_tot, na.rm = TRUE)
    ) %>%
    rename(name = r32)

# =======================================================
# PASO 2: Armar las ARISTAS (Flujos Netos)
# =======================================================
# A. Calcular transiciones brutas
transiciones <- df_mujeres_96 %>%
    arrange(id_trabajador, tiempo) %>%
    group_by(id_trabajador) %>%
    mutate(r32_destino = lead(r32)) %>%
    ungroup() %>%
    filter(!is.na(r32_destino), r32 != r32_destino) %>%
    count(origen = r32, destino = r32_destino, name = "flujo_bruto")

# B. Calcular flujo neto
flujos_netos <- transiciones %>%
    left_join(transiciones,
              by = c("origen" = "destino", "destino" = "origen"),
              suffix = c("_ida", "_vuelta")) %>%
    replace_na(list(flujo_bruto_vuelta = 0)) %>%
    mutate(flujo_neto = flujo_bruto_ida - flujo_bruto_vuelta) %>%
    filter(flujo_neto > 0)

# C. Aplicar el filtro del 5%
aristas_finales <- flujos_netos %>%
    left_join(nodos, by = c("origen" = "name")) %>%
    filter(flujo_neto > (0.005 * trabajadores)) %>%
    select(origen, destino, peso = flujo_neto)

# =======================================================
# PASO 3: Construir y dibujar el GRAFO
# =======================================================
grafo_mujeres <- graph_from_data_frame(d = aristas_finales, vertices = nodos, directed = TRUE)

ggraph(grafo_mujeres, layout = 'fr') +
    geom_edge_link(aes(width = peso), alpha = 0.5, arrow = arrow(length = unit(4, 'mm'))) +
    geom_node_point(aes(size = trabajadores, color = ingreso_promedio)) +
    geom_node_text(aes(label = name), vjust = 2, size = 3) +
    scale_edge_width(range = c(0.5, 3), name = "Flujo Neto") +
    scale_color_viridis_c(option = "magma", name = "Salario Promedio") +
    scale_size_continuous(range = c(3, 12), name = "Total Mujeres") +
    theme_void() +
    labs(title = "Red de Movilidad Laboral Neta - Mujeres (1996)")
