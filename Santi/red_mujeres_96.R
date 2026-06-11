# Cargar librerías
library(tidyverse)
install.packages("arrow")
library(arrow)
library(igraph)
library(ggraph)
library(lubridate)

# =======================================================
# PASO PREVIO: Conexión al dataset en disco
# =======================================================

# Abrir el CSV en disco sin cargarlo a la RAM

#dataset_csv <- open_dataset("dataset/MLER.csv", format = "csv")

# Escribir directamente a Parquet de forma ultra rápida
#write_parquet(dataset_csv, "dataset/MLER.parquet")

# ds <- open_dataset("./dataset/MLER.parquet")
# ds <- open_dataset("./materiales/edges_hombres.parquet")
ds <- open_dataset("./materiales/edges_mujeres.parquet")
head(ds)
df <- readRDS("./materiales/edges_mujeres.rds")
# =======================================================
# PASO 0: Filtrar la base (Ejemplo: Mujeres, Año 1996)
# =======================================================
df_96 <- ds %>%
    filter(year(tiempo) == 1996) %>%  # Extrae el año
    collect() # ¡ESTA ES LA CLAVE!
# collect() ejecuta la consulta en C++ y solo trae a la RAM las mujeres de 1996.

# =======================================================
# PASO 1: Armar los NODOS (Atributos estáticos) DEL R34!!
# =======================================================

desc_r34 <- readRDS("~/Repos/Cs-Datos-e520-MLER-grupo-5/materiales/desc_r34.rds")
nodos <- df_96 %>%
    group_by(r34) %>%
    summarise(
        trabajadores = n_distinct(id_trabajador)
        # ,ingreso_promedio = mean(rem_tot, na.rm = TRUE)
    ) %>%
    #rename(name = r34) %>%
    mutate(
        descripcion = desc_r34$descripcion[ match(r34, desc_r34$r34) ]
    ) %>%
    rename(name = r34) %>%
    ungroup()



# =======================================================
# PASO 2: Armar las ARISTAS (Flujos Netos)
# =======================================================
# A. Calcular transiciones brutas
transiciones <- df_96 %>%
    arrange(id_trabajador, tiempo) %>%
    count(origen = r34, destino = sig_r34, name = "flujo_bruto")

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
# =======================================================77777

grafo_mujeres <- graph_from_data_frame(d = aristas_finales, vertices = nodos, directed = TRUE)

ggraph(grafo_mujeres, layout = 'fr') +
    geom_edge_link(aes(width = peso), alpha = 0.5, arrow = arrow(length = unit(4, 'mm'))) +
    geom_node_point(aes(size = trabajadores
                        )) +
    geom_node_text(aes(label = descripcion), vjust = 2, size = 3) +

    scale_edge_width(range = c(0.5, 3), name = "Flujo Neto") +
#    scale_color_viridis_c(option = "magma", name = "Salario Promedio") +
    scale_size_continuous(range = c(3, 12), name = "Total Mujeres") +
    theme_void() +
    labs(title = "Red de Movilidad Laboral Neta - Mujeres (1996)")
