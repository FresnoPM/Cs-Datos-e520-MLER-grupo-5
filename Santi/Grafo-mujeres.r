
library(tidyverse) # incluye dplyr
library(arrow)
library(TraMineR)
library(ggseqplot)
library(ggraph)
library(igraph)
library(lubridate)


# Set up de la red --------------------------------------------------------



# Cargo el paquete
secuencia_sectores<- read_parquet("./materiales/secuencia_sectores_tiempo_mujeres.parquet")

cat("1. Preparando la base (asignando IDs)...\n")

# El objeto de secuencias no tiene ID, así que le creamos uno por fila
df_wide <- secuencia_sectores %>%
    mutate(id_mujer = row_number())

cat("2. Aplanando la matriz (pivot_longer). Esto toma unos segundos...\n")
# Pasamos de 312 columnas a formato largo
df_long <- df_wide %>%
    pivot_longer(
        cols = -id_mujer,
        names_to = "mes_calendario",
        values_to = "estado"
    ) %>%
    # Convertimos el nombre de la columna (ej. "1996-01-01") a formato de Fecha real
    mutate(mes_calendario = ymd(mes_calendario))


cat("3. Detectando el t=0 (El evento de Licencia)...\n")
# Buscamos a las "Mamis" y su mes exacto de inicio
eventos_t0 <- df_long %>%
    # Filtramos solo los estados que arrancan con "Licencia"
    filter(str_detect(estado, "^Licencia")) %>%
    # Agrupamos por mujer y nos quedamos exclusivamente con su PRIMERA licencia
    group_by(id_mujer) %>%
    slice_min(mes_calendario, n = 1) %>%
    ungroup() %>%
    # Preparamos los datos para el cruce
    rename(mes_t0 = mes_calendario, estado_origen = estado) %>%
    # LA MAGIA DEL EVENT STUDY: Le sumamos 36 meses exactos al mes de inicio
    mutate(mes_t36 = mes_t0 %m+% months(36))

cat("4. Calculando el Destino en t=36 y armando la Red...\n")
# Cruzamos la tabla contra sí misma para ver el futuro de esa mujer
red_mamis <- eventos_t0 %>%
    inner_join(
        df_long,
        # Hacemos match por la misma mujer, pero igualando el mes_t36 con el calendario
        by = c("id_mujer" = "id_mujer", "mes_t36" = "mes_calendario")
    ) %>%
    rename(estado_destino = estado) %>%

    # Agrupamos y contamos para armar el edgelist (Origen, Destino, Peso)
    count(estado_origen, estado_destino, name = "weight") %>%
    # Lo pasamos a porcentaje para que sea comparable con el código de Lu
    mutate(weight = weight / sum(weight)) %>%
    arrange(desc(weight))

cat("\n=== ARISTAS DE LA RED DE TRATAMIENTO (t=0 a t=36) ===\n")
print(head(red_mamis, 10))


cat("5. Aislando al Grupo de Control (No Mamis)...\n")
# Guardamos los IDs de las mujeres que YA usamos en el tratamiento
id_mamis <- eventos_t0$id_mujer

# Filtramos la base larga: nos quedamos con las que NO son mamis
# y miramos SOLO los meses donde estaban trabajando ("Activo")
candidatas_placebo <- df_long %>%
    filter(!id_mujer %in% id_mamis) %>%
    filter(str_detect(estado, "^Activo"))

cat("6. Asignando el t=0 Placebo (Fechas dispersas aleatorias)...\n")
# Fijamos una semilla para que el randomizador siempre dé el mismo resultado. Criterio "Random Sampling".
set.seed(2026)

eventos_control_t0 <- candidatas_placebo %>%
    group_by(id_mujer) %>%
    # ACA ESTA LA MAGIA: Elegimos 1 mes Activo al azar por cada mujer
    slice_sample(n = 1) %>%
    ungroup() %>%
    rename(mes_t0 = mes_calendario, estado_origen = estado) %>%
    # Sumamos los 36 meses de horizonte, igual que a las mamis
    mutate(mes_t36 = mes_t0 %m+% months(36))

cat("7. Calculando el Destino en t=36 para el Control...\n")
# Cruzamos contra la base general para ver qué pasó en t=36
red_control <- eventos_control_t0 %>%
    left_join(
        df_long,
        by = c("id_mujer" = "id_mujer", "mes_t36" = "mes_calendario")
    ) %>%
    rename(estado_destino = estado) %>%

    # EL FIX MAGICO: Destruimos el factor y lo pasamos a texto libre
    mutate(estado_destino = as.character(estado_destino)) %>%
    # Ahora sí, podemos inyectar nuestro string en los vacíos sin que R se queje
    mutate(estado_destino = replace_na(estado_destino, "Fuera del Sistema (9999)")) %>%

    # Armamos el edgelist y calculamos porcentajes
    count(estado_origen, estado_destino, name = "weight") %>%
    mutate(weight = weight / sum(weight)) %>%
    arrange(desc(weight))

cat("\n=== ARISTAS DE LA RED DE CONTROL (t=0 a t=36) ===\n")
print(head(red_control, 10))


# Envolvemos el código de Lu en una función reutilizable
graficar_red_comparativa <- function(edgelist, titulo_grafico) {

    # 1. Creamos el objeto de red estricto de igraph
    grafo <- graph_from_data_frame(edgelist, directed = TRUE)

    # 2. Aplicamos el filtro de Lu: eliminamos ruido menor al 2%
    tolerancia_peso <- 0.02
    grafo <- delete_edges(grafo, which(E(grafo)$weight < tolerancia_peso))

    # Limpiamos los nombres de los nodos por si traen basura (adaptación de Lu)
    V(grafo)$label <- V(grafo)$name %>%
        str_remove("^\\[-> ") %>%
        str_remove("\\]$")

    # 3. Renderizamos usando la estética exacta de Lu
    ggraph(grafo, layout = "fr") +
        geom_edge_arc(
            aes(width = weight),
            arrow = arrow(length = unit(2, "mm"), type = "closed"),
            end_cap = circle(3, "mm"),
            color = "grey80",
            strength = 0.36
        ) +
        # Usamos un color unificado base (Lu puede inyectar acá su vector 'colores_sectores' si lo tiene en el environment)
        geom_node_point(
            color = "#003f5c",
            size = 5
        ) +
        geom_node_text(
            aes(label = label),
            repel = TRUE,
            size = 3,
            color = "grey10",
            max.overlaps = 20
        ) +
        scale_edge_width(range = c(0.3, 3), guide = "none") +
        labs(
            title = titulo_grafico,
            subtitle = paste("Aristas: probabilidad de transición >= ", tolerancia_peso)
        ) +
        theme_graph(base_family = "sans") +
        theme(
            plot.title = element_text(size = 14, face = "bold"),
            plot.subtitle = element_text(size = 10, color = "grey40")
        )
}

# --- GENERAMOS LOS DOS GRÁFICOS FINALES ---

# Red del Grupo de Control (No Mamis)
grafico_control <- graficar_red_comparativa(
    edgelist = red_control,
    titulo_grafico = "Red de Transiciones - Grupo de Control (No Madres) t=36"
)

# Red del Grupo de Tratamiento (Mamis)
grafico_tratamiento <- graficar_red_comparativa(
    edgelist = red_mamis,
    titulo_grafico = "Red de Transiciones - Grupo de Tratamiento (Madres) t=36"
)

# Visualizarlos en el panel
grafico_control
grafico_tratamiento



# Analisis Analitico ------------------------------------------------------

cat("========================================================\n")
cat("ANÁLISIS ANALÍTICO: EL DELTA DEL CHILD PENALTY (t=0 a t=36)\n")
cat("========================================================\n")

# 1. Calculamos la fuga basal (Grupo de Control)
fuga_control <- red_control %>%
    # Filtramos cualquier destino que sea expulsión o suspensión temporal
    filter(str_detect(estado_destino, "Fuera|Pausa")) %>%
    # Le sacamos el "Activo: " para poder cruzar los datos
    mutate(sector = str_remove(estado_origen, "^Activo: ")) %>%
    group_by(sector) %>%
    # Sumamos los porcentajes de todas las fugas de ese sector y pasamos a base 100
    summarise(tasa_fuga_control = sum(weight) * 100)

# 2. Calculamos la fuga por maternidad (Grupo de Tratamiento)
fuga_mamis <- red_mamis %>%
    filter(str_detect(estado_destino, "Fuera|Pausa")) %>%
    # Le sacamos el "Licencia: "
    mutate(sector = str_remove(estado_origen, "^Licencia: ")) %>%
    group_by(sector) %>%
    summarise(tasa_fuga_mamis = sum(weight) * 100)

# 3. El cruce definitivo: La magnitud de la penalidad
analisis_comparativo <- inner_join(fuga_control, fuga_mamis, by = "sector") %>%
    mutate(
        # La resta exacta: cuánto daño extra hace la maternidad
        child_penalty_neto = tasa_fuga_mamis - tasa_fuga_control
    ) %>%
    # Ordenamos de mayor a menor penalidad
    arrange(desc(child_penalty_neto))

# Redondeamos para que la lectura en consola sea limpia
analisis_comparativo <- analisis_comparativo %>%
    mutate(across(where(is.numeric), ~round(., 2)))

print(analisis_comparativo)
