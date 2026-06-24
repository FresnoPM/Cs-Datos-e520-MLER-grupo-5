library(tidyverse)
library(arrow)
library(TraMineR)
library(ggseqplot)
library(ggraph)
library(igraph)
library(lubridate)
library(purrr)

# ========================================================================
# 1. PREPARACIÓN DE DATOS BASE
# ========================================================================
secuencia_sectores <- read_parquet("./materiales/secuencia_sectores_tiempo_mujeres_20_34.parquet")

cat("1. Preparando la base (asignando IDs)...\n")
df_wide <- secuencia_sectores %>%
    mutate(id_mujer = row_number())

cat("2. Aplanando la matriz (pivot_longer). Esto toma unos segundos...\n")
df_long <- df_wide %>%
    pivot_longer(
        cols = -id_mujer,
        names_to = "mes_calendario",
        values_to = "estado"
    ) %>%
    mutate(mes_calendario = ymd(mes_calendario))

# NOTA: Asegurate de que el parquet ya esté filtrado para mujeres de 20 a 34 años.
# Si no lo está, deberías agregar un filter() acá si tenés la columna de edad disponible.

fecha_maxima_panel <- max(df_long$mes_calendario, na.rm = TRUE)
cat("Fecha maxima observada en el panel:", as.character(fecha_maxima_panel), "\n")

# ========================================================================
# 2. FUNCIONES DE VALIDACIÓN DE ANTIGÜEDAD (El aporte de Claude)
# ========================================================================
chequear_antiguedad_12m <- function(eventos_df) {
    eventos_df %>%
        mutate(fila_id = row_number()) %>%
        tidyr::crossing(offset = 1:12) %>%
        mutate(mes_prev = mes_t0 %m-% months(offset)) %>%
        left_join(
            df_long,
            by = c("id_mujer" = "id_mujer", "mes_prev" = "mes_calendario")
        ) %>%
        group_by(fila_id) %>%
        summarise(
            meses_activa_previos = sum(str_detect(estado, "^Activo"), na.rm = TRUE),
            .groups = "drop"
        ) %>%
        mutate(antiguedad_ok = meses_activa_previos == 12) %>%
        select(fila_id, antiguedad_ok)
}

aplicar_filtro_antiguedad <- function(eventos_df, etiqueta) {
    eventos_df <- eventos_df %>% mutate(fila_id = row_number())
    chequeo <- chequear_antiguedad_12m(eventos_df)
    n_antes <- nrow(eventos_df)

    resultado <- eventos_df %>%
        left_join(chequeo, by = "fila_id") %>%
        filter(antiguedad_ok) %>%
        select(-fila_id, -antiguedad_ok)

    n_despues <- nrow(resultado)
    cat(sprintf(
        "[%s] antiguedad 12m: %d -> %d filas (se descarta %.1f%% por inestabilidad reciente)\n",
        etiqueta, n_antes, n_despues, 100 * (n_antes - n_despues) / n_antes
    ))
    resultado
}

# ========================================================================
# 3. TRATAMIENTO (MAMIS): Proxy de 3 meses + Filtro de 12 meses previos
# ========================================================================
cat("3. Detectando el t=0 (Mamis: Proxy 3 meses)...\n")
eventos_t0_brutos <- df_long %>%
    group_by(id_mujer) %>%
    arrange(mes_calendario, .by_group = TRUE) %>%
    mutate(
        es_licencia = str_detect(estado, "^Licencia"),
        # EL PROXY ESTRICTO RECUPERADO
        es_maternidad_proxy = es_licencia &
            lead(es_licencia, n = 1, default = FALSE) &
            lead(es_licencia, n = 2, default = FALSE)
    ) %>%
    filter(es_maternidad_proxy) %>%
    slice_min(mes_calendario, n = 1) %>%
    ungroup() %>%
    select(-es_licencia, -es_maternidad_proxy) %>%
    rename(mes_t0 = mes_calendario, estado_origen = estado) %>%
    mutate(mes_t36 = mes_t0 %m+% months(36))

cat("   -> Mamis brutas detectadas:", nrow(eventos_t0_brutos), "\n")
eventos_t0_filtrado <- aplicar_filtro_antiguedad(eventos_t0_brutos, "MAMIS")

# ========================================================================
# 4. CONTROL (NO MAMIS): Placebo + Filtro de 12 meses previos
# ========================================================================
cat("4. Aislando al Grupo de Control y asignando Placebo...\n")
id_mamis <- eventos_t0_brutos$id_mujer # Excluimos a TODAS las que alguna vez fueron madres

candidatas_placebo <- df_long %>%
    filter(!id_mujer %in% id_mamis) %>%
    filter(str_detect(estado, "^Activo"))

set.seed(2026)
eventos_control_t0_brutos <- candidatas_placebo %>%
    group_by(id_mujer) %>%
    slice_sample(n = 1) %>%
    ungroup() %>%
    rename(mes_t0 = mes_calendario, estado_origen = estado) %>%
    mutate(mes_t36 = mes_t0 %m+% months(36))

eventos_control_filtrado_previo <- aplicar_filtro_antiguedad(eventos_control_t0_brutos, "CONTROL_CANDIDATAS")

# ========================================================================
# 5. MATCHING 1 A 1: Igualando tamaño y composición sectorial
# ========================================================================
cat("5. Ejecutando Matching 1 a 1 por sector...\n")
set.seed(2026)

receta_sectores_mamis <- eventos_t0_filtrado %>%
    mutate(sector_base = str_remove(estado_origen, "^Licencia: ")) %>%
    count(sector_base, name = "meta_n")

eventos_control_t0 <- eventos_control_filtrado_previo %>%
    mutate(sector_base = str_remove(estado_origen, "^Activo: ")) %>%
    inner_join(receta_sectores_mamis, by = "sector_base") %>%
    group_by(sector_base) %>%
    sample_frac(size = 1, replace = FALSE) %>%
    filter(row_number() <= meta_n) %>%
    ungroup() %>%
    select(-sector_base)

cat("   -> Control final balanceado:", nrow(eventos_control_t0), "mujeres.\n")

# ========================================================================
# 6. PROCESAMIENTO Y ARMADO DE REDES (Fix 1 y Fix 2 intactos)
# ========================================================================
procesar_evento <- function(eventos_df, etiqueta) {
    procesado <- eventos_df %>%
        left_join(
            df_long,
            by = c("id_mujer" = "id_mujer", "mes_t36" = "mes_calendario")
        ) %>%
        rename(estado_destino = estado) %>%
        mutate(estado_destino = as.character(estado_destino)) %>%
        mutate(
            estado_destino = case_when(
                !is.na(estado_destino)        ~ estado_destino,
                mes_t36 > fecha_maxima_panel  ~ NA_character_,
                TRUE                          ~ "Fuera del Sistema (9999)"
            )
        )

    n_total      <- nrow(procesado)
    n_censura    <- sum(is.na(procesado$estado_destino))
    n_fuga_codif <- sum(procesado$estado_destino == "Fuera del Sistema (9999)", na.rm = TRUE)

    cat(sprintf(
        "[%s] total=%d | censura real=%d (%.1f%%) | recodif a 'Fuera'=%d (%.1f%%)\n",
        etiqueta, n_total, n_censura, 100 * n_censura / n_total,
        n_fuga_codif, 100 * n_fuga_codif / n_total
    ))

    procesado %>% filter(!is.na(estado_destino))
}

cat("\n6. Calculando el Destino en t=36...\n")
mamis_procesadas  <- procesar_evento(eventos_t0_filtrado, "MAMIS")
control_procesado <- procesar_evento(eventos_control_t0, "CONTROL")

red_mamis <- mamis_procesadas %>%
    count(estado_origen, estado_destino, name = "weight") %>%
    group_by(estado_origen) %>%
    mutate(weight = weight / sum(weight)) %>%
    ungroup() %>%
    arrange(desc(weight))

red_control <- control_procesado %>%
    count(estado_origen, estado_destino, name = "weight") %>%
    group_by(estado_origen) %>%
    mutate(weight = weight / sum(weight)) %>%
    ungroup() %>%
    arrange(desc(weight))

# ========================================================================
# 7. GRÁFICOS Y ANÁLISIS ANALÍTICO
# ========================================================================
graficar_red_comparativa <- function(edgelist, titulo_grafico) {
    grafo <- graph_from_data_frame(edgelist, directed = TRUE)
    tolerancia_peso <- 0.02
    grafo <- delete_edges(grafo, which(E(grafo)$weight < tolerancia_peso))

    V(grafo)$label <- V(grafo)$name %>%
        str_remove("^\\[-> ") %>%
        str_remove("\\]$")

    ggraph(grafo, layout = "fr") +
        geom_edge_arc(
            aes(width = weight),
            arrow = arrow(length = unit(2, "mm"), type = "closed"),
            end_cap = circle(3, "mm"),
            color = "grey80",
            strength = 0.36
        ) +
        geom_node_point(color = "#003f5c", size = 5) +
        geom_node_text(
            aes(label = label), repel = TRUE, size = 3,
            color = "grey10", max.overlaps = 20
        ) +
        scale_edge_width(range = c(0.3, 3), guide = "none") +
        labs(
            title = titulo_grafico,
            subtitle = paste("Aristas: probabilidad transicion >= ", tolerancia_peso)
        ) +
        theme_graph(base_family = "sans") +
        theme(
            plot.title = element_text(size = 14, face = "bold"),
            plot.subtitle = element_text(size = 10, color = "grey40")
        )
}

grafico_control <- graficar_red_comparativa(red_control, "Control (No Madres) t=36")
grafico_tratamiento <- graficar_red_comparativa(red_mamis, "Tratamiento (Madres) t=36")

print(grafico_control)
print(grafico_tratamiento)

cat("\n========================================================\n")
cat("ANÁLISIS ANALÍTICO DEFINITIVO (t=0 a t=36)\n")
cat("========================================================\n")

fuga_control <- red_control %>%
    filter(str_detect(estado_destino, "Fuera|Pausa")) %>%
    mutate(sector = str_remove(estado_origen, "^Activo: ")) %>%
    group_by(sector) %>%
    summarise(tasa_fuga_control = sum(weight) * 100)

fuga_mamis <- red_mamis %>%
    filter(str_detect(estado_destino, "Fuera|Pausa")) %>%
    mutate(sector = str_remove(estado_origen, "^Licencia: ")) %>%
    group_by(sector) %>%
    summarise(tasa_fuga_mamis = sum(weight) * 100)

analisis_comparativo <- inner_join(fuga_control, fuga_mamis, by = "sector") %>%
    mutate(child_penalty_neto = tasa_fuga_mamis - tasa_fuga_control) %>%
    arrange(desc(child_penalty_neto)) %>%
    mutate(across(where(is.numeric), ~round(., 2)))

print(analisis_comparativo, n = Inf)
