# ==============================================================================
# 1. LIBRERÍAS
# ==============================================================================
library(tidyverse)
library(arrow)
library(TraMineR)
library(dplyr)
library(tidyr)
library(lubridate)
library(data.table)
# convertir el MLER al formato parquet (solo debe hacerse una vez)
#datos <- read.csv("./materiales/MLER.csv")
#write_parquet(datos, "MLER.parquet")

# ==============================================================================
# 2. FILTRO INICIAL (igual que antes, Arrow hace el trabajo pesado)
# ==============================================================================

df_crudo <- open_dataset("./materiales/MLER.parquet") %>%
    mutate(
        anio = as.integer(tiempo %/% 100),
        mes  = as.integer(tiempo %% 100),
        edad = anio - fnacim
    ) %>%
    filter(provi %in% c(2, 14, 82, 94)
           #, edad >= 25
           , edad <= 35) %>%
    select(id_trabajador, tiempo, sexo, rem_tot, edad, anio, mes, letra, r34) %>%
    collect() %>%
    setDT()   # convierte a data.table en memoria, sin copiar

# ==============================================================================
# 3. DETECCIÓN DEL EVENTO — versión data.table
# ==============================================================================

# "0" Sin dato
# "1" Mujer
# "2" Hombre

# Paso 1: ordenar (setorder modifica en lugar, no crea copia)
setorder(df_crudo, id_trabajador, tiempo)

# Paso 2: flag de licencia
df_crudo[, en_licencia := as.integer(rem_tot == 0)]

# Paso 3: ID de racha consecutiva (rleid = equivalente a consecutive_id)
df_crudo[, racha_id := rleid(en_licencia), by = id_trabajador]

# Paso 4: duración de cada racha (una sola agrupación)
df_crudo[, meses_racha := .N, by = .(id_trabajador, racha_id)]

# Paso 5: condición pre-computada  ← sexo == 2L (mujer), no 1L
df_crudo[, licencia_valida := (sexo == 1L & en_licencia == 1L & meses_racha >= 3L)]

# Paso 6: es_madre_proxy
df_crudo[, es_madre_proxy := as.integer(any(licencia_valida)), by = id_trabajador]

# Paso 7: tiempo_mes_cero
df_crudo[, tiempo_mes_cero := NA_integer_]
df_crudo[es_madre_proxy == 1L,
         tiempo_mes_cero := min(tiempo[licencia_valida]),
         by = id_trabajador]

# 0 AL GRUPO DE CONTROL (Mujeres sin licencia)

edad_mediana_maternidad <- as.integer(round(
    median(df_crudo$edad[df_crudo$es_madre_proxy == 1L], na.rm = TRUE)
))

# Placebo t=0: mes en que la mujer SIN licencia alcanza esa misma edad mediana
df_crudo[sexo == 1L & es_madre_proxy == 0L,          # ← cambió sexo==2L por esto
         tiempo_mes_cero := min(tiempo[edad == edad_mediana_maternidad], na.rm = TRUE),
         by = id_trabajador]

df_final <- df_crudo[is.finite(tiempo_mes_cero)]

df_final[, meses_relativos := ((anio - (tiempo_mes_cero %/% 100L)) * 12L) +
             (mes  - (tiempo_mes_cero %% 100L))]




# PREPARACIÓN DE ESTADOS Y BALANCEO DEL PANEL PARA TRAMINER


# df_final <- readRDS( "./materiales/df_MLER_licencias.rds")
ventana_inicio <- -24L
ventana_fin    <- 36L

# Filtramos la ventana
df_ventana <- df_final[meses_relativos >= ventana_inicio & meses_relativos <= ventana_fin]

# Estado base
df_ventana[, estado := fifelse(en_licencia == 1L, "Licencia", "Activo")]

# ── Anti-pluriempleo ANTES del merge ──────────────────────────────────────────
# Si una persona tiene dos empleos en el mismo mes, priorizamos "Licencia" sobre "Activo"
df_ventana[, estado := fifelse(any(estado == "Licencia"), "Licencia", "Activo"),
           by = .(id_trabajador, meses_relativos)]
df_ventana <- unique(df_ventana[, .(id_trabajador, meses_relativos, estado, sexo, es_madre_proxy)])

# Tabla de referencia de sexo (para rellenar luego sin NAs)
sexo_ref <- unique(df_ventana[, .(id_trabajador, sexo, es_madre_proxy)])

# Esqueleto: todos los trabajadores × todos los meses de la ventana
todos_ids <- unique(df_ventana$id_trabajador)
esqueleto <- CJ(id_trabajador = todos_ids, meses_relativos = ventana_inicio:ventana_fin)

# ── EL MERGE QUE FALTABA ──────────────────────────────────────────────────────
df_balanceado <- merge(
    esqueleto,
    df_ventana[, .(id_trabajador, meses_relativos, estado)],
    by  = c("id_trabajador", "meses_relativos"),
    all.x = TRUE   # left join: meses sin observación quedan en NA
)
setDT(df_balanceado)

# Meses sin registro en el MLER = Fuera del Sistema Formal
df_balanceado[is.na(estado), estado := "Fuera del Sistema"]

# Pegamos sexo desde la tabla de referencia (nunca tiene NAs)
df_balanceado <- merge(df_balanceado, sexo_ref, by = "id_trabajador", all.x = TRUE)
df_balanceado[, grupo := fifelse(es_madre_proxy == 1L, "Tratamiento", "Control")]

# 6. PIVOT A MATRIZ DE SECUENCIAS (FORMATO ANCHO)

df_ancho <- dcast(df_balanceado, id_trabajador + grupo ~ meses_relativos, value.var = "estado")

columnas_tiempo <- as.character(ventana_inicio:ventana_fin)

matriz_tratamiento <- as.data.frame(df_ancho[grupo == "Tratamiento", ..columnas_tiempo])
matriz_control     <- as.data.frame(df_ancho[grupo == "Control",     ..columnas_tiempo])

# EVENT STUDY (GRÁFICO DE ÍNDICES)

alfabeto <- c("Activo", "Licencia", "Fuera del Sistema")
colores  <- c("#1f77b4", "#ff7f0e", "#d62728") # Azul, Naranja, Rojo

seq_tratamiento <- seqdef(matriz_tratamiento, alphabet = alfabeto, cpal = colores,
labels = c("Activo Formal", "Licencia", "Inactivo / Informal"))

seq_control     <- seqdef(matriz_control, alphabet = alfabeto, cpal = colores,
                          labels = c("Activo Formal", "Licencia", "Inactivo / Informal"))

par(mfrow = c(1, 2))
posicion_mes_cero <- abs(ventana_inicio) + 1L

seqIplot(seq_tratamiento,
         main = "Mujeres CON licencia (Tratamiento)",
         xlab = "Meses Relativos (0 = Inicio de Licencia)",
         ylab = "Trayectorias Individuales",
         border = NA, space = 0, sortv = "from.start", with.legend = FALSE)
abline(v = posicion_mes_cero, col = "black", lwd = 2, lty = 2)

# 1. Averiguamos cuántas mujeres hay en tu grupo de tratamiento
n_tratamiento <- nrow(seq_tratamiento)

# 2. Le pedimos al seqIplot del control que dibuje solo esa misma cantidad, elegida al azar
seqIplot(seq_control,
         main = "Mujeres SIN licencia (Control)",
         xlab = "Meses Relativos (0 = Edad Placebo)",
         ylab = "",
         border = NA,
         space = 0,
         sortv = "from.start",
         with.legend = "right",
         idxs = sample(1:nrow(seq_control), n_tratamiento)) # <-- ESTA ES LA MAGIA

abline(v = 24, col = "black", lwd = 2, lty = 2)

par(mfrow = c(1, 1))

#
#
#
#
# ####################################################

# ==============================================================================
# ALTERNATIVA: Event Study con ggplot2 (mucho más legible)
# ==============================================================================
library(ggplot2)

# Calculamos las proporciones por mes relativo y sexo
df_event <- df_balanceado[, .(             # ← saca el filtro sexo %in% c(1L,2L)
    prop_activo   = mean(estado == "Activo"),
    prop_licencia = mean(estado == "Licencia"),
    prop_fuera    = mean(estado == "Fuera del Sistema"),
    n             = .N
), by = .(meses_relativos, grupo)]         # ← grupo en vez de sexo

# Línea 215: ya no necesitamos mapear sexo a etiqueta, grupo ya tiene el texto
# Borrar la línea: df_event[, grupo := fifelse(...)]

ggplot(df_event, aes(x = meses_relativos,  # ← corregido el typo "moses"
                     y = prop_activo,
                     color = grupo, linetype = grupo)) +
    geom_line(linewidth = 1.2) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = 0.8) +
    scale_color_manual(values = c(
        "Tratamiento" = "#d62728",
        "Control"     = "#1f77b4"
    )) +
    scale_x_continuous(breaks = seq(-24, 36, by = 6)) +
    labs(
        title    = "Penalidad por Maternidad — Empleo Formal Registrado",
        subtitle = "Mujeres 25-35 años | CABA, Córdoba, Santa Fe y Tierra del Fuego",
        x        = "Meses Relativos al Evento (t=0)",
        y        = "Proporción en Empleo Formal",
        color    = NULL, linetype = NULL,
        caption  = "Control: mujeres sin licencia ≥ 3 meses. Fuente: MLER."
    ) +
    theme_minimal(base_size = 13) +
    theme(legend.position = "top")











################################################################################

##################################  Grafo  #####################################

################################################################################
library(ggraph)
library(patchwork)
library(igraph)
# ==============================================================================
# 1. ARMADO DE NODOS (A prueba de números enteros)
# ==============================================================================
df_nodos <- df_final %>%
    filter(sexo == 1, meses_relativos %in% c(0, 4)) %>%
    mutate(
        nodo = case_when(
            en_licencia == 1 ~ "Licencia (Maternidad)",
            letra == 1 ~ "Agro",
            letra == 2 ~ "Pesca",
            letra == 3 ~ "Mineria",
            letra == 4 ~ "Industria",
            letra == 5 ~ "Electricidad, gas y agua",
            letra == 6 ~ "Construcción",
            letra == 7 ~ "Comercio",
            letra == 8 ~ "Hoteleria y Restaurantes",
            letra == 9 ~ "Transporte",
            letra == 10 ~ "Finanzas",
            letra == 11 ~ "Inmobiliaria",
            letra == 12 ~ "Enseñanza",
            letra == 13 ~ "Servicios salud",
            letra == 14 ~ "Servicios sociales",
            TRUE ~ "Otros"
        )
    )

# ==============================================================================
# 2. FUNCIÓN PARA EXTRAER FLUJOS Y EL NODO "FUERA DEL SISTEMA" (CORREGIDA)
# ==============================================================================
extraer_flujos <- function(datos) {
    datos %>%
        select(id_trabajador, meses_relativos, nodo) %>%
        # EL FIX: Nos quedamos con el primer sector que aparezca por persona y por mes
        distinct(id_trabajador, meses_relativos, .keep_all = TRUE) %>%
        # Pivot a formato ancho
        pivot_wider(names_from = meses_relativos, values_from = nodo, names_prefix = "mes_") %>%
        rename(Origen = mes_0, Destino = mes_4) %>%
        # Ahora sí, los vacíos son NA reales y se reemplazan correctamente
        mutate(Destino = ifelse(is.na(Destino), "Fuera del Sistema", Destino)) %>%
        filter(!is.na(Origen)) %>%
        # Agrupamos y contamos el grosor de la flecha
        count(Origen, Destino, name = "Cantidad") %>%
        arrange(desc(Cantidad))
}

aristas_trat <- extraer_flujos(df_nodos %>% filter(es_madre_proxy == 1))
aristas_ctrl <- extraer_flujos(df_nodos %>% filter(es_madre_proxy == 0))

# ==============================================================================
# 3. CREACIÓN DE LOS GRAFOS CON IGRAPH
# ==============================================================================
# Filtramos flujos menores para que la red no sea una mancha ilegible
grafo_trat <- graph_from_data_frame(aristas_trat %>% filter(Cantidad > 5), directed = TRUE)
grafo_ctrl <- graph_from_data_frame(aristas_ctrl %>% filter(Cantidad > 50), directed = TRUE)

# ==============================================================================
# 4. RENDERIZADO ESTÉTICO CON GGRAPH
# ==============================================================================
cat("Dibujando redes...\n")

p1 <- ggraph(grafo_trat, layout = 'fr') +
    geom_edge_link(aes(width = Cantidad, alpha = Cantidad),
                   arrow = arrow(length = unit(4, 'mm')),
                   end_cap = circle(5, 'mm'), color = "#d62728") +
    geom_node_point(size = 7, color = "#ff7f0e") +
    geom_node_text(aes(label = name), repel = TRUE, size = 3.5, fontface = "bold") +
    scale_edge_width(range = c(0.5, 3)) +
    theme_void() +
    ggtitle("Tratamiento: Flujos Post-Licencia")

p2 <- ggraph(grafo_ctrl, layout = 'fr') +
    geom_edge_link(aes(width = Cantidad, alpha = Cantidad),
                   arrow = arrow(length = unit(4, 'mm')),
                   end_cap = circle(5, 'mm'), color = "#1f77b4") +
    geom_node_point(size = 7, color = "#2ca02c") +
    geom_node_text(aes(label = name), repel = TRUE, size = 3.5, fontface = "bold") +
    scale_edge_width(range = c(0.2, 2)) +
    theme_void() +
    ggtitle("Control: Mujeres misma edad (Sin Licencia)")

# Imprimimos en paralelo
p1 + p2

# Guardar el dataset limpio y balanceado
saveRDS(df_final, file = "./materiales/df_MLER_licencias.rds")


