library(tidyverse) # incluye dplyr
library(arrow)
library(TraMineR)

if(!exists("colores_MLER", mode="function")) source("./colores_MLER.r")
colores_sectores <- colores_MLER[[1]]
colores_favoritos <- colores_MLER[[2]]

####
#### FUNCIONES ####
####

reemplazar_fuera_con_pausa <- function(fila, relleno = "Fuera") { # recibe una fila a la vez
    no_relleno <- which(fila != relleno)
    if (length(no_relleno) < 2) return(fila)
    primer_activo <- min(no_relleno)
    ultimo_activo <- max(no_relleno)
    entre <- seq_along(fila) > primer_activo &
             seq_along(fila) < ultimo_activo &
             fila == relleno
    fila[entre] <- "Pausa"
    return(fila) # devuelve una fila que luego se intertara en la matriz
}
crear_secuencia <- function(ds,
                            muestra = 0,
                            edad_min = 17,
                            edad_max = 50,
                            debug = FALSE,
                            relleno = "Fuera",
                            tipo = "edad") {
    df_sectores <- ds %>%
        select(
            id_trabajador
            , nodo
            , edad
            , tiempo
        ) %>%
        filter(
            edad <= edad_max
            & edad >= edad_min
            & !is.na(edad)
            ) %>%
        collect()
    if(tipo == "edad"){
    df_sectores<- df_sectores%>%
        select(
            id_trabajador
            , nodo
            , {{tipo}}
        ) %>%
    distinct(id_trabajador, edad, .keep_all = TRUE)
    }else{
        df_sectores<- df_sectores%>%
            select(
                id_trabajador
                , nodo
                , {{tipo}}
            ) %>%
            distinct(id_trabajador, tiempo, .keep_all = TRUE)

    }


    if (muestra != 0) {
        indices_muestra <- sample(unique(df_sectores$id_trabajador), size = muestra)
        df_sectores <- df_sectores %>% filter(.$id_trabajador %in% indices_muestra)
    }

    alfabeto <- c(relleno
                  , "Pausa"
                  , sort(unique(df_sectores$nodo), decreasing = FALSE)
        )

    df_secuencias <- df_sectores %>%
        pivot_wider(            # tidyverse
            names_from = {{tipo}},
            values_from = nodo,
            values_fill = relleno,
            names_sort = TRUE # Mantenemos la corrección de edades
        ) %>%
        arrange(id_trabajador)

    matriz_estados <- as.data.frame(df_secuencias %>% select(-id_trabajador))

    # Reemplazar "Fuera" entre estados activos/licencia con "Pausa"
    temp <- t(  apply(matriz_estados, 1, reemplazar_fuera_con_pausa, relleno = relleno)  )
    colnames(temp) <- colnames(matriz_estados)
    matriz_estados <- as.data.frame(temp)

    secuencia_sectores <- seqdef(
        data = matriz_estados,
        alphabet = alfabeto,
        states = alfabeto,
        cpal = colores_sectores[c(1:length(alfabeto))],
        id = df_secuencias$id_trabajador
    )
    return(secuencia_sectores)

}
###
### Creo las secuencicas para graficarlas luego ###
###

ds_original_muj <- open_dataset("./materiales/MLER_mujeres.parquet")

secuencia_sectores_muj_edad <- crear_secuencia(ds_original_muj , muestra = 0, tipo = "edad")
write_parquet(secuencia_sectores_muj_edad, "./materiales/secuencia_sectores_edad_mujeres.parquet")

secuencia_sectores_muj_tiempo <- crear_secuencia(ds_original_muj , muestra = 0, tipo = "tiempo")
write_parquet(secuencia_sectores_muj_tiempo, "./materiales/secuencia_sectores_tiempo_mujeres.parquet")

# ds_original_hom <- open_dataset("./materiales/MLER_hombres.parquet")
# secuencia_sectores_hom_tiempo <- crear_secuencia(ds_original_hom , muestra = 0 , tipo = "tiempo")
# write_parquet(secuencia_sectores_hom_tiempo, "./materiales/secuencia_sectores_tiempo_hombres.parquet")
#
### Decido si voy a trbajar con hombres o mujeres, con edad o mes
###
###
sexo = "Mujeres" ; secuencia_sectores <- secuencia_sectores_muj_edad
# sexo = "Mujeres" ; secuencia_sectores <- secuencia_sectores_muj_tiempo
# sexo = "Hombres" ; secuencia_sectores <- secuencia_sectores_hom_tiempo



# #############
# ARMO LA RED #
# #############

set.seed(2001)

library(ggseqplot)
library(ggraph)
library(igraph)

matriz_transiciones <- seqtrate(secuencia_sectores)


red_sectores <- graph_from_adjacency_matrix(
    matriz_transiciones
    ,mode = "directed"
    ,weighted = TRUE
    #,diag = FALSE # diag true hace que se grafiquen las transiciones a si mismos
)

V(red_sectores)$color <- colores_sectores
V(red_sectores)$label <- V(red_sectores)$name |> # limpio labels de prefijos y sufijos
    str_remove("^\\[-> ") |>
    str_remove("\\]$")

tolerancia_peso <- 0.02
red_sectores <- delete_edges(red_sectores, which(E(red_sectores)$weight < tolerancia_peso)) # eilmino los vínculos insignificantes (con peso demasiado bajo)
#red_sectores <- simplify(red_sectores, remove.loops = TRUE) # elimino los bucles (transiciones a sí mismo) para que no se vean en el gráfico


ggraph(red_sectores, layout = "fr") +
    geom_edge_arc(
        aes(width = weight
        #    , alpha = weight ###
        ),
        arrow = arrow(length = unit(2, "mm"), type = "closed"),
        end_cap = circle(3, "mm"),
        color = "grey80",
        strength = 0.15
    ) +
    geom_node_point(
        aes(color = I(color)),
        size = 5
    ) +
    geom_node_text(
        aes(label = label),
        repel = TRUE,
        size = 2.5,
        color = "grey10",
        max.overlaps = 20
    ) +
    scale_edge_width(range = c(0.3, 3), guide = "none") +
    scale_edge_alpha(range = c(0.2, 0.9), guide = "none") + ###
    labs(
        title = paste("Red de transiciones sectoriales - ", sexo, " 17 a 50 años"),
        subtitle = paste( "Aristas: probabilidad de transición ≥ ", tolerancia_peso)
    ) +
    theme_graph(base_family = "sans") +
    theme(
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 10, color = "grey40")
    )


cat("Edges >= 2%:", sum(E(red_sectores)$weight >= 0.02), "\n")
# Edges >= 2%: 139
cat("Edges >= 5%:", sum(E(red_sectores)$weight >= 0.05), "\n")
# Edges >= 5%: 98
cat("Edges >= 1%:", sum(E(red_sectores)$weight >= 0.01), "\n")
# Edges >= 1%: 180
cat("Total edges:", ecount(red_sectores), "\n")
# Total edges: 584
cat("Self-loops:", sum(which_loop(red_sectores)), "\n")
#Self-loops: 31



# ===================================================================
# SEQ I Plot x edad
# ===================================================================


#secuencia_chica <- crear_secuencia(ds_original_muj , muestra = 100, tipo = "edad")
cpal(secuencia_sectores)[1] <-  "#c9c9c9"         # "Fuera" , Default : "#1d1d1d"         # "Fuera"

edades_clave <- c(5, 10, 15, 20, 25, 30, 35)
ggseqiplot(
    secuencia_sectores
    , sortv = "from.start", labels = label_wrap(20),  lwd = 1.5
    ) + geom_vline(xintercept = edades_clave
    ) + ggtitle(
        paste("Trayectorias Individuales - ", sexo, " (1996-2021)")
    ) + xlab("Edades") + ylab("1 línea = 1 persona")


####################################
# Distribución Sectorial
####################################
ggseqdplot(
    secuencia_sectores
    ) + geom_vline(xintercept = edades_clave
    ) + ggtitle(
        paste("Distribución Sectorial - ", sexo, " (1996-2021)")
    ) + xlab("Edades") + ylab("Proporción")

##################################
# Gráfico de Tiempo Medio de Permanencia
# #############################3

secuencia_sectores_permanencia <- seqrecode(secuencia_sectores_muj_tiempo, recodes = list("%" = c("Fuera")))


mt <- seqmeant(secuencia_sectores_permanencia, serr = FALSE)

df_mt <- tibble(
    state     = rownames(mt),
    mean_time = as.numeric(mt[, "Mean"]),
    color     = cpal(secuencia_sectores_permanencia)
) |>
    mutate(
        tipo   = case_when(
            str_starts(state, "Activo: ")   ~ "Activo",
            str_starts(state, "Licencia: ") ~ "Licencia",
            TRUE                            ~ "Pausa"
        ),
        sector = case_when(
            str_starts(state, "Activo: ")   ~ str_remove(state, "^Activo: "),
            str_starts(state, "Licencia: ") ~ str_remove(state, "^Licencia: "),
            TRUE                            ~ state
        )
    ) |>
    filter(tipo != "Pausa")

# Order sectors by total mean time (Activo + Licencia) descending
sector_order <- df_mt |>
    group_by(sector) |>
    summarise(total = sum(mean_time), .groups = "drop") |>
    arrange(desc(total)) |>
    pull(sector)

# State factor: Activo states first (bottom of stack), Licencia on top
state_levels <- c(paste0("Licencia: ", sector_order),
                  paste0("Activo: ",   sector_order)
)
state_levels <- state_levels[state_levels %in% df_mt$state]

df_mt <- df_mt |>
    mutate(
        sector = factor(sector, levels = sector_order),
        state  = factor(state,  levels = state_levels)
    )

color_map <- setNames(df_mt$color, as.character(df_mt$state))

ggplot(df_mt, aes(x = sector, y = mean_time, fill = state)) +
    geom_col() +
    # geom_text(
    #     aes(label = sprintf("%.2f", mean_time)),
    #     position = position_stack(vjust = 0.5),
    #     color    = "white",
    #     size     = 3,
    #     fontface = "bold"
    # ) +
    scale_fill_manual(values = color_map) +
    labs(
        title = paste("Tiempo Medio de Permanencia -", sexo, "(1996-2021)"),
        x     = NULL,
        y     = "Meses promedio",
        fill  = NULL
    ) +
    theme_minimal() +
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1)
    )
