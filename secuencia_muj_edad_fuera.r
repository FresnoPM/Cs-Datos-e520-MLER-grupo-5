library(tidyverse)
library(arrow)
library(TraMineR)
library(ggseqplot)
library(igraph)

library(paletteer)
library(scales)

if(!exists("colores_MLER", mode="function")) source("./colores_MLER.r")
colores_sectores <- colores_MLER[[1]]
colores_favoritos <- colores_MLER[[2]]

ds_original <- open_dataset("./materiales/MLER_mujeres.parquet")
sexo = "Mujeres"

set.seed(2001)

crear_secuencia <- function(ds,
                            muestra = 0,
                            edad_min = 17,
                            edad_max = 50,
                            debug = FALSE,
                            relleno = "Fuera") {
    df_sectores_edad <- ds %>%
        select(
            id_trabajador #, tiempo
            ,letra
            ,nodo
            ,edad
        ) %>%
        filter(edad <= edad_max &
                   edad >= edad_min & !is.na(edad)) %>%
        distinct(id_trabajador, edad, .keep_all = TRUE) %>%
        collect() %>%
        select(
            id_trabajador # , tiempo
            ,nodo
            ,edad
        )
    relleno <- "Fuera"
    alfabeto <- c(
        sort(unique(df_sectores_edad$nodo), decreasing = FALSE)
        , relleno
        )
    print(alfabeto)

    df_secuencias <- df_sectores_edad %>%
        pivot_wider(
            # tidyverse
            names_from = edad,
            values_from = nodo,
            values_fill = relleno,
            names_sort = TRUE # Mantenemos la corrección de edades
        ) %>%
        arrange(id_trabajador)

    matriz_estados <- as.data.frame(df_secuencias %>% select(-id_trabajador))

    secuencia_sectores <- seqdef(
        data = matriz_estados,
        alphabet = alfabeto,
        states = alfabeto,
        cpal = colores_sectores,
        id = df_secuencias$id_trabajador
    )
    if (muestra != 0) {
        indices_muestra <- sample(1:nrow(secuencia_sectores)
                                  , muestra)
        secuencia_sectores <- secuencia_sectores[indices_muestra, ]
    }
    return(secuencia_sectores)

}

secuencia_sectores <- crear_secuencia(ds_original , muestra = 0)

# ########################
#
# ARMO LA RED
#
# ########################
matriz_transiciones <- seqtrate(secuencia_sectores)
# Convert the transition matrix into a directed igraph object
# Weights represent the transition probabilities
red_sectores <- graph_from_adjacency_matrix(
    matriz_transiciones
    ,mode = "directed"
    ,weighted = TRUE
    #,diag = FALSE # diag true hace que se grafiquen las transiciones a si mismos
)
# Basic igraph visualization
plot(
    red_sectores,
    edge.arrow.size = 0.1,
    #     vertex.label.cex = 1.2,
    edge.width = E(red_sectores)$weight * 4
) # Scale edges by probability

# ===================================================================
# SEQ I Plot x edad
# ===================================================================


secuencia_chica <- crear_secuencia(ds_original , muestra = 1000)
edades_clave <- c(5, 10, 15, 20, 25, 30, 35)
ggseqiplot(
    secuencia_sectores, sortv = "from.start", labels = label_wrap(20)
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
    )  + ggtitle(
        paste("Distribución Sectorial - ", sexo, " (1996-2021)")
    ) + xlab("Edades") + ylab("Proporción")


# Gráfico de Tiempo Medio de Permanencia
seqmtplot(
    secuencia_sectores,
    main = paste("Tiempo Medio de Permanencia - ", sexo, " (1996-2021)"),
    ylab = "Meses promedio",
    border = NA,
    with.legend = "right"
)
