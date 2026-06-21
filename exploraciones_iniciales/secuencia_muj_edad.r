library(tidyverse)
library(arrow)
library(TraMineR)
library(paletteer)
library(scales)
library(ggseqplot)
library(igraph)

desc_letra <- readRDS("./materiales/desc_letra.rds")
ds_original <- open_dataset("./materiales/df_mujer_real.parquet")
sexo = "Mujeres"

df_sectores_edad <- ds_original %>%
    select(
        id_trabajador #, tiempo
        ,letra
        ,edad
    ) %>%
    filter(edad <= 65) %>%
    distinct(id_trabajador, edad, .keep_all = TRUE) %>%
    collect() %>%
    mutate(letra = desc_letra$descripcion[match(letra, desc_letra$letra)]) %>%
    select(
        id_trabajador # , tiempo
        ,letra
        ,edad
    )
#unique(df_sectores_edad$edad)
relleno <- "Fuera"
alfabeto_sectores <- c(unique(df_sectores_edad$letra), relleno)

df_secuencias <- df_sectores_edad %>%
    pivot_wider( # tidyverse
        names_from = edad,
        values_from = letra,
        values_fill = relleno,
        names_sort = TRUE # Mantenemos la corrección de edades
    ) %>%
    arrange(id_trabajador)

matriz_estados <- as.data.frame(df_secuencias %>% select(-id_trabajador))

secuencia_sectores <- seqdef(
    data = matriz_estados,
    alphabet = alfabeto_sectores,
    states = alfabeto_sectores,
    id = df_secuencias$id_trabajador
)
 #############################################

matriz_transiciones <- seqtrate(secuencia_sectores)

# Convert the transition matrix into a directed igraph object
# Weights represent the transition probabilities
red_sectores <- graph_from_adjacency_matrix(matriz_transiciones
                                            ,mode = "directed"
                                            ,weighted = TRUE
                                            #,diag = FALSE
                                            )
# Basic igraph visualization
plot(red_sectores,
     edge.arrow.size = 0.5,
#     vertex.label.cex = 1.2,
     edge.width = E(red_sectores)$weight * 20) # Scale edges by probability






# ====================================================================
# SEQ I Plot x edad
# ====================================================================
set.seed(2001)

# indices_muestra <- sample(1:nrow(secuencia_sectores), 10000)
# secuencia_muestra <- secuencia_sectores[indices_muestra, ]

cpal(secuencia_sectores) <- paletteer_d("trekcolors::lcars_cardassian", n = length(alfabeto_sectores))
edades_clave <- c(5, 10, 15, 20, 25, 30, 35, 40, 45)
ggseqiplot(secuencia_sectores, sortv = "from.start", labels = label_wrap(20)) + geom_vline(xintercept = edades_clave)  + ggtitle(paste(
    "Trayectorias Individuales - ", sexo
    #," (Muestra Aleatoria n=10.000)"
    , " (1996-2021)"
)) + xlab("Edades") + ylab("1 línea = 1 persona") #+ guides(fill = guide_legend(keywidth = 1.5, label.theme = element_text(size = 8)))

####################################
ggseqdplot(
    secuencia_sectores
)  + geom_vline(xintercept = edades_clave)  + ggtitle(paste(
    "Distribución Sectorial - ", sexo, " (1996-2021)"
)) + xlab("Edades") + ylab("Proporción") #+ scale_colour_discrete(labels = function(x) str_wrap(x, width = 8))




# # Gráfico de Tiempo Medio de Permanencia
# seqmtplot(
#     secuencia_sectores,
#     # Usamos la base completa
#     main = "Tiempo Medio de Permanencia - ",
#     sexo ,
#     " (1996-2021)",
#     ylab = "Meses promedio",
#     border = NA,
#     with.legend = "right"
# )
