library(tidyverse)
library(arrow)
library(TraMineR)
library(ggseqplot)
library(dplyr)
library(data.table)
    # library(paletteer)
    # library(scales)
    # library(igraph)
    # #df_original_muj <- read_parquet("./materiales/MLER_mujeres.parquet")
    # df_original_hom <- read_parquet("./materiales/MLER_hombres.parquet")

selecciono_por_edad <- function(ds,
                        edad_min = 17,
                        edad_max = 50,
                        cantidad=0,
                        debug=FALSE){


    df_sectores <- ds %>%
        select(id_trabajador
               , tiempo
               #, rem_tot_real
               , edad
               #, letra
               , nodo_final) %>%
        distinct(id_trabajador, edad, .keep_all = TRUE) %>%
        collect() %>%
        filter(!is.na(edad) & .$edad <= edad_max & .$edad >= edad_min) %>%
        setDT()

    if(cantidad != 0){
        df_sectores <- df_sectores %>% filter( .$id_trabajador %in% sample( unique(.$id_trabajador), size = cantidad )  )
    }

    fuera <- "Fuera"
    pausa <- "Pausa"

    alfabeto <<- c(unique(df_sectores$nodo_final), fuera, pausa)

    if(debug==TRUE){
        print(class(df_sectores))
        print(nrow(df_sectores))
    }
    return(df_sectores)

}



#ds_original_muj <- open_dataset("./materiales/MLER_mujeres.parquet", format = "parquet")
sexo = "Mujeres"
alfabeto <- c()
df_sectores_edad <- selecciono_por_edad(ds_original_muj, cantidad=5, debug=TRUE)


df_secuencias <- df_sectores_edad %>%
    pivot_wider( # tidyverse
        names_from = edad,
        values_from = nodo_final,
        values_fill = fuera,
        names_sort = TRUE # Mantenemos la corrección de edades
    ) %>%
    arrange(id_trabajador)

matriz_estados <- as.data.frame(  df_secuencias %>% select(-id_trabajador, -tiempo)  )
secuencia_sectores <- seqdef(
    data = matriz_estados,
    alphabet = alfabeto,
    states = alfabeto,
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
