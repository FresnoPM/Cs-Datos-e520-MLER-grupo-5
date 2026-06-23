library(tidyverse) # incluye dplyr
library(arrow)
library(TraMineR)

if(!exists("colores_MLER", mode="function")) source("./colores_MLER.r")
colores_sectores <- colores_MLER[[1]]
colores_favoritos <- colores_MLER[[2]]
set.seed(2001)

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
### Empiezo acá ###
ds_original_muj <- open_dataset("./materiales/MLER_mujeres.parquet")
secuencia_sectores_muj_edad <- crear_secuencia(ds_original_muj , muestra = 0, tipo = "edad")
write_parquet(secuencia_sectores_muj_edad, "./materiales/secuencia_sectores_edad_mujeres.parquet")


secuencia_sectores_muj_tiempo <- crear_secuencia(ds_original_muj , muestra = 0, tipo = "tiempo")
write_parquet(secuencia_sectores_muj_tiempo, "./materiales/secuencia_sectores_tiempo_mujeres.parquet")

# ds_original_hom <- open_dataset("./materiales/MLER_hombres.parquet")
# secuencia_sectores_hom <- crear_secuencia(ds_original_hom , muestra = 0)
# write_parquet(secuencia_sectores_hom, "./materiales/secuencia_sectores_edad_hombres.parquet")
#
### Decido si voy a trbajar con hombres o mujeres
sexo = "Mujeres" ; secuencia_sectores <- secuencia_sectores_muj
# sexo = "Hombres" ; secuencia_sectores <- secuencia_sectores_hom



# #############
# ARMO LA RED #
# #############
matriz_transiciones <- seqtrate(secuencia_sectores)

library(ggseqplot)
library(igraph)

red_sectores <- graph_from_adjacency_matrix(
    matriz_transiciones
    ,mode = "directed"
    ,weighted = TRUE
    #,diag = FALSE # diag true hace que se grafiquen las transiciones a si mismos
)

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
cpal(secuencia_chica)[31] <-  "#c9c9c9"         # "Fuera" , Default : "#1d1d1d"         # "Fuera"

edades_clave <- c(5, 10, 15, 20, 25, 30, 35)
ggseqiplot(
    secuencia_chica, sortv = "from.start", labels = label_wrap(20)
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
secuencia_sectores_permanencia <- crear_secuencia(ds_original , muestra = 10000)
secuencia_sectores_permanencia <- seqrecode(secuencia_sectores_permanencia, recodes = list("%" = c("Fuera")))

# no entiendo por qué me queda chiquito el gráfico

par(mfrow=c(1, 2)) # Adjust panel layout
dev.new(width = 10, height = 2, noRStudioGD = TRUE)

seqmtplot(
    secuencia_sectores_permanencia,
    main = paste("Tiempo Medio de Permanencia - ", sexo, " (1996-2021)"),
    ylab = "Meses promedio",
    border = NA,
    yaxis = FALSE, ylim = c(0, 3),
    with.legend = FALSE
)

axis(2, at = seq(from = 0, to = 2, by = 0.1)) # Add custom axis
seqlegend(secuencia_sectores_permanencia ) # Plot legend separately
