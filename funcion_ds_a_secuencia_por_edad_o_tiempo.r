library(tidyverse) # incluye dplyr
library(arrow)
library(TraMineR)


if(!exists("colores_MLER", mode="function")) source("./colores_MLER.r")
colores_sectores <- colores_MLER[[1]]
colores_favoritos <- colores_MLER[[2]]
colores_estados <- colores_MLER[[3]]


# Funciones:
# reemplazar_fuera_con_pausa recibe la matriz de transiciones y detecta los periodos entre 2 estados licencia-activo o activo-activo como "pausa"
# crear_secuencia genera una secuencia tipo traminer a partir de un dataset columnar, tiene como argumento el tipo de periodo que se usará en cada columna de la secuencia, default es tipo = "edad" y puede ser definido tipo = "tiempo como argumento al llamar la función.

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
                            edad_min = 15,
                            edad_max = 100,
                            debug = FALSE,
                            relleno = "Fuera",
                            tipo = "edad") {
    if(debug) inicio <- Sys.time()

    df_sectores <- ds %>%
        select(
            id_trabajador
            #, nodo
            , periodo
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
            # , nodo
            , periodo
            , {{tipo}}
        ) %>%
    distinct(id_trabajador, edad, .keep_all = TRUE)
    }else{
        df_sectores<- df_sectores%>%
            select(
                id_trabajador
                # , nodo
                , periodo
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
                  # , sort(unique(df_sectores$nodo), decreasing = FALSE)
                  , sort(unique(df_sectores$periodo), decreasing = FALSE)
        )

    df_secuencias <- df_sectores %>%
        pivot_wider(            # tidyverse
            names_from = {{tipo}},
            # values_from = nodo,
            values_from = periodo,
            values_fill = relleno,
            names_sort = TRUE # Mantenemos la corrección de edades
        ) %>%
        arrange(id_trabajador)

    matriz_estados <- df_secuencias %>%
        select(-id_trabajador) %>%
        as.data.frame()
    rownames(matriz_estados) <- as.character(df_secuencias$id_trabajador)

    # Reemplazar "Fuera" entre estados activos/licencia con "Pausa"
    temp <- t(apply(matriz_estados, 1, reemplazar_fuera_con_pausa, relleno = relleno))
    colnames(temp) <- colnames(matriz_estados)
    matriz_estados <- as.data.frame(temp)

    secuencia_sectores <- seqdef(
        data = matriz_estados,
        alphabet = alfabeto,
        states = alfabeto,
        cpal = colores_sectores[c(1:length(alfabeto))],
        id = rownames(matriz_estados)
    )
    if (debug) {
        message("Duración procesamiento: ", difftime(Sys.time(), inicio, units = "mins"))
    }
    return(secuencia_sectores)

}
# Pasos:
# 1) Creo las secuencias para graficarlas luego ###
# grabo el output en un archivo .parquet para poder consultarlo sin tener que correr todo el script cada vez.
ds_original_muj <- open_dataset("./materiales/MLER_mujeres.parquet")
# ds_original_muj <- open_dataset("./materiales/MLER_mujeres_20_34.parquet")
# ds_original_muj <- open_dataset("./materiales/MLER_mujeres_18_38.parquet")
# ds_original_muj <- open_dataset("./materiales/mler_licencias_distintas.parquet")
#
# secuencia_sectores_muj_edad <- crear_secuencia(ds_original_muj , muestra = 0, tipo = "edad")
# write_parquet(secuencia_sectores_muj_edad, "./materiales/secuencia_sectores_edad_mujeres_18_38.parquet")

secuencia_sectores_muj_tiempo <- crear_secuencia(mler_base_new , debug = TRUE, muestra = 0, tipo = "tiempo")
write_parquet(secuencia_sectores_muj_tiempo, "./materiales/secuencia_sectores_activos_tiempo_mujeres_20_35_licencias_dintintas_prueba_fuera.parquet")

# 1.a) Si quiero trabajar con hombres :
# ds_original_hom <- open_dataset("./materiales/MLER_hombres.parquet")
#
# secuencia_sectores_hom_tiempo <- crear_secuencia(ds_original_hom , muestra = 0 , tipo = "tiempo")
# write_parquet(secuencia_sectores_hom_tiempo, "./materiales/secuencia_sectores_tiempo_hombres.parquet")
# secuencia_sectores_hom_edad <- crear_secuencia(ds_original_hom , muestra = 0, tipo = "edad")
# write_parquet(secuencia_sectores_hom_edad, "./materiales/secuencia_sectores_edad_hombres.parquet")


### Decido si voy a trbajar con hombres o mujeres, con edad o mes
###

# 2) Voy a trabajar con una de las secuencias. Elijo una y la asigno a la variable genérica "secuencia_sectores" para no tener que cambiar el dato cada vez que aparece. Asigno el sexo para renderizarlo en los gráficos y no confundirme a la hora de poner los títulos ni olvidarme algún título sin cambiar.

sexo = "Mujeres"
secuencia_sectores <- secuencia_sectores_muj_edad
# secuencia_sectores <- secuencia_sectores_muj_tiempo


# 2.a) también, si estoy haciéndolo en una sesión separada puedo cargar el daaset directamente
# secuencia_sectores <- read_parquet("./materiales/secuencia_sectores_edad_mujeres.parquet")
# secuencia_sectores <- read_parquet("./materiales/secuencia_sectores_tiempo_mujeres.parquet")




