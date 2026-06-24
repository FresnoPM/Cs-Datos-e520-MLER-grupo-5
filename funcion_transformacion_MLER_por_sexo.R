library(arrow)
library(dplyr)
library(tidyr)
library(lubridate)

# Funciones:
# Transformar elimina casos de pluriempleo en cada mes para cada trabajador, dejando la fila con mayor remuneración total real e identifica cuáles periodos son activos y cuáles fueron con licencia. No distingue tipos de licencia.

transformar <- function(ds, cantidad = 0, debug = FALSE) {
    if(debug) inicio <- Sys.time()
    # -- Fase Arrow: reducir datos antes del collect ---------------------------
    if (cantidad != 0) {
        ids <- ds |>
            dplyr::distinct(id_trabajador) |>
            dplyr::collect() |>
            dplyr::pull(id_trabajador) |>
            sample(size = cantidad)
        ds <- ds |> dplyr::filter(id_trabajador %in% ids)
    }

    # Una fila por (id_trabajador, tiempo): mayor rem_tot_real; empates rotos arbitrariamente
    df <- ds |>
        dplyr::collect() |>
        dplyr::group_by(id_trabajador, tiempo) |>
        dplyr::slice_max(rem_tot_real, n = 1, with_ties = FALSE) |>
        dplyr::ungroup()
    # if(debug) message("Cantidad de líneas elimnadas: ", nrow(ds) - nrow(df))
    # -- Fase R: operaciones no soportadas por Arrow ---------------------------
    df_transformado <- df |>
        dplyr::arrange(id_trabajador, tiempo) |>
        dplyr::mutate(
            licencia = ifelse(
                rem_tot_real == 0 &
                    dplyr::lag(letra) == letra &
                    dplyr::lag(id_trabajador) == id_trabajador
                , 1
                , 0
            ),
            # Sector base a 1 dígito
            desc_letra = dplyr::case_when(
                letra == 1  ~ "Agro",
                letra == 2  ~ "Pesca",
                letra == 3  ~ "Mineria",
                letra == 4  ~ "Industria",
                letra == 5  ~ "Electricidad/Gas/Agua",
                letra == 6  ~ "Construcción",
                letra == 7  ~ "Comercio",
                letra == 8  ~ "Hotelería/Restaurantes",
                letra == 9  ~ "Transporte",
                letra == 10 ~ "Finanzas",
                letra == 11 ~ "Inmobiliaria",
                letra == 12 ~ "Enseñanza",
                letra == 13 ~ "Servicios Salud",
                letra == 14 ~ "Servicios Sociales",
                TRUE        ~ "Otros"
            ),
            # Si está en licencia, lo marcamos en el nodo
            nodo = dplyr::if_else(licencia == 1,
                                  paste0("Licencia: ", desc_letra),
                                  paste0("Activo: ", desc_letra))
        ) |>
        # agrega la columna duracion_letra para ver la duración de cada periodo de actividad o licencia en cada sector para cada id_trabajador
                dplyr::group_by(id_trabajador) |>
                dplyr::mutate(.run = cumsum(letra != dplyr::lag(letra, default = dplyr::first(letra)))) |>
                dplyr::add_count(.run, name = "duracion_letra") |>
                dplyr::select(-.run) |>

        dplyr::ungroup() |>
        dplyr::relocate(desc_letra, duracion_letra, .after = letra) |>
        dplyr::mutate(id = dplyr::row_number(), .before = 1)

    if (debug) {

        message("edad NA: ", sum(is.na(df_transformado$edad)))
        message("Duración procesamiento: ", difftime(Sys.time(), inicio, units = "mins"))
        }


    df_transformado
}

# Cantidad_repetidas valida que no haya líneas repetidas en un dataframe dado
cantidad_repetidas <- function(df) {
    nrow(df) - dplyr::n_distinct(df$id_trabajador, df$tiempo)
}

# Pasos:
#
# 1) Cargo el dataset con las columnas del MLER pero separado por género y con algunas transformaciones básicas como conversión de rem_tot a valores reales del periodo por ejemplo

ds_original_muj <- open_dataset("./materiales/MLER_mujeres_INCOMPL.parquet")

# 2) Corro el script con una muestra pequeña para corroborar que no haya errores y detectarlos a tiempo, cuando lo considero aceptable lo aplico al dataset completo (tarda 12 minutos)
df_transformado_muj_muestra <- transformar(ds_original_muj, cantidad = 100, debug = TRUE)

# df_transformado_muj <- transformar(ds_original_muj, debug = TRUE)

# 2.1) Verifico si se logró el objetivo comparando con la versión anterior

cantidad_repetidas(df_transformado_muj) # 0

# comparo con la versión anterior a este ajuste
df_transformado_anterior <- read_parquet("./materiales/MLER_mujeres.parquet")
cantidad_repetidas(df_transformado_anterior) # 636709

# 3) Guardo el dataset transformado en un archivo parquet.
# OJO: No eliminé las filas que  no tienen dato en la columna "edad" por las dudas que queramos incluirlas en otros cálculos.
#
# Mujer tiene (23 de junio 2026) 179.923 ocurrencias de NA en la columna "edad" luego de correr el script de "transformar"

write_parquet(df_transformado_muj, "./materiales/MLER_mujeres.parquet")

# Repito el proceso para hombres
ds_original_hom <- open_dataset("./materiales/MLER_hombres_INCOMPL.parquet")
df_transformado_hom <- transformar(ds_original_hom,
                                       # cantidad = 100,
                                        debug = TRUE)
write_parquet(df_transformado_hom_test, "./materiales/MLER_hombres.parquet")
