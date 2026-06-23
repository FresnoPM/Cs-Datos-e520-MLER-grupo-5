library(arrow)
library(dplyr)
library(tidyr)
library(lubridate)

# Agrego columna licencia y duracion# Agrego columna licencia y duracion
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

    # -- Fase R: operaciones no soportadas por Arrow ---------------------------
    df_transformado <- df |>
        dplyr::arrange(id_trabajador, tiempo) |>
        dplyr::mutate(
            licencia = as.integer(
                rem_tot_real == 0 &
                    dplyr::lag(letra) == letra &
                    dplyr::lag(id_trabajador) == id_trabajador
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
        # dplyr::rename(desc_r32 = descripcion_sector) |>
        # dplyr::group_by(id_trabajador, letra, r32) |>
        dplyr::add_count(name = "duracion_letra") |>
        dplyr::ungroup() |>
        dplyr::relocate(desc_letra, duracion_letra, .after = letra) |>
        dplyr::mutate(id = dplyr::row_number(), .before = 1)

    if (debug) {
        message("edad NA: ", sum(is.na(df_transformado$edad)))
        message("Duración procesamiento: ", Sys.time()-inicio , " segundos")
        }

    df_transformado
}

#ds_original_muj <- open_dataset("./materiales/MLER_mujeres_INCOMPL.parquet")

# df_transformado_muj <- transformar(ds_original_muj, cantidad = 1000)
df_transformado_muj_test <- transformar(ds_original_muj, cantidad = 0, debug = TRUE)
write_parquet(df_transformado_muj, "./materiales/MLER_mujeres.parquet")



# df_transformado_hom <- transformar(open_dataset("./materiales/MLER_hombres_INCOMPL.parquet"))
# write_parquet(df_transformado_hom, "./materiales/MLER_hombres.parquet")


#       MUJERES
#       1 edad is num           15.160.008
#       2 edad is na               186.361
#
#       HOMBRES
#       1 edad is num           33.170.777
#       2 edad is na               618.107
#