library(arrow)
library(dplyr)
library(tidyr)
library(lubridate)

# Agrego columna licencia y duracion# Agrego columna licencia y duracion
transformar <- function(ds, cantidad = 0, debug = FALSE) {

    # -- Fase Arrow: reducir datos antes del collect ---------------------------
    if (cantidad != 0) {
        ids <- ds |>
            distinct(id_trabajador) |>
            collect() |>
            pull(id_trabajador) |>
            sample(size = cantidad)
        ds <- ds |> filter(id_trabajador %in% ids)
    }

    # Una fila por (id_trabajador, tiempo): mayor rem_tot_real; empates se rompen tras collect()
    df <- ds |>
        group_by(id_trabajador, tiempo) |>
        filter(rem_tot_real == max(rem_tot_real, na.rm = TRUE)) |>
        ungroup() |>
        collect() |>
        group_by(id_trabajador, tiempo) |>
        slice_head(n = 1) |>
        ungroup()

    # -- Fase R: operaciones no soportadas por Arrow ---------------------------
    df_transformado <- df |>
        arrange(id_trabajador, tiempo) |>
        mutate(
            licencia = as.integer(
                rem_tot_real == 0 &
                    lag(letra) == letra &
                    lag(id_trabajador) == id_trabajador
            ),
            # Sector base a 1 dígito
            desc_letra = case_when(
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
                .default    ~ "Otros"
            ),
            # Si está en licencia, lo marcamos en el nodo
            nodo = if_else(licencia == 1,
                           paste0("Licencia: ", desc_letra),
                           paste0("Activo: ", desc_letra))
        ) |>
        rename(desc_r32 = descripcion_sector) |>
        group_by(id_trabajador, letra, r32) |>
        add_count(name = "duracion_letra") |>
        ungroup() |>
        relocate(desc_letra, duracion_letra, .after = letra) |>
        mutate(id = row_number(), .before = 1)

    if (debug) message("edad NA: ", sum(is.na(df_transformado$edad)))

    df_transformado
}


# df_transformado_muj_test <- transformar(open_dataset("./materiales/MLER_mujeres_INCOMPL.parquet"), cantidad = 1000)
df_transformado_muj_test <- transformar(ds_original_muj, cantidad = 10000, debug = TRUE)
df_transformado_hom <- transformar(open_dataset("./materiales/MLER_hombres_INCOMPL.parquet"))

write_parquet(df_transformado_muj, "./materiales/MLER_mujeres.parquet")
write_parquet(df_transformado_hom, "./materiales/MLER_hombres.parquet")


#       MUJERES
#       1 edad is num           15.160.008
#       2 edad is na               186.361
#
#       HOMBRES
#       1 edad is num           33.170.777
#       2 edad is na               618.107
#