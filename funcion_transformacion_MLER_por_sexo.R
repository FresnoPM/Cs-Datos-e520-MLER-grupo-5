library(arrow)
library(dplyr)
library(tidyr)
library(lubridate)

# Agrego columna licencia y duracion
transformar <- function(ds, cantidad = 0, debug = FALSE) {

    # -- Fase Arrow (lazy): operaciones empujadas al lector de Parquet ----------

    if (cantidad != 0) {
        ids <- ds |>
            select(id_trabajador) |>
            collect() |>
            distinct() |>
            pull(id_trabajador) |>
            sample(size = cantidad)
        ds <- ds |> filter(id_trabajador %in% ids)
    }

    # Una sola fila por (id_trabajador, tiempo): la de mayor rem_tot_real
    # Los empates se rompen después del collect()
    ds <- ds |>
        group_by(id_trabajador, tiempo) |>
        filter(rem_tot_real == max(rem_tot_real, na.rm = TRUE)) |>
        ungroup()

    # -- Collect: trae a memoria solo lo que pasó los filtros ------------------
    df <- collect(ds)

    # Rompe empates residuales arbitrariamente
    df <- df |>
        group_by(id_trabajador, tiempo) |>
        slice_head(n = 1) |>
        ungroup()

    # -- Fase R: operaciones no soportadas por Arrow ---------------------------
    df_transformado <- df |>

        arrange(id_trabajador, tiempo) |>

        mutate(
            licencia = ifelse(
                rem_tot_real == 0 & lag(letra) == letra & lag(id_trabajador) == id_trabajador,
                1, 0),

            # Mapeamos el sector base (a 1 dígito como pidió el profe)
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
                TRUE        ~ "Otros"
            )
        ) |>

        mutate(
            # ACA ESTA LA MAGIA: Si está en licencia, le pegamos la palabra al sector
            nodo = ifelse(licencia == 1,
                          paste0("Licencia: ", desc_letra),
                          paste0("Activo: ", desc_letra))
        ) |>
        rename(desc_r32 = descripcion_sector) |>
        group_by(id_trabajador, letra, r32) |>
        add_count(letra) |> rename(duracion_letra = n) |>
        ungroup() |>

        relocate(desc_letra,    .after = letra) |>
        relocate(duracion_letra, .after = desc_letra) |>

        arrange(id_trabajador, tiempo) |>
        mutate(id = row_number(), .before = 1)

    if (debug == TRUE) {
        print(paste0("edad NA: ", df_transformado |> count(is.na(edad))))

    }

    return(df_transformado)
}


# df_transformado_muj_test <- transformar(open_dataset("./materiales/MLER_mujeres_INCOMPL.parquet"), cantidad = 1000)
df_transformado_muj_test <- transformar(ds_original_muj, cantidad = 1000, debug = TRUE)
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