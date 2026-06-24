library(arrow)
library(tidyr)
library(lubridate)

# Agrego columna licencia y duracion
transformar <- function(ds, cantidad=0, debug=FALSE){
    if(cantidad != 0){
        ds <- ds %>% dplyr::filter( .$id_trabajador %in% sample( unique(.$id_trabajador), size = cantidad )  )
        }

    df_transformado <- ds %>%

        # Keep only the highest-paid row per (id_trabajador, tiempo) combination
        dplyr::group_by(id_trabajador, tiempo) %>%
        dplyr::slice_max(rem_tot_real, n = 1, with_ties = FALSE) %>%
        dplyr::ungroup() %>%

        dplyr::mutate(
            licencia = ifelse(
                rem_tot_real == 0 & lag(letra) == letra & lag(id_trabajador) == id_trabajador,
                1, 0),

            # Mapeamos el sector base (a 1 dígito como pidió el profe)
            desc_letra = dplyr::case_when(
                letra == 1 ~ "Agro",
                letra == 2 ~ "Pesca",
                letra == 3 ~ "Mineria",
                letra == 4 ~ "Industria",
                letra == 5 ~ "Electricidad/Gas/Agua",
                letra == 6 ~ "Construcción",
                letra == 7 ~ "Comercio",
                letra == 8 ~ "Hotelería/Restaurantes",
                letra == 9 ~ "Transporte",
                letra == 10 ~ "Finanzas",
                letra == 11 ~ "Inmobiliaria",
                letra == 12 ~ "Enseñanza",
                letra == 13 ~ "Servicios Salud",
                letra == 14 ~ "Servicios Sociales",
                TRUE ~ "Otros"
            )
        ) %>%

        dplyr::mutate(
            # ACA ESTA LA MAGIA: Si está en licencia, le pegamos la palabra al sector
            nodo = ifelse(licencia == 1,
                                paste0("Licencia: ", desc_letra),
                                paste0("Activo: ", desc_letra)
            )
        ) %>%
        dplyr::rename(  desc_r32 = descripcion_sector  ) %>%
        dplyr::group_by(  id_trabajador, letra, r32  ) %>%
            #dplyr::add_count(r32) %>% dplyr::rename(duracion_r32 = n) %>%
            dplyr::add_count(  letra  ) %>% dplyr::rename(  duracion_letra = n   ) %>%
        dplyr::ungroup() %>% # Groups: 381.171

        dplyr::relocate(  desc_letra, .after = letra  ) %>%
        dplyr::relocate(  duracion_letra, .after = desc_letra  ) %>%

        dplyr::arrange(  id_trabajador, tiempo  ) %>%
        dplyr::mutate(  id = dplyr::row_number(), .before = 1  )

    if(debug==TRUE){
            print(   paste0("edad NA: ", df_transformado %>% dplyr::count(is.na(.$edad)) )   )
        }

    return(   df_transformado   )
}


#ds_original_muj <- read_parquet("./materiales/MLER_mujeres_INCOMPL.parquet")
#ds_original_hom <- read_parquet("./materiales/MLER_hombres_INCOMPL.parquet")
df_transformado_muj <- transformar(df_original_muj)
df_transformado_hom <- transformar(df_original_hom)

write_parquet(df_transformado_muj, "./materiales/MLER_mujeres.parquet")
write_parquet(df_transformado_hom, "./materiales/MLER_hombres.parquet")

# saveRDS(df_transformado_muj, file = "./materiales/MLER_mujeres.rds")
# saveRDS(df_transformado_hom, file = "./materiales/MLER_hombres.rds")


#       MUJERES
#       1 edad is num           15.160.008
#       2 edad is na               186.361
#
#       HOMBRES
#       1 edad is num           33.170.777
#       2 edad is na               618.107
#

