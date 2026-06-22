library(arrow)
library(tidyr)
library(lubridate)

# Agrego columna licencia y duracion
transformar <- function(df, cantidad=0, debug=FALSE){
    if(cantidad != 0){
        df <- df %>% dplyr::filter( .$id_trabajador %in% sample( unique(df$id_trabajador), size = cantidad )  )
        }

    df_transformado <- df %>%
        dplyr::mutate(
            licencia = ifelse(rem_tot_real == 0, 1, 0),

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
            nodo_final = ifelse(licencia == 1,
                                paste0("Licencia_", desc_letra),
                                paste0("Activo_", desc_letra)
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


#df_original_muj <- read_parquet("./materiales/MLER_mujeres_INCOMPL.parquet")
#df_original_hom <- read_parquet("./materiales/MLER_hombres_INCOMPL.parquet")
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


