library(arrow)
library(tidyr)
library(lubridate)

# Agrego columna licencia y duracion
transformar <- function(df){

    df_transformado <- df %>%
        dplyr::mutate(
            licencia = ifelse(rem_tot_real == 0, 1, 0)
        ) %>%
        dplyr::group_by(id_trabajador, letra, r32) %>%
        dplyr::add_count(r32) %>% dplyr::rename(duracion = n) %>%
        dplyr::ungroup() %>% # Groups: 381.171
        dplyr::arrange(id_trabajador, tiempo)

    df_transformado %>% dplyr::count(is.na(.$edad))

    return(df_transformado)
}

df_original_muj <- read_parquet("./materiales/MLER_mujeres_INCOMPL.parquet")
df_transformado_muj <- transformar(df_original_muj)
write_parquet(df_transformado_muj, "./materiales/MLER_mujeres.parquet")

saveRDS(df_transformado_muj, file = "./materiales/MLER_mujeres.rds")

df_original_hom <- read_parquet("./materiales/MLER_hombres_INCOMPL.parquet")
df_transformado_hom <- transformar(df_original_hom)
write_parquet(df_transformado_hom, "./materiales/MLER_hombres.parquet")

saveRDS(df_transformado_hom, file = "./materiales/MLER_hombres.rds")


#       MUJERES
#       1 FALSE           15.160.008
#       2 TRUE               186.361
#
#       HOMBRES
#       1 FALSE           33.170.777
#       2 TRUE               618.107
