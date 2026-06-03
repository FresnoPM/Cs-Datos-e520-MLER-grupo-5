library(data.table)
MLER_csv <- fread("./materiales/MLER.csv")
library(haven)
MLER_dta <- read_dta("./materiales/MLER.dta")
# A tibble: 6 × 13
library(dplyr)
head(MLER_csv)

df <- as.data.frame(MLER_csv)

#
# # Verifico que no haya personas que hayan cambiado de sexo durante los periodos
# df %>%
#     group_by(id_trabajador) %>%
#     summarise(unique_count = n_distinct(sexo)) %>%
#     filter(unique_count!=1) # A tibble: 0 × 2
# # Efectivamente: No hubo transiciones de género. Por lo tanto podemos realizar una separación de la db por género para que sea más sencillo de operar
#
#
# df_sin_dato <- df |> filter(sexo==0) # son 6 registros (3 id_trabajador, de los cuales 1 no tiene provincia) que no tienen fecha de nacimiento y todos están asignado a organizaciones con tramo_empleo=4, o sea que es una empresa chica, con menos de 9 empleados. Por lo tanto los desestimaría.

df_mujer <- df %>% filter(sexo==1)
nrow(df_mujer) # 15.346.369

df_hombre <- df %>% filter(sexo==2)

# write.csv(df_mujer, "materiales/df_mujer.csv", row.names = FALSE)
# write.csv(df_hombre, "materiales/df_hombre.csv", row.names = FALSE)
