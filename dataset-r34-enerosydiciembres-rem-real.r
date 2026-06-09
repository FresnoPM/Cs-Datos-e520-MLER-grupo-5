library(dplyr)
library(tidyr)
library(lubridate)
 id_test1 <- head(unique(df_mujer_real$id_trabajador))
 id_test2 <- tail(unique(df_mujer_real$id_trabajador))
 id_test3 <- sample(unique(df_mujer_real$id_trabajador), size = 15)



df_mujer_real_r34_transiciones <- df_mujer_real %>%
    filter(.$id_trabajador %in% id_test3) %>%
    select(id_trabajador, tiempo, rem_tot, r34, edad) %>%
    group_by(id_trabajador,r34) %>% # [ grupos]
    # add_count(id_trabajador,r34) %>%
    arrange(desc(id_trabajador), tiempo) %>%
    ungroup()%>%
    mutate(
        sig_r34 = ifelse(
            id_trabajador == lead(id_trabajador, default = 999),
            lead(r34, default = 999),
            999), # sig_r34 es a donde transiciona en el sig registro, sólo es 999 cuando es el último registro de ese id_trabajador (sale del sistema)
        intervalo = ifelse(
            id_trabajador == lead(id_trabajador, default = 999),
            interval( ymd(tiempo), ymd( lead(tiempo))) %/% months(1),
            999) # intervalo en cuánto tiempo pasa hasta el siguiente registro, sólo es 999 cuando es el último registro de ese id_trabajador
    )



# relleno los eneros y diciembres faltantes
# if intervalo > 1  , intervalo != 999
#   while  lead(tiempo) > próximo enero : agregá enero
#   while  lead(tiempo) > próximo diciembre : agregá diciembre
#