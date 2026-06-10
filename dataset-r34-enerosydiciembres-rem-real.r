library(dplyr)
library(tidyr)
library(lubridate)
df_original <- readRDS("./materiales/df_mujer_real.rds")



df_original <- df_original %>%
    select(id_trabajador, tiempo, r34) %>%
    group_by(id_trabajador, r34) %>%
    arrange(desc(id_trabajador), tiempo) %>%
    ungroup() %>%
    mutate(
        intervalo = ifelse(
            id_trabajador == lead(id_trabajador, default = 999),
            interval(ymd(tiempo), ymd(lead(tiempo))) %/% months(1),
            999
        ) # intervalo en cuánto tiempo pasa hasta el siguiente registro, sólo es 999 cuando es el último registro de ese id_trabajador. Es una variable temporal, al final será eliminada.
    ) %>% filter(.$intervalo > 0)

## cálculos auxiliares - creo un df donde sólo haya el inicio y el fin de cada periodo de ausencia para, luego calcular los eneros y diciembres en el medio
identifico_intervalos <- function(df) {
    inicio_index <- which(df$intervalo > 1 & df$intervalo != 999)
    fin_index <- inicio_index + 1

    inicio_df  <- df %>%
        select(id_trabajador, tiempo, intervalo) %>%
        slice(inicio_index) %>%
        rename(inicio = tiempo)

    fin_df <- df %>%
        select(tiempo) %>%
        slice(fin_index) %>%
        rename(fin = tiempo)

    inicio_fin <- bind_cols(inicio_df, fin_df)
    return(inicio_fin)

}
inicio_fin <- identifico_intervalos(df_original)

calculo_eneros <- function(inicio, intervalo, id_trabajador, fin) {
    start_time <- Sys.time()
    enero = update(    inicio, year = year(inicio) + 1, month = 1, day = 1 )
    a <- tibble(
        id_trabajador = numeric(),
        enero = Date(),
        r34 = numeric()
    )
    while (fin>enero) {
        a <- a %>% add_row (
            id_trabajador = id_trabajador,
            enero = enero,
            r34 = 999
        )


        enero = ymd(enero) %m+% years(1)
    }
    return(a)
}

# seleccion1 <- inicio_fin %>% slice(1:50000)
# eneros1 <- pmap_dfr(seleccion1, calculo_eneros)
# seleccion2 <- inicio_fin %>% slice(50001:100000)
# eneros2 <- pmap_dfr(seleccion2, calculo_eneros)
# seleccion3 <- inicio_fin %>% slice(100001:150000)
# eneros3 <- pmap_dfr(seleccion3, calculo_eneros)
# seleccion4 <- inicio_fin %>% slice(150001:200000)
# eneros4 <- pmap_dfr(seleccion4, calculo_eneros)
# seleccion5 <- inicio_fin %>% slice(200001:250000)
# eneros5 <- pmap_dfr(seleccion5, calculo_eneros)
# seleccion6 <- inicio_fin %>% slice(250001:300000)
# eneros6 <- pmap_dfr(seleccion6, calculo_eneros)
# seleccion7 <- inicio_fin %>% slice(300001:311276)
# eneros7 <- pmap_dfr(seleccion7, calculo_eneros)

library(data.table)
eneros_list <- list(eneros1, eneros2, eneros3, eneros4, eneros5, eneros6, eneros7)
nuevos_nodos <- rbindlist(eneros_list, fill = TRUE)
diciembres_list <- list()


df_con_nuevos_nodos <- bind_rows(df_original %>% select(-intervalo)
                                 , nuevos_nodos) %>% arrange(id_trabajador, tiempo)

df_solo_eneros_y_diciembres <- df_con_nuevos_nodos %>% filter(month(tiempo) == 1 |
                                                                  month(tiempo) == 12) %>% arrange(id_trabajador, tiempo)

df_sig_r34 <- df_solo_eneros_y_diciembres %>%
    mutate(
        sig_r34 = ifelse(id_trabajador == lead(id_trabajador, default = 999), lead(r34), 0),
        # sig_r34 es a donde transiciona en el sig registro: 999 es cuando se va un ratito, sólo es 0 cuando es el último registro de ese id_trabajador (sale del sistema)

    )
saveRDS(df_sig_r34, file = "./materiales/edges.rds")
