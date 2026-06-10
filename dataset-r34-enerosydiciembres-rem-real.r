library(dplyr)
library(tidyr)
library(lubridate)
df_original <- readRDS("./materiales/df_mujer_real.rds")

df_original <- df_original %>%
    select(id_trabajador, tiempo, r34, edad) %>%
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
        select(id_trabajador, tiempo, intervalo) %>%       # , fnacim
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
    # , fnacim
    start_time <- Sys.time()
    enero = update(inicio,
                   year = year(inicio) + 1,
                   month = 1,
                   day = 1)
    a <- tibble(
        id_trabajador = numeric(),
        tiempo = Date(),
        r34 = numeric()
        # posible mejora: edad
    )
    while (fin > enero) {
        a <- a %>% add_row (
            id_trabajador = id_trabajador,
            tiempo = enero,
            r34 = 999
            # posible mejora: edad
        )


        enero = ymd(enero) %m+% years(1)
    }
    return(a)
}
calculo_diciembres <- function(inicio, intervalo, id_trabajador, fin) {
    # , fnacim
    start_time <- Sys.time()
    diciembre = update(inicio,
                       year = year(inicio) + 1,
                       month = 12,
                       day = 1)
    a <- tibble(
        id_trabajador = numeric(),
        tiempo = Date(),
        r34 = numeric()
        # posible mejora: edad
    )
    while (fin > diciembre) {
        a <- a %>% add_row (
            id_trabajador = id_trabajador,
            tiempo = diciembre,
            r34 = 999
            # posible mejora: edad
        )


        diciembre = ymd(diciembre) %m+% years(1)
    }
    return(a)
}

# seleccion1 <- inicio_fin %>% slice(1:50000)
# seleccion2 <- inicio_fin %>% slice(50001:100000)
# seleccion3 <- inicio_fin %>% slice(100001:150000)
# seleccion4 <- inicio_fin %>% slice(150001:200000)
# seleccion5 <- inicio_fin %>% slice(200001:250000)
# seleccion6 <- inicio_fin %>% slice(250001:300000)
# seleccion7 <- inicio_fin %>% slice(300001:311276)


# eneros1 <- map_dfr(seleccion1, calculo_eneros) ...
# diciembress1 <- map_dfr(seleccion1, calculo_diciembres) ...

library(data.table)
eneros_list <- list(
    eneros1,
    eneros2,
    eneros3,
    eneros4,
    eneros5,
    eneros6,
    eneros7
)

diciembres_list <- list(
    diciembres1,
    diciembres2,
    diciembres3,
    diciembres4,
    diciembres5,
    diciembres6,
    diciembres7
)
nuevos_edges <- rbindlist(append(eneros_list, diciembres_list),
                          fill = TRUE,
                          use.names = TRUE)
df_completo_eneros_diciembres <- bind_rows(df_original %>%
                                               select(-intervalo) %>%
                                               filter(month(tiempo) == 1 | month(tiempo) == 12)
                                           , nuevos_edges) %>%
    arrange(id_trabajador, tiempo) %>%
    mutate(
        sig_r34 = ifelse(id_trabajador == lead(id_trabajador, default = 999), lead(r34), 0),
        # sig_r34 es a donde transiciona en el sig registro: 999 es cuando se va un ratito, sólo es 0 cuando es el último registro de ese id_trabajador (sale del sistema)
    )

saveRDS(df_completo_eneros_diciembres, file = "./materiales/edges.rds")
