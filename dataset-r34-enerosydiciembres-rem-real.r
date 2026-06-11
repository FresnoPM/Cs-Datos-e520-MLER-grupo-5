library(dplyr)
library(tidyr)
library(lubridate)
library(purrr)
library(arrow)
# rm(list=ls())
# df_original <- readRDS("./materiales/df_hombre.rds")
# df_original <- readRDS("./materiales/df_mujer.rds")

df_original <- df_original %>%
    select(id_trabajador, tiempo, r34) %>%       # , edad)
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

seleccion1 <- inicio_fin %>% slice(1:50000)
seleccion2 <- inicio_fin %>% slice(50001:100000)
seleccion3 <- inicio_fin %>% slice(100001:150000)
seleccion4 <- inicio_fin %>% slice(150001:200000)
seleccion5 <- inicio_fin %>% slice(200001:250000)
seleccion6 <- inicio_fin %>% slice(250001:300000)
seleccion7 <- inicio_fin %>% slice(300001:350000)
seleccion8 <- inicio_fin %>% slice(350001:400000)
seleccion9 <- inicio_fin %>% slice(400001:450000)
seleccion10 <- inicio_fin %>% slice(450001:500000)
seleccion11 <- inicio_fin %>% slice(500001:550000)
seleccion12 <- inicio_fin %>% slice(550001:600000)
seleccion12 <- inicio_fin %>% slice(600001:650000)
seleccion13 <- inicio_fin %>% slice(650001:700000)
seleccion14 <- inicio_fin %>% slice(700001:750000)
seleccion15 <- inicio_fin %>% slice(750001:800000)
seleccion16 <- inicio_fin %>% slice(800001:850000)

calculo_eneros <- function(inicio, intervalo, id_trabajador, fin) {
    enero = update( inicio, year = year(inicio) + 1, month = 1, day = 1 )
    a <- tibble(
        id_trabajador = numeric(),
        #inicio = Date(),
        tiempo = Date(),
        r34 = numeric()
    )
    while (fin>enero) {
        a <- a %>% add_row (
            id_trabajador = id_trabajador,
            #inicio = inicio,
            tiempo = enero,
            r34 = 999
        )


        enero = ymd(enero) %m+% years(1)
    }
    return(a)
}


calculo_diciembres <- function(inicio, intervalo, id_trabajador, fin) {
    diciembre = as.Date(ifelse(month(inicio)<12,
                           update( inicio, year = year(inicio), month = 12, day = 1 ),
                           update( inicio, year = year(inicio) + 1, month = 12, day = 1 )
    ))
    a <- tibble(
        id_trabajador = numeric(),
        #inicio = Date(),
        tiempo = Date(),
        r34 = numeric()
    )
    while (fin>diciembre) {
        a <- a %>% add_row (
            id_trabajador = id_trabajador,
            #inicio = inicio,
            tiempo = diciembre,
            r34 = 999
        )


        diciembre = ymd(diciembre) %m+% years(1)
    }
    return(a)
}




eneros1 <- pmap_dfr(seleccion1, calculo_eneros) #
diciembres1 <- pmap_dfr(seleccion1, calculo_diciembres) #


start_time <- Sys.time(); eneros2 <- pmap_dfr(seleccion2, calculo_eneros); eneros3 <- pmap_dfr(seleccion3, calculo_eneros); eneros4 <- pmap_dfr(seleccion4, calculo_eneros); eneros5 <- pmap_dfr(seleccion5, calculo_eneros); eneros6 <- pmap_dfr(seleccion6, calculo_eneros); eneros7 <- pmap_dfr(seleccion7, calculo_eneros) ; Sys.time() - start_time;

# para hombres agrego estas selecciones de acá:
# start_time <- Sys.time(); eneros8 <- pmap_dfr(seleccion8, calculo_eneros) ; eneros9 <- pmap_dfr(seleccion9, calculo_eneros) ; eneros10 <- pmap_dfr(seleccion10, calculo_eneros); eneros11 <- pmap_dfr(seleccion11, calculo_eneros); eneros12 <- pmap_dfr(seleccion12, calculo_eneros); eneros13 <- pmap_dfr(seleccion13, calculo_eneros) ; eneros14 <- pmap_dfr(seleccion14, calculo_eneros); eneros15 <- pmap_dfr(seleccion15, calculo_eneros) ; eneros16 <- pmap_dfr(seleccion16, calculo_eneros);Sys.time() - start_time # Time difference of 1.399611 hours


start_time <- Sys.time(); diciembres2 <- pmap_dfr(seleccion2, calculo_diciembres);  diciembres3 <- pmap_dfr(seleccion3, calculo_diciembres); diciembres4 <- pmap_dfr(seleccion4, calculo_diciembres); diciembres5 <- pmap_dfr(seleccion5, calculo_diciembres); diciembres6 <- pmap_dfr(seleccion6, calculo_diciembres); diciembres7 <- pmap_dfr(seleccion7, calculo_diciembres) ; Sys.time() - start_time

# para hombres agrego estas selecciones de acá:
# start_time <- Sys.time(); diciembres8 <- pmap_dfr(seleccion8, calculo_diciembres); diciembres9 <- pmap_dfr(seleccion9, calculo_diciembres); diciembres10 <- pmap_dfr(seleccion10, calculo_diciembres);diciembres11 <- pmap_dfr(seleccion11, calculo_diciembres);diciembres12 <- pmap_dfr(seleccion12, calculo_diciembres);diciembres13 <- pmap_dfr(seleccion13, calculo_diciembres);diciembres14 <- pmap_dfr(seleccion14, calculo_diciembres); diciembres15 <- pmap_dfr(seleccion15, calculo_diciembres);diciembres16 <- pmap_dfr(seleccion16, calculo_diciembres);Sys.time() - start_time # Time difference of 33.06524 mins


library(data.table)
eneros_diciembres <- list(
    eneros1,
    eneros2,
    eneros3,
    eneros4,
    eneros5,
    eneros6,
    eneros7,
    # eneros8,
    # eneros9,
    # eneros10,
    # eneros11,
    # eneros12,
    # eneros13,
    # eneros14,
    # eneros15,
    # eneros16,
    diciembres1,
    diciembres2,
    diciembres3,
    diciembres4,
    diciembres5,
    diciembres6,
    diciembres7
    # diciembres8,
    # diciembres9,
    # diciembres10,
    # diciembres11,
    # diciembres12,
    # diciembres13,
    # diciembres14,
    # diciembres15,
    # diciembres16
)

nuevos_edges <- rbindlist(eneros_diciembres,
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

# saveRDS(df_completo_eneros_diciembres, file = "./materiales/edges_hombres.rds")
# saveRDS(df_completo_eneros_diciembres, file = "./materiales/edges_mujeres.rds")
