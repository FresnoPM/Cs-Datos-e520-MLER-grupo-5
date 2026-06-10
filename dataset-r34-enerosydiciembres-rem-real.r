library(dplyr)
library(tidyr)
library(lubridate)
 id_test1 <- head(unique(df_mujer_real$id_trabajador))
 id_test2 <- tail(unique(df_mujer_real$id_trabajador))
 id_test3 <- sample(unique(df_mujer_real$id_trabajador), size = 5)
 id_seleccionadas <- c(201796,442849)

df <- df_mujer_real %>%
    filter(.$id_trabajador %in% id_seleccionadas) %>%
    select(id_trabajador, tiempo, r34) %>%
    group_by(id_trabajador,r34) %>% # [ grupos]
    # add_count(id_trabajador,r34) %>%
    arrange(desc(id_trabajador), tiempo) %>%
    ungroup()%>%
    mutate(
        intervalo = ifelse(
            id_trabajador == lead(id_trabajador, default = 999),
            interval( ymd(tiempo), ymd( lead(tiempo))) %/% months(1),
            999) # intervalo en cuánto tiempo pasa hasta el siguiente registro, sólo es 999 cuando es el último registro de ese id_trabajador. Es una variable temporal, al final será eliminada.
    ) %>% filter(.$intervalo > 0)




## cálculos auxiliares - creo un df donde sólo haya el inicio y el fin de cada periodo de ausencia para, luego calcular los eneros y diciembres en el medio
identifico_intervalos <- function(df) {
    inicio_index <- which(df$intervalo > 1 & df$intervalo !=999)
    fin_index <- inicio_index + 1

    inicio_df  <- df %>%
        select(id_trabajador, tiempo) %>%
        slice(inicio_index) %>%
        rename(inicio = tiempo)

    fin_df <- df %>%
        select(tiempo)%>%
        slice(fin_index) %>%
        rename(fin = tiempo)

    inicio_fin <- bind_cols(inicio_df, fin_df)
    return(inicio_fin)

}
inicio_fin <- identifico_intervalos(df)

calculo_nuevos_nodos <- function(df){

    new_rows <- data.frame(id_trabajador = numeric() , tiempo = Date(), r34 = numeric())
    for (i in 1:nrow(df)) {
        # Access data from the current row
        id_trabajador <- df[i, "id_trabajador"][[1]]
        inicio <- as.Date( df[i, "inicio"][[1]])
        fin <- as.Date( df[i, "fin"][[1]])
        enero <- update(inicio, year = year(inicio) + 1, month = 1, day = 1)

        while (fin > enero) {
            new_item <- data.frame(
                id_trabajador = id_trabajador,
                tiempo = enero,
                r34 = 999
            )
            new_rows <- new_rows %>% add_row (new_item)
            enero = ymd(enero) %m+% years(1)
        }

        diciembre <- update(inicio, year = year(inicio) + 1, month = 12, day = 1)
        while (fin > diciembre) {
            new_item <- data.frame(
                id_trabajador = id_trabajador,
                tiempo = diciembre,
                r34 = 999
            )
            new_rows <- new_rows %>% add_row (new_item)
            diciembre = ymd(diciembre) %m+% years(1)
        }
        #
    }
    return(new_rows)
}

nuevos_nodos <- calculo_nuevos_nodos(inicio_fin)

df_con_nuevos_nodos <- bind_rows(
    df %>% select(-intervalo)
    , nuevos_nodos
) %>% arrange(id_trabajador, tiempo)

df_solo_eneros_y_diciembres <- df_con_nuevos_nodos %>% filter( month(tiempo) == 1 | month(tiempo) == 12 ) %>% arrange(id_trabajador, tiempo)


# ····················3 acá va lo del archivo pruebas# ····················3 acá va lo del archivo pruebas

# al finalizar de agregar todos los eneros y diciembres fuera del sistema agrego  la siguiente r_34

df_sig_r34 <- df_solo_eneros_y_diciembres %>%
    mutate(
    sig_r34 = ifelse(
    id_trabajador == lead(id_trabajador, default = 999),
    lead(r34),
    0), # sig_r34 es a donde transiciona en el sig registro: 999 es cuando se va un ratito, sólo es 0 cuando es el último registro de ese id_trabajador (sale del sistema)

)
