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
            999) # intervalo en cuánto tiempo pasa hasta el siguiente registro, sólo es 999 cuando es el último registro de ese id_trabajador
    ) %>% filter(.$intervalo > 0)




## cálculos auxiliares

inicio_index <- which(df$intervalo > 1 & df$intervalo !=999)
inicio_df  <- df %>%
    select(id_trabajador, tiempo, intervalo) %>%
    mutate(orig_id = row_number(), .before = 1) %>%
    slice(inicio_index) %>%
    rename(inicio = tiempo)

fin_index <- inicio_index + 1
fin_df <- df %>%
    select(tiempo)%>%
    slice(fin_index) %>%
    rename(fin = tiempo)

result <- bind_cols(inicio_df, fin_df)
print(result , n=30 )





# para cada línea en el df de result
    inicio <- ymd(result$inicio)
    fin <- ymd(result$fin)

    enero <- update(inicio, year = year(inicio) + 1, month = 1, day = 1)
    diciembre <- update(inicio, year = year(inicio) + 12, month = 1, day = 1)


while ( fin > enero ) {

    df %>% add_row(
        id_trabajador = result$id_trabajador,
        tiempo = enero,
        r34 = 999
    )

    enero <- enero %m+% years(1)
}

while ( fin > diciembre ) {

    df %>% add_row(
        id_trabajador = result$id_trabajador,
        tiempo = diciembre,
        r34 = 999
    )

    diciembre <- diciembre %m+% years(1)
}



# al finalizar de agregar todos los eneros y diciembres fuera del sistema agrego  la siguiente r_34

df <- df %>%
    mutate(
    sig_r34 = ifelse(
    id_trabajador == lead(id_trabajador, default = 999),
    lead(r34, default = 999),
    999), # sig_r34 es a donde transiciona en el sig registro, sólo es 999 cuando es el último registro de ese id_trabajador (sale del sistema)

)