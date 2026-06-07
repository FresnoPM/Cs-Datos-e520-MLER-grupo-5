library(dplyr)
library(tidyr)
library(lubridate)
# df_mujer <- readRDS("./materiales/df_mujer.rds")
# id_test1 <- head(unique(df_mujer$id_trabajador))
# id_test2 <- tail(unique(df_mujer$id_trabajador))
# id_test3 <- sample(unique(df_mujer$id_trabajador), size = 5)



transicion_letra <- df_mujer %>%
    # filter(.$id_trabajador %in% id_test3) %>%
    select(id_trabajador, id_relacion, tiempo, letra, r32, r34, edad) %>%
    group_by(id_trabajador,letra) %>% # [348,299 grupos]
    add_count(id_trabajador,letra) %>%
    arrange(desc(id_trabajador), tiempo) %>%
    ungroup()%>%
    mutate(
        sig_letra = ifelse(
            id_trabajador == lead(id_trabajador, default = 999),
            lead(letra, default = 999),
            999), # sig_letra es a donde transiciona en el sig registro, sólo es 999 cuando es el último registro de ese id_trabajador
        intervalo = ifelse(
            id_trabajador == lead(id_trabajador, default = 999),
            interval( ymd(tiempo), ymd( lead(tiempo))) %/% months(1),
            999)
    ) %>%
    group_by(id_trabajador) %>%
    add_count(id_trabajador, name = "total_aportes") %>%
    ungroup()

tolerancia = as.numeric(2)
transicion_letra_volver <- rbind(transicion_letra, uncount(
    transicion_letra %>%
        filter( intervalo >= as.numeric(tolerancia),
                intervalo != 999
        ) # fin filter
    , 1) %>%  # fin uncount
        mutate (
            id_relacion = 999,
            letra = 999,
            r32 = 999,
            r34 = 999,
            edad = round(as.numeric(edad)+(as.numeric(intervalo)/12), digits = 0),
            n = intervalo,
            tiempo = ymd(as.Date(tiempo)) %m+% months(as.numeric(intervalo-1))
        ) %>%
        mutate(
            intervalo = 1,
            # cambio  intervalo a 1 porque es lo que tarda desde el mes anterior a volver hasta el mes a volver
        )
    # hasta acá trabajamos sólo con las nuevas rows que van a ser bindeadas al final
) %>% # fin rbind
    arrange(desc(id_trabajador), tiempo, letra) %>%
    # ordeno para que se intercalen como corresponde porque rbind pone los uncount nuevos al final de todo y hago hincapié en también ordenar por letra porque así evitamos intercaladode diferentes letras en mismo periodo

    mutate(sig_letra = ifelse( intervalo >= as.numeric(tolerancia), 999, sig_letra))
    # ajusto  la transición hacia afuera del sistema

View(transicion_letra_volver)



# --------- Interpretación de cada línea ---------
transicion_letra_volver <- transicion_letra_volver %>% mutate(interpretacion = case_when(
    sig_letra == 999 & intervalo == 999 ~ paste("sale del sistema definitivamente luego de realizar aportes por ", total_aportes, " meses"),
    sig_letra == 999                    ~ paste("sale del sistema por los próximos ", intervalo, " meses"),
    letra == 999                        ~ paste("vuelve al sistema después de ", dplyr::lag(intervalo), " meses" ),
    intervalo == 0                      ~ "tiene otro trabajo en el mismo periodo",
    letra == sig_letra & intervalo == 1 ~ "continúa en la misma letra",
    letra != sig_letra & intervalo == 1 ~ "continua con otra letra",
)
)




##########################################
##########################################
###### REMUNERACION ANUAL POR LETRA ######
##########################################
##########################################
##########################################

rem_por_anio_mujer <- df_mujer %>% select(tiempo, letra, r34, rem_tot, edad)%>%
    dplyr::filter( letra  != as.numeric(0) , rem_tot != as.numeric(0) ) %>%
    mutate(anio = year(tiempo)) %>%
    summarise(
        .by = c(anio, letra),
        media = mean(rem_tot, na.rm = TRUE),
        median = median(rem_tot, na.rm = TRUE),
        min = min(rem_tot, na.rm = TRUE),
        max = max(rem_tot, na.rm = TRUE),
        #.groups = "drop_last"
    ) %>% ungroup() %>% arrange(letra, anio)


salario_por_anio_hombre <- df_hombre %>% select(tiempo, letra, r34, rem_tot, edad)%>%
    dplyr::filter( letra  != as.numeric(0) , rem_tot != as.numeric(0) ) %>%
    mutate(anio = year(tiempo)) %>%
    summarise(
        .by = c(anio, letra),
        media = mean(rem_tot, na.rm = TRUE),
        median = median(rem_tot, na.rm = TRUE),
        min = min(rem_tot, na.rm = TRUE),
        max = max(rem_tot, na.rm = TRUE),
        #.groups = "drop_last"
    ) %>% ungroup() %>% arrange(letra, anio)
View(salario_por_anio_hombre)

