library(dplyr)
library(tidyr)
library(lubridate)

transicion_letra <- df_mujer %>%
    select(id_trabajador, id_relacion, tiempo, letra, r32, r34, edad) %>%
    group_by(id_trabajador,letra) %>% # [348,299 grupos]
    add_count(id_trabajador,letra) %>%
    arrange(desc(id_trabajador), tiempo) %>%
    ungroup()%>%
    mutate(
        sig_letra = ifelse(
            id_trabajador == lead(id_trabajador, default = 99),  lead(letra, default = 99), 99),
        intervalo = ifelse(
            id_trabajador == lead(id_trabajador), interval( ymd(tiempo), ymd(lead(tiempo) )) %/% months(1), 99)

    )


transicion_letra <- transicion_letra %>% mutate(

    interpretacion = case_when(
        sig_letra == 99 & intervalo == 99 ~ "sale del sistema definitivamente",
        letra != sig_letra & intervalo != 99 ~ paste("sale del sistema por ", intervalo, " meses y cambia de letra"),
        letra == sig_letra & intervalo == 1 ~ "continúa en la misma letra",
        letra == sig_letra & intervalo != 1 ~ paste("sale del sistema por ", intervalo, " meses pero vuelve a la misma letra"),
        letra != sig_letra & intervalo == 1 ~ "transiciona de letra inmediatamente"
    )
)

View(transicion_letra)

saveRDS(transicion_letra, file = "materiales/df_mujer_transicion.rds")
