library(data.table)
MLER_csv <- fread("./materiales/MLER.csv")
df <- as.data.frame(MLER_csv)

calcular_edad <- function(tiempo, fnacim){
    edad = as.integer((tiempo-fnacim) / (30.4375*12))
    return(edad)
    # cuando un registro tiene fnacim = NA edad = NA también
}

library(dplyr)

chunk_size <- 1000

# MANEJO DE VALORES DE TIEMPO
# Transformo "tiempo" y "fnacim" en fechas
# Luego calculo la edad al momento del registro
# Lo hago en memoria así que lo divido por partes para que no se me cuelgue el sistema
# ¿Debería reescribir el CSV y luego volver a cargarlo?
df <- df %>%
        mutate(chunk_id = (row_number() - 1) %/% chunk_size) %>%
        group_by(chunk_id) %>%
    ###--- MIS MODIFICACIONES ---###
    mutate(
        tiempo = as.Date(paste0(tiempo,"01"), format = "%Y%m%d")
      , fnacim = as.Date(paste0(fnacim,"0101"), format = "%Y%m%d")
    ) %>%
    mutate(
        edad = calcular_edad(tiempo, fnacim)
    ) %>%
    ###--- MIS MODIFICACIONES ---###
        ungroup() %>%
        select(-chunk_id) # Remove the helper column


df_mujer <- df %>% filter(sexo==1)
# 15.346.369    registros totales
#    210.418    id_trabajador únicos

df_hombre <- df %>% filter(sexo==2)
# 33.788.884    rows
#    376.291    id_trabajador únicos
