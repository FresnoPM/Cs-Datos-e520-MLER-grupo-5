library(data.table)
library(dplyr)
MLER_csv <- fread("./materiales/MLER.csv")
df <- as.data.frame(MLER_csv)

calcular_edad <- function(tiempo, fnacim){
  edad = as.integer((tiempo-fnacim) / (30.4375*12))
  return(edad)
  # OJO! cuando un registro tiene fnacim = NA edad = NA también
}

# calcular las mutaciones de df original es muy caro, así que, para prevenir descuidos, pongo una verificación previa de forma que sólo lo haga si no fue calculado ya
if (class(df$tiempo)!="Date" ||
    class(df$fnacim)!="Date" ||
    !"edad" %in% colnames(df)) {

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

  # Verifico que la transformación haya sido correcta
  if (class(df$tiempo)!="Date" ||
      class(df$fnacim)!="Date" ||
      !"edad" %in% colnames(df)) { print("Transformación exitosa")}
  saveRDS(df, file = "df.rds")
  # Borro objetos auxiliares
  rm(chunk_size)
}

df_mujer <- df %>% filter(sexo==1)
# 15.346.369    registros totales
#    210.418    id_trabajador únicos
saveRDS(df_mujer, file = "df_mujer.rds")

df_hombre <- df %>% filter(sexo==2)
# 33.788.884    rows
#    376.291    id_trabajador únicos
saveRDS(df_hombre, file = "df_hombre.rds")

# -------------------------------------------
#
#
# Después de todo ese esfuerzo de cálculo en memoria quiero conservar mis dfs así que guardo todo el entorno
# save.image(file = ".RData")
# Si  lo quiero recuperar:
# (load(".RData"))
# Para recuperar los dataframes por separado
# df <- readRDS("df.rds")
# df_mujer <- readRDS("df_mujer.rds")
# df_hombre <- readRDS("df_hombre.rds")