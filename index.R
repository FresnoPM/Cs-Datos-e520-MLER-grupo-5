library(data.table)
MLER_csv <- fread("./materiales/MLER.csv")
library(dplyr)
head(MLER_csv)

df <- as.data.frame(MLER_csv)

calcular_edad <- function(fnacim, tiempo){
    tiempo = as.integer(substr(tiempo, 1, 4))
    edad = tiempo - fnacim
    return(edad)
    # cuando un registro tiene fnacim = NA edad = NA también
}

df_mujer <- df %>% filter(sexo==1)%>%
    mutate( edad = calcular_edad(.$fnacim, .$tiempo))
# 15.346.369 registros totales
# 210.418 id_trabajador únicos

df_hombre <- df %>% filter(sexo==2)%>%
    mutate( edad = calcular_edad(.$fnacim, .$tiempo))
# 33.788.884 rows
# 376291 id_trabajador únicos

# write.csv(df_mujer, "materiales/df_mujer.csv", row.names = FALSE)
# write.csv(df_hombre, "materiales/df_hombre.csv", row.names = FALSE)
