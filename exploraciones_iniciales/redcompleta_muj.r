library(tidyverse)
library(arrow)
library(TraMineR)
library(paletteer)
desc_letra <- readRDS("./materiales/desc_letra.rds")
# PASO 1: Filtrado y categorización de sectores

ds <- open_dataset("./materiales/MLER.parquet")

df_muj_sectores <- ds %>%
    filter(sexo == 1) %>%
    select(id_trabajador, tiempo, letra) %>%
    # Si hay pluriempleo, se queda con el sector del primer trabajo que aparezca en el mes.
    distinct(id_trabajador, tiempo, .keep_all = TRUE) %>%
    collect() %>%
    mutate(
        letra = desc_letra$descripcion[ match(letra, desc_letra$letra) ]
    ) %>%

    select(id_trabajador, tiempo, letra)
df_secuencias_muj <- df_muj_sectores %>%
    pivot_wider(
        names_from = tiempo,
        values_from = letra,
        values_fill = "Salida temporal",
        names_sort = TRUE # Mantenemos la corrección de la línea de tiempo
    ) %>%
    arrange(id_trabajador)

matriz_estados_muj <- as.data.frame(df_secuencias_muj %>% select(-id_trabajador))

alfabeto_sectores <- desc_letra$descripcion
colores_sectores <- paletteer_d("khroma::discreterainbow", n = 17)
secuencia_muj_sectores <- seqdef(
    data = matriz_estados_muj,
    alphabet = alfabeto_sectores,
    states = alfabeto_sectores,
    cpal = colores_sectores,
    id = df_secuencias_muj$id_trabajador
)
# ====================================================================
# PASO 4: El Gráfico Multicolores
# ====================================================================
seqdplot(
    secuencia_muj_sectores,
    main = "Distribución Sectorial - Mujeres (1996-2021)",
    ylab = "Proporción de Trabajadores",
    xlab = "Meses",
    labels = alfabeto_sectores, xtstep = 12,
    border = NA,
    with.legend = "right"
    , cex.legend = 0.6
)
# ====================================================================
# PASO 5: Muestreo Aleatorio y Gráfico de Índice (Trayectorias)
# ====================================================================

# 1. Fijamos una "semilla" para que el random sea reproducible.
# Esto asegura que si corrés el código mañana, te toque la misma gente y el gráfico no cambie.
set.seed(2001)

# 2. Elegimos 500 números al azar entre el 1 y el total de hombres que tenemos
indices_muestra <- sample(1:nrow(secuencia_muj_sectores), 10000)

# 3. Recortamos nuestro objeto TraMineR usando esos índices
secuencia_muj_muestra <- secuencia_muj_sectores[indices_muestra, ]

# 3. Assign the colors to the sequence object
cpal(secuencia_muj_muestra) <- paletteer_d("trekcolors::lcars_cardassian", n = 17)

# 4. Generamos el gráfico de Índice (seqiplot)
seqiplot(
    secuencia_muj_muestra,
    main = "Trayectorias Individuales - Mujeres (Muestra Aleatoria n=10.000)",
    ylab = "Trabajadores (1 línea = 1 persona)",
    xlab = "1996 - 2021",

    # Aplicamos la magia sugerida por el manual para que sea legible
    border = NA,       # Saca el borde negro que ensucia la imagen
    space = 0,         # Apila las líneas sin espacios en blanco en el medio
    idxs = 0,          # tlim=0 le dice a TraMineR: "Graficá a los 500 enteros, no solo 10"
    xtstep = 12,
    # Ubicación de la leyenda
    with.legend = "right",
    sortv = "from.start"
    , cex.legend = 0.6

)





# Gráfico de Tiempo Medio de Permanencia
seqmtplot(
    secuencia_muj_sectores, # Usamos la base completa, no la muestra de 500
    main = "Tiempo Medio de Permanencia - Mujeres (1996-2021)",
    ylab = "Meses promedio",
    border = NA,
    with.legend = "right"
)
