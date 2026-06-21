library(tidyverse)
library(arrow)
library(TraMineR)
library(paletteer)
desc_letra <- readRDS("./materiales/desc_letra.rds")
# PASO 1: Filtrado y categorización de sectores

ds <- open_dataset("./materiales/MLER.parquet")

df_hom_sectores <- ds %>%
    #filter(sexo == 1) %>%
    filter(sexo == 2) %>%
    select(id_trabajador, tiempo, letra) %>%
    # Si hay pluriempleo, se queda con el sector del primer trabajo que aparezca en el mes.
    distinct(id_trabajador, tiempo, .keep_all = TRUE) %>%
    collect() %>%
    mutate(
        letra = desc_letra$descripcion[ match(letra, desc_letra$letra) ]
    ) %>%

    select(id_trabajador, tiempo, letra)
df_secuencias_hom <- df_hom_sectores %>%
    pivot_wider(
        names_from = tiempo,
        values_from = letra,
        values_fill = "Salida temporal",
        names_sort = TRUE # Mantenemos la corrección de la línea de tiempo
    ) %>%
    arrange(id_trabajador)

matriz_estados_hom <- as.data.frame(df_secuencias_hom %>% select(-id_trabajador))

alfabeto_sectores <- desc_letra$descripcion
colores_sectores <- paletteer_d("khroma::discreterainbow", n = 17)
secuencia_hom_sectores <- seqdef(
    data = matriz_estados_hom,
    alphabet = alfabeto_sectores,
    states = alfabeto_sectores,
    cpal = colores_sectores,
    id = df_secuencias_hom$id_trabajador
)
# ====================================================================
# PASO 4: El Gráfico Multicolores
# ====================================================================
seqdplot(
    secuencia_hom_sectores,
    main = "Distribución Sectorial - Varones (1996-2021)",
    ylab = "Proporción de Trabajadores",
    xlab = "Meses",
    labels = alfabeto_sectores, xtstep = 12,
    border = NA,
    with.legend = "right"
)
# ====================================================================
# PASO 5: Muestreo Aleatorio y Gráfico de Índice (Trayectorias)
# ====================================================================

# 1. Fijamos una "semilla" para que el random sea reproducible.
# Esto asegura que si corrés el código mañana, te toque la misma gente y el gráfico no cambie.
set.seed(2001)

# 2. Elegimos 500 números al azar entre el 1 y el total de hombres que tenemos
indices_muestra <- sample(1:nrow(secuencia_hom_sectores), 10000)

# 3. Recortamos nuestro objeto TraMineR usando esos índices
secuencia_hom_muestra <- secuencia_hom_sectores[indices_muestra, ]

# 3. Assign the colors to the sequence object
cpal(secuencia_hom_muestra) <- paletteer_d("trekcolors::lcars_cardassian", n = 17)

# 4. Generamos el gráfico de Índice (seqiplot)
seqiplot(
    secuencia_hom_muestra,
    main = "Trayectorias Individuales - Varones (Muestra Aleatoria n=10.000)",
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
    secuencia_hom_sectores, # Usamos la base completa, no la muestra de 500
    main = "Tiempo Medio de Permanencia - Varones (1996-2021)",
    ylab = "Meses promedio",
    border = NA,
    with.legend = "right"
)
