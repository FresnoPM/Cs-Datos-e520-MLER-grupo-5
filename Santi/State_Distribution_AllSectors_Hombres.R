library(tidyverse)
library(arrow)
library(TraMineR)

# PASO 1: Filtrado y categorización de sectores

ds <- open_dataset("./materiales/MLER.parquet")

df_hom_sectores <- ds %>%
    filter(
        sexo == 2,
        tiempo >= 199901, tiempo <= 200312
    ) %>%
    # Traemos la columna de la letra de la actividad
    select(id_trabajador, tiempo, letra) %>%

    # OJO ACÁ: Agregamos .keep_all = TRUE para que distinct no nos borre la columna 'letra'
    # Si hay pluriempleo, se queda con el sector del primer trabajo que aparezca en el mes.
    distinct(id_trabajador, tiempo, .keep_all = TRUE) %>%
    collect() %>%
    # Mapeamos las letras a nuestros 5 macro-sectores
    # Primero lo hago con 5 macro-sectores, luego intento hacerlo con todos a ver qué sale.
    mutate(estado = case_when(
        letra == 1 ~ "Agro",
        letra %in% c(2, 3, 4, 5) ~ "Industria",
        letra == 6 ~ "Construcción",
        letra == 7 ~ "Comercio",
        TRUE ~ "Servicios"
    )) %>%
    # Nos quedamos solo con lo que le importa a TraMineR
    select(id_trabajador, tiempo, estado)

# ====================================================================
# PASO 2: Transformación a Secuencia
# ====================================================================
df_secuencias_hom <- df_hom_sectores %>%
    pivot_wider(
        names_from = tiempo,
        values_from = estado,
        values_fill = "Fuera del sistema",
        names_sort = TRUE # Mantenemos la corrección de la línea de tiempo
    ) %>%
    arrange(id_trabajador)

matriz_estados_hom <- as.data.frame(df_secuencias_hom %>% select(-id_trabajador))

# ====================================================================
# PASO 3: Creación del Objeto TraMineR con Sectores
# ====================================================================
# Definimos el nuevo alfabeto ampliado
alfabeto_sectores <- c("Agro", "Industria", "Construcción", "Comercio", "Servicios", "Fuera del sistema")

# Asignamos una paleta de colores bien contrastante para que se distingan en el gráfico
colores_sectores <- c(
    "#27AE60", # Verde para Agro
    "#F39C12", # Naranja para Industria
    "#7F8C8D", # Gris para Construcción
    "#8E44AD", # Violeta para Comercio
    "#2980B9", # Azul para Servicios
    "#000"  # negro para Fuera del sistema
)

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
    main = "Distribución Sectorial - Varones (1999-2003)",
    ylab = "Proporción de Trabajadores",
    xlab = "Meses",
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
indices_muestra <- sample(1:nrow(secuencia_hom_sectores), 100)

# 3. Recortamos nuestro objeto TraMineR usando esos índices
secuencia_hom_muestra <- secuencia_hom_sectores[indices_muestra, ]

# 4. Generamos el gráfico de Índice (seqiplot)
seqiplot(
    secuencia_hom_muestra,
    main = "Trayectorias Individuales - Varones (Muestra Aleatoria n=100)",
    ylab = "Trabajadores (1 línea = 1 persona)",
    xlab = "Meses (1999 - 2003)",

    # Aplicamos la magia sugerida por el manual para que sea legible
    border = NA,       # Saca el borde negro que ensucia la imagen
    space = 0,         # Apila las líneas sin espacios en blanco en el medio
    tlim = 0,          # tlim=0 le dice a TraMineR: "Graficá a los 500 enteros, no solo 10"

    # Ubicación de la leyenda
    with.legend = "right"

)


# Gráfico de Tiempo Medio de Permanencia
seqmtplot(
    secuencia_hom_sectores, # Usamos la base completa, no la muestra de 500
    main = "Tiempo Medio de Permanencia - Varones (1999-2003)",
    ylab = "Meses promedio",
    border = NA,
    with.legend = "right"
)
