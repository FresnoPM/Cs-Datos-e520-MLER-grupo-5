library(tidyverse)
library(arrow)

# Set up ------------------------------------------------------------------


# PASO 1: Filtrar en disco y traer a la RAM
ds <- open_dataset("./dataset/MLER.parquet")

# Traemos solo a las mujeres y únicamente en los dos meses de interés
df_salto_crisis <- ds %>%
    filter(
        sexo == 1,
        tiempo %in% c(200012, 200101) # 200012 = Diciembre 2000 | 200101 = Enero 2001
    ) %>%
    # Seleccionamos solo las columnas críticas para ahorrar muchísima memoria RAM
    select(id_trabajador, tiempo, r34) %>%
    collect()

# PASO 2: Armar las Transiciones (Origen y Destino)
transiciones_brutas <- df_salto_crisis %>%
    arrange(id_trabajador, tiempo) %>%
    group_by(id_trabajador) %>%
    mutate(
        origen = r34,
        destino = lead(r34),             # El rubro del mes siguiente
        tiempo_destino = lead(tiempo)    # El mes siguiente
    ) %>%
    ungroup() %>%

    # Nos quedamos ESTRICTAMENTE con las filas donde vemos el salto de Dic a Ene.
    # Esto elimina los NA de quienes desaparecieron de la base en enero.
    filter(tiempo == 200012, tiempo_destino == 200101)


# PASO 3: Consolidar la tabla de 3 columnas para el Heatmap

transiciones_reales <- transiciones_brutas %>%
    # Opcional pero recomendado: filtramos a las que se quedaron en el mismo rubro
    # para que la diagonal principal no "encandile" el heatmap y nos deje ver la movilidad real.
    filter(origen != destino) %>%

    # Contamos el volumen de cada salto intersectorial
    count(origen, destino, name = "flujo_bruto")

# Ver el resultado
print(head(transiciones_reales)) # Esto es basicamente el armado de una tabla de frecuencias con 3 columnas
# las 3 columnas van a ser fundamentales para el heatmap.


# Heatmap -----------------------------------------------------------------


# 1. Transformar la lista a Matriz Ancha (Adyacencia)
matriz_adyacencia <- transiciones_reales %>%
    pivot_wider(names_from = destino, values_from = flujo_bruto, values_fill = 0) %>%
    column_to_rownames("origen") %>%
    as.matrix()


# 2. El Clustering Jerárquico (El requerimiento de la Clase 13)
# Calculamos distancias y agrupamos las filas y columnas similares
distancias <- dist(matriz_adyacencia)
clustering <- hclust(distancias)

# Obtenemos el orden óptimo de los rubros
orden_optimo <- rownames(matriz_adyacencia)[clustering$order]

# 3. Dibujar el Heatmap Ordenado con ggplot2
transiciones_reales %>%
    # Forzamos a que ggplot respete el orden del clustering jerárquico
    mutate(
        origen = factor(origen, levels = orden_optimo),
        destino = factor(destino, levels = orden_optimo)
    ) %>%
    ggplot(aes(x = destino, y = origen, fill = flujo_bruto)) +
    geom_tile() +
    scale_fill_viridis_c(option = "inferno", name = "Volumen\nde Traspaso") +
    theme_minimal() +
    theme(
        # Ocultamos los nombres de los 500 ejes porque no van a entrar
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank()
    ) +
    labs(
        title = "Heatmap de Transiciones Laborales (r34)",
        subtitle = "Ordenado por clustering jerárquico para revelar comunidades sectoriales",
        x = "Rubro de Destino (t+1)",
        y = "Rubro de Origen (t)"
    )


# Heatmap filtrando sectores relevantes -----------------------------------

# PASO 1: Filtrar solo los rubros r34 con alto movimiento bruto

# Contamos el movimiento total por rubro para identificar los "hubs"
rubros_principales <- transiciones_reales %>%
    group_by(origen) %>%
    summarise(volumen_total = sum(flujo_bruto)) %>%
    # Elegimos un umbral (ej. rubros que muevan más de 100 personas de forma neta/bruta)
    # Jugá con este número para achicar o agrandar la matriz final
    filter(volumen_total > 5) %>%
    pull(origen)
print(length(rubros_principales))
# Filtramos la matriz original para quedarnos solo con el cruce de esos rubros top
transiciones_filtradas <- transiciones_reales %>%
    filter(origen %in% rubros_principales, destino %in% rubros_principales)


# PASO 2: Matriz y Clustering con los datos podados
matriz_adyacencia <- transiciones_filtradas %>%
    pivot_wider(names_from = destino, values_from = flujo_bruto, values_fill = 0) %>%
    column_to_rownames("origen") %>%
    as.matrix()

distancias <- dist(matriz_adyacencia)
clustering <- hclust(distancias)
orden_optimo <- rownames(matriz_adyacencia)[clustering$order]

# PASO 3: El Heatmap definitivo con nombres legibles
transiciones_filtradas %>%
    mutate(
        origen = factor(origen, levels = orden_optimo),
        destino = factor(destino, levels = orden_optimo)
    ) %>%
    ggplot(aes(x = destino, y = origen, fill = flujo_bruto)) +
    geom_tile(color = "white", size = 0.1) + # Le agrega una sutil grilla blanca alrededor de cada cuadradito
    scale_fill_viridis_c(option = "inferno", name = "Trabajadores") +
    theme_minimal() +
    theme(
        # Rotamos el texto del eje X a 90 grados y achicamos la letra para que se lea perfecto
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 6),
        axis.text.y = element_text(size = 6),
        panel.grid = element_blank()
    ) +
    labs(
        title = "Matriz de Transición Laboral Femenina Reconfigurada",
        subtitle = "Principales ramas r34 ordenadas por afinidad estructural",
        x = "Rubro de Destino (Enero)",
        y = "Rubro de Origen (Diciembre)"
    )

# El ultimo grafico quedó muy bien. Pero solo esta hecho PARA LAS MUJERES con salto diciembre a enero de 2001.
# Asi que el heatmap final representa la movilidad laboral de las mujeres entre esos dos meses, mostrando solo los rubros con mayor movimiento.
# Si queremos hacer lo mismo para los hombres, tenemos que repetir el proceso pero filtrando por sexo == 2 y usando la variable r32 en lugar de r34.
# Y finalmente comparar.
# La interpretacion exhaustiva del grafico queda pendiente. Primero subo esto a github y lo discutimos el viernes.
