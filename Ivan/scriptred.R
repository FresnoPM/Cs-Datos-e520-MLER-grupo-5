
library(dplyr)
library(lubridate)
library(purrr)
library(tidyr)


# Filtrado por edad de 20 a 36 --------------------------------------------
#Usa df_mujer_real

df_mujer_joven <- df_mujer_real %>%
  filter(edad >= 25 & edad <= 36)

nrow(df_mujer_joven)


# Filtrado por licencia de 3 meses ----------------------------------------


df_primera_licencia <- df_mujer_joven %>%
  group_by(id_trabajador, tiempo) %>%
  summarise(rem_tot_mes = sum(rem_tot_real, na.rm = TRUE), .groups = "drop") %>%
  arrange(id_trabajador, tiempo) %>%
  group_by(id_trabajador) %>%
  mutate(
    es_licencia = if_else(rem_tot_mes == 0, 1, 0),
    hay_gap = if_else(
      is.na(lag(tiempo)) | tiempo != lag(tiempo) %m+% months(1),
      1, 0
    ),
    cambio_estado = if_else(
      es_licencia != lag(es_licencia, default = first(es_licencia)) | hay_gap == 1,
      1, 0
    ),
    bloque_consecutivo = cumsum(cambio_estado)
  ) %>%
  group_by(id_trabajador, bloque_consecutivo) %>%
  mutate(
    meses_seguidos = if_else(es_licencia == 1, n(), 0L)
  ) %>%
  filter(meses_seguidos >= 3) %>%
  # Nos quedamos con la primera racha larga de cada trabajadora
  group_by(id_trabajador) %>%
  filter(bloque_consecutivo == min(bloque_consecutivo)) %>%
  # Guardamos inicio (primer mes de la racha) y fin (último mes de la racha)
  summarise(
    primer_mes_licencia = min(tiempo),
    ultimo_mes_licencia = max(tiempo),
    .groups = "drop"
  ) %>%
  # El fin de licencia es el mes siguiente al último mes con rem=0
  mutate(fin_licencia = ultimo_mes_licencia %m+% months(1)) %>%
  select(-ultimo_mes_licencia) %>%
  # Descartamos trabajadoras cuyo fin_licencia no existe en el panel
  semi_join(
    df_mujer_joven %>%
      group_by(id_trabajador, tiempo) %>%
      summarise(rem_tot_mes = sum(rem_tot_real, na.rm = TRUE), .groups = "drop") %>%
      filter(rem_tot_mes > 0),
    by = c("id_trabajador", "fin_licencia" = "tiempo")
  ) %>%
  ungroup()


nrow(df_primera_licencia)
# (Opcional) Revisamos cuántos registros quedaron tras el filtro
nrow(df_primera_licencia2)


# Union para sacar la traectoria de toda la vida --------------------------


df_trayectoria_post <- df_mujer_real %>%
  # Traemos el mes de retorno de cada trabajadora
  inner_join(
    df_primera_licencia %>% select(id_trabajador, fin_licencia),
    by = "id_trabajador"
  ) %>%
  # Filtramos desde el mes de retorno inclusive
  filter(tiempo >= fin_licencia) %>%
  # Seleccionamos las variables relevantes
  select(id_trabajador, id_relacion, tiempo, edad, r34, rem_tot_real) %>%
  arrange(id_trabajador, tiempo, id_relacion)
nrow(df_trayectoria_post)
n_distinct(df_trayectoria_post$id_trabajador)


# Cambios laborales -------------------------------------------------------
df_cambios <- df_trayectoria_post %>%
  select(id_trabajador, tiempo, r34) %>%
  arrange(id_trabajador, tiempo) %>%
  group_by(id_trabajador, tiempo) %>%
  summarise(sectores = list(unique(r34)), .groups = "drop") %>%
  group_by(id_trabajador) %>%
  mutate(sectores_ant = lag(sectores)) %>%
  filter(!is.na(sectores_ant)) %>%
  mutate(
    salidas = map2(sectores_ant, sectores, ~ setdiff(.x, .y)),
    entradas = map2(sectores, sectores_ant, ~ setdiff(.x, .y))
  ) %>%
  filter(lengths(salidas) > 0 | lengths(entradas) > 0) %>%
  mutate(
    pares = map2(salidas, entradas, ~ crossing(
      r34_origen = .x,
      r34_destino = .y
    ))
  ) %>%
  filter(lengths(pares) > 0) %>%
  unnest(pares) %>%
  select(id_trabajador, tiempo, r34_origen, r34_destino) %>%
  ungroup()

nrow(df_cambios)
n_distinct(df_cambios$id_trabajador)

# Guardamos las incorporaciones puras por si las necesitamos después
df_incorporaciones <- df_cambios %>%
  filter(is.na(r34_origen))

# Nos quedamos solo con transiciones válidas para la red
df_cambios1 <- df_cambios %>%
  filter(!is.na(r34_origen) & !is.na(r34_destino))

df_edges <- df_cambios %>%
  filter(!is.na(r34_origen) & !is.na(r34_destino)) %>%
  group_by(r34_origen, r34_destino) %>%
  summarise(peso = n(), .groups = "drop") %>%
  arrange(desc(peso))

nrow(df_edges)
head(df_edges, 10)

# Nodos -------------------------------------------------------------------
library(igraph)

# Base: todos los r34 presentes en df_trayectoria_post de las mujeres filtradas
nodos_r34 <- df_trayectoria_post %>%
  distinct(r34) %>%
  filter(!is.na(r34))

# 1. r32 asociado a cada r34 (tomamos el más frecuente en caso de inconsistencias)
r34_r32 <- df_trayectoria_post %>%
  filter(!is.na(r34), !is.na(r32)) %>%
  group_by(r34, r32) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(r34) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  select(r34, r32)

# 2. Remuneración real media y edad media post-licencia
r34_rem_edad <- df_trayectoria_post %>%
  filter(!is.na(r34)) %>%
  group_by(r34) %>%
  summarise(
    rem_media = mean(rem_tot_real, na.rm = TRUE),
    edad_media = mean(edad, na.rm = TRUE),
    .groups = "drop"
  )

# 3. Cantidad de personas que pasaron por el sector (ingresos totales)
r34_ingresos_totales <- df_trayectoria_post %>%
  filter(!is.na(r34)) %>%
  group_by(r34) %>%
  summarise(
    personas_totales = n_distinct(id_trabajador),
    .groups = "drop"
  )

# 4. Personas al final del periodo (último mes de cada trabajadora)
r34_final <- df_trayectoria_post %>%
  filter(!is.na(r34)) %>%
  group_by(id_trabajador) %>%
  filter(tiempo == max(tiempo)) %>%
  ungroup() %>%
  group_by(r34) %>%
  summarise(
    personas_final = n_distinct(id_trabajador),
    .groups = "drop"
  )

# 5. Flujos netos desde df_edges
r34_entradas <- df_edges %>%
  group_by(r34 = r34_destino) %>%
  summarise(entradas = sum(peso), .groups = "drop")

r34_salidas <- df_edges %>%
  group_by(r34 = r34_origen) %>%
  summarise(salidas = sum(peso), .groups = "drop")

r34_flujo <- full_join(r34_entradas, r34_salidas, by = "r34") %>%
  mutate(
    entradas = replace_na(entradas, 0),
    salidas = replace_na(salidas, 0),
    flujo_neto = entradas - salidas
  )

# Ensamblamos la tabla de nodos
df_nodos <- nodos_r34 %>%
  left_join(r34_r32, by = "r34") %>%
  left_join(r34_rem_edad, by = "r34") %>%
  left_join(r34_ingresos_totales, by = "r34") %>%
  left_join(r34_final, by = "r34") %>%
  left_join(r34_flujo, by = "r34")

nrow(df_nodos)
head(df_nodos)

r34_r32 <- df_mujer_joven %>%
  filter(!is.na(r34), !is.na(r32)) %>%
  group_by(r34, r32) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(r34) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  select(r34, r32)

# Construimos la red
g <- graph_from_data_frame(
  d = df_edges,
  vertices = df_nodos,
  directed = TRUE
)

# Calculamos betweenness y degree y los agregamos a df_nodos
df_nodos <- df_nodos %>%
  mutate(
    betweenness = betweenness(g, normalized = TRUE),
    degree_in = degree(g, mode = "in"),
    degree_out = degree(g, mode = "out"),
    degree_total = degree(g, mode = "all")
  )

head(df_nodos)

names(df_nodos)


# Red ---------------------------------------------------------------------

library(dplyr)
library(tidygraph)

# 1. Escudo 1: Forzar el ID a character para evitar problemas entre números y factores
df_nodos_clean <- df_nodos %>%
  mutate(
    r34 = as.character(r34), # Forzamos a texto
    personas_totales = ifelse(is.na(personas_totales), 0, personas_totales),
    tamaño_nodo = personas_totales + 1
  )

# Lista de nodos que realmente existen en nuestra tabla
nodos_validos <- df_nodos_clean$r34

# 2. Escudo 2 y 3: Renombrar a 'from' / 'to' y filtrar nodos fantasma
df_edges_clean <- df_cambios %>%
  filter(!is.na(r34_origen) & !is.na(r34_destino)) %>%
  mutate(
    from = as.character(r34_origen),
    to = as.character(r34_destino)
  ) %>%
  # ¡Este es el filtro clave para evitar el error!
  # Solo conservamos aristas si TANTO el origen como el destino existen en df_nodos
  filter(from %in% nodos_validos & to %in% nodos_validos) %>%
  group_by(from, to) %>%
  summarise(peso_flujo = n(), .groups = 'drop')

# 3. Creación de la red
red_laboral <- tbl_graph(
  nodes = df_nodos_clean,
  edges = df_edges_clean,
  directed = TRUE,
  node_key = "r34"
)

# Verifica que se haya creado correctamente
print(red_laboral)

# Nodos en los movimientos que NO están en la base de nodos
nodos_perdidos <- setdiff(
  union(df_cambios$r34_origen, df_cambios$r34_destino), 
  df_nodos$r34
)

print(nodos_perdidos)

# Intentos de red 2 -------------------------------------------------------


library(ggraph)
library(ggplot2)

# Fijamos semilla para que la distribución del layout sea reproducible
set.seed(123)

# Generación del gráfico
ggraph(red_laboral, layout = 'fr') + 
  
  # 1. Aristas (flechas) con grosor dinámico
  geom_edge_link(
    aes(width = peso_flujo, alpha = 1),
    arrow = arrow(length = unit(2.5, 'mm'), type = "closed"),
    end_cap = circle(4, 'mm'), # Separación entre la flecha y el centro del nodo
    color = "gray55",
    show.legend = FALSE
  ) +
  
  # 2. Nodos con tamaño y color dinámicos
  geom_node_point(
    aes(size = tamaño_nodo, color = rem_media)
  ) +
  
  # 3. Ajuste de escalas de estética
  scale_color_viridis_c(
    option = "plasma", 
    na.value = "grey80",
    name = "Remuneración\nMedia"
  ) +
  scale_size_continuous(
    range = c(2, 12),
    name = "Personas"
  ) +
  scale_edge_width_continuous(range = c(0.3, 2)) +
  
  # 4. Tema minimalista
  theme_graph(background = "#f9f9f9") +
  labs(
    title = "Movilidad laboral en mujeres",
    subtitle = "De 20 a 36 años, a partir de una licencia de 3 meses o más",
    caption = "Nodos son r34"
  ) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 18, color = "#333333"),
    plot.subtitle = element_text(size = 12, color = "#666666")
  )
options(scipen = 999)


library(ggraph)
library(ggplot2)

set.seed(123)

ggraph(red_laboral, layout = 'fr') + 
  
  # 1. Aristas (flechas)
  geom_edge_link(
    aes(width = peso_flujo, alpha = peso_flujo),
    arrow = arrow(length = unit(2.5, 'mm'), type = "closed"),
    end_cap = circle(4, 'mm'), 
    color = "gray55",
    show.legend = FALSE
  ) +
  
  # 2. Nodos: color cambiado a 'r32'
  geom_node_point(
    aes(size = tamaño_nodo, color = as.factor(r32)) # as.factor asegura que R lo trate como categoría
  ) +
  
  # 3. Escalas (usamos _d para datos discretos/categóricos)
  scale_color_viridis_d(
    option = "plasma", 
    na.value = "grey80",
    name = "Código r32"
  ) +
  scale_size_continuous(
    range = c(2, 12),
    name = "Personas\n(Totales + 1)"
  ) +
  scale_edge_width_continuous(range = c(0.3, 2)) + 
  
  # 4. Tema
  theme_graph(background = "#f9f9f9") +
  labs(
    title = "Red de Movilidad Laboral",
    subtitle = "Nodos coloreados por agrupación r32",
    caption = "Visualización generada con ggraph"
  ) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 18, color = "#333333"),
    plot.subtitle = element_text(size = 12, color = "#666666")
  )



library(dplyr)
library(tidygraph)
library(ggraph)
library(ggplot2)



# dfdg --------------------------------------------------------------------


# Extraemos la combinación única de r32 y letra para evitar duplicar filas
mapeo_letras <- df_mujer_joven %>%
  select(r32, letra) %>%
  distinct()

# Agregamos 'letra' a df_nodos y preparamos los atributos
df_nodos_actualizado <- df_nodos %>%
  left_join(mapeo_letras, by = "r32") %>%
  mutate(
    r34 = as.character(r34),
    letra = as.factor(letra), # Forzamos a factor para la escala discreta
    personas_totales = ifelse(is.na(personas_totales), 0, personas_totales),
    tamaño_nodo = personas_totales + 1
  )



nodos_validos <- df_nodos_actualizado$r34

df_edges_clean <- df_cambios %>%
  filter(!is.na(r34_origen) & !is.na(r34_destino)) %>%
  mutate(
    from = as.character(r34_origen),
    to = as.character(r34_destino)
  ) %>%
  filter(from %in% nodos_validos & to %in% nodos_validos) %>%
  group_by(from, to) %>%
  summarise(peso_flujo = n(), .groups = 'drop')

red_laboral <- tbl_graph(
  nodes = df_nodos_actualizado,
  edges = df_edges_clean,
  directed = TRUE,
  node_key = "r34"
)

+

set.seed(123)

ggraph(red_laboral, layout = 'fr') + 
  
  # Flechas de la red
  geom_edge_link(
    aes(width = peso_flujo, alpha = peso_flujo),
    arrow = arrow(length = unit(2.5, 'mm'), type = "closed"),
    end_cap = circle(4, 'mm'), 
    color = "gray55",
    show.legend = FALSE
  ) +
  
  # Nodos coloreados por la variable 'letra'
  geom_node_point(
    aes(size = tamaño_nodo, color = letra)
  ) +
  
  # Escalas estéticas discretas (opción 'turbo' o 'viridis' van muy bien para letras/sectores)
  scale_color_viridis_d(
    option = "turbo", 
    na.value = "grey80",
    name = "Sector (Letra)"
  ) +
  scale_size_continuous(
    range = c(2, 12),
    name = "Personas\n(Totales + 1)"
  ) +
  scale_edge_width_continuous(range = c(0.3, 2)) + 
  
  # Estilo final del gráfico
  theme_graph(background = "#f9f9f9") +
  labs(
    title = "Red de Movilidad Laboral",
    subtitle = "Agrupación visual por sectores ('letra')",
    caption = "Visualización generada con ggraph"
  ) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 18, color = "#333333")
  )


# 1. Definir las etiquetas en el orden exacto (0 al 14)
descripciones_sector <- c(
  "Sin definir", "Agropecuario", "Pesca", "Mineria", "Industria",
  "Electricidad, gas y agua", "Construcción", "Comercio",
  "Hotelería y restaurantes", "Transporte", "Sector financiero",
  "Inmobiliaria", "Enseñanza", "Servicios sociales y de salud",
  "Servicios comunitarios, sociales y personales n.c.p."
)

# 2. Agregar etiquetas al crear df_nodos_actualizado
mapeo_letras <- df_mujer_joven %>% select(r32, letra) %>% distinct()

df_nodos_actualizado <- df_nodos %>%
  left_join(mapeo_letras, by = "r32") %>%
  mutate(
    r34 = as.character(r34),
    # factor() reemplaza los números 0:14 por las descripciones
    letra_desc = factor(letra, levels = 0:14, labels = descripciones_sector),
    personas_totales = ifelse(is.na(personas_final), 0, personas_final),
    tamaño_nodo = personas_final
  )

# 3. Recrear aristas y la red
nodos_validos <- df_nodos_actualizado$r34

df_edges_clean <- df_cambios %>%
  filter(!is.na(r34_origen) & !is.na(r34_destino)) %>%
  mutate(from = as.character(r34_origen), to = as.character(r34_destino)) %>%
  filter(from %in% nodos_validos & to %in% nodos_validos) %>%
  group_by(from, to) %>%
  summarise(peso_flujo = n(), .groups = 'drop')

red_laboral <- tbl_graph(nodes = df_nodos_actualizado, edges = df_edges_clean, directed = TRUE, node_key = "r34")

# 4. Graficar
set.seed(123)
ggraph(red_laboral, layout = 'fr') + 
  geom_edge_link(
    aes(width = peso_flujo, alpha = peso_flujo),
    arrow = arrow(length = unit(2.5, 'mm'), type = "closed"),
    end_cap = circle(4, 'mm'), color = "gray55", show.legend = FALSE
  ) +
  geom_node_point(
    aes(size = tamaño_nodo, color = letra_desc, shape = letra_desc) 
  ) +
  scale_color_viridis_d(
    option = "turbo", 
    na.translate = TRUE, na.value = "grey80", # na.translate asegura que los NA se vean
    name = "Sector de Actividad"
  ) +
  # 3. Forzamos 15 formas geométricas distintas (combinando sólidas, huecas y cruces)
  scale_shape_manual(
    values = c(15, 16, 17, 18, 19, 8, 3, 4, 1, 2, 5, 6, 7, 9, 10),
    name = "Sector de Actividad"
  ) +
  scale_size_continuous(range = c(2, 12), name = "Personas\n(Totales + 1)") +
  scale_edge_width_continuous(range = c(0.3, 2)) + 
  theme_graph(background = "#f9f9f9") +
  scale_edge_width_continuous(range = c(0.3, 2)) + 
  
  # Añade base_family = "sans" aquí
  theme_graph(base_family = "sans", background = "#f9f9f9") +
  
  theme(legend.position = "right")

saveRDS(df_nodos_actualizado,file='nodosfiltr.rds')
saveRDS(df_edges_clean,file='edgesfiltr.rds')
