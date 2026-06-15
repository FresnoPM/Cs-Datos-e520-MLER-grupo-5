
# Librerias ---------------------------------------------------------------
library(dplyr)
library(tidyr)
library(lubridate)
library(igraph)
library(ggraph)
library(ggplot2)
library(patchwork)
# Codigo para sacar nodos y edges------------------------------------------------------------------

# Unir todo y sacar años y mes
df_mler <- bind_rows(
  df_hombre_real |> mutate(sexo_cat = "Hombre"),
  df_mujer_real |> mutate(sexo_cat = "Mujer")
) |> 
  mutate(
    anio = year(tiempo),
    mes = month(tiempo)
  ) |> 
  filter(anio >= 1996 & anio <= 2021)

# Sacar todas las remuneraciones de los trabajadores (discontinuidad usada para sacar 999 y movimientos)
ingresos_sistema <- df_mler |> 
  group_by(id_trabajador) |> 
  summarize(
    fecha_ingreso_abs = min(anio * 12 + mes),
    sexo_cat = first(sexo_cat),
    .groups = "drop"
  )

# En caso de multiples trabajos, se usa sólo el de mayor remuneración
panel_dici <- df_mler |> 
  filter(mes == 12) |> 
  group_by(id_trabajador, anio) |> 
  slice_max(order_by = rem_tot_real, n = 1, with_ties = FALSE) |> 
  ungroup()

# Stock de personas por nodo
stock_r34 <- panel_dici |> 
  group_by(r34, anio) |> 
  summarize(
    q_hombres = sum(sexo_cat == "Hombre"),
    q_mujeres = sum(sexo_cat == "Mujer"),
    rem_prom = mean(rem_tot_real, na.rm = TRUE), 
    edad_prom = mean(edad, na.rm = TRUE),
    .groups = "drop"
  ) |> 
  mutate(r34 = as.character(r34))

# Stock 999 (fuera del sistema)
# El calculo es un quilombo por donde se mire, recomiendo a esperar tener el archivo de Lucia
anios_red <- 1996:2021
lista_999 <- list()

for (a in anios_red) {
  activos_dici <- panel_dici |> filter(anio == a) |> pull(id_trabajador)
  
  fuera_dici <- ingresos_sistema |> 
    filter(fecha_ingreso_abs <= (a * 12 + 12)) |> 
    filter(!id_trabajador %in% activos_dici) |> 
    count(sexo_cat)
  
  lista_999[[as.character(a)]] <- tibble(
    r34 = "999", anio = a,
    q_hombres = sum(fuera_dici$n[fuera_dici$sexo_cat == "Hombre"]),
    q_mujeres = sum(fuera_dici$n[fuera_dici$sexo_cat == "Mujer"]),
    rem_prom = 0, edad_prom = 0
  )
}

stock_999 <- bind_rows(lista_999)
# Calcular el porcentaje de mujeres por nodo
nodos_long <- bind_rows(stock_r34, stock_999) |> 
  mutate(
    pct_mujeres = if_else((q_hombres + q_mujeres) > 0, (q_mujeres / (q_hombres + q_mujeres)) * 100, 0)
  )

# Calculo de entradas netas, 1996 vs 1997 por ejemplo
nodos_ganancias <- nodos_long |> 
  arrange(r34, anio) |> 
  group_by(r34) |> 
  mutate(
    # Etiqueta explícita, ej: "1996_1997"
    periodo = paste0(anio, "_", anio + 1),
    ganancia_hombres = lead(q_hombres) - q_hombres,
    ganancia_mujeres = lead(q_mujeres) - q_mujeres
  ) |> 
  ungroup() |> 
  # Eliminamos 2021 de entradas porque no existe 2022 para comparar
  filter(anio < 2021) 

# Union de los atributos de los nodos
nodos_estaticos_wide <- nodos_long |> 
  pivot_wider(
    id_cols = r34,
    names_from = anio,
    values_from = c(q_hombres, q_mujeres, rem_prom, edad_prom, pct_mujeres),
    names_glue = "{.value}_{anio}", 
    values_fill = 0
  )

nodos_ganancias_wide <- nodos_ganancias |> 
  pivot_wider(
    id_cols = r34,
    names_from = periodo,
    values_from = c(ganancia_hombres, ganancia_mujeres),
    names_glue = "{.value}_{periodo}", 
    values_fill = 0
  )
# Agrega el sector 999. Recordar tener ojo con el nodo
descripciones <- desc_r34 |> 
  mutate(r34 = as.character(r34)) |> 
  rename(descripcion = descripcion) |> # Se ajusta a los nombres de R
  bind_rows(tibble(r34 = "999", descripcion = "Fuera del sistema"))
# Union de todo en los nodos finales
nodos_finales <- descripciones |> 
  left_join(nodos_estaticos_wide, by = "r34") |> 
  left_join(nodos_ganancias_wide, by = "r34") |> 
  mutate(across(where(is.numeric), \(x) coalesce(x, 0))) |> 
  relocate(r34, descripcion)


# Creacion de edges
grid_anios <- expand_grid(
  id_trabajador = unique(ingresos_sistema$id_trabajador),
  anio = anios_red
) |> 
  left_join(ingresos_sistema |> select(id_trabajador, fecha_ingreso_abs, sexo_cat), by = "id_trabajador") |> 
  filter((anio * 12 + 12) >= fecha_ingreso_abs)
# Aristas finales, 
aristas_maestras <- grid_anios |> 
  left_join(panel_dici |> select(id_trabajador, anio, r34), by = c("id_trabajador", "anio")) |> 
  mutate(r34 = replace_na(as.character(r34), "999")) |> 
  arrange(id_trabajador, anio) |> 
  group_by(id_trabajador) |> 
  mutate(
    # Generar la etiqueta clara del cambio interanual
    periodo = paste0(anio, "_", anio + 1),
    origen = r34,
    destino = lead(r34)
  ) |> 
  ungroup() |> 
  filter(!is.na(destino) & origen != destino) |> 
  count(periodo, sexo = sexo_cat, origen, destino, name = "peso") # Peso individual dividido por genero

glimpse(nodos_finales)
head(aristas_maestras)


# Heatmap fallido ---------------------------------------------------------

# 1. VERIFY THE DATA (If this returns 0, your network is empty and WILL fail)
print(nrow(aristas_96_97))

# Load patchwork to combine the plots
library(patchwork)
crear_heatmap_genero <- function(genero_elegido, aristas, nodos) {
  
  # 1. Filter edges for the specific gender
  aristas_sub <- aristas |> filter(sexo == genero_elegido)
  
  # 2. Build the network object normally (r34 remains the structural ID)
  red_sub <- graph_from_data_frame(
    d = aristas_sub |> select(origen, destino, everything()), 
    vertices = nodos, 
    directed = TRUE
  )
  
  # 3. Create a named vector (dictionary) to translate r34 IDs into Descriptions
  # Format required by ggplot2: c("ID" = "Description")
  nombres_ejes <- setNames(nodos$descripcion, nodos$r34)
  
  # 4. Generate the matrix heatmap plot
  p <- ggraph(red_sub, layout = 'matrix') + 
    geom_edge_tile(aes(fill = peso)) + 
    
    # REQUIREMENT: Make the nodes have their names
    # Pass the translation dictionary to the 'labels' argument
    scale_x_discrete(drop = FALSE, labels = nombres_ejes) +
    scale_y_discrete(drop = FALSE, labels = nombres_ejes) +
    
    # REQUIREMENT: Blue for lowest, Red for highest
    # Must use 'scale_edge_fill_gradient' for ggraph edge geometries
    scale_edge_fill_gradient(
      low = "blue",  
      high = "red",  
      name = "Flujo de\nTrabajadores"
    ) +
    
    labs(
      title = paste("Dinámica de Transiciones Laborales (1996-1997) -", genero_elegido),
      x = "Sector de Destino (Hacia)",
      y = "Sector de Origen (Desde)",
      caption = "Nota: Incluye el nodo 999 (Fuera del sistema)"
    ) +
    
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 4, color = "#222222"),
      axis.text.y = element_text(size = 4, color = "#222222"),
      plot.title = element_text(size = 18, face = "bold", color = "#2c3e50"),
      legend.position = "right",
      legend.title = element_text(face = "bold")
    )
  
  return(p)
}
# 2. CREATE THE INDIVIDUAL PLOTS (Using the function from the previous step)
# (Make sure to run the 'crear_heatmap_genero' function block first)
heatmap_hombres <- crear_heatmap_genero("Hombre", aristas_96_97, nodos_finales)
heatmap_mujeres <- crear_heatmap_genero("Mujer", aristas_96_97, nodos_finales)

# 3. COMBINE THE PLOTS
# The '/' operator stacks them vertically. 
# You can use '+' if you prefer them side-by-side.
heatmap_combinado <- heatmap_hombres / heatmap_mujeres

# 4. SAFELY EXPORT TO PDF
# ggsave handles the device correctly and prevents corrupted blank files
ggsave(
  filename = "Heatmap_Moderno_Separado_1996_1997.pdf", 
  plot = heatmap_combinado, 
  width = 14, 
  height = 18, 
  device = "pdf")



#  Red de flujos ----------------------------------------------------------

crear_red_genero_separada <- function(anio_base, genero_elegido, aristas_maestras, nodos_finales) {
  
  anio_destino <- anio_base + 1
  periodo_str <- paste0(anio_base, "_", anio_destino)
  
  # 1. Filtrar aristas, remover fantasmas, y ORDENAR COLUMNAS PARA IGRAPH
  aristas_yr <- aristas_maestras |> 
    filter(periodo == periodo_str, sexo == genero_elegido) |> 
    filter(peso > 0) |> 
    filter(origen %in% nodos_finales$r34 & destino %in% nodos_finales$r34) |> 
    # SOLUCIÓN CRÍTICA: Forzar a que origen y destino sean la columna 1 y 2 [1]
    select(origen, destino, everything())
  
  col_stock <- if_else(genero_elegido == "Hombre", 
                       paste0("q_hombres_", anio_destino), 
                       paste0("q_mujeres_", anio_destino))
  
  col_pct_act <- paste0("pct_mujeres_", anio_destino)
  col_pct_prev <- paste0("pct_mujeres_", anio_base)
  
  # 2. Extraer atributos de nodos
  nodos_yr <- nodos_finales |> 
    select(r34, descripcion, 
           stock = all_of(col_stock), 
           pct_actual = all_of(col_pct_act),
           pct_previo = all_of(col_pct_prev)) |> 
    mutate(
      size_node = log(stock + 1) + 1,
      inc_pct_mujeres = pct_actual - pct_previo,
      label_node = if_else(r34 == "999", "FUERA DEL\nSISTEMA (999)", descripcion)
    ) |> 
    filter(stock > 0 | r34 == "999" | r34 %in% aristas_yr$origen | r34 %in% aristas_yr$destino)
  
  # 3. Construir el objeto igraph (Ahora las columnas 1 y 2 son correctas)
  red <- graph_from_data_frame(d = aristas_yr, vertices = nodos_yr, directed = TRUE)
  
  # 4. Interceptar y manipular coordenadas (Aislar el 999)
  lay <- create_layout(red, layout = "fr")
  max_x <- max(lay$x, na.rm = TRUE)
  max_y <- max(lay$y, na.rm = TRUE)
  rango_x <- max_x - min(lay$x, na.rm = TRUE)
  
  idx_999 <- which(lay$name == "999")
  if(length(idx_999) > 0) {
    lay$x[idx_999] <- max_x + (rango_x * 0.5)
    lay$y[idx_999] <- max_y + (rango_x * 0.5)
  }
  
  # 5. Generar el gráfico moderno
  p <- ggraph(lay) + 
    geom_edge_fan(aes(edge_width = log(peso) + 1), 
                  edge_colour = "#bdc3c7", alpha = 0.4,
                  arrow = arrow(length = unit(1.5, 'mm'), type = "closed"), 
                  end_cap = circle(4, 'mm')) +
    scale_edge_width_continuous(range = c(0.2, 2.5), guide = "none") +
    
    geom_node_point(aes(size = size_node, fill = inc_pct_mujeres), 
                    shape = 21, color = "#2c3e50", stroke = 0.8) +
    scale_size_continuous(range = c(3, 15), guide = "none") +
    
    scale_fill_gradient2(low = "#e74c3c", mid = "#f8f9fa", high = "#2980b9", 
                         midpoint = 0, name = "Cambio % Mujeres\n(Interanual)") +
    
    geom_node_text(aes(label = ifelse(name == "999" | size_node > quantile(size_node, 0.90, na.rm = TRUE), label_node, "")), 
                   repel = TRUE, size = 3.5, fontface = "bold", color = "#34495e") +
    
    labs(
      title = paste("Red de Flujos Laborales -", genero_elegido, "-", periodo_str),
      subtitle = paste("El tamaño del nodo refleja el volumen exacto en", anio_destino),
      caption = "Color: Azul indica mayor integración femenina, Rojo indica pérdida."
    ) +
    # SOLUCIÓN CRÍTICA: Forzar una fuente universal ("sans") para evitar el colapso del PDF
    theme_graph(background = "white", base_family = "sans") +
    theme(
      plot.title = element_text(size = 18, face = "bold", color = "#2c3e50"),
      plot.subtitle = element_text(size = 13, color = "#7f8c8d"),
      legend.position = "right"
    )
  
  return(p)
}
p_h_00 <- crear_red_genero_separada(2000, "Hombre", aristas_maestras, nodos_finales)
p_m_00 <- crear_red_genero_separada(2000, "Mujer", aristas_maestras, nodos_finales)
p_h_01 <- crear_red_genero_separada(2001, "Hombre", aristas_maestras, nodos_finales)
p_m_01 <- crear_red_genero_separada(2001, "Mujer", aristas_maestras, nodos_finales)

# PDF con todo unido
pdf("Redes_Separadas_Genero_2000_2002.pdf", width = 14, height = 10)
print(p_h_00)
print(p_m_00)
print(p_h_01)
print(p_m_01)
dev.off()


# Red de flujos NETOS -----------------------------------------------------
aristas_netas <- aristas_maestras |> 
  # Crear identificadores de pares sin dirección para agrupar (A->B y B->A)
  mutate(
    nodo_1 = pmin(origen, destino),
    nodo_2 = pmax(origen, destino)
  ) |> 
  group_by(periodo, sexo, nodo_1, nodo_2) |> 
  # Calcular la diferencia neta
  summarize(
    flujo_1_a_2 = sum(peso[origen == nodo_1], na.rm = TRUE),
    flujo_2_a_1 = sum(peso[origen == nodo_2], na.rm = TRUE),
    .groups = "drop"
  ) |> 
  # Reconstruir la arista dirigida basada SÓLO en la dirección neta ganadora
  mutate(
    origen_neto = if_else(flujo_1_a_2 >= flujo_2_a_1, nodo_1, nodo_2),
    destino_neto = if_else(flujo_1_a_2 >= flujo_2_a_1, nodo_2, nodo_1),
    peso_neto = abs(flujo_1_a_2 - flujo_2_a_1)
  ) |> 
  # Eliminar los empates exactos (Neto 0) y renombrar columnas para igraph
  filter(peso_neto > 0) |> 
  select(origen = origen_neto, destino = destino_neto, periodo, sexo, peso = peso_neto)

crear_red_neta_separada <- function(anio_base, genero_elegido, aristas_netas, nodos_finales) {
  
  anio_destino <- anio_base + 1
  periodo_str <- paste0(anio_base, "_", anio_destino)
  
  # Filtrar para el año/género exacto usando la nueva base de Flujos Netos
  aristas_yr <- aristas_netas |> 
    filter(periodo == periodo_str, sexo == genero_elegido) |> 
    filter(peso > 0) |> 
    filter(origen %in% nodos_finales$r34 & destino %in% nodos_finales$r34) |> 
    select(origen, destino, everything())
  
  col_stock <- if_else(genero_elegido == "Hombre", 
                       paste0("q_hombres_", anio_destino), 
                       paste0("q_mujeres_", anio_destino))
  
  col_pct_act <- paste0("pct_mujeres_", anio_destino)
  col_pct_prev <- paste0("pct_mujeres_", anio_base)
  
  nodos_yr <- nodos_finales |> 
    select(r34, descripcion, 
           stock = all_of(col_stock), 
           pct_actual = all_of(col_pct_act),
           pct_previo = all_of(col_pct_prev)) |> 
    mutate(
      size_node = log(stock + 1) + 1,
      inc_pct_mujeres = pct_actual - pct_previo,
      label_node = if_else(r34 == "999", "FUERA DEL\nSISTEMA (999)", descripcion)
    ) |> 
    filter(stock > 0 | r34 == "999" | r34 %in% aristas_yr$origen | r34 %in% aristas_yr$destino)
  
  red <- graph_from_data_frame(d = aristas_yr, vertices = nodos_yr, directed = TRUE)
  
  lay <- create_layout(red, layout = "fr")
  max_x <- max(lay$x, na.rm = TRUE)
  max_y <- max(lay$y, na.rm = TRUE)
  rango_x <- max_x - min(lay$x, na.rm = TRUE)
  
  idx_999 <- which(lay$name == "999")
  if(length(idx_999) > 0) {
    lay$x[idx_999] <- max_x + (rango_x * 0.5)
    lay$y[idx_999] <- max_y + (rango_x * 0.5)
  }
  
  p <- ggraph(lay) + 
    # MODIFICACIÓN 1 & 2: geom_edge_link para flujos netos únicos, 
    # y asignación directa de alpha y color constante.
    geom_edge_link(aes(edge_width = log(peso) + 1), 
                   edge_colour = "grey50", edge_alpha = 0.5,
                   arrow = arrow(length = unit(1.5, 'mm'), type = "closed"), 
                   end_cap = circle(4, 'mm')) +
    scale_edge_width_continuous(range = c(0.2, 2.5), guide = "none") +
    
    geom_node_point(aes(size = size_node, fill = inc_pct_mujeres), 
                    shape = 21, color = "#2c3e50", stroke = 0.8) +
    scale_size_continuous(range = c(3, 15), guide = "none") +
    
    scale_fill_gradient2(low = "#e74c3c", mid = "#f8f9fa", high = "#2980b9", 
                         midpoint = 0, name = "Cambio % Mujeres\n(Interanual)") +
    
    geom_node_text(aes(label = ifelse(name == "999" | size_node > quantile(size_node, 0.90, na.rm = TRUE), label_node, "")), 
                   repel = TRUE, size = 3.5, fontface = "bold", color = "#34495e") +
    
    labs(
      title = paste("Red de Flujos Laborales Netos -", genero_elegido, "-", periodo_str),
      subtitle = paste("El tamaño del nodo refleja el volumen exacto en", anio_destino),
      caption = "Color: Azul indica mayor integración femenina. Aristas: Flujo Neto Constante (Gris)."
    ) +
    theme_graph(background = "white", base_family = "sans") +
    theme(
      plot.title = element_text(size = 18, face = "bold", color = "#2c3e50"),
      plot.subtitle = element_text(size = 13, color = "#7f8c8d"),
      legend.position = "right"
    )
  
  return(p)
}

# Graficos
p_h_00 <- crear_red_neta_separada(2000, "Hombre", aristas_netas, nodos_finales)
p_m_00 <- crear_red_neta_separada(2000, "Mujer", aristas_netas, nodos_finales)
p_h_01 <- crear_red_neta_separada(2001, "Hombre", aristas_netas, nodos_finales)
p_m_01 <- crear_red_neta_separada(2001, "Mujer", aristas_netas, nodos_finales)

pdf("Redes_Netas_Separadas_Genero_2000_2002.pdf", width = 14, height = 10)
print(p_h_00)
print(p_m_00)
print(p_h_01)
print(p_m_01)
dev.off()


# Otro intento de heatmap -------------------------------------------------

# Isolate the gross flows specifically for the 2000-2001 transition
aristas_00_01 <- aristas_maestras |> 
  filter(periodo == "2000_2001") |> 
  filter(peso > 0) # Log(0) is undefined (-Inf), so we ensure strict positivity

crear_heatmap_log_genero <- function(genero_elegido, aristas, nodos) {
  
  # Filter edges for the specific gender
  aristas_sub <- aristas |> filter(sexo == genero_elegido)
  
  # Build the network object 
  red_sub <- graph_from_data_frame(
    d = aristas_sub |> select(origen, destino, everything()), 
    vertices = nodos, 
    directed = TRUE
  )
  
  # Create translation dictionary for r34 -> Descriptions
  nombres_ejes <- setNames(nodos$descripcion, nodos$r34)
  
  # Generate the matrix heatmap plot
  p <- ggraph(red_sub, layout = 'matrix') + 
    # REQUIREMENT: Potency of the logarithmic flux
    geom_edge_tile(aes(fill = log(peso))) + 
    
    # REQUIREMENT: Label the r34 sectors
    scale_x_discrete(drop = FALSE, labels = nombres_ejes) +
    scale_y_discrete(drop = FALSE, labels = nombres_ejes) +
    
    # REQUIREMENT: Dark blue to red gradient
    scale_edge_fill_gradient(
      low = "darkblue",  
      high = "red",  
      name = "Log(Flujo de\nTrabajadores)"
    ) +
    
    labs(
      title = paste("Matriz de Transiciones Laborales (2000-2001) -", genero_elegido),
      subtitle = "El color representa el logaritmo del flujo de trabajadores",
      x = "Sector de Destino (Hacia)",
      y = "Sector de Origen (Desde)",
      caption = "Nota: Incluye el nodo 999 (Fuera del sistema)"
    ) +
    
    # Modern theme using universal 'sans' font to prevent graphic device crash
    theme_minimal(base_family = "sans") +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 4, color = "#222222"),
      axis.text.y = element_text(size = 4, color = "#222222"),
      plot.title = element_text(size = 18, face = "bold", color = "#2c3e50"),
      plot.subtitle = element_text(size = 13, color = "#7f8c8d"),
      legend.position = "right",
      legend.title = element_text(face = "bold")
    )
  
  return(p)
}

# Create the individual plot objects
heatmap_hombres_00_01 <- crear_heatmap_log_genero("Hombre", aristas_00_01, nodos_finales)
heatmap_mujeres_00_01 <- crear_heatmap_log_genero("Mujer", aristas_00_01, nodos_finales)

# Combine using patchwork
heatmap_combinado_00_01 <- heatmap_hombres_00_01 / heatmap_mujeres_00_01

# Safely export to PDF
ggsave(
  filename = "Heatmap_LogFlux_Separado_2000_2001.pdf", 
  plot = heatmap_combinado_00_01, 
  width = 14, 
  height = 18, 
  device = "pdf"
)



