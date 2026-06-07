

# Codigo ------------------------------------------------------------------
library(tidyverse)
library(igraph)
library(tidygraph)
library(lubridate)

#Funcion de RED -----------------------------------------------------------
build_status_flow_network <- function(df_gender) {

  # Calcular base 1996 y 1997
  nodes_1996 <- df_gender |>
    mutate(year = year(tiempo)) |>
    filter(year == 1996) |>
    group_by(name = r34) |>
    summarize(workers_1996 = n_distinct(id_trabajador), .groups = "drop")

  nodes_1997 <- df_gender |>
    mutate(year = year(tiempo)) |>
    filter(year == 1997) |>
    group_by(name = r34) |>
    summarize(workers_1997 = n_distinct(id_trabajador), .groups = "drop")

  # Nodos y trajectoria
  nodes <- df_gender |>
    distinct(name = r34) |>
    drop_na(name) |>
    left_join(nodes_1996, by = "name") |>
    left_join(nodes_1997, by = "name") |>
    mutate(
      # NA por 0
      workers_1996 = replace_na(workers_1996, 0),
      workers_1997 = replace_na(workers_1997, 0),

      # Ganador o perdedor en el año
      net_change = workers_1997 - workers_1996,
      status = case_when(
        net_change > 0 ~ "Net Gainer",
        net_change < 0 ~ "Net Loser",
        TRUE ~ "Neutral"
      )
    )

  # Calculo neto
  gross_transitions <- df_gender |>
    arrange(id_trabajador, tiempo) |>
    group_by(id_trabajador) |>
    mutate(r34_next = lead(r34)) |>
    ungroup() |>
    filter(!is.na(r34_next) & r34 != r34_next) |>
    count(from = r34, to = r34_next, name = "gross_flow") |>
    drop_na(from, to)

  # Limpiar links que no sean al menos el 1% de los trabajadores iniciales
  edges_filtered <- gross_transitions |>
    left_join(
      gross_transitions |> select(from = to, to = from, flow_reverse = gross_flow),
      by = c("from", "to")
    ) |>
    mutate(
      flow_reverse = replace_na(flow_reverse, 0),
      net_flow = gross_flow - flow_reverse
    ) |>
    filter(net_flow > 0) |>
    left_join(nodes |> select(name, workers_1996), by = c("from" = "name")) |>
    # Filtrar el 1% comparado a la base
    filter(net_flow >= (0.01 * workers_1996)) |>
    select(from, to, weight = net_flow)

  # Grafo
  net <- graph_from_data_frame(d = edges_filtered, vertices = nodes, directed = TRUE) |>
    as_tbl_graph()

  return(net)
}

#REQUIERE CARGAR df_..._real U OTRO
# df_hombre_real <- readRDS("./materiales/df_hombre.rds")
# df_mujer_real <- readRDS("./materiales/df_mujer.rds")
net_men_status <- build_status_flow_network(df_hombre_real)
net_women_status <- build_status_flow_network(df_mujer_real)


# Visualizacion -----------------------------------------------------------

library(ggraph)
library(patchwork)
library(scales)
# Grafico
plot_status_network <- function(net, title_label) {

  a <- grid::arrow(type = "closed", length = unit(0.12, "inches"))

  p <- ggraph(net, layout = "fr") +

    # Links
    geom_edge_fan(aes(edge_width = weight),
                  arrow = a, end_cap = circle(0.12, "inches"),
                  color = "gray60", show.legend = FALSE) +
    scale_edge_width(range = c(0.4, 2.5)) +

    # NODES: Size mapped to 1996 baseline; Color mapped to Gainer/Loser Status
    geom_node_point(aes(size = log(workers_1996), color = status), alpha = 0.9) +

    geom_node_text(aes(label = name), repel = TRUE, size = 3.5,
                   color = "gray20", fontface = "bold") +

    scale_size_continuous(trans = "log10", range = c(3, 15), labels = scales::comma) +


    #Colores
    scale_color_manual(values = c("Net Gainer" = "green",
                                  "Net Loser" = "red",
                                  "Neutral" = "gray50")) +

    theme_void() +
    theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5)) +
    labs(title = title_label,
         size = "Workers (1996 Baseline)",
         color = "Sector Trajectory\n(1996 vs 1997)")

  return(p)
}
# Combinar graficos
plot_men <- plot_status_network(net_men_status, "Male Mobility (>= 1% Flow)")
plot_women <- plot_status_network(net_women_status, "Female Mobility (>= 1% Flow)")

#agregar 'plot_women | ...' para los dos juntos

final_status_grid <- plot_men +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Structural Labor Macro-Mobility by Gender",
    subtitle = "Node Size: 1996 Baseline | Node Color: Net Workforce Trajectory (1996 vs 1997)",
    theme = theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
                  plot.subtitle = element_text(size = 13, hjust = 0.5, color = "gray30"))
  ) &
  theme(legend.position = "bottom")

print(final_status_grid)
