library(tidyverse)
library(igraph)
library(tidygraph)
library(ggraph)
library(lubridate)
library(patchwork)
library(scales)

# Define the requested periods
periods <- list(
  c(1996, 1999), c(1999, 2003), c(2003, 2011), 
  c(2011, 2017), c(2017, 2019), c(2019, 2021)
)

# -------------------------------------------------------------------
# 1. NETWORK GENERATION FUNCTION (Fixed for Directed Louvain)
# -------------------------------------------------------------------
build_full_r34_network <- function(start_year, end_year, data_gender) {
  
  # A. NODES: Baseline size with a safe floor of 1 to prevent log(0) errors
  nodes_start <- data_gender |> 
    mutate(year = year(tiempo)) |> 
    filter(year == start_year) |> 
    group_by(name = as.character(r34)) |> 
    summarize(workers_start = n_distinct(id_trabajador), .groups = "drop")
  
  nodes <- data_gender |> 
    distinct(name = as.character(r34)) |> 
    drop_na(name) |> 
    left_join(nodes_start, by = "name") |> 
    mutate(workers_start = replace_na(workers_start, 1)) 
  
  # B. EDGES: All Net Flows (No Thresholds)
  period_data <- data_gender |> 
    mutate(year = year(tiempo)) |> 
    filter(year >= start_year & year <= end_year)
  
  gross_transitions <- period_data |> 
    arrange(id_trabajador, tiempo) |> 
    group_by(id_trabajador) |> 
    mutate(r34_next = lead(as.character(r34))) |> 
    ungroup() |> 
    filter(!is.na(r34_next) & as.character(r34) != r34_next) |> 
    count(from = as.character(r34), to = r34_next, name = "gross_flow") |> 
    drop_na(from, to)

  edges_net <- gross_transitions |> 
    left_join(
      gross_transitions |> select(from = to, to = from, reverse_flow = gross_flow),
      by = c("from", "to")
    ) |> 
    mutate(
      reverse_flow = replace_na(reverse_flow, 0),
      net_flow = gross_flow - reverse_flow
    ) |> 
    filter(net_flow > 0) |> 
    select(from, to, weight = net_flow)

  # C. BUILD DIRECTED GRAPH
  net_directed <- graph_from_data_frame(d = edges_net, vertices = nodes, directed = TRUE)
  
  # D. TEMPORARY UNDIRECTED GRAPH FOR LOUVAIN
  # We collapse bidirectional paths and sum their weights to satisfy the Louvain algorithm
  net_undirected <- as.undirected(net_directed, mode = "collapse", 
                                  edge.attr.comb = list(weight = "sum", "ignore"))
  louvain_partition <- cluster_louvain(net_undirected, weights = E(net_undirected)$weight)
  
  # E. BUILD FINAL TIDYGRAPH WITH ALL METRICS
  net_final <- as_tbl_graph(net_directed) |> 
    activate(nodes) |> 
    mutate(
      # Extract the safe Louvain communities
      community = as.factor(membership(louvain_partition)),
      
      # Technical Centralities (Normalized to handle disconnected components)
      degree_cent = centrality_degree(mode = "all"),
      betweenness_cent = centrality_betweenness(weights = weight, directed = TRUE),
      closeness_cent = centrality_closeness(mode = "all", normalized = TRUE)
    )
  
  return(net_final)
}

# -------------------------------------------------------------------
# 2. GENERATE ALL NETWORKS & THE GLOBAL CENTRALITY TABLE
# -------------------------------------------------------------------
centrality_list <- list()
networks_men <- list()
networks_women <- list()

for (p in periods) {
  period_label <- paste(p[1], p[2], sep = "_")
  
  # Build networks
  net_m <- build_full_r34_network(p[1], p[2], df_hombre_real) 
  net_w <- build_full_r34_network(p[1], p[2], df_mujer_real)  
  
  networks_men[[period_label]] <- net_m
  networks_women[[period_label]] <- net_w
  
  # Extract node properties for the unified table
  nodes_m <- net_m |> as_tibble() |> mutate(period = period_label, gender = "Men")
  nodes_w <- net_w |> as_tibble() |> mutate(period = period_label, gender = "Women")
  
  centrality_list[[paste0(period_label, "_M")]] <- nodes_m
  centrality_list[[paste0(period_label, "_W")]] <- nodes_w
  
  print(paste("Successfully built networks & calculated centralities for:", period_label))
}

# Combine into a single massive dataframe for direct comparison
global_centrality_table <- bind_rows(centrality_list) |> 
  select(period, gender, r34_name = name, community, degree_cent, betweenness_cent, closeness_cent, workers_start) |> 
  arrange(period, r34_name, gender)

# View the comparison table
print(head(global_centrality_table, 15))

# -------------------------------------------------------------------
# 3. MODERN AESTHETIC VISUALIZATION FUNCTION
# -------------------------------------------------------------------
plot_modern_louvain <- function(net, title_label) {
  
  a <- grid::arrow(type = "closed", length = unit(0.08, "inches"))
  
  p <- ggraph(net, layout = "fr") + 
    
    # EDGES: Log10 scale for the width and alpha to handle dense, unfiltered flows
    geom_edge_fan(aes(edge_width = weight, edge_alpha = 1), 
                  arrow = a, end_cap = circle(0.12, "inches"), 
                  color = "gray60", show.legend = FALSE) + 
    scale_edge_width_continuous(trans = "log10", range = c(0.15, 2.5)) +
    scale_edge_alpha_continuous(trans = "log10", range = c(0.15, 0.8)) +
    
    # NODES: Size by log10 workers, Color uniquely maps to the Louvain Community
    geom_node_point(aes(size = workers_start, color = community), alpha = 0.95) + 
    geom_node_text(aes(label = name), repel = TRUE, size = 3, color = "black", fontface = "bold") + 
    
    scale_size_continuous(trans = "log10", range = c(2, 12), labels = scales::comma) + 
    scale_color_viridis_d(option = "turbo") + # Distinct qualitative colors for communities
    
    theme_void() + 
    theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
          legend.position = "right") + 
    labs(title = title_label, size = "Log10(Workers)", color = "Louvain\nEcosystem")
  
  return(p)
}

# Plots

plot_men_9699 <- plot_modern_louvain(networks_men[["1996_1999"]], "Men (1996-1999)")
plot_women_9699 <- plot_modern_louvain(networks_women[["1996_1999"]], "Women (1996-1999)")

plot_men_9903 <- plot_modern_louvain(networks_men[["1999_2003"]], "Men (1999-2003)")
plot_women_9903 <- plot_modern_louvain(networks_women[["1999_2003"]], "Women (1999-2003)")

plot_men_0311 <- plot_modern_louvain(networks_men[["2003_2011"]], "Men (2003-2011)")
plot_women_0311 <- plot_modern_louvain(networks_women[["2003_2011"]], "Women (2003-2011)")

plot_men_1117 <- plot_modern_louvain(networks_men[["2011_2017"]], "Men (2011-2017)")
plot_women_1117 <- plot_modern_louvain(networks_women[["2011_2017"]], "Women (2011-2017)")

plot_men_1719 <- plot_modern_louvain(networks_men[["2017_2019"]], "Men (2017-2019)")
plot_women_1719 <- plot_modern_louvain(networks_women[["2017_2019"]], "Women (2017-2019)")

plot_men_1921 <- plot_modern_louvain(networks_men[["2019_2021"]], "Men (2019-2021)")
plot_women_1921 <- plot_modern_louvain(networks_women[["2019_2021"]], "Women (2019-2021)")

#Vis

final_gridw1 <- plot_women_9699 +
  plot_annotation(
    title = "Macro-Mobility & Louvain Labor Ecosystems (r34)",
    subtitle = "Nodes colored by algorithmically detected labor communities."
  )
print(final_gridw1)

final_gridw2 <- plot_women_9903 +
  plot_annotation(
    title = "Macro-Mobility & Louvain Labor Ecosystems (r34)",
    subtitle = "Nodes colored by algorithmically detected labor communities."
  )
print(final_gridw2)

final_gridw3 <- plot_women_0311 +
  plot_annotation(
    title = "Macro-Mobility & Louvain Labor Ecosystems (r34)",
    subtitle = "Nodes colored by algorithmically detected labor communities."
  )
print(final_gridw3)

final_gridw4 <- plot_women_1117 +
  plot_annotation(
    title = "Macro-Mobility & Louvain Labor Ecosystems (r34)",
    subtitle = "Nodes colored by algorithmically detected labor communities."
  )
print(final_gridw4)

final_gridw5 <- plot_women_1719 +
  plot_annotation(
    title = "Macro-Mobility & Louvain Labor Ecosystems (r34)",
    subtitle = "Nodes colored by algorithmically detected labor communities. All links included."
  )
print(final_gridw5)

final_gridw6 <- plot_women_1921 +
  plot_annotation(
    title = "Macro-Mobility & Louvain Labor Ecosystems (r34)",
    subtitle = "Nodes colored by algorithmically detected labor communities."
  )
print(final_gridw6)

final_gridm1 <- plot_men_9699 +
  plot_annotation(
    title = "Macro-Mobility & Louvain Labor Ecosystems (r34)",
    subtitle = "Nodes colored by algorithmically detected labor communities."
  )
print(final_gridm1)

final_gridm2 <- plot_men_9903 +
  plot_annotation(
    title = "Macro-Mobility & Louvain Labor Ecosystems (r34)",
    subtitle = "Nodes colored by algorithmically detected labor communities."
  )
print(final_gridm2)

final_gridm3 <- plot_men_0311 +
  plot_annotation(
    title = "Macro-Mobility & Louvain Labor Ecosystems (r34)",
    subtitle = "Nodes colored by algorithmically detected labor communities."
  )
print(final_gridm3)

final_gridm4 <- plot_men_1117 +
  plot_annotation(
    title = "Macro-Mobility & Louvain Labor Ecosystems (r34)",
    subtitle = "Nodes colored by algorithmically detected labor communities."
  )
print(final_gridm4)

final_gridm5 <- plot_men_1719 +
  plot_annotation(
    title = "Macro-Mobility & Louvain Labor Ecosystems (r34)",
    subtitle = "Nodes colored by algorithmically detected labor communities. All links included."
  )
print(final_gridm5)

final_gridm6 <- plot_men_1921 +
  plot_annotation(
    title = "Macro-Mobility & Louvain Labor Ecosystems (r34)",
    subtitle = "Nodes colored by algorithmically detected labor communities."
  )
print(final_gridm6)



#Animacion

install.packages('animation')
library(animation)

# NOTE: If R cannot automatically find ImageMagick on your system, 
# you must point it to the installation path using the code below. 
# Adjust the path to match your specific computer!
# ani.options(convert = "C:/Program Files/ImageMagick-7.1.1-Q16-HDRI/convert.exe")

# -------------------------------------------------------------------
# 1. ANIMATE THE WOMEN'S NETWORK
# -------------------------------------------------------------------
saveGIF({
  # Because these are ggplot/patchwork objects, you must explicitly print() them
  print(final_gridw1)
  print(final_gridw2)
  print(final_gridw3)
  print(final_gridw4)
  print(final_gridw5)
  print(final_gridw6)
}, 
# Set the time (in seconds) that each frame appears
interval = 1.0, 
movie.name = "female_mobility_evolution.gif")

print("Successfully generated Female Network Animation.")

# -------------------------------------------------------------------
# 2. ANIMATE THE MEN'S NETWORK
# -------------------------------------------------------------------
saveGIF({
  # Assuming your male plots are named similarly with an 'm'
  print(final_gridm1)
  print(final_gridm2)
  print(final_gridm3)
  print(final_gridm4)
  print(final_gridm5)
  print(final_gridm6)
}, 
interval = 1.0, 
movie.name = "male_mobility_evolution.gif")

print("Successfully generated Male Network Animation.")






