library(networkflow)
library(ggraph)
library(tidyverse)
library(igraph)
library(biblionetwork)
library(RColorBrewer)
library(jsonlite)
library(bibliometrix)
library(biblionetwork)
library(sigmajs)
library(tidygraph)
library(furrr)
library(viridisLite)
library(colorspace)

### Helper Functions


source("v3/00_aux.R")
source("v3/00_force_atlas2.R")

temporal_networks_with_names <- readRDS("v3/Temporal Networks/temporal_networks_with_names.RDS")


alluv <- networks_to_alluv(temporal_networks_with_names, intertemporal_cluster_column = "dynamic_cluster_leiden",
                           node_id = "oa_id") |> 
  distinct(dynamic_cluster_leiden, window, .keep_all = T)

alluv_filt <- alluv |> 
  filter(length_cluster > 1 & share_cluster_max > 4)  %>%
  mutate(cluster_label_short = cluster_label %>%
           str_split(",") %>%                  # split by comma
           map_chr(~ str_trim(paste(head(.x, 2), collapse = ", ")))) 


temporal_networks_with_names_filt <- lapply(temporal_networks_with_names, function(g) {
    g %>%
      activate(nodes) %>%
      filter(dynamic_cluster_leiden %in% unique(alluv_filt$dynamic_cluster_leiden)) %>%
      convert(to_subgraph)
  })


windows <- names(temporal_networks_with_names_filt)

all_clusters <- temporal_networks_with_names_filt %>%
  map(~ {
    nodes_tbl <- .x %>% activate(nodes) %>% as_tibble()
    nodes_tbl$dynamic_cluster_leiden
  }) %>%
  unlist() %>%
  unique() %>%
  sort()

palette_colors <- palette_colors <- qualitative_hcl(
  n = length(all_clusters),
  palette = "Dark 3"  # or "Set 3", "Set 2", or create your own (see below)
)

cluster_color_map <- tibble(
  dynamic_cluster_leiden = all_clusters,
  color = palette_colors
)

save_json <- function(index){
  
  window <- windows[index]
  
  data_index <- temporal_networks_with_names_filt[[index]] 
  
  
  n_nodes <- data_index |> 
    activate(nodes) |> 
    as_tibble() |> 
    group_by(dynamic_cluster_leiden) |> 
    summarise(N_nodes = n(), cluster_label = unique(cluster_label)) 
  
  cluster_labels <-  n_nodes |> 
    select(dynamic_cluster_leiden, cluster_label)
  
  from_to_field <- data_index |> 
    activate(edges) |> 
    as_tibble() |> 
    filter(dynamic_cluster_leiden_from %in% n_nodes$dynamic_cluster_leiden & dynamic_cluster_leiden_to %in% n_nodes$dynamic_cluster_leiden) |> 
    mutate(
      dynamic_cluster_leiden_min = pmin(dynamic_cluster_leiden_from, dynamic_cluster_leiden_to),
      dynamic_cluster_leiden_max = pmax(dynamic_cluster_leiden_from, dynamic_cluster_leiden_to)
    ) |> 
    group_by(dynamic_cluster_leiden_min, dynamic_cluster_leiden_max) |> 
    summarise(N_edges = n(), .groups = "drop")
  
  # --- Within-field vs between-field edges ---
  within_edges_df <- from_to_field |> 
    filter(dynamic_cluster_leiden_min == dynamic_cluster_leiden_max) |> 
    transmute(dynamic_cluster_leiden = dynamic_cluster_leiden_min, within_edges = N_edges) 
  
  between_edges_df <- from_to_field |> 
    filter(dynamic_cluster_leiden_min != dynamic_cluster_leiden_max) |> 
    # duplicate edges so both fields get counted
    tidyr::pivot_longer(cols = c(dynamic_cluster_leiden_min, dynamic_cluster_leiden_max), names_to = NULL, 
                        values_to = "dynamic_cluster_leiden") |> 
    group_by(dynamic_cluster_leiden) |> 
    summarise(between_edges = sum(N_edges), .groups = "drop") 
  
  
  # --- Final summary ---
  between_within_df <- n_nodes |> 
    full_join(within_edges_df, by = "dynamic_cluster_leiden") |> 
    full_join(between_edges_df, by = "dynamic_cluster_leiden") |> 
    mutate(
      across(c(within_edges, between_edges), ~replace_na(.x, 0)),
      total_edges = within_edges + between_edges,
      external_ratio = between_edges / total_edges,
      internal_ratio = within_edges / total_edges
    ) 
  
  
  
  
  
  g <- as.igraph(data_index)
  
  cluster_membership <- as.numeric(factor(V(g)$dynamic_cluster_leiden))
  g_field <- contract(g, mapping =cluster_membership, 
                      vertex.attr.comb = list(dynamic_cluster_leiden  = unique,
                                              cluster_label = unique,
                                              field_1 = unique,
                                              cited_by_count = sum,
                                              window = unique))

  # Make sure both sides are the same type (character)
  V(g_field)$name <- as.character(V(g_field)$dynamic_cluster_leiden)
  allowed <- as.character(n_nodes$dynamic_cluster_leiden)
  
  # Filter vertices safely
  g_field <- delete_vertices(g_field, !(V(g_field)$dynamic_cluster_leiden %in% allowed))
  
  attr_to_keep <- c("weight")
  
  
  for (attr in edge_attr_names(g_field)){
    
    if(!(attr %in% attr_to_keep)){
     
    g_field <- delete_edge_attr(g_field, attr) 
      
    }
  }
  
  
  g_field <- simplify(g_field, remove.loops = T, edge.attr.comb = "sum")

  
  V(g_field)$size <- as.numeric(V(g_field)$cited_by_count)
  V(g_field)$color <- cluster_color_map$color[match(V(g_field)$dynamic_cluster_leiden, cluster_color_map$dynamic_cluster_leiden)]
  V(g_field)$label <- V(g_field)$cluster_label
  
  # g_field <- delete_vertex_attr(g_field, name = "title")
  # g_field <- delete_vertex_attr(g_field, name = "publication_year")
  # g_field <- delete_vertex_attr(g_field, name = "domain_1")
  # g_field <- delete_vertex_attr(g_field, name = "authors")
  
  V(g_field)$N_nodes <- n_nodes$N_nodes
  
  E(g_field)$dist <- 1 / E(g_field)$weight
  
  V(g_field)$degree <- degree(g_field)
  V(g_field)$betweeness <- betweenness(g_field, weights = E(g_field)$dist)
  V(g_field)$closeness <- closeness(g_field, weights = E(g_field)$dist)
  
  
  V(g_field)$between_edges <- between_within_df$between_edges
  V(g_field)$within_edges <- between_within_df$within_edges
  V(g_field)$total_edges <- between_within_df$total_edges
  V(g_field)$external_ratio <- between_within_df$external_ratio
  V(g_field)$internal_ratio <- between_within_df$internal_ratio
  
  ### NEW: Compute ForceAtlas2-like coordinates (FR layout in igraph)
  coords <- layout.forceatlas2(
    g_field,
    iterations = 500,
    k = 500,
    gravity = 1,
    ks = 0.05,
    delta = 1,
    plotstep = 0
  )  
  
  
  coords_df <- tibble(
    x = normalize(coords[,1]),
    y = normalize(coords[,2])
  )  
  # Normalize to [-1, 1]
  
  
  
  # --- Prepare JSON output ---
  nodes_json <- tibble(
    id = V(g_field)$name,
    label_full = V(g_field)$label,
    size_citations = normalize_size(V(g_field)$cited_by_count),
    size_nodes = normalize_size(V(g_field)$N_nodes),
    color = V(g_field)$color,
    citations = V(g_field)$cited_by_count, 
    field = V(g_field)$field_1,
    numbernodes = V(g_field)$N_nodes,
    degree = V(g_field)$degree,
    betweeness = V(g_field)$betweeness,
    closeness = V(g_field)$closeness,
    between_edges = V(g_field)$between_edges,
    withinedges = V(g_field)$within_edges,
    totaledges = V(g_field)$total_edges,
    externalratio = V(g_field)$external_ratio,
    internalratio = V(g_field)$internal_ratio,
    window = V(g_field)$window,
    stringsAsFactors = FALSE
  ) %>% bind_cols(coords_df)  |> 
    mutate(window = window) |> 
    mutate(label  =  sapply(strsplit(label_full, ","), function(x) paste(head(x, 5), collapse = ",")))
  
  edges_json <- tibble(
    id = paste0("e", seq_len(ecount(g_field))),
    source = as.character(ends(g_field, E(g_field))[,1]),
    target = as.character(ends(g_field, E(g_field))[,2]),
    weight = E(g_field)$weight,
    stringsAsFactors = FALSE
  ) 
  
  # --- Write JSON to disk ---
  write(
    toJSON(nodes_json, dataframe = "rows", auto_unbox = TRUE, pretty = TRUE),
    file = sprintf("v3/sigma_network_cluster_supernodes/rolling_network/public/windows_field_aggregate/nodes_window_%03d.json", index)
  )
  
  write(
    toJSON(edges_json, dataframe = "rows", auto_unbox = TRUE, pretty = TRUE),
    file = sprintf("v3/sigma_network_cluster_supernodes/rolling_network/public/windows_field_aggregate/edges_window_%03d.json", index)
  )
  
  saveRDS(nodes_json,   file = paste0("v3/Evolution Fields Cluster/", window, "_metrics.RDS"))
  
  message("Saved window ", index, " (", window, ") with ", vcount(g_field), " nodes and ", ecount(g_field), " edges.")


}


index_vec <- seq(1,length(temporal_networks_with_names_filt),1)

map(index_vec, save_json)
