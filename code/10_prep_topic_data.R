library(tidyverse)
library(tidygraph)
library(networkflow)




extract_nodes <- function(index){
  
  
  data_tmp <- temporal_networks_with_names[[index]] |> 
    activate("nodes") |> 
    as_tibble()
  
  
  return(data_tmp)
  
}

temporal_networks_with_names <- readRDS("Temporal Networks/temporal_networks_with_names.RDS")


index_vec <- seq(1, length(temporal_networks_with_names))

nodes <- map_dfr(index_vec, extract_nodes) |> 
  distinct(oa_id, .keep_all = T) |> 
  mutate(cluster_label_short = cluster_label %>%
           str_split(",") %>%                  # split by comma
           map_chr(~ str_trim(paste(head(.x, 3), collapse = ", ")))) |> 
  drop_na(abstract) |> 
  select(oa_id, field_1, title, abstract)


saveRDS(nodes, "node_field_data.RDS")




