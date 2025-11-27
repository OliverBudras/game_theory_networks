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




temporal_networks_with_names <- readRDS("v3/Temporal Networks/temporal_networks_with_names.RDS")


windows <- names(temporal_networks_with_names)


save_json <- function(index){
  
  window <- windows[index]
  
  data_index <- temporal_networks_with_names[[index]]
  
  nodes <- data_index |> 
    activate(nodes) |> 
    as_tibble() |> 
    drop_na(field_1)
  
  
  from_to_field <- data_index |> 
    activate(edges) |> 
    as_tibble() |> 
    select(Source, Target, weight) |> 
    left_join(nodes |> select(oa_id, field_1), by = c("Source" = "oa_id")) |> 
    rename(field_from = field_1) |> 
    left_join(nodes |> select(oa_id, field_1), by = c("Target" = "oa_id")) |> 
    rename(field_to = field_1) |> 
    # make field-pairs undirected
    mutate(
      field_min = pmin(field_from, field_to),
      field_max = pmax(field_from, field_to)
    ) |> 
    group_by(field_min, field_max) |> 
    summarise(N_edges = n(), .groups = "drop")

  
  saveRDS(from_to_field,   file = paste0("v3/Field_Field_Data/", window, "_field_field_data.RDS"))
  

  
}


index_vec <- seq(1,length(temporal_networks_with_names),1)

map(index_vec, save_json)

