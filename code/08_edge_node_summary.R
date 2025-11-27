library(tidyverse)
library(jsonlite)

files_nodes_fields <- list.files(path = "sigma_network_aggregate/rolling_network/public/windows_field_aggregate", pattern = "nodes", full.names = TRUE)

files_edges_fields <- list.files(path = "sigma_network_aggregate/rolling_network/public/windows_field_aggregate", pattern = "edges", full.names = TRUE)

files_nodes_articles <- list.files(path = "sigma_network/rolling_network/public/windows_field", pattern = "nodes", full.names = TRUE)

files_edges_articles <- list.files(path = "sigma_network/rolling_network/public/windows_field", pattern = "edges", full.names = TRUE)


### Fields

naming_data <- lapply(files_nodes_fields, function(f) {
  fromJSON(f, flatten = TRUE) |> as.data.frame()
}) 


json_to_df <- function(files, naming_list){
  
  
  data_list <- lapply(files, function(f) {
    fromJSON(f, flatten = TRUE) |> as.data.frame()
  }) 
  
  names(data_list) <- sapply(naming_list, function(df) unique(df$window))
  
  data_df <- data_list |> 
    bind_rows(.id = "window_name")
  
}
  
nodes_fields <- json_to_df(files = files_nodes_fields, naming_list = naming_data)
edges_fields <- json_to_df(files = files_edges_fields, naming_list = naming_data)

nodes_articles <- json_to_df(files = files_nodes_articles, naming_list = naming_data)
edges_articles <- json_to_df(files = files_edges_articles, naming_list = naming_data)



