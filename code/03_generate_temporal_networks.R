library(networkflow)
library(ggraph)
library(tidyverse)
library(igraph)
library(biblionetwork)
library(RColorBrewer)
library(jsonlite)
library(bibliometrix)
library(sigmajs)
library(tidygraph)
library(furrr)


#source("00_force_atlas2.R")

### Load data


data <- readRDS("v3/data_final_1980.RDS") 

### Prepare Nodes with Meta and References as Edges


nodes <- data |> 
  select(oa_id, title, abstract, doi, publication_year, cited_by_count,type, journal = source_display_name,
         referenced_works, domain_1, field_1, authorships) |> 
  unnest(authorships, keep_empty = TRUE) |> 
  group_by(across(oa_id:field_1)) |> 
  summarise(authors = paste(display_name, collapse = ";"), .groups = "drop")


references <- data %>% 
  select(oa_id, referenced_works) %>% 
  unnest_longer(referenced_works)

### Build dynamic networks


temporal_networks <- build_dynamic_networks(nodes = nodes,
                                            directed_edges = references,
                                            source_id = "oa_id",
                                            target_id = "referenced_works",
                                            time_variable = "publication_year",
                                            time_window = 5,
                                            cooccurrence_method = "coupling_similarity",
                                            overlapping_window = T,
                                            edges_threshold = 3,
                                            filter_components = T)

### Add Clusters based on Leiden Algorithm

temporal_networks <- networkflow::add_clusters(temporal_networks, clustering_method = "leiden", objective_function = "modularity",
                                               seed = 123)


temporal_networks <- merge_dynamic_clusters(temporal_networks,cluster_id = "cluster_leiden", node_id= "oa_id", similarity_type = "partial",
                                            threshold_similarity = 0.5001)

# temporal_networks <- color_networks(temporal_networks, column_to_color = "dynamic_cluster_leiden")


temporal_networks_with_names <- name_clusters(graphs = temporal_networks,
                                              method = "tf-idf",
                                              name_merged_clusters = TRUE,
                                              cluster_id = "dynamic_cluster_leiden",
                                              text_columns = c("title", "abstract"),
                                              nb_terms_label = 50,
                                              clean_word_method = "lemmatise")





saveRDS(temporal_networks_with_names, "v3/Temporal Networks/temporal_networks_with_names.RDS")




