library(tidyverse)
library(networkflow)
library(ggalluvial)
library(quanteda)
library(quanteda.textstats)

temporal_networks_with_names <- readRDS("v2/Temporal Networks/temporal_networks_with_names.RDS")

alluv <- networks_to_alluv(temporal_networks_with_names, intertemporal_cluster_column = "dynamic_cluster_leiden",
                           node_id = "oa_id") |> 
  distinct(dynamic_cluster_leiden, window, .keep_all = T)

alluv_filt <- alluv |> 
  filter(length_cluster > 1 & share_cluster_max > 4)  %>%
  mutate(cluster_label_short = cluster_label %>%
           str_split(",") %>%                  # split by comma
           map_chr(~ str_trim(paste(head(.x, 2), collapse = ", ")))) |> 
  filter(lengths(gregexpr("\\W+", cluster_label)) > 5)


labels <- alluv_filt %>%
  group_by(dynamic_cluster_leiden) |> 
  slice_max(share_cluster_window)  |>  # first window
  ungroup()


fill_labels <- setNames(labels$cluster_label_short, labels$dynamic_cluster_leiden)


ggplot(alluv_filt,
       aes(x = window,
           stratum = dynamic_cluster_leiden,
           alluvium = dynamic_cluster_leiden,
           y = share_cluster_window,
           fill = dynamic_cluster_leiden)) +
  geom_flow(stat = "alluvium", lode.guidance = "forward", alpha = 0.8) +
  geom_stratum(width = 1/12, color = "black") +
  scale_fill_viridis_d(labels = fill_labels, option = "plasma") +
  theme_minimal() +
  theme(legend.title = element_blank())



# Folder with the RDS files
rds_folder <- "v2/Evolution Fields Cluster/"

# List all RDS files
rds_files <- list.files(rds_folder, pattern = "_metrics\\.RDS$", full.names = TRUE)

# Read and combine
all_metrics <- rds_files %>%
  map_dfr(function(f) {
    df <- readRDS(f)
    
    # Extract window from filename
    # Assumes filenames like "1970-1974_metrics.RDS"
    window <- str_extract(basename(f), "\\d{4}-\\d{4}")
    
    df %>% mutate(window = window)
  }) |> 
  drop_na() |> 
  mutate(window = factor(window, levels = unique(window))) |> 
  group_by(window) |> 
  mutate(totalnumbernodes = sum(numbernodes),
         totalnumberedges = sum(totaledges)) |> 
  ungroup() |> 
  mutate(relative_size_nodes = numbernodes/totalnumbernodes,
         relative_size_edges = totaledges/totalnumberedges,
         relative_size_within_edges = withinedges/totalnumberedges,
         relative_size_between_edges = between_edges/totalnumberedges) |> 
  rename(dynamic_cluster_leiden = id) |> 
  left_join(alluv, by = c("dynamic_cluster_leiden", "window")) |> 
  filter(length_cluster > 3 & share_cluster_max > 1) %>%
  mutate(cluster_label_short = cluster_label %>%
           str_split(",") %>%                  # split by comma
           map_chr(~ str_trim(paste(head(.x, 2), collapse = ", ")))) 



df_long <- all_metrics %>%
  tidyr::pivot_longer(cols = c(internalratio, externalratio,degree,
                               betweeness, closeness, between_edges, withinedges,
                               numbernodes, totaledges, relative_size_nodes,
                               relative_size_edges,relative_size_within_edges,
                               relative_size_between_edges, citations, share_cluster_window),
                      names_to = "type", values_to = "value")

plot_type <- function(data){
  
  data <- df_long
  
  data_plot <- data |> 
    group_by(window, type) |> 
    summarise(value = mean(value))
  
  ggplot(data_plot, aes(x = window, y = value)) +
    geom_jitter(width = 0.2, height = 0, alpha = 0.7) +
    theme_minimal() +
    facet_wrap(~type, scales="free") +
    theme(axis.text = element_blank()) +
    labs(y = "Metric", x = "Window")
  
  
}

plot_type(df_long)


plot_type_field <- function(data,metric){
  
  data_plot <- data |> 
    filter(type %in% metric)
  
  ggplot(data_plot, aes(x = window, y = value,color=type)) +
    geom_jitter(width = 0.2, height = 0, alpha = 0.7) +
    theme_minimal() +
    facet_wrap(~stringr::str_wrap(dynamic_cluster_leiden, width = 30), ncol = 3) +
    theme(axis.text = element_blank(),
          legend.position = "none",
          strip.text.x = element_text(size = 9, margin = margin(t = 5, b = 5)),
          strip.text.y = element_text(size = 9, margin = margin(l = 5, r = 5)),
          plot.margin = grid::unit(c(1,1,1,1), "cm")) +
    labs(y = toupper(metric), x = "Window")
  
  
  
}


plot_type_field_tile <- function(data,metric){
  
  data_plot <- data |> 
    filter(type %in% metric)
  
  
  ggplot(data_plot, aes(x = window, y = cluster_label_short, fill = value)) +
    geom_tile() +
    scale_fill_viridis_c(option = "magma") +
    theme_minimal() +
    labs(x = "Window", y = "Cluster", fill = "Value")
  
  
  
}





plot_type_field_tile(df_long, metric = c("relative_size_nodes")) # Nodes(Field)/Nodes(TotalWindow)
plot_type_field_tile(df_long, metric = c("share_cluster_window")) # Nodes(Field)/Nodes(TotalWindow)

plot_type_field_tile(df_long, metric = c("citations")) # Nodes(Field)/Nodes(TotalWindow)

plot_type_field_tile(df_long, metric = c("relative_size_edges")) # Edges(Field)/Edges(TotalWindow)
plot_type_field_tile(df_long, metric = c("relative_size_within_edges", "relative_size_between_edges"))
# Edges(WithinField)/Edges(TotalWindow), Edges(BetweenField)/Edges(TotalWindow)
plot_type_field_tile(df_long, metric = c("internalratio"))
plot_type_field_tile(df_long, metric = c("externalratio"))
# Edges(WithinField)/Edges(WithinField)+Edges(BetweenField), Edges(BetweenField)/Edges(WithinField)+Edges(BetweenField)
plot_type_field_tile(df_long, metric = c("degree"))
plot_type_field_tile(df_long, metric = c("closeness"))
plot_type_field_tile(df_long, metric = c("betweeness"))



### Cluster Similarity

terms <- alluv_filt |> select(dynamic_cluster_leiden, cluster_label) |> 
  distinct() %>%
  mutate(
    cluster_label = cluster_label %>%
      str_replace_all("\\s*,\\s*", ",") %>%   # remove spaces around commas
      str_replace_all("\\s+", "_")            # replace remaining spaces with underscores
  ) |> 
  filter(cluster_label != "no_name") |> 
  filter(lengths(gregexpr("\\W+", cluster_label)) > 5)

terms_corp <- corpus(terms, docid_field = "dynamic_cluster_leiden", text_field = "cluster_label")
toks <- tokens(terms_corp, remove_punct = T, remove_separators = T) |> 
  tokens_wordstem()
dfm <- dfm(toks) |> 
  dfm_tfidf()

simil <- textstat_simil(dfm, method = "cosine")

sim_df <- as.data.frame(as.table(as.matrix(simil))) %>%
  filter(Var1 != Var2) |> 
  filter(Freq > 0.6) |> 
  group_by(Var1, Var2) |> 
  distinct(Freq)

