library(tidyverse)
library(igraph)
library(sigmajs)
library(biblionetwork)
library(RColorBrewer)
library(shiny)
library(jsonlite)
library(ggalluvial)
library(networkflow)
library(ggraph)
library(tidygraph)


### Test for the first 10 year bucket

data <- readRDS("data_final_1980.RDS") 

temporal_networks_with_names <- readRDS("Temporal Networks/temporal_networks_with_names.RDS") |> 
  networks_to_alluv(node_id = "oa_id", intertemporal_cluster_column = "dynamic_cluster_leiden")  |> 
  filter(length_cluster > 3 & share_cluster_max > 1)

meta <- temporal_networks_with_names |> 
  select(oa_id, window, dynamic_cluster_leiden, cluster_label) 

data_ref <- data |> 
  select(oa_id, referenced_works) |> 
  unnest_longer(referenced_works) |> 
  left_join(meta) |> 
  rename(dynamic_cluster_leiden_to = dynamic_cluster_leiden,
         cluster_label_to = cluster_label) |> 
  left_join(meta |> select(-window), by = c("referenced_works" = "oa_id")) |> 
  rename(dynamic_cluster_leiden_from = dynamic_cluster_leiden,
         cluster_label_from = cluster_label)

internal_coverage <- data_ref |> 
  group_by(cluster_label_from) |> 
  summarise(N = n()) |> 
  ungroup() |> 
  mutate(N_total = sum(N), rel_coverage = N/N_total)

internal_coverage |> 
  ggplot(aes(cluster_label_from,rel_coverage)) +
  geom_bar(stat = "identity")

# internal coverage is rather low, but it's unclear whether this is due to the OA database or due to the data we use
# it is likely that articles cite earlier articles not present in OA 
# also likely that articles cite other articles which do not belong to game theory
# should we only consider references which belong to game theory? might make sense, because this ensures that game-theoretical knowledge
# is passed on


cit_link <- data_ref |> 
  drop_na(dynamic_cluster_leiden_to, dynamic_cluster_leiden_from, cluster_label_to, cluster_label_from) |> 
  group_by(window, dynamic_cluster_leiden_to, dynamic_cluster_leiden_from, cluster_label_to, cluster_label_from) |> 
  summarise(N =n())  |>
  group_by(dynamic_cluster_leiden_to, window) |> 
  mutate(N_total = sum(N), N_rel =N/N_total)  %>%
  mutate(cluster_label_short_from = cluster_label_from %>%
           str_split(",") %>%                  # split by comma
           map_chr(~ str_trim(paste(head(.x, 2), collapse = ", "))))  %>%
  mutate(cluster_label_short_to = cluster_label_to %>%
           str_split(",") %>%                  # split by comma
           map_chr(~ str_trim(paste(head(.x, 2), collapse = ", "))))  |> 
  mutate(window_start = as.numeric(sub("-.*", "", window)))


coc <- cit_link |> 
  filter(dynamic_cluster_leiden_to != dynamic_cluster_leiden_from) |> 
  group_by(window, dynamic_cluster_leiden_to, cluster_label_to) |> 
  summarise(N_outside = sum(N)) |> 
  rename(dynamic_cluster_leiden = dynamic_cluster_leiden_to,
         cluster_label = cluster_label_to)

cic <- cit_link |> 
  filter(dynamic_cluster_leiden_to == dynamic_cluster_leiden_from) |> 
  group_by(window, dynamic_cluster_leiden_to, cluster_label_to) |> 
  summarise(N_inside = sum(N)) |> 
  rename(dynamic_cluster_leiden = dynamic_cluster_leiden_to,
         cluster_label = cluster_label_to)

outside_give <- cit_link |> 
  filter(dynamic_cluster_leiden_to != dynamic_cluster_leiden_from) |> 
  group_by(window, dynamic_cluster_leiden_from, cluster_label_from) |> 
  summarise(N_give_out= sum(N)) |> 
  rename(dynamic_cluster_leiden = dynamic_cluster_leiden_from,
         cluster_label = cluster_label_from)


inside_give <- cit_link |> 
  filter(dynamic_cluster_leiden_to == dynamic_cluster_leiden_from) |> 
  group_by(window, dynamic_cluster_leiden_from, cluster_label_from) |> 
  summarise(N_give_in= sum(N)) |> 
  rename(dynamic_cluster_leiden = dynamic_cluster_leiden_from,
         cluster_label = cluster_label_from)


field_overview <- coc |> 
  left_join(cic) |> 
  left_join(outside_give) |>
  left_join(inside_give) |> 
  mutate_at(vars(N_outside:N_give_in),~replace_na(.x, 0)) |> 
  mutate(coc = N_outside/(N_outside+N_inside),
         cic = N_inside/(N_outside+N_inside),
         outside_give = N_give_out/(N_give_out+N_give_in),
         inside_give = N_give_in/(N_give_out+N_give_in)) |> 
  mutate_all(~replace_na(.x, 0)) %>%
  mutate(cluster_label_short = cluster_label %>%
           str_split(",") %>%                  # split by comma
           map_chr(~ str_trim(paste(head(.x, 2), collapse = ", ")))) 

field_overview_long <- field_overview |> 
  pivot_longer(cols = c(coc, cic, outside_give, inside_give), names_to = "variable", values_to = "fraction") |> 
  mutate(window_start = as.numeric(sub("-.*", "", window)))


field_overview_long |>
  filter(variable %in% c("cic", "coc")) |> 
  ggplot(aes(x = window_start, y = fraction, fill = variable)) +
  geom_area(alpha = 0.8) +
  scale_y_continuous(labels = scales::percent) +
  labs(y = "Fraction", x = "Year") +
  theme_minimal() +
  facet_wrap(~cluster_label_short)


field_overview_long |>
  filter(variable %in% c("outside_give", "inside_give")) |> 
  ggplot(aes(x = window_start, y = fraction, fill = variable)) +
  geom_area(alpha = 0.8) +
  scale_y_continuous(labels = scales::percent) +
  labs(y = "Fraction", x = "Year") +
  theme_minimal() +
  facet_wrap(~cluster_label_short)




cit_link |> 
  ggplot(aes(x = cluster_label_short_to, y = cluster_label_short_to, fill = N_rel)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(option = "plasma") +
  facet_wrap(~ window, ncol=6) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1),
    panel.grid = element_blank()
  )





ggplot(cit_link, aes(x = window_start, y = N_rel, color = cluster_label_short_from)) +
  geom_line() +
  facet_wrap(~ cluster_label_short_to)




cit_link |> 
  filter(window %in% c("1980-1984", "2000-2004", "2020-2024")) |> 
  filter(N_rel > 0.05) |> 
  ggplot(aes(axis1 = cluster_label_short_from, axis2 = cluster_label_short_to, y = N_rel)) +
  geom_alluvium(aes(fill = cluster_label_short_from), width = 1/12, alpha = 0.8) +
  geom_stratum(width = 1/12, fill = "grey80", color = "black") +
  geom_text(stat = "stratum", 
            aes(label = str_wrap(after_stat(stratum), width = 15))) +  
  facet_wrap(~ window, ncol = 4, scales = "free_y") +
  labs(y = "Fraction of outgoing citations",
       x = NULL,
       title = "Cross-field citation flows (5-year moving windows)") +
  theme_minimal()+
  theme(legend.position = "none")


cit_link |> 
  filter(cluster_label_short_to  == "cognitive,  networks")  |> 
  filter(N_rel > 0.05) |>  
  ggplot(aes(axis1 = cluster_label_short_from, axis2 = cluster_label_short_to, y = N_rel)) +
  geom_alluvium(aes(fill = cluster_label_short_from), width = 1/12, alpha = 0.8) +
  geom_stratum(width = 1/12, fill = "grey80", color = "black") +
  geom_text(stat = "stratum", 
            aes(label = str_wrap(after_stat(stratum), width = 15))) +  
  facet_wrap(~ window, ncol = 4, scales = "free_y") +
  labs(y = "Fraction of outgoing citations",
       x = NULL,
       title = "Cross-field citation flows (5-year moving windows)") +
  theme_minimal()+
  theme(legend.position = "none")
