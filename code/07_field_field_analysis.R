library(tidyverse)

# Folder with the RDS files
rds_folder <- "Field_Field_Data/"

# List all RDS files
rds_files <- list.files(rds_folder, pattern = "\\.RDS$", full.names = TRUE)

# Read and combine
all_metrics <- rds_files %>%
  map_dfr(function(f) {
    df <- readRDS(f)
    
    # Extract window from filename
    # Assumes filenames like "1970-1974_metrics.RDS"
    window <- str_extract(basename(f), "\\d{4}-\\d{4}")
    
    df %>% mutate(window = window)  |> 
      filter(field_min != field_max) |> 
      mutate(pair = paste(field_min, field_max, sep = " – "))
  }) |> 
  group_by(window) |> 
  mutate(N_edges_rel = N_edges/sum(N_edges)) |> 
  separate(window, into = c("start", "end"), remove=F, sep="-") |> 
  mutate_at(vars(start, end), as.numeric) |> 
  filter(start >= 1980)


all_metrics_selected_fields <- all_metrics |> 
  filter(!field_min %in% c("Arts and Humanities", "Biochemistry, Genetics and Molecular Biology", "Health Professions",
                           "Neuroscience", "Medicine", "Earth and Planetary Sciences", "Energy", 
                           "Pharmacology, Toxicology and Pharmaceutics", "Materials Science",
                           "Immunology and Microbiology") & 
           !field_max %in%  c("Arts and Humanities", "Biochemistry, Genetics and Molecular Biology", "Health Professions",
                              "Neuroscience", "Medicine", "Earth and Planetary Sciences", "Energy",
                              "Pharmacology, Toxicology and Pharmaceutics", "Materials Science", "Immunology and Microbiology"))


all_metrics_selected_fields2 <- all_metrics |> 
  filter(field_min %in% c("Computer Science", "Decision Sciences", "Social Sciences", "Engineering", "Economics, Econometrics and Finance") & 
           field_max %in%  c("Computer Science", "Decision Sciences", "Social Sciences", "Engineering", "Economics, Econometrics and Finance"))


ggplot(all_metrics, aes(x = field_min, y = field_max, fill = N_edges)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(option = "plasma") +
  facet_wrap(~ window) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )


ggplot(all_metrics_selected_fields, aes(x = field_min, y = field_max, fill = N_edges)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(option = "plasma") +
  facet_wrap(~ window) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )




ggplot(all_metrics_selected_fields2, aes(x = field_min, y = field_max, fill = N_edges)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(option = "plasma") +
  facet_wrap(~ window) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )


ggplot(all_metrics_selected_fields2, aes(x = field_min, y = field_max, fill = N_edges_rel)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(option = "plasma") +
  facet_wrap(~ window) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )

ggplot(all_metrics_selected_fields2, aes(x = window, y = N_edges, group = pair, color = pair)) +
  geom_line() +
  geom_point(size = 1) +
  facet_wrap(~ pair) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_blank()) 

ggplot(all_metrics_selected_fields2, aes(x = window, y = N_edges_rel, group = pair, color = pair)) +
  geom_line() +
  geom_point(size = 1) +
  facet_wrap(~ pair) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_blank()) 
