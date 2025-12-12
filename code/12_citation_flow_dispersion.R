library(tidyverse)
library(networkflow)


data <- readRDS("data_final_1980.RDS") 

data_references <- readRDS("data_references.RDS")


fields_of_interest <- c("Computer Science", "Business, Management and Accounting", "Decision Sciences", "Social Sciences",
                        "Engineering", "Economics, Econometrics and Finance")


## Extracting authors

authors <- data |> 
  select(oa_id, authorships, cited_by_count) |> 
  unnest(cols = c(authorships)) |> 
  select(oa_id, display_name, cited_by_count) 

## Authors as strings

authors_string <- authors |> 
  group_by(oa_id) |> 
  summarise(authors = paste(display_name, collapse = ";"), .groups = "drop") 

meta <- data |> 
  select(oa_id, title, publication_year, year_bucket_10, cited_by_count, topic_1, subfield_1, field_1, domain_1) |> 
  left_join(authors_string)

fields <- data |> 
  select(oa_id, topics) |> 
  unnest(cols = c(topics)) |> 
  mutate(type_no = paste0(type, "_", i)) |> 
  pivot_wider(id_cols = oa_id, names_from = type_no, values_from = display_name) |> 
  select(oa_id, field_1)

data_ref <- data |> 
  select(oa_id, referenced_works) |> 
  left_join(meta |> select(oa_id, publication_year, field_1)) |> 
  unnest_longer(referenced_works) |> 
  rename(oa_id_to = oa_id, oa_id_from = referenced_works, field_to = field_1) |> 
  left_join(data_references |> select(oa_id, field_1), by=c("oa_id_from"="oa_id")) |> 
  rename(field_from = field_1) 


windows_moving <- tibble(
  start = 1980:2020,
  end   = start + 4,
  window_moving = paste0(start, "-", end)
)

windows <- data |> 
  distinct(publication_year)

windows$window <- cut(
  windows$publication_year,
  breaks = c(seq(1980, 2021, by = 4), 2025),  # last bin ends just after 2025
  right  = FALSE,
  labels = c(
    paste(seq(1980, 2016, 4), seq(1984, 2020, 4), sep = "-"),
    "2021-2024"
  )
)



cit_link <- data_ref |> 
  drop_na(field_to, field_from) |> 
  group_by(publication_year, field_to, field_from) |> 
  summarise(N =n()) 

cit_link_moving <- cit_link %>%
  inner_join(
    windows_moving,
    by = character(),   # cross join
  ) %>%
  filter(publication_year >= start,
         publication_year <= end)


cit_link_moving <- cit_link_moving %>%
  group_by(window_moving, field_to, field_from) %>%
  summarise(N = sum(N), .groups = "drop") %>%
  group_by(window_moving, field_to) %>%
  mutate(N_total = sum(N),
         N_rel = N / N_total) %>%
  ungroup() |> 
  filter(field_to %in% fields_of_interest & field_from %in% fields_of_interest)

disp_overall <- cit_link_moving |> 
  group_by(field_to) |> 
  summarise(entropy = sum(-N_rel*log(N_rel)))



disp_window <- cit_link_moving |> 
  group_by(field_to, window_moving) |> 
  summarise(entropy = sum(-N_rel*log(N_rel)))
  


disp_window |> 
  ggplot(aes(window_moving, entropy)) +
  geom_point() +
  facet_wrap(~field_to)
  
  
  


