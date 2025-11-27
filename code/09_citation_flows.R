library(tidyverse)
library(igraph)
library(sigmajs)
library(biblionetwork)
library(RColorBrewer)
library(shiny)
library(jsonlite)
library(ggalluvial)

### Test for the first 10 year bucket

data <- readRDS("data_final_1980.RDS") 

data_pairs_filtered <- readRDS("data_pairs_filtered.RDS")

# use data_pairs, because it contains articles prior to 1980 (articles from 1980 cannot cite articles from 1980)



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

fields <- data_pairs_filtered |> 
  select(oa_id, topics) |> 
  unnest(cols = c(topics)) |> 
  mutate(type_no = paste0(type, "_", i)) |> 
  pivot_wider(id_cols = oa_id, names_from = type_no, values_from = display_name) |> 
  select(oa_id, field_1)

data_ref <- data |> 
  select(oa_id, referenced_works) |> 
  left_join(meta |> select(oa_id, publication_year)) |> 
  unnest_longer(referenced_works) |> 
  rename(oa_id_to = oa_id, oa_id_from = referenced_works) |> 
  left_join(fields, by = c("oa_id_to" = "oa_id")) |> 
  rename(field_to = field_1) |> 
  left_join(fields |> select(oa_id, field_1), by=c("oa_id_from"="oa_id")) |> 
  rename(field_from = field_1)

internal_coverage <- data_ref |> 
  group_by(field_from) |> 
  summarise(N = n()) |> 
  ungroup() |> 
  mutate(N_total = sum(N), rel_coverage = N/N_total)

internal_coverage |> 
  ggplot(aes(field_from,rel_coverage)) +
  geom_bar(stat = "identity")

# internal coverage is rather low, but it's unclear whether this is due to the OA database or due to the data we use
# it is likely that articles cite earlier articles not present in OA 
# also likely that articles cite other articles which do not belong to game theory
# should we only consider references which belong to game theory? might make sense, because this ensures that game-theoretical knowledge
# is passed on

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
  ungroup()

coc <- cit_link |> 
  filter(field_to != field_from) |> 
  group_by(publication_year, field_to) |> 
  summarise(N_outside = sum(N)) |> 
  rename(field = field_to)

cic <- cit_link |> 
  filter(field_to == field_from) |> 
  group_by(publication_year, field_to) |> 
  summarise(N_inside = sum(N)) |> 
  rename(field = field_to)

outside_give <- cit_link |> 
  filter(field_to != field_from) |> 
  group_by(publication_year, field_from) |> 
  summarise(N_give_out= sum(N)) |> 
  rename(field = field_from)


inside_give <- cit_link |> 
  filter(field_to == field_from) |> 
  group_by(publication_year, field_from) |> 
  summarise(N_give_in= sum(N)) |> 
  rename(field = field_from)


field_overview <- coc |> 
  left_join(cic) |> 
  left_join(outside_give) |>
  left_join(inside_give) |> 
  mutate_all(~replace_na(.x, 0)) |> 
  mutate(coc = N_outside/(N_outside+N_inside),
         cic = N_inside/(N_outside+N_inside),
         outside_give = N_give_out/(N_give_out+N_give_in),
         inside_give = N_give_in/(N_give_out+N_give_in)) |> 
  mutate_all(~replace_na(.x, 0))

field_overview_long <- field_overview |> 
  pivot_longer(cols = c(coc, cic, outside_give, inside_give), names_to = "variable", values_to = "fraction") |> 
  filter(!field %in% c("Arts and Humanities", "Biochemistry, Genetics and Molecular Biology", "Health Professions",
         "Neuroscience", "Medicine", "Earth and Planetary Sciences", "Energy", 
         "Pharmacology, Toxicology and Pharmaceutics", "Materials Science",
         "Immunology and Microbiology", "Chemical Engineering"))

field_overview_long |>
  filter(variable %in% c("cic", "coc")) |> 
  ggplot(aes(x = publication_year, y = fraction, fill = variable)) +
  geom_area(alpha = 0.8) +
  scale_y_continuous(labels = scales::percent) +
  labs(y = "Fraction", x = "Year") +
  theme_minimal() +
  facet_wrap(~field)


field_overview_long |>
  filter(variable %in% c("outside_give", "inside_give")) |> 
  ggplot(aes(x = publication_year, y = fraction, fill = variable)) +
  geom_area(alpha = 0.8) +
  scale_y_continuous(labels = scales::percent) +
  labs(y = "Fraction", x = "Year") +
  theme_minimal() +
  facet_wrap(~field)



cit_link_filt <- cit_link |> 
  filter(field_to %in% c("Computer Science", "Decision Sciences", "Social Sciences", "Engineering", "Economics, Econometrics and Finance") & 
        field_from %in%  c("Computer Science", "Decision Sciences", "Social Sciences", "Engineering", "Economics, Econometrics and Finance")) |>
  group_by(field_to, publication_year) |> 
  mutate(N_total = sum(N), N_rel =N/N_total) |> 
  left_join(windows)

cit_link_filt_moving <- cit_link_moving |> 
  filter(field_to %in% c("Computer Science", "Decision Sciences", "Social Sciences", "Engineering", "Economics, Econometrics and Finance") & 
           field_from %in%  c("Computer Science", "Decision Sciences", "Social Sciences", "Engineering", "Economics, Econometrics and Finance")) |>
  group_by(field_to, window) |> 
  mutate(N_total = sum(N), N_rel =N/N_total)



cit_link_filt |> 
  ggplot(aes(x = field_to, y = field_from, fill = N_rel)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(option = "plasma") +
  facet_wrap(~ window, ncol=6) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1),
    panel.grid = element_blank()
  )

cit_link_filt |> 
  ggplot(aes(x = field_to, y = field_from, fill = N_rel)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(option = "plasma") +
  facet_wrap(~ publication_year, ncol=6) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1),
    panel.grid = element_blank()
  )


  
ggplot(cit_link_filt, aes(x = publication_year, y = N_rel, color = field_from)) +
  geom_line() +
  facet_wrap(~ field_to)
  
  
ggplot(cit_link_filt_moving, aes(x = window_mid, y = N_rel, color = field_from)) +
  geom_line() +
  facet_wrap(~ field_to)




ggplot(cit_link_filt_moving,
       aes(axis1 = field_from, axis2 = field_to, y = N_rel)) +
  geom_alluvium(aes(fill = field_from), width = 1/12, alpha = 0.8) +
  geom_stratum(width = 1/12, fill = "grey80", color = "black") +
  geom_text(stat = "stratum", 
            aes(label = str_wrap(after_stat(stratum), width = 15))) + 
  labs(y = "Fraction of outgoing citations",
       x = NULL,
       title = "Cross-field citation flows (5-year moving windows)") +
  theme_minimal()+
  theme(legend.position = "none")
  
ggplot(cit_link_filt,
       aes(axis1 = field_from, axis2 = field_to, y = N_rel)) +
  geom_alluvium(aes(fill = field_from), width = 1/12, alpha = 0.8) +
  geom_stratum(width = 1/12, fill = "grey80", color = "black") +
  geom_text(stat = "stratum", 
            aes(label = str_wrap(after_stat(stratum), width = 15))) +  
  facet_wrap(~ window, ncol = 4, scales = "free_y") +
  labs(y = "Fraction of outgoing citations",
       x = NULL,
       title = "Cross-field citation flows (5-year moving windows)") +
  theme_minimal()+
  theme(legend.position = "none")

