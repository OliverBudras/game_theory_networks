library(openalexR)
library(tidyverse)
library(here)
library(bibliometrix)
library(bibliometrixData)
library(wordcloud)
library(plotly)
library(fuzzyjoin)
library(stringdist)
library(readxl)


clean_journal <- function(x) {
  x %>%
    str_to_lower() %>%
    str_replace_all("&", "and") %>%
    #str_replace_all("[^a-z ]", "") %>%
    str_replace_all("^the", "") |> 
    str_replace_all("\\(|\\)|\\.|\\,|\\-", " ") |> 
    str_replace_all("abacus a journal of accounting finance and business studies|abacus new york", "abacus") |>  # abacus mismatch fix
    str_replace_all("4or a quarterly journal of operations research", "4or") |>  # 4or fix
    str_replace_all("physica d nonlinear phenomena", "physica d") |> 
    str_replace_all("physica a statistical mechanics and its applications", "physica a") |> 
    str_replace_all("ieee transactions on systems man and cybernetics systems", "ieee transactions on systems man and cybernetics") |> 
    str_squish()
}



data <- readRDS("data_references.RDS")



journals <- data |> 
  select(oa_id, source_display_name) |> 
  drop_na() |> 
  mutate(Journal = clean_journal(source_display_name))


discipline_info <- read_xlsx("discipline_info_ob_ep.xlsx")


journal_info <- read_csv("journal_info.csv")


discipline_journal_map <- journal_info |> 
  left_join(discipline_info) |> 
  mutate_at(vars(Journal, Journal_abbrev), clean_journal)

matched <- stringdist_left_join(
  journals,
  discipline_journal_map,
  by = c("Journal" = "Journal"),
  method = "jw",
  max_dist = 0.1,
  distance_col = "distance_col"
)

check_correct <- matched |> 
  group_by(oa_id) |> 
  slice_min(distance_col, n = 1) |>
  filter(distance_col == 0) 


check_na <- matched |> 
  filter(is.na(distance_col)) |> 
  select(Journal.x, Journal.y, distance_col) |> 
  group_by(Journal.x) |> 
  summarise(N = n())


check_mismatch <- matched |> 
  select(oa_id, Journal.x, Journal.y, distance_col) |> 
  group_by(oa_id) |> 
  slice_min(distance_col, n = 1) |> 
  filter(distance_col != 0) |> 
  group_by(Journal.x) |> 
  summarise(N = n())




data_final <- data |> 
  left_join(check_correct) |> 
  select(-Journal.y) |> 
  drop_na(discipline) 


saveRDS(data_final, "data_references_final.RDS")

