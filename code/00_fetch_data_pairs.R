library(openalexR)
library(tidyverse)
library(here)
library(readxl)
library(cli)
library(purrr)

jel_terms <- read_excel("v3/JEL_code_terms.xlsx")

game_terms <- read_excel("v3/game_description_terms.xlsx")

keywords_pairs <- merge(jel_terms, game_terms, by = NULL)

pairs_string <- keywords_pairs |> 
  mutate(pair = paste0('"', Term.x, '"', " AND ", '"', Term.y, '"'))

  
  

fetch_pair <- function(pair) {
  # Use exact match queries
  
  #fulltext_search <- oa_fetch("works",fulltext.search = pair, verbose = TRUE)
  title_search <- oa_fetch("works", title.search = pair, verbose = TRUE)
  abstract_search <- oa_fetch("works", abstract.search = pair, verbose = TRUE)
  
  
  pair_name <- str_replace_all(pair, '"', '') |> 
    str_replace_all(" AND ", "_AND_") |> 
    str_replace_all(" ", "_")
  
  result_pre <- bind_rows(title_search, abstract_search) 
  
  if(nrow(result_pre) == 0){
    
    result <- data.frame(id = character())
    
  } else{
    
    
    result <- result_pre |> 
      distinct(id, .keep_all = TRUE) |> 
      mutate(query = pair_name)
    
    
  }
  
  saveRDS(result, here(paste0("v3/Data/Titles and Abstracts/", pair_name, ".RDS")))
}


results <- map(pairs_string$pair, fetch_pair,.progress = list(
  type = "iterator", 
  format = "Calculating {cli::pb_bar} {cli::pb_percent}",
  clear = TRUE))



