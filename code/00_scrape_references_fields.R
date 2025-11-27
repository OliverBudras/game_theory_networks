library(tidyverse)
library(furrr)
library(openalexR)

data <- readRDS("v3/data_final_1980.RDS") 


references <- data |> 
  select(oa_id, referenced_works) |> 
  unnest_longer(referenced_works) |> 
  distinct(referenced_works) |> 
  filter(referenced_works != "https://openalex.org/W6672461286")


load_references <- function(batch_ids){
  
  search_query <- str_replace_all(batch_ids, "https://openalex.org/", "")
  
  reference_meta <- oa_fetch(
    "works",
    identifier   = search_query,
    abstract = FALSE,
    options  = list(select = c("id","topics")),
    verbose  = FALSE
  )
  
  if (is.null(reference_meta)) {
    meta_final <- data.frame(id = batch_ids, topics = NA)
  } else {
    meta_final <- reference_meta
  }
  
  return(meta_final)
}

batch_size <- 50
reference_ids <- references[[1]]



# Create batches
batches <- split(reference_ids, ceiling(seq_along(reference_ids) / batch_size))


# Run over batches
results <- map_dfr(batches, load_references,.progress = list(
  type = "iterator", 
  format = "Calculating {cli::pb_bar} {cli::pb_percent}",
  clear = TRUE))  |> 
  rename(oa_id = id) |> 
  unnest(cols = c(topics))  |> 
  mutate(type_no = paste0(type, "_", i)) |> 
  pivot_wider(id_cols = oa_id, names_from = type_no, values_from = display_name) |> 
  select(oa_id, field_1) |> 
  drop_na(field_1)

saveRDS(results, "v3/data_references.RDS")


