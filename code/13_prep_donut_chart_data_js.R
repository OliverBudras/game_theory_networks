library(tidyverse)




data <- readRDS("field_topic_data.RDS") |> 
  select(-text, -embedding) |> 
  filter(topic != -1) |> 
  mutate(topic_keywords = str_replace_all(topic_keywords, "\\[", ""),
         topic_keywords = str_replace_all(topic_keywords, "\\]", ""),
         topic_keywords = str_replace_all(topic_keywords, "\\'", "")) |> 
  group_by(field_1, topic, topic_keywords) |> 
  summarise(N = n()) |> 
  group_by(field_1) |> 
  mutate(N_rel = N / sum(N)) 


write_csv(data,"sigma_network_field_supernodes/rolling_network/public/bertopic_topics.csv")

