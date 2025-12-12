library(tidyverse)
library(tidytext)

data <- readRDS("field_topic_data.RDS") |> 
  select(-text, -embedding) |> 
  filter(topic != -1) |> 
  mutate(topic_keywords = str_replace_all(topic_keywords, "\\[", ""),
         topic_keywords = str_replace_all(topic_keywords, "\\]", ""),
         topic_keywords = str_replace_all(topic_keywords, "\\'", ""))




field_topic_overview <- data |> 
  group_by(field_1, topic, topic_keywords) |> 
  summarise(N = n())



field_topic_overview |> 
  group_by(field_1) |> 
  mutate(N_rel = N / sum(N)) |> 
  slice_max(order_by = N_rel, n = 3, with_ties = FALSE) |>
  ungroup() |>
  ggplot(aes(
    x = reorder_within(topic_keywords, N_rel, field_1),
    y = N_rel,
    fill = topic_keywords
  )) +
  geom_col() +
  labs(x = "") +
  scale_x_reordered() +
  facet_wrap(~ field_1, scales = "free_x")  +
  labs(x = "") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "bottom", legend.direction = "vertical") +
  scale_fill_discrete("Keywords", guide = guide_legend(ncol = 4))
