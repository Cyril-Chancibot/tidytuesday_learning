library(tidyverse)
library(patchwork)
library(scales)
library(RColorBrewer)

england_wales_names <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-06-16/england_wales_names.csv')
ni_names <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-06-16/ni_names.csv')
scotland_names <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-06-16/scotland_names.csv')


#Are boys or girls names more likely to be unique ?

sum <- bind_rows(england_wales_names, ni_names, scotland_names) |> 
  summarize(
    Total = sum(Number, na.rm = TRUE),
    .by = c("Name", "Sex")
  ) |> 
  arrange(desc(Total))

sum |> 
  filter(Total != 0) |> 
  mutate(
    Total_Group = case_when(
      Total <= 10    ~ "Unique",
      Total <= 100   ~ "Rare",
      Total <= 10000 ~ "Common",
      TRUE           ~ "Most used"
    ),
    Total_Group = fct_relevel(Total_Group, "Unique", "Rare", "Common", "Most used")
  ) |> 
  
  ggplot(aes(x = Total_Group, fill = Sex)) +
  geom_bar(position = "fill") + 
  geom_hline(yintercept = 0.5, color = "red", linetype = "dashed", linewidth = 1) +
  labs(
    title = "Girls names uniqueness dominance over boys names",
    subtitle = "Proportions given by names from the UK",
    x = NULL,
    y = NULL
  ) +
  scale_y_continuous(
    labels = label_percent()
  ) + 
  scale_fill_manual(values = c(Boy = "#43a2ca", Girl = "#fa9fb5")) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(face = "italic", color = "grey30", size = 12),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(margin = margin(b = 20), face = "bold", color = "grey50", size = 12),
    axis.text.y = element_text(face = "plain", color = "grey50", size = 10),
    legend.title = element_blank(),
    legend.text = element_text(face = "plain", color = "grey50", size = 10),
    panel.background = element_rect(fill = "white"),
  )

#Bridgerton trend in charts

scotland_names |> 
    filter(Year %in% c(2024, 2025) & Sex == "Girl") |> 
    summarize(
      Number = sum(Number, na.rm = TRUE),
      .by = c(Year, Name)
    ) |> 
    mutate(
      Rank = min_rank(desc(Number)),
      .by = Year
    ) |> 
    arrange(Rank) |> 
    filter(Name == "Daphne" | Name == "Eloise" | Name == "Penelope") |> 
  
  ggplot(aes(x = Year, y = Rank, color = Name)) + 
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  geom_text(aes(x = 2025.1, y = 172, label = "Daphne"), color = "#66c2a5") +
  geom_text(aes(x = 2025.1, y = 91, label = "Eloise"), color = "#fc8d62") +
  geom_text(aes(x = 2025.1, y = 71, label = "Penelope"), color = "#8da0cb") +
  guides(color = "none") +
  scale_color_manual(values = c(Daphne = "#66c2a5",
                               Eloise = "#fc8d62",
                               Penelope = "#8da0cb"
                            )
                ) +
  scale_y_reverse() +
  scale_x_continuous(
    breaks = seq(2024, 2025, .by = 1),
    limits = c(2024, 2025.2)
  ) +
  labs(
    title = "Bridgerton syndrom",
    subtitle = "The uprising of Bridergton names for babies",
    x = NULL,
    y = "Rang"
  )  +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(face = "italic", color = "grey30", size = 12),
    axis.text.x = element_text(margin = margin(b = 20), face = "bold", color = "grey50", size = 12),
    axis.text.y = element_text(face = "plain", color = "grey50", size = 10)
  )



