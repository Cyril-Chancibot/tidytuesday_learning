library(tidyverse)

game_films <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-06-09/game_films.csv')

glimpse(game_films)

#Which video game franchise has generated the most film adaptations ?

game_films |> 
  summarize(
    amount_of_film_adaptation = n(),
    mean_box_office = mean(worldwide_box_office, na.rm = TRUE),
    .by = original_game_publisher
  ) |> 
  filter(!is.na(original_game_publisher)) |>  #NA values are mostly publisher that have done 1 single game/film
  slice_max(amount_of_film_adaptation, n = 10) |> 
  arrange(desc(mean_box_office)) |> 
  
  ggplot(
    aes(
      x = amount_of_film_adaptation,
      y = mean_box_office, 
      fill = original_game_publisher
    )
  ) +
  labs(
    title = "Mean box office revenue for different game publisher",
    subtitle = "by the 10 most published",
    x = "Number of film adaptation",
    y = "Mean box office revenue",
    fill = "Game publisher"
  ) +
  geom_col()

#Nintendo Pokemon Company seems to be the most lucrative one, it's most likely due to the fact that the public is kids and parents that accompany them

#Do audiences and critics agree ?

game_films |> 
  select(c("rotten_tomatoes", "metacritic", "title")) |> 
  mutate(
    ratio = rotten_tomatoes / metacritic,
    .before = rotten_tomatoes
  ) |> 
  filter(
    !is.na(rotten_tomatoes),
    !is.na(metacritic)
  ) |> 
  
  ggplot(aes(x = rotten_tomatoes, y = metacritic, color = ratio)) +
  labs(
    title = "Rotten tomatoes vs Metacritic scores : Do they agree ?",
    x = "Rotten tomatoes scores (/100)",
    y = "Metacritic scores (/100)",
    color = "Rotten tomatoes / Metacritic scores"
  ) +
  geom_point() + 
  geom_smooth(color = "black",se = FALSE) 

#Except some outliers, both sites seems to fairly agree