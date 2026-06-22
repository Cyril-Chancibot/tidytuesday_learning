library(tidyverse)

plp <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-06-02/eplp.csv')

View(plp)
glimpse(plp)

#Which countries were the first to implement co-parent leave policies ?

plp |> 
  filter(co_ld != 0) |> 
  slice_min(year, by = country) |> 
  arrange(year) |> 
  mutate(
    country = fct_reorder(country, year) |> fct_rev()
  ) |> 
  
  ggplot(aes(x = year, y = country)) +
  geom_segment(aes(x = 1970, xend = year, y = country, yend = country), color = "black") +
  geom_point(color = "midnightblue", size = 2) +
  
  coord_cartesian(xlim = c(1970, 2025)) +
  theme_minimal() +
  labs(
    title = "Timeline of Coparenting Leave Implementation in Western Europe ",
    x = "Year",
    y = NULL
  )

#Has parenting leave decrease in any countries ?


plp |>
  distinct(country, year, co_ld) |> 
  mutate(
    type_de_ligne = if_else(country == "DK", "Cible", "Autres pays") 
  ) |> 
  
  # CORRECTION 1 : L'astuce magique du tri absolu
  # En testant (== "Cible"), tous les "Autres pays" (Faux) iront en haut, 
  # et la "Cible" (Vrai) ira à la toute fin du tableau, garantie d'être dessinée en dernier !
  arrange(type_de_ligne == "Cible") |> 
  
  # CORRECTION 2 : On rajoute "linewidth" dans les esthétiques (aes)
  ggplot(aes(x = year, y = co_ld, group = country, color = type_de_ligne, linewidth = type_de_ligne)) +
  geom_line() + 
  
  scale_color_manual(
    values = c(
      "Cible" = "red",         
      "Autres pays" = "gray85" 
    )
  ) +
  # CORRECTION 3 : On définit manuellement l'épaisseur selon le type de ligne
  scale_linewidth_manual(
    values = c(
      "Cible" = 1.5,       # La ligne rouge sera belle et épaisse
      "Autres pays" = 0.5  # Les lignes grises seront toutes fines
    )
  ) +
  theme_minimal() +
  labs(
    title = "Le Danemark (DK) a fortement réduit ses congés parentaux",
    x = "Année",
    y = "Semaines de congé"
  ) +
  guides(color = "none", linewidth = "none") # On nettoie la légende
    
  


