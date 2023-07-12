
library(ggplot2)
library(maps)
map <- map_data('world')
p <- ggplot() +
  geom_polygon(data = map, aes(x = long, y = lat, group = group), fill = 'lightblue', color = 'black') +
  coord_equal() +
  labs(title = 'Carte du monde', subtitle = 'Exemple de carte géographique') +
  theme_minimal()
ggsave('carte.png', plot = p)
