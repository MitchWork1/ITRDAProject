library(tidyverse)
library(palmerpenguins)
library(ggthemes)

data = penguins

ggplot(data, aes(x=flipper_length_mm, y = body_mass_g)) +
  geom_point(aes(color = species, shape = species)) + 
  geom_smooth(method = "lm") +
  labs(
    title = "Body mass and flipper length",
    subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
    x = "Flipper length (mm)", y = "Body mass (g)",
    color = "Species", shape = "Species"
  ) +
  scale_color_colorblind()

#Percentage bar
ggplot(penguins, aes(x = island, fill = species)) +
  geom_bar(position = "fill")

#Multiple Graphs
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(aes(color = species, shape = species)) +
  facet_wrap(~island)
             
#Saving plots ggsave(filelname = "FileName.png")