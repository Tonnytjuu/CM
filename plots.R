library(tidyverse)
library(spotifyr)
library(ggplot2)

cm <- get_playlist_audio_features("", "0Exrs3zznekmCiCjhGZ3X9")
cm1 <- get_playlist_audio_features("", "2tO0uLN5SxReQo533Ekoo4")
cm2 <- get_playlist_audio_features("", "5aGC1K6IyQkXicMaaIA82Y")
cm3 <- get_playlist_audio_features("", "5CMf4RYXFouuTKqI6RZ8iY")

data1 <- data.frame(
  x=cm1 %>% select('valence'),
  y=cm1 %>% select('energy')
)

data2 <- data.frame(
  x=cm2 %>% select('valence'),
  y=cm2 %>% select('energy')
)

data3 <- data.frame(
  x=cm3 %>% select('valence'),
  y=cm3 %>% select('energy')
)

plot <- ggplot() + geom_point(data1, mapping=aes(valence, energy, color='red')) +
  geom_point(data2, mapping=aes(valence, energy, color='blue')) +
  geom_point(data3, mapping=aes(valence, energy, color='green')) + 
  ggtitle("Valence and energy of the songs in my corpus")

plot + scale_color_hue(labels = c("Rap", "Metal", "House/Rest")) + theme(legend.title=element_blank())
