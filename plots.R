library(tidyverse)
library(spotifyr)
library(ggplot2)
library(flexdashboard)
library(readr)
library(lubridate)
library(plotly)

cm <- get_playlist_audio_features("", "0Exrs3zznekmCiCjhGZ3X9")
cm1 <- get_playlist_audio_features("", "2tO0uLN5SxReQo533Ekoo4")
cm2 <- get_playlist_audio_features("", "5aGC1K6IyQkXicMaaIA82Y")
cm3 <- get_playlist_audio_features("", "5CMf4RYXFouuTKqI6RZ8iY")

fig <- plot_ly()
fig <- fig %>%
  add_trace(
    data=cm1,
    x=~valence,
    y=~energy,
    type = "scatter",
    hoverinfo='text',
    mode = "markers",
    name='Rap',
    marker = list(color='green'),
    text= paste(
                " Song name: ", cm1$track.name,
                "<br>",
                "Valence: ", cm1$valence,
                "<br>",
                "Energy: ", cm1$energy
    )
)

fig <- plot_ly()
fig <- fig %>%
  add_trace(
    data=cm1,
    x=~valence,
    y=~energy,
    type = "scatter",
    hoverinfo='text',
    mode = "markers",
    name='Rap',
    marker = list(color='green'),
    text= paste(
      cm1$track.name,
      "<br>",
      "Valence: ", cm1$valence,
      "<br>",
      "Energy: ", cm1$energy
    )
  )

fig <- fig %>%
  add_trace(
    data=cm2,
    x=~valence,
    y=~energy,
    type = "scatter",
    hoverinfo='text',
    mode = "markers",
    name='Metal',
    marker = list(color='red'),
    text= paste(
      cm2$track.name,
      "<br>",
      "Valence: ", cm2$valence,
      "<br>",
      "Energy: ", cm2$energy
    )
  )

fig <- fig %>%
  add_trace(
    data=cm3,
    x=~valence,
    y=~energy,
    type = "scatter",
    hoverinfo='text',
    mode = "markers",
    marker = list(color='blue'),
    name='House/rest',
    text= paste(
      cm3$track.name,
      "<br>",
      "Valence: ", cm3$valence,
      "<br>",
      "Energy: ", cm3$energy
    )
  )

fig

fig2 <- plot_ly()
fig2 <- fig2 %>%
  add_trace(
    data=cm1,
    x=~danceability,
    y=~tempo,
    type = "scatter",
    hoverinfo='text',
    mode = "markers",
    size=~loudness,
    name='Rap',
    marker = list(color='green'),
    text= paste(
      cm1$track.name,
      "<br>",
      "Danceability: ", cm1$danceability,
      "<br>",
      "Tempo: ", cm1$tempo,
      "<br>",
      "Loudness: ", cm1$loudness
    )
  )

fig2 <- fig2 %>%
  add_trace(
    data=cm2,
    x=~danceability,
    y=~tempo,
    type = "scatter",
    hoverinfo='text',
    mode = "markers",
    size=~loudness,
    name='Metal',
    marker = list(color='red'),
    text= paste(
      cm2$track.name,
      "<br>",
      "Danceability: ", cm2$danceability,
      "<br>",
      "Tempo: ", cm2$tempo,
      "<br>",
      "Loudness: ", cm2$loudness
    )
  )

fig2 <- fig2 %>%
  add_trace(
    data=cm3,
    x=~danceability,
    y=~tempo,
    type = "scatter",
    hoverinfo='text',
    mode = "markers",
    size=~loudness,
    name='House/rest',
    marker = list(color='blue'),
    text= paste(
      cm3$track.name,
      "<br>",
      "Danceability: ", cm3$danceability,
      "<br>",
      "Tempo: ", cm3$tempo,
      "<br>",
      "Loudness: ", cm3$loudness
    )
  )

fig2
