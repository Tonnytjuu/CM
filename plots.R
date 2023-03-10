library(tidyverse)
library(spotifyr)
library(ggplot2)
library(readr)
library(lubridate)
library(plotly)
library(compmus)

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

awaken<-
  get_tidy_audio_analysis("3jevgr3fYdv9wYO3IDJq2a") |>
  select(segments) |>
  unnest(segments) |>
  select(start, duration, pitches)

awaken |>
  mutate(pitches = map(pitches, compmus_normalise, "euclidean")) |>
  compmus_gather_chroma() |> 
  ggplot(
    aes(
      x = start + duration / 2,
      width = duration,
      y = pitch_class,
      fill = value
    )
  ) +
  geom_tile() +
  labs(x = "Time (s)", y = NULL, fill = "Magnitude") +
  theme_minimal() +
  scale_fill_viridis_c()

awaken_timbre <-
  get_tidy_audio_analysis("3jevgr3fYdv9wYO3IDJq2a") |> # Change URI.
  compmus_align(bars, segments) |>                     # Change `bars`
  select(bars) |>                                      #   in all three
  unnest(bars) |>                                      #   of these lines.
  mutate(
    pitches =
      map(segments,
          compmus_summarise, pitches,
          method = "rms", norm = "manhattan"              # Change summary & norm.
      )
  ) |>
  mutate(
    timbre =
      map(segments,
          compmus_summarise, timbre,
          method = "rms", norm = "manhattan"              # Change summary & norm.
      )
  )

awaken_timbre |>
  compmus_gather_timbre() |>
  ggplot(
    aes(
      x = start + duration / 2,
      width = duration,
      y = basis,
      fill = value
    )
  ) +
  geom_tile() +
  labs(x = "Time (s)", y = NULL, fill = "Magnitude") +
  scale_fill_viridis_c() +                              
  theme_classic()

q_timbre <-
  get_tidy_audio_analysis("5ouJ45sdbOh6QdzkVodn0Z") |> # Change URI.
  compmus_align(bars, segments) |>                     # Change `bars`
  select(bars) |>                                      #   in all three
  unnest(bars) |>                                      #   of these lines.
  mutate(
    pitches =
      map(segments,
          compmus_summarise, pitches,
          method = "rms", norm = "euclidean"              # Change summary & norm.
      )
  ) |>
  mutate(
    timbre =
      map(segments,
          compmus_summarise, timbre,
          method = "rms", norm = "euclidean"              # Change summary & norm.
      )
  )

q_timbre |>
  compmus_gather_timbre() |>
  ggplot(
    aes(
      x = start + duration / 2,
      width = duration,
      y = basis,
      fill = value
    )
  ) +
  geom_tile() +
  labs(x = "Time (s)", y = NULL, fill = "Magnitude") +
  scale_fill_viridis_c() +                              
  theme_classic()

circshift <- function(v, n) {
  if (n == 0) v else c(tail(v, n), head(v, -n))
}

#      C     C#    D     Eb    E     F     F#    G     Ab    A     Bb    B
major_chord <-
  c(   1,    0,    0,    0,    1,    0,    0,    1,    0,    0,    0,    0)
minor_chord <-
  c(   1,    0,    0,    1,    0,    0,    0,    1,    0,    0,    0,    0)
seventh_chord <-
  c(   1,    0,    0,    0,    1,    0,    0,    1,    0,    0,    1,    0)

major_key <-
  c(6.35, 2.23, 3.48, 2.33, 4.38, 4.09, 2.52, 5.19, 2.39, 3.66, 2.29, 2.88)
minor_key <-
  c(6.33, 2.68, 3.52, 5.38, 2.60, 3.53, 2.54, 4.75, 3.98, 2.69, 3.34, 3.17)

chord_templates <-
  tribble(
    ~name, ~template,
    "Gb:7", circshift(seventh_chord, 6),
    "Gb:maj", circshift(major_chord, 6),
    "Bb:min", circshift(minor_chord, 10),
    "Db:maj", circshift(major_chord, 1),
    "F:min", circshift(minor_chord, 5),
    "Ab:7", circshift(seventh_chord, 8),
    "Ab:maj", circshift(major_chord, 8),
    "C:min", circshift(minor_chord, 0),
    "Eb:7", circshift(seventh_chord, 3),
    "Eb:maj", circshift(major_chord, 3),
    "G:min", circshift(minor_chord, 7),
    "Bb:7", circshift(seventh_chord, 10),
    "Bb:maj", circshift(major_chord, 10),
    "D:min", circshift(minor_chord, 2),
    "F:7", circshift(seventh_chord, 5),
    "F:maj", circshift(major_chord, 5),
    "A:min", circshift(minor_chord, 9),
    "C:7", circshift(seventh_chord, 0),
    "C:maj", circshift(major_chord, 0),
    "E:min", circshift(minor_chord, 4),
    "G:7", circshift(seventh_chord, 7),
    "G:maj", circshift(major_chord, 7),
    "B:min", circshift(minor_chord, 11),
    "D:7", circshift(seventh_chord, 2),
    "D:maj", circshift(major_chord, 2),
    "F#:min", circshift(minor_chord, 6),
    "A:7", circshift(seventh_chord, 9),
    "A:maj", circshift(major_chord, 9),
    "C#:min", circshift(minor_chord, 1),
    "E:7", circshift(seventh_chord, 4),
    "E:maj", circshift(major_chord, 4),
    "G#:min", circshift(minor_chord, 8),
    "B:7", circshift(seventh_chord, 11),
    "B:maj", circshift(major_chord, 11),
    "D#:min", circshift(minor_chord, 3)
  )

key_templates <-
  tribble(
    ~name, ~template,
    "Gb:maj", circshift(major_key, 6),
    "Bb:min", circshift(minor_key, 10),
    "Db:maj", circshift(major_key, 1),
    "F:min", circshift(minor_key, 5),
    "Ab:maj", circshift(major_key, 8),
    "C:min", circshift(minor_key, 0),
    "Eb:maj", circshift(major_key, 3),
    "G:min", circshift(minor_key, 7),
    "Bb:maj", circshift(major_key, 10),
    "D:min", circshift(minor_key, 2),
    "F:maj", circshift(major_key, 5),
    "A:min", circshift(minor_key, 9),
    "C:maj", circshift(major_key, 0),
    "E:min", circshift(minor_key, 4),
    "G:maj", circshift(major_key, 7),
    "B:min", circshift(minor_key, 11),
    "D:maj", circshift(major_key, 2),
    "F#:min", circshift(minor_key, 6),
    "A:maj", circshift(major_key, 9),
    "C#:min", circshift(minor_key, 1),
    "E:maj", circshift(major_key, 4),
    "G#:min", circshift(minor_key, 8),
    "B:maj", circshift(major_key, 11),
    "D#:min", circshift(minor_key, 3)
  )

bb <-
  get_tidy_audio_analysis("6toQdWWc4noiOk3Eo5mVDS") |>
  compmus_align(bars, segments) |>
  select(bars) |>
  unnest(bars) |>
  mutate(
    pitches =
      map(segments,
          compmus_summarise, pitches,
          method = "mean", norm = "manhattan"
      )
  )

bb |> 
  compmus_match_pitch_template(
    key_templates,         # Change to chord_templates if descired
    method = "manhattan",  # Try different distance metrics
    norm = "euclidean"     # Try different norms
  ) |>
  ggplot(
    aes(x = start + duration / 2, width = duration, y = name, fill = d)
  ) +
  geom_tile() +
  scale_fill_viridis_c(guide = "none") +
  theme_minimal() +
  labs(x = "Time (s)", y = "")

p <- plot_ly(data = cm1, 
             x = cm1$key_mode,
             hoverinfo='text',
             name='rap',
             text= paste(cm1$key_mode),
             type='histogram')

p <- p %>% add_trace(data = cm2, 
                x = cm2$key_mode,
                hoverinfo='text',
                name="metal",
                text= paste(cm2$key_mode),
                type='histogram')

p <- p %>% add_trace(data = cm3, 
                     x = cm3$key_mode,
                     hoverinfo='text',
                     name="house/rest",
                     text= paste(cm3$key_mode),
                     type='histogram')

#p %>% layout(xaxis= list(showticklabels = FALSE))
p

cm_sorted <- cm[order(cm$tempo, decreasing=TRUE),] 
fig <- plot_ly(
  
  type = 'table',
  
  header = list(
    
    values = c("Song","Key", "Tempo"),
    
    align = c("center", "center", "center"),
    
    line = list(width = 1, color = 'black'),
    
    fill = list(color = c("grey", "grey", "grey")),
    
    font = list(family = "Arial", size = 14, color = "white")
    
  ),
  
  cells = list(
    
    values = rbind(cm_sorted$track.name, cm_sorted$key_mode, cm_sorted$tempo),
    
    align = c("center", "center", "center"),
    
    line = list(color = "black", width = 1),
    
    font = list(family = "Arial", size = 12, color = c("black"))
    
  ))


fig
