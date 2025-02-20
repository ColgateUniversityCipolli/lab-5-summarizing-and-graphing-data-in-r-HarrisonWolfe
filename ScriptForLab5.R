library(tidyverse)

#Step 1
allentown.data = read_csv("data/essentia.data.allentown.csv")
full.data = read_csv("data/essentia.data.csv")

loudness.artist.data = full.data |>
  group_by(artist) |>
  summarize(#Finds the stats
    IQR = quantile(overall_loudness, .75) - quantile(overall_loudness, 0.25),
    Min = min(overall_loudness),
    LF = quantile(overall_loudness, 0.25) - 1.5*IQR,
    UF = quantile(overall_loudness, .75) + 1.5*IQR,
    Max = max(overall_loudness)) |> #Puts all the stats in columns after to make it easier to look at
  mutate(out.of.range = ifelse(allentown.data$overall_loudness > Max | allentown.data$overall_loudness < Min, "True", "False")) |>
  mutate(unusual = ifelse(allentown.data$overall_loudness > UF | allentown.data$overall_loudness < LF, "True", "False")) |>
  mutate(description = case_when(out.of.range == "True" ~ "Out of Range",
                                 unusual == "True" ~ "Outlying",
                                 TRUE ~ "Within Range"))

compare = function(feature){#This function takes all the data and puts it into a data frame and returns
  artist.data = full.data |>
    group_by(artist) |>
    summarize(
      IQR = quantile(get(feature), .75, na.rm=T) - quantile(get(feature), 0.25, na.rm=T),
      Min = min(get(feature), na.rm=T),
      LF = quantile(get(feature), 0.25, na.rm=T) - 1.5*IQR,
      UF = quantile(get(feature), .75, na.rm=T) + 1.5*IQR,
      Max = max(get(feature))) |>
    mutate(out.of.range = ifelse(allentown.data[[feature]] > Max | allentown.data[[feature]] < Min, "True", "False")) |>
    mutate(unusual = ifelse(allentown.data[[feature]] > UF | allentown.data[[feature]] < LF, "True", "False")) |>
    mutate(!!feature := case_when(out.of.range == "True" ~ "Out of Range",
                                   unusual == "True" ~ "Outlying",
                                   TRUE ~ "Within Range")) |>
    select(artist, !!sym(feature))
  return(artist.data)
  
}

#Step 2
#I need a for loop to show 

features = full.data |>
  select(-c("artist", "album","track","chords_scale","chords_key","key","mode")) |>
  colnames()


comparing.data = full.data |>
  group_by(artist) |>
  summarize() |>
  select(artist)


for(i in 1:length(features)){
  results = compare(features[i])
  comparing.data = comparing.data |>
    right_join(results, by = "artist", suffix = c("", ""))
}
  


  

compare(features[1])


