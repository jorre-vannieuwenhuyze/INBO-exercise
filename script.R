library(tidyverse)

data <- "case1.csv" |>
  read_csv2()


data |>
  ggplot(aes(x=maand,y=sd)) +
  geom_line() +
  facet_wrap(~AquaComponent,scales="free_y")

data |>
  ggplot(aes(x=maand,y=max)) +
  geom_line() +
  facet_wrap(~AquaComponent,scales="free_y")


tmp <- data |>
  pivot_wider(id_cols='maand',names_from='AquaComponent',values_from='mean') |>
  GGally::ggpairs()
