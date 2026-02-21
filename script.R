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


set.seed(3454)
simdata <- "simulated_oxygen_duplo.csv" |>
  read_csv() |>
  mutate(
    Oxygen_mg_L=Oxygen_mg_L+runif(n(),-.5,.5),
    add=runif(n(),-2,2),
    add=add-mean(add),
    Oxygen_mg_L2=Oxygen_mg_L+add,
    ) 
simdata |>
  ggplot(aes(x=Month,y=Oxygen_mg_L)) +
  geom_point()
simdata |>
  ggplot(aes(x=Month,y=Oxygen_mg_L2)) +
  geom_point()

simdata |>
  mutate(code=sprintf("%i/%.4f",Month,Oxygen_mg_L2)) |>
  summarize(code=str_c(code,collapse=",")) |>
  pull(code) |>
  cat()
simdata$Oxygen_mg_L2 |> mean()

