library(tidyverse)

data <- "case1.csv" |>
  read_csv2()


data |>
  ggplot(aes(x=maand,y=sd)) +
  geom_line() +
  facet_wrap(~AquaComponent,scales="free_y")

data |>
  ggplot(aes(x=maand,y=mean)) +
  geom_line() +
  facet_wrap(~AquaComponent,scales="free_y")


data |>
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
simdata_agg <- simdata |>
  summarise(
    Oxygen_mg_L=mean(Oxygen_mg_L),
    Oxygen_mg_L2=mean(Oxygen_mg_L2),
    .by=Month
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
simdata_agg |>
  mutate(code=sprintf("(%i,%.4f)",Month,Oxygen_mg_L2)) |>
  summarize(code=str_c(code,collapse=" -- ")) |>
  pull(code) |>
  cat()



set.seed(54613)
ACdata <- data |>
  filter(AquaComponent=="O2verz") |>
  mutate(
    add=runif(n(),-50,50),
    add=add-mean(add),
    mean2=mean+add,
    )
ggplot(ACdata,aes(x=maand)) +
  geom_line(aes(y=mean)) +
  geom_line(aes(y=mean2),color="red") +
  ylim(0,180)
mean(ACdata$mean)
ACdata |>
  mutate(code=sprintf("(%i,%.4f) circle(1pt)",maand,mean2)) |>
  summarize(code=str_c(code,collapse=" -- ")) |>
  pull(code) |>
  cat()


corrdata <- data |>
  filter(AquaComponent %in% c("O2verz","pH_veld")) |>
  pivot_wider(id_cols = maand, names_from = AquaComponent, values_from = mean)
corrdata |>
  mutate(code=sprintf("%.4f/%.4f",O2verz,pH_veld)) |>
  summarize(code=str_c(code,collapse=",")) |>
  pull(code) |>
  cat()

