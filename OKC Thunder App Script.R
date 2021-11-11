library(tidyverse)
data <- read_csv("shots_data.csv")
data <- data%>%mutate(
  Corner3 = if_else(abs(x) > 22 & abs(y) <= 7.8, 1,0), 
  Distance = sqrt((x ^ 2) + (y ^ 2)),
  ShotType = if_else((abs(x) > 22) | (Distance >= 23.75), 3, 2),
  EFG = (fgmade * ShotType / 2),
  Shot_Location = case_when(
    ShotType == 2 ~ "2PT",
    ShotType == 3 & Corner3 == 0 ~ "NC3",
    ShotType == 3 & Corner3 == 1 ~ "C3"
  )
)

# EFG
data%>%
  group_by(team, Shot_Location)%>%
  summarize(EFG = mean(EFG))


# Shot Distribution
data%>%
  group_by(team, Shot_Location)%>%
  count()%>%
  group_by(team)%>%
  mutate(Perc = n/sum(n))



            