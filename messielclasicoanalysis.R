install.packages("tidyverse")
install.packages("devtools")
devtools::install_github("statsbomb/SDMTools")
devtools::install_github("statsbomb/StatsBombR")
devtools::install_github("FCrSTATS/SBpitch")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("ggsoccer")
install.packages("ggrepel")


library(tidyverse)
library(StatsBombR)
library(ggplot2)
library(dplyr)
library(ggsoccer)
library(ggrepel)
library(SBpitch)


pbp <- FreeCompetitions() %>%
  filter(competition_id == 11)

matches <- FreeMatches(pbp) %>%
  filter((home_team.home_team_id == 217 & away_team.away_team_id == 220) | (home_team.home_team_id == 220 & away_team.away_team_id == 217))

match_data <- free_allevents(MatchesDF = matches) 
match_data = allclean(match_data) 

messi_data <- match_data %>%
  filter(player.id == 5503) %>%
  filter(type.name == "Shot" & (shot.type.name != "Penalty" | is.na(shot.type.name))) %>%
  mutate(goal = ifelse(shot.outcome.name=="Goal", 1, 0)) %>%
  summarize(shots = n(), locationx = location.x, locationy = location.y, xG = shot.statsbomb_xg, outcome = goal)

ggplot()+
  annotate_pitch(dimensions = pitch_statsbomb)+
  theme_pitch()+
  coord_flip(xlim = c(60,120),
             ylim = c(0,80)) +
  geom_point(data = messi_data, 
             aes(x = locationx, 
                 y = locationy, 
                 color = as.factor(outcome), 
                 size = xG)) +
  labs(title = "Lionel Messi Career Non-Penalty Shot Map", subtitle = "El Clásico in La Liga", caption = "Amrit Vignesh") +
  theme(plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), plot.subtitle = element_text(size = 10, hjust = 0.5)) +
  scale_colour_manual(values = c("#FF0000", "#00FF00"), 
                      name = "Shot Outcome", 
                      labels = c("No Goal", "Goal")) + 
  geom_text(data = messi_data,
            aes(x=80,
                y=65,
                label = paste("Non-Penalty Shots: ", shots))) +  
  geom_text(data = messi_data,
            aes(x=75,
                y=65,
                label = paste("Net npxG: ", round(sum(xG), digits = 2)))) +
  geom_text(data = messi_data,
            aes(x=70,
                y=65,
                label = paste("Non-Penalty Goals: ", sum(outcome))))

  


