install.packages("tidyverse")
library(tidyverse)
nhl <- read.csv("nhl_pit_sj_game6.csv")

nhl$cumulative_shots <- cumsum(nhl$shot_attempt == 1)

nhl_team_stats <- nhl %>% 
  filter(event %in% c("blocked_shot", "faceoff", "giveaway", "hit", "shot")) %>% 
  select(event, team) %>% 
  ggplot(aes(x = event, fill = team)) + geom_bar(position = "dodge") + 
  scale_fill_manual(values = c("yellow", "cyan4")) + 
  labs(title = " 2016-2017 NHL Stanley Cup Finals Game 6 Team Stats",
       x = "Game Statistics",
       y = "Counts",
       fill = "Team") + 
  theme_classic() + 
  theme(legend.position = "bottom",
        plot.title = element_text(size = 17),
        axis.title = element_text(size = 15)) +
  scale_x_discrete(labels = c("Blocked Shots", "Faceoffs", "Giveaways", "Hits","Shots on Goal"))
nhl_team_stats


