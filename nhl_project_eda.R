
library(tidyverse)

nhl <- readr::read_csv("nhl_pit_sj_game6.csv")
str(nhl)

nhl_1 <- tibble()
nhl_1 <- nhl %>%
  filter(period == 1) %>%
  mutate(new_time = period_time)
nhl_2 <- nhl %>%
  filter(period == 2) %>%
  mutate(new_time = period_time + 20)
nhl_3 <- nhl %>%
  filter(period == 3) %>%
  mutate(new_time = period_time + 40)

nhl_all <- rbind(nhl_1, nhl_2, nhl_3)

nhl$period_time <- as.numeric(nhl$period_time)
nhl$period_time <- nhl$period_time / 60
nhl$period_time

nhl$cumulative_shots <- cumsum(nhl$shot_attempt)

nhl_all %>%
  ggplot(aes(x = new_time, y = cumulative_shots)) +
  geom_line() +
  geom_rect(
    aes(xmin = 7.8333, xmax = 9.8333, ymin = 0, ymax = Inf), alpha = 0.005, fill = "cyan") +
  geom_rect(
    aes(xmin = 45.4333, xmax = 47.4333, ymin = 0, ymax = Inf), alpha = 0.005, fill = "yellow") +
  geom_rect(
    aes(xmin = 51.0333, xmax = 53.0333, ymin = 0, ymax = Inf), alpha = 0.005, fill = "cyan") +
  geom_rect(
    aes(xmin = 59.8333, xmax = 60, ymin = 0, ymax = Inf), alpha = 0.005, fill = "yellow") +
  labs(title = "Shot Increase over the Duration of Power Plays",
       x = "Time",
       y = "Total Shots") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggsave("EDAline.png")

  

  

nhl_team_stats <- nhl %>% 
  filter(event %in% c("blocked_shot", "faceoff", "giveaway", "hit", "shot")) %>% 
  select(event, team) %>% 
  ggplot(aes(x = event, fill = team)) + geom_bar(position = "dodge") + 
  scale_fill_manual(values = c("yellow", "cyan4")) + 
  labs(title = "Do These Team Statistics Impact Winning?",
       x = "Game Statistics",
       y = "Counts",
       fill = "Team") + 
  theme_classic() + 
  theme(legend.position = "bottom",
        plot.title = element_text(size = 17),
        axis.title = element_text(size = 15)) +
  scale_x_discrete(labels = c("Blocked Shots", "Faceoffs", "Giveaways", "Hits","Shots on Goal"))
nhl_team_stats
ggsave("Alex_Team_Stats.png", width = 8, height = 6)
