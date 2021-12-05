# Loads the necessary libraries to perform the data visualizations and manipulations
library(tidyverse)
library(readr)
library(splitstackshape)
library(here)

# Reads a the board game .csv file and saves it into a data subfolder
data <- read_csv(here("Data/data_output.csv"))

# Filters the dataframe to only include games published in 1950 or later and with at least 25 ratings
modern_and_popular <- data %>%
  filter(yearpublished >= 1950 &
           users_rated >= 25)

# Stores and plots a histogram depicting the number of games per year
num_games <- ggplot(modern_and_popular) +
  geom_histogram(aes(yearpublished),
                 binwidth = 1,
                 color = "black",
                 fill = "lightblue") +
  theme_bw() +
  labs(title = "Number of Games From Each Year",
       x = "Year Published",
       y = "") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
num_games

# Filters the dataframe to only include games published in 1950 or later and with at least 25 ratings
# cSplit tidies the "category" column by denoting a separate row for each category listed (using commas as the delimiter)
# Renames the "Expansion for Base-game" category to "Expansion"
modern_and_popular_category_long <- data %>%
  filter(yearpublished >= 1950 &
           users_rated >= 25)  %>%
  cSplit(splitCols = "category",
         direction = "long") %>%
  mutate(category =
           ifelse(category == "Expansion for Base-game",
                  "Expansion", as.character(category)))

# Stores a tibble depicting the 6 most commonly listed categories
cat_count <- modern_and_popular_category_long %>%
  count(category, sort = TRUE) %>%
  head(6)
# Filters the dataframe to only include games tagged under one of the six most commonly listed categories
six_category_limit <- modern_and_popular_category_long %>%
  filter(category %in% cat_count$category)

# Stores and plots six boxplots (one for each category) on one axis, depicting the distribution of playing times for games in each category
playing_time_by_category <- ggplot(six_category_limit) +
  geom_boxplot(aes(category, playingtime)) +
  coord_cartesian(ylim = c(0, 400)) +
  labs(title = "Playing Time for Different Board Games by Category",
       y = "",
       x = "") +
  theme_bw(base_size = 10) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))
playing_time_by_category

# Stores and plots six dotplots (one for each category) on separate axes, depicting the distribution in ratings over the years for each category. Includes a trendline for each dotplot
year_vs_avg_ratings <- ggplot(six_category_limit) +
  geom_point(aes(yearpublished, average_rating,
                 color = category)) +
  geom_smooth(aes(x = yearpublished,
                  y = average_rating,
                  group = category),
              se = FALSE, size = 1,
              color = "black",
              method = "loess") +
  facet_wrap(~ category) +
  theme_bw() +
  labs(title = "Average Rating Over Time by Category",
       x = "Year", y = "") +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold"))
year_vs_avg_ratings

# Stores and plots a line plot comparing the ratings trendlines for each category (on one axis)
ratings_changes_by_cat <- ggplot(six_category_limit) +
  geom_smooth(aes(x = yearpublished, y = average_rating,
                  group = category, color = category),
              se = FALSE, size = 1,
              method = "loess") +
  theme_bw(base_size = 12) +
  labs(title = "Ratings Changes by Category",
       x = "Year Published", y = "Average Rating",
       color = guide_legend(title = "Game Category")) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_text(hjust = 0.5))
ratings_changes_by_cat

# Filters the dataframe to only include games published in 1970, 1985, 2000, or 2015
selected_years <- six_category_limit %>%
  filter(yearpublished %in% c(1970, 1985, 2000, 2015))
# Stores and plots four bar graphs (one for each selected year) depicting the number of games published in that year for each category
num_per_cat <- ggplot(selected_years) +
  geom_bar(aes(category,
               fill = category)) +
  facet_wrap(~ yearpublished, scales = "free_y") +
  theme_bw(base_size = 12) +
  labs(title = "Number of Games by Category Throughout the Years",
       x = "", y = "") +
  theme(plot.title = element_text(hjust = 0.5, face ="bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

num_per_cat

# Filters for games with an explicit estimated playing time
games_with_play_time <- modern_and_popular %>%
  filter(playingtime > 0)
# Stores and plots a dotplot depicting the correlation between playing time and average rating
playing_time_vs_avg_rating <- ggplot(games_with_play_time) +
  geom_point(aes(playingtime, average_rating)) + 
  labs(title = "Ratings for Games Sorted by Playing Time (Up to 250)",
       x = "Playing Time", y = "Average Rating") +
  coord_cartesian(xlim = c(0, 250)) +
  theme_bw(base_size = 10) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
playing_time_vs_avg_rating

# Filters for games with an explicit minimum age requirement
games_with_min_age <- modern_and_popular %>%
  filter(minage > 0)
# Stores and plots a dotplot depicting the correlation between minimum age and complexity 
min_age_vs_complex <- ggplot(games_with_min_age) +
  geom_point(aes(minage, average_complexity)) +
  coord_cartesian(xlim = c(0, 25)) +
  theme_bw(base_size = 10) +
  labs(title = "Interaction of Complexity and Minimum Age",
       x = "Minimum Age", y = "Average Complexity") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  geom_smooth(aes(minage, average_complexity),
              se = FALSE, size = 1.5,
              color = "red",
              method = "loess")
min_age_vs_complex

# Filters for games with minimum player requirements from 1-6 players
num_player_filter <- modern_and_popular %>%
  filter((minplayers > 0) & (minplayers <= 6)) %>%
  mutate(minplayers = as.character(minplayers))
# Stores and plots 6 box plots (one for each player threshold) on one axis, depicting the distibution of playing times for games under each minimum player threshold
num_players_and_time <- ggplot(num_player_filter) +
  geom_boxplot(aes(minplayers, playingtime)) +
  coord_cartesian(ylim = c(0, 200)) +
  theme_bw(base_size = 10) +
  labs(title = "Distribution of Playing Times for Games with Player Limits",
       x = "Minimum Number of Players", y = "Playing Time") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
num_players_and_time
