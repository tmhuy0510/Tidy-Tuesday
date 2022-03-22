library(tidyverse)
library(elo)


ufc_raw <- read_csv("./data/ufc.csv")


View(ufc_raw)
dim(ufc_raw)
# Comment: 6012 rows and 144 columns


ufc_raw %>% colnames()
# B and R stand for blue and red respectively
ufc_raw %>% colnames() %>% str_count("^B_") %>% sum()
# 69 features for B
ufc_raw %>% colnames() %>% str_count("^B_") %>% sum()
# 69 features for R
ufc_raw %>% colnames() %>% str_count("^[^RB]|^R[^_]|^B[^_]") %>% sum()
# 6 shared features for R: 
# "Referee", "date", "location", "Winner", "title_bout" and "weight_class"


ufc_raw %>% count(Winner)
# As for non-title bouts, newer fighters are likely to be assigned the blue corner
# As for title bouts, challengers are likely to be assigned the blue corner


ufc_raw <- ufc_raw %>%
  mutate(match_id = row_number())
# Add a primary key to the original data set


# Split the original data set into 2 data sets each of which represents
# observations for fighters at the blue corner or red corner


# Fighters at the red corner
r_df <- ufc_raw %>% select(-starts_with("B_")) # Notice the minus sign
# Fighters at the blue corner
b_df <- ufc_raw %>% select(-starts_with("R_")) # Notice the minus sign
# The resulting data frames should have 76 columns


# Playing around with elo packages (Part 1)
# Elo scores of team A and team B before they play
elo.A <- c(1500, 1500)
elo.B <- c(1500, 1600)
# Calculate the probability team A beats team B
elo.prob(elo.A, elo.B)
# Result in reality when team A and team B play
wins.A <- c(1, 0)
# We can calculate wins.A from raw scores
points.A <- c(4, 1)
points.B <- c(3, 3)
wins.A <- score(points.A, points.B)
# Calculate the score update after the two teams play
elo.update(wins.A, elo.A, elo.B, k = 20)
# Calculate the new elo scores after the update
elo.calc(wins.A, elo.A, elo.B, k = 20)


# Playing around with elo packages (Part 2)
# Input data and formula
dat <- data.frame(elo.A = c(1500, 1500), 
                  elo.B = c(1500, 1600),
                  wins.A = c(1, 0),
                  k = 20)
form <- wins.A ~ elo.A + elo.B + k(k)
# Calculate the probability team A beats team B
elo.prob(form, data = dat)
# Calculate the score update after the two teams play
elo.update(form, data = dat)
# Calculate the new elo scores after the update
elo.calc(form, data = dat)


# Playing around with elo packages (Part 3)
# Input data
data(tournament)
str(tournament)
# Determine if home team beats visitor team
tournament$wins.A <- tournament$points.Home > tournament$points.Visitor
# Calculate the running elo scores after a series of matches
elo_tour <- elo.run(wins.A ~ team.Home + team.Visitor, data = tournament, k = 20)
# Directly calculate the running elo scores after a series of matches
elo_tour <- elo.run(score(points.Home, points.Visitor) ~ team.Home + team.Visitor, 
                    data = tournament, k = 20)
# Initial elo scores and final elo scores
elo_tour$initial.elos
final.elos(elo_tour)
# How elo scores of all teams change during the season
as_tibble(elo_tour)
# Rank of each team at the end of the season
rank.teams(elo_tour)


# Change values of `Winner` column of `r_df` to 0, 0.5 or 1
r_df <- r_df %>% mutate(Winner = case_when(
  Winner == "Red" ~ 1,
  Winner == "Blue" ~ 0,
  TRUE ~ 0.5
))
# Change values of `Winner` column of `b_df` to 0, 0.5 or 1
b_df <- b_df %>% mutate(Winner = case_when(
  Winner == "Red" ~ 0,
  Winner == "Blue" ~ 1,
  TRUE ~ 0.5
))


# Remove all prefixes of "R_" and "B_" from column names of 2 data sets
r_df <- r_df %>% 
  rename_all(.funs = function(x) str_replace(x, "R_", ""))
b_df <- b_df %>% 
  rename_all(.funs = function(x) str_replace(x, "B_", ""))
# Check if these 2 data sets have all the same column names in pair
all(colnames(r_df) == colnames(b_df))
# All the column names of 2 data sets are the same in pair


# Combine 2 data sets in a row-wise manner to get a tidy data set
tidy_df <- bind_rows(r_df, b_df)
# Check the tidy data set's dimension
dim(tidy_df)
# There should be 6012 x 2 = 12024 rows and 76 columns
# We can now save the tidy data set as an R object for future use
saveRDS(tidy_df, "./r_objects/tidy_ufc.rds")


# Prepare a data frame to calculate elo scores
elo_df <- ufc_raw %>% select(match_id, 1:8) %>%
  rename(fighter = "R_fighter",
         opponent = "B_fighter") %>% 
  mutate(Winner = case_when(
    Winner == "Red" ~ 1,
    Winner == "Blue" ~ 0,
    TRUE ~ 0.5
  ))
saveRDS(elo_df, "./r_objects/elo_df.rds")
# There should be 6012 rows and 9 columns 


# Calculate all info related to elo scores
elo_fighter <- elo.run(Winner ~ fighter + opponent,
                       data = elo_df,
                       k = 20)


# Get the fighter rankings on the basis of elo scores at the end of the
# time when data were collected
elo_fighter_rank <- elo_fighter %>% 
  rank.teams() %>% 
  tibble(names = names(.),
         ranking = .) %>% 
  arrange(ranking)
# Jon Jones is ranked as No.1


# Get the info related to elo scores of each fighter over time
elo_info_by_date_by_fighter <- elo_fighter %>% 
  as_tibble() %>% 
  mutate(date = elo_df$date,
         match_id = elo_df$match_id) %>%
  select(contains(".A"), date) %>%
  rename_all(.funs = function(x) str_replace(x, ".A", "")) 


# Get the info related to elo scores of No.1 fighter over time then make a plot
elo_info_by_date_by_fighter %>%
  semi_join(elo_fighter_rank %>% filter(ranking == 1), 
            by = c("team" = "names")) %>%
  pivot_longer(c(p, update, elo), 
               names_to = "elo_info", 
               values_to = "value") %>% 
  ggplot(aes(date, value)) + 
  geom_line(show.legend = FALSE) +
  geom_point(show.legend = FALSE) +
  facet_wrap(vars(elo_info), scales = "free_y", nrow = 3) +
  labs(x = NULL, y = NULL, 
       title = "Fighting Performace of Jon Jones",
       subtitle = "He had been unbeatable over this period of time.") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")
# Jon Jones had been unbeatable over this period of time
# There was only one time Jon Jones faced another fighter whose elo scores were higher than his
# Generally, elo scores of Jon Jones decreases over this period of time
# There is a fatal mistake when not using the tidy data set in this case


# Get the number of distinct fighters each year and make a plot
elo_df %>% 
  mutate(year = str_extract(date, "\\d{4}") %>% as.numeric()) %>% 
  group_by(year) %>% 
  summarise(num_fighters = n_distinct(fighter)) %>% 
  ggplot(aes(year, num_fighters)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = seq(1994, 2020, by = 2)) +
  labs(x = "Year", y = "Number of fighters",
       title = "There has been an increasing trend in the number of UFC fighters each year",
       subtitle = "2017 saw a dip in the number of UFC fighters")


# Examine how K affects the top 10 overall fighters
k_sensitivity <- function(k){
  results <- elo.run(Winner ~ fighter + opponent, 
                     k = k,
                     data = elo_df)
  
  results %>% 
    rank.teams() %>% 
    tibble(names = names(.),
           ranking = .) %>% 
    filter(ranking <= 10) %>% 
    arrange(ranking)
}


# Create each top 10 list corresponding to each k
k_rankings <- tibble(k = seq(1, 101, by = 10)) %>% 
  mutate(top_10_rankings = map(k, k_sensitivity)) %>% 
  unnest(top_10_rankings)


# Plot a heatmap using k values, fighters and their rankings 
k_rankings %>% 
  complete(k, names) %>% 
  group_by(names) %>% 
  mutate(avg_ranking = mean(ranking, na.rm = TRUE)) %>% 
  ggplot(aes(k, reorder(names, -avg_ranking))) +
  geom_tile(aes(fill = ranking)) +
  scale_x_continuous(breaks = seq(1, 101, by = 10), 
                     minor_breaks = NULL) +
  labs(x = "k factor", y = NULL, fill = "Rank:") +
  theme(panel.background = element_rect(fill = "white"),
        axis.ticks.y = element_blank())
# It is reasonable to pick k from 61 to 81. In this case, use k = 81


# Create a helper function which give top 10 fighters for each weight class
top_10_elo_by_weight_class <- function(df) {
  elo.run(Winner ~ fighter + opponent,
          k = 81,
          data = df) %>% 
    rank.teams() %>% 
    tibble(names = names(.),
           ranking = .) %>% 
    arrange(ranking) %>% 
    slice(1:10)
}


# Get top 10 fighters for each weight class
top_10_fighters_by_weight_class <- elo_df %>% 
  group_by(weight_class) %>% 
  nest() %>% 
  mutate(data = map(data, top_10_elo_by_weight_class)) %>% 
  unnest(cols = c(data))
top_10_fighters_by_weight_class


# Create the history of elo and its parameters for each match 
elo_history <- elo_df %>% 
  elo.run(Winner ~ fighter + opponent, data = ., k = 81) %>% 
  as_tibble() %>% 
  rename("fighter" = 1, "opponent" = 2, 
         "fighter_prob" = 3, "fighter_wins" = 4, 
         "fighter_elo_change" = 5, "opponent_elo_change" = 6, 
         "fighter_elo" = 7, "opponent_elo" = 8) %>% 
  mutate(match_id = elo_df$match_id,
         weight_class = elo_df$weight_class,
         date = elo_df$date) %>% 
  arrange(weight_class, match_id)


# Create the history of elo and its parameters for each fighter in a match
fighter_elo_history <- elo_history %>% 
  select(fighter, match_id, date, weight_class, 
         fighter_prob, fighter_wins, fighter_elo_change, fighter_elo)
opponent_elo_history <- elo_history %>% 
  select(opponent, match_id, date, weight_class, 
         fighter_prob, fighter_wins, opponent_elo_change, opponent_elo) %>% 
  mutate(fighter_prob = 1 - fighter_prob,
         fighter_wins = 1 - fighter_wins) %>% 
  rename_all(.funs = function(x) str_replace(x, "opponent", "fighter"))


# Create a tidy data set on the history of elo and its parameters for each fighter
tidy_elo_history <- bind_rows(fighter_elo_history, opponent_elo_history)
# Save this as an R object for future use
saveRDS(tidy_elo_history, "./r_objects/tidy_elo_history.rds")


# Create plots showing the changes of elo and its parameters for a fighter
tidy_elo_history %>% 
  filter(fighter == "Jon Jones") %>%
  pivot_longer(c(fighter_elo, fighter_prob, fighter_elo_change),
               names_to = "elo_para",
               values_to = "value") %>% 
  ggplot(aes(date, value)) +
  geom_line() +
  geom_point() +
  facet_wrap(vars(elo_para), nrow = 3, scales = "free_y") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(x = NULL, y = NULL, 
       title = "Fighting Performace of Jon Jones")
# When using the tidy data set, we see Jon Jones got beaten 1 time
# The reason for this discovery is that he was not always assigned the blue corner


# We can predict the probability that a fighter beats his/her opponent 
predict(elo_fighter, 
        newdata = tibble(fighter = "Khabib Nurmagomedov", 
                         opponent = "Conor McGregor")
        )


# We can see what matches have biggest elo change for fighters
elo_history %>% 
  group_by(fighter_elo_change > 0) %>% 
  slice_max(abs(fighter_elo_change), n = 5) %>% 
  ggplot(aes(x = fighter_elo_change, 
             y = reorder(as_factor(match_id), fighter_elo_change), 
             fill = fighter_elo_change > 0)) +
  geom_col(show.legend = FALSE) +
  scale_x_continuous(breaks = seq(-70, 80, by = 10), minor_breaks = NULL) +
  labs(x = "Elo change of the fighter assigned the blue corner",
       y = "Match ID",
       title = "Biggest increases and decreases in elo")


# Get info for matches have biggest elo change
# Decrease in elo of blue corner fighter 
elo_history %>% select(match_id, fighter, opponent, fighter_elo, opponent_elo) %>% 
  filter(match_id %in% c(1032, 3616, 1602, 2177, 3566))
# Increase in elo of blue corner fighter
elo_history %>% select(match_id, fighter, opponent, fighter_elo, opponent_elo) %>% 
  filter(match_id %in% c(3111, 5769, 3734, 5315, 4780))
# It turns out the only lost match of Jon Jones is the biggest upset in the history of UFC