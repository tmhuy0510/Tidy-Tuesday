library(tidyverse)
library(tidymodels)
library(lubridate)
library(DT)

# Getting Data ------------------------------------------------------------

if (!file.exists("data")) {
  dir.create("data")
}

if (!file.exists("./data/airmen.csv")) {
  url = "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-08/airmen.csv"
  download.file(url, destfile = "./data/airmen.csv")
}

airmen_raw <- read_csv("./data/airmen.csv")

# Understanding Data ------------------------------------------------------

View(airmen_raw)
glimpse(airmen_raw)

airmen_raw <- airmen_raw %>%
  mutate(graduated_year = year(graduation_date))

airmen_raw$name[duplicated(airmen_raw$name)]
airmen_raw %>% 
  filter(name %in% airmen_raw$name[duplicated(airmen_raw$name)]) %>% 
  select(name, graduation_date, rank_at_graduation)
# There are 3 airmen whose name occurs twice

airmen_raw %>% count(graduated_year, sort = TRUE)
# The year of 1947 is absent from the list
# There are 11 NA values
# A vast majority of airmen graduated from 1943 to 1945

airmen_raw %>% count(rank_at_graduation, sort = TRUE)
# Capt and Captain can be grouped
# N/A, Unk and NA can be grouped
# 2nd Lt, Flight Officer and 1st Lt are dominant

airmen_raw %>% count(graduated_from, sort = TRUE)
# TAFF is the most dominant

airmen_raw %>% count(pilot_type, sort = TRUE)
# Liaison pilot and Liason pilot can be grouped
# Single engine and Twin engine are the most common types

airmen_raw %>% count(state, sort = TRUE)
# There are 49 states
# NA and Unk can be grouped
# Haiti and HT can be grouped
# In and IN can be grouped

airmen_raw %>% 
  arrange(desc(number_of_aerial_victory_credits)) %>% 
  select(name, number_of_aerial_victory_credits)
# The maximum number of credits is 4
# There are 72 airmen with at least 1 credit
# The number of credits is not always a whole number

sum(!is.na(airmen_raw$reported_lost))
sum(!is.na(airmen_raw$reported_lost_date))
sum(!is.na(airmen_raw$reported_lost_location))

airmen_raw %>% 
  filter(!is.na(reported_lost)) %>% 
  select(name, contains("lost"))
# Same location and time for 2 airmen

# Cleaning Data -----------------------------------------------------------

airmen <- airmen_raw %>%
  mutate(rank_at_graduation = case_when(
    rank_at_graduation == "Capt" ~ "Captain",
    is.na(rank_at_graduation) | rank_at_graduation == "N/A" | rank_at_graduation == "Unk" ~ "Unknown",
    TRUE ~ rank_at_graduation
    )
  ) %>% 
  mutate(pilot_type = case_when(
    pilot_type == "Liason pilot" ~ "Liaison pilot",
    TRUE ~ pilot_type
    )
  ) %>% 
  mutate(state = case_when(
    is.na(state) | state == "Unk" ~ "Unknown",
    state == "Haiti" ~ "HT",
    state == "In" ~ "IN",
    TRUE ~ state
    )
  )

# Exploratory Data Analysis 1 -----------------------------------------------

grad_rank <- c("2nd Lt", "Flight Officer")
pilot <- c("Single engine", "Twin engine")

airmen %>% 
  filter(rank_at_graduation %in% grad_rank,
         pilot_type %in% pilot) %>% 
  count(rank_at_graduation, pilot_type) %>% 
  pivot_wider(names_from = pilot_type, values_from = n)

airmen %>% 
  filter(rank_at_graduation %in% grad_rank,
         pilot_type %in% pilot) %>% 
  ggplot(aes(rank_at_graduation, fill = pilot_type)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(x = "Rank at graduation", y = "Percent", fill = NULL) +
  theme(legend.position = "top")

# Statistical Inference and Hypothesis Testing ----------------------------

grad_rank_vs_pilot <- airmen %>% 
  filter(rank_at_graduation %in% grad_rank,
         pilot_type %in% pilot) %>% 
  select(rank_at_graduation, pilot_type)

observed_diff_props <- grad_rank_vs_pilot %>% 
  specify(pilot_type ~ rank_at_graduation, success = "Single engine") %>% 
  calculate(stat = "diff in props", order = grad_rank)
observed_diff_props

# Statistical Inference using Simulating Method of Bootstrap
bootstrap_dist <- grad_rank_vs_pilot %>% 
  specify(pilot_type ~ rank_at_graduation, success = "Single engine") %>% 
  generate(reps = 1000, type = "bootstrap") %>% 
  calculate(stat = "diff in props", order = grad_rank)

bootstrap_dist %>% visualize()

percentile_ci <- bootstrap_dist %>% 
  get_ci(type = "percentile")
percentile_ci

bootstrap_dist %>% visualize() + 
  shade_ci(percentile_ci) +
  labs(x = "Difference in proportions of single engine \n between 2nd Lt and Flight Officer",
       y = "Count",
       subtitle = "0 is not included in the confidence interval of 0.95")

# Hypothesis Testing using Simulating Method of Permutation
null_dist <- grad_rank_vs_pilot %>% 
  specify(pilot_type ~ rank_at_graduation, success = "Single engine") %>% 
  hypothesise(null = "independence") %>% 
  generate(reps = 1000, type = "permute") %>% 
  calculate(stat = "diff in props", order = grad_rank)

p_value <- null_dist %>%
  get_p_value(observed_diff_props, "right")
p_value

null_dist %>% 
  visualize() + 
  shade_p_value(observed_diff_props, "right") + 
  labs(x = "Difference in proportions of single engine \n between 2nd Lt and Flight Officer",
       y = "Count",
       subtitle = "p value is 0 which means the null hypothesis can be rejected")

# Exploratory Data Analysis 2 -----------------------------------------------

airmen_vs_states <- airmen %>% 
  count(state)

# Spatial Distribution of Airmen across the USA ---------------------------

states_map <- map_data("state") %>% 
  as_tibble()

states_name_and_abb <- tibble(
  state_name = c(state.name %>% str_to_lower(), "district of columbia"),
  state_abb = c(state.abb, "DC")
)

states_map <- states_map %>% 
  left_join(states_name_and_abb, by = c("region" = "state_name"))

states_abb_labels <- states_map %>% 
  group_by(state_abb) %>% 
  summarise(mean_long = mean(long),
            mean_lat = mean(lat))

airmen_vs_states <- airmen_vs_states %>% 
  right_join(states_map, by = c("state" = "state_abb")) %>% 
  mutate(n = replace_na(n, 0))

airmen_vs_states %>% 
  ggplot(aes(x = long, y = lat)) +
  geom_polygon(aes(fill = n, group = group), color = "white") +
  geom_text(aes(x = mean_long, y = mean_lat, label = state_abb), 
            data = states_abb_labels,
            size = 3.5, hjust = 0.5, color = "red") +
  scale_fill_viridis_c(option = "E") +
  labs(x = NULL,
       y = NULL,
       title = "Spatial distribution of airmen across the USA",
       subtitle = "IL, NY and PA are states with the highest number of airmen",
       fill = "No. of airmen:") +
  theme_dark() +
  scale_x_continuous(labels = NULL, breaks = NULL) +
  scale_y_continuous(labels = NULL, breaks = NULL)
