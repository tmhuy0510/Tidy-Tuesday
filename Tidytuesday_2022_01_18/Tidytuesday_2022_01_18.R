library(tidyverse)
library(tidytext)
library(textrecipes)
library(tidymodels)


chocolate_raw <- read_csv("./data/chocolate.csv")
country_continent <- read_csv("./data/country_continent.csv")


View(chocolate_raw)
glimpse(chocolate_raw)


purrr::map_dbl(chocolate_raw, n_distinct)
purrr::map_dbl(chocolate_raw, ~ sum(is.na(.)))


# Column `company_location`
chocolate_by_company_location <- chocolate_raw %>%
  group_by(company_location) %>% 
  summarize(n = n(), 
            mean_rating = mean(rating),
            median_rating = median(rating),
            std_dev_rating = sd(rating))
chocolate_by_company_location
# Comment:
# USA manufacturers accounts for 45% of observations
# American and European manufacturers account for 85% of observations
chocolate_by_company_location %>% 
  filter(n >= 20) %>% 
  slice_max(mean_rating, n = 5)
# Comment: Top 5: Australia, Denmark, Switzerland, Canada and Spain


# Column `country_of_bean_origin`
chocolate_by_country_of_bean_origin <- chocolate_raw %>%
  group_by(country_of_bean_origin) %>% 
  summarize(n = n(), 
            mean_rating = mean(rating),
            median_rating = median(rating),
            std_dev_rating = sd(rating))
chocolate_by_country_of_bean_origin
# Comment:
# American origin accounts for 70% of observations
# European origin accounts for 0% of observations
chocolate_by_country_of_bean_origin %>% 
  filter(n >= 20) %>% 
  slice_max(mean_rating, n = 5)
# Comment: 
# Top 5: Vietnam, Papua New Guinea, Madagascar, Haiti and Brazil
# Blend has the lowest rating


# Column `cocoa_percent`
chocolate_raw <- chocolate_raw %>% 
  mutate(cocoa_percent = parse_number(cocoa_percent))
# Previous type of `cocoa_percent` is `chr`
chocolate_raw$cocoa_percent %>% summary()
# Comment: 5-number summary
chocolate_raw %>% 
  ggplot(aes(cocoa_percent)) +
  geom_histogram(fill = "steelblue", color = "white", binwidth = 5) +
  labs(x = "Cocoa Percent", y = "Count")
# Comment:
# Highest count in the range from 67.5 to 72.5
# Right-skewed distribution
cor(chocolate_raw$cocoa_percent, chocolate_raw$rating)
# Comment: This is a very weak negative correlation


# Column `ingredients`
chocolate_by_ingredients <- chocolate_raw %>%
  group_by(ingredients) %>% 
  summarize(n = n(), 
            mean_rating = mean(rating),
            median_rating = median(rating),
            std_dev_rating = sd(rating))
chocolate_by_ingredients
# Comment: 
# The only column has `NA` values
# `B` is present in all chocolates
chocolate_by_ingredients %>% 
  filter(n >= 20) %>% 
  arrange(desc(n))
# Comment: 
# `3- B,S,C` have the highest mean rating and number of reviews
# Top 5 of `n` is exactly the same as top 5 of `mean_rating`


# Column `most_memorable_characteristics`
chocolate_by_characteristic_word <- chocolate_raw %>% 
  select(most_memorable_characteristics, rating) %>% 
  unnest_tokens(characteristic_word, most_memorable_characteristics,
                token = "regex", pattern = ",") %>% 
  mutate(characteristic_word = characteristic_word %>% str_squish()) %>% 
  group_by(characteristic_word) %>% 
  summarize(n = n(),
            mean_rating = mean(rating),
            median_rating = median(rating),
            std_dev_rating = sd(rating))
chocolate_by_characteristic_word %>%
  filter(n >= 20) %>% 
  arrange(desc(n))
# Comment: Top 10 words describing chocolate taste
chocolate_by_characteristic_word %>%
  slice_max(mean_rating, n = 5)
# Comment: Top 5 words with highest rating
chocolate_by_characteristic_word %>%
  slice_min(mean_rating, n = 5)
# Comment: Top 5 words with lowest rating


# Column `rating`
chocolate_raw$rating %>% summary()
# Comment: 5-number summary
chocolate_raw %>% 
  ggplot(aes(rating)) +
  geom_histogram(fill = "steelblue", color = "white", binwidth = 0.25) +
  labs(x = "Rating", y = "Count")
# Comment:
# A large proportion of ratings are greater than or equal to 2.5
# Left-skewed distribution


# EDA1
mean_rating <- chocolate_raw$rating %>% mean()
chocolate_raw %>% 
  select(most_memorable_characteristics, rating) %>% 
  unnest_tokens(characteristic_word, most_memorable_characteristics,
                token = "regex", pattern = ",") %>% 
  mutate(characteristic_word = characteristic_word %>% str_squish()) %>% 
  mutate(rating_cat = case_when(rating > mean_rating ~ "Good",
                                rating <= mean_rating ~ "Bad")) %>% 
  count(rating_cat, characteristic_word, sort = TRUE) %>% 
  reshape2::acast(characteristic_word ~ rating_cat,
                  value.var = "n", fill = 0) %>% 
  wordcloud::comparison.cloud(max.words = 100, random.order = FALSE,
                              colors = c("darkred", "darkgreen"),
                              title.bg.colors = "white",
                              rot.per = 0.25)


#
chocolate_raw <- chocolate_raw %>% 
  left_join(country_continent, c("company_location" = "country")) %>% 
  rename(comp_loc_continent = continent) %>% 
  left_join(country_continent, c("country_of_bean_origin" = "country")) %>% 
  rename(continent_of_bean_origin = continent) %>% 
  mutate(ingredients = str_replace_all(ingredients, "S\\*", "OS")) %>% 
  separate(ingredients, c("num_ingreds", "ingreds"), sep = "-")


# Build models


#
set.seed(123)
#
choco_split <- initial_split(chocolate_raw)
choco_train <- training(choco_split)
choco_test <- testing(choco_split)
#
choco_folds <- vfold_cv(choco_train)
choco_folds


#
#
choco_rec <- recipe(rating ~ most_memorable_characteristics, 
                    data = choco_train) %>%
  step_tokenize(most_memorable_characteristics) %>%
  step_tokenfilter(most_memorable_characteristics, 
                   max_tokens = 200) %>%
  step_tf(most_memorable_characteristics)
#
#
rf_spec <- rand_forest(trees = 500) %>%
  set_mode("regression")
#
svm_spec <- svm_linear() %>%
  set_mode("regression")
#
#
svm_wf <- workflow(choco_rec, svm_spec)
#
rf_wf <- workflow(choco_rec, rf_spec)


#
#
contrl_preds <- control_resamples(save_pred = TRUE)
#
#
svm_rs <- fit_resamples(
  svm_wf,
  resamples = choco_folds,
  control = contrl_preds
)
#
ranger_rs <- fit_resamples(
  rf_wf,
  resamples = choco_folds,
  control = contrl_preds
)


#
#
collect_metrics(svm_rs)
#
collect_metrics(ranger_rs)
# Comment:


#
bind_rows(
  collect_predictions(svm_rs) %>%
    mutate(mod = "SVM"),
  collect_predictions(ranger_rs) %>%
    mutate(mod = "ranger")
) %>%
  ggplot(aes(rating, .pred, color = id)) +
  geom_abline(lty = 2, color = "gray50", size = 1.2) +
  geom_jitter(width = 0.5, alpha = 0.5) +
  facet_wrap(vars(mod)) +
  coord_fixed()


# 
#
final_fitted <- last_fit(svm_wf, choco_split)
#
collect_metrics(final_fitted)
#
final_wf <- extract_workflow(final_fitted)
#
predict(final_wf, choco_test[55, ])
# 
final_wf %>% 
  tidy() %>%
  filter(term != "Bias") %>%
  group_by(estimate > 0) %>%
  slice_max(abs(estimate), n = 10) %>%
  ungroup() %>%
  mutate(term = str_remove(term, "tf_most_memorable_characteristics_")) %>%
  ggplot(aes(estimate, fct_reorder(term, estimate), fill = estimate > 0)) +
  geom_col(alpha = 0.8) +
  scale_fill_discrete(labels = c("low ratings", "high ratings")) +
  labs(y = NULL, fill = "More from...")


#
#
choco_rec_multi <- recipe(rating ~ comp_loc_continent + 
                            continent_of_bean_origin +
                            ingreds + cocoa_percent +
                            most_memorable_characteristics,
                          data = choco_train) %>% 
  step_dummy(comp_loc_continent) %>% 
  step_dummy(continent_of_bean_origin) %>% 
  step_unknown(ingreds) %>% 
  step_tokenize(ingreds) %>% 
  step_tf(ingreds) %>% 
  step_tokenize(most_memorable_characteristics) %>%
  step_tokenfilter(most_memorable_characteristics, 
                   max_tokens = 200) %>%
  step_tf(most_memorable_characteristics) %>% 
  step_normalize(all_predictors())
#
svm_spec <- svm_linear() %>%
  set_mode("regression")
#
svm_multi_wf <- workflow(choco_rec_multi, svm_spec)


#
#
contrl_preds <- control_resamples(save_pred = TRUE)
#
svm_multi_rs <- fit_resamples(
  svm_multi_wf,
  resamples = choco_folds,
  control = contrl_preds
)


#
collect_metrics(svm_multi_rs)


#
#
split_comma <- function(x) {
  str_split(x, ",") %>% map(str_squish)
}
choco_rec_compound_words <- recipe(rating ~ most_memorable_characteristics,
                                   data = choco_train) %>% 
  step_tokenize(most_memorable_characteristics, 
                custom_token = split_comma) %>% 
  step_tokenfilter(most_memorable_characteristics, max_tokens = 200) %>% 
  step_tf(most_memorable_characteristics)
#
svm_spec
#
svm_compound_words_wf <- workflow(choco_rec_compound_words, svm_spec)


#
#
contrl_preds <- control_resamples(save_pred = TRUE)
#
svm_compound_words_rs <- fit_resamples(
  svm_compound_words_wf,
  resamples = choco_folds,
  control = contrl_preds
)


#
collect_metrics(svm_compound_words_rs)


# 
#
multi_final_fitted <- last_fit(svm_multi_wf, choco_split)
#
collect_metrics(multi_final_fitted)
#
multi_final_wf <- extract_workflow(multi_final_fitted)
#
predict(multi_final_wf, choco_test[55, ])
# 
multi_final_wf %>% 
  tidy() %>%
  filter(term != "Bias") %>%
  group_by(estimate > 0) %>%
  slice_max(abs(estimate), n = 10) %>%
  ungroup() %>%
  mutate(term = str_remove(term, "tf_most_memorable_characteristics_")) %>%
  ggplot(aes(estimate, fct_reorder(term, estimate), fill = estimate > 0)) +
  geom_col(alpha = 0.8) +
  scale_fill_discrete(labels = c("low ratings", "high ratings")) +
  labs(y = NULL, fill = "More from...")


#
#
country_continent_map <- read_csv("./data/country_continent_map.csv")
#
world <- map_data("world") %>% as_tibble()
#
world <- world %>% left_join(country_continent_map, by = c("region" = "country")) %>% 
  filter(!is.na(continent))
# 
results <- chocolate_raw %>% 
  count(continent_of_bean_origin)
#
results_world <- left_join(world, results,
                            by = c("continent" = "continent_of_bean_origin")) %>% 
  mutate(n = replace_na(n, 0))
#
#
continent_label <- results_world %>% 
  group_by(continent) %>% 
  summarize(long = mean(long),
            lat = mean(lat))
#
continent_label <- tibble(continent = c("Africa", "Asia", "Europe", "North America", "Oceania", "South America"),
                          long = c(25, 90, 90, -92.5, 135, -60),
                          lat = c(4, 35, 62.5, 50, -25, -20))
# 
plain <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  plot.background = element_rect(fill = "gray70"),
  legend.background = element_rect(fill = "gray70"),
  panel.background = element_rect(fill = "gray70"),
  strip.background = element_rect(fill = "gray70")
)
#
ggplot(data = results_world,
       aes(x = long, y = lat)) +
  coord_fixed(1.5) +
  geom_polygon(aes(fill = n, group = group)) +
  geom_text(aes(label = continent), data = continent_label, 
            fontface = "bold") +
  scale_fill_distiller(NULL, palette = "Spectral") +
  labs(subtitle = "") +
  plain

