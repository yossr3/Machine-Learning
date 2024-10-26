library(tidyverse)

hotels <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-11/hotels.csv")


hotel_stays <- hotels %>%
  filter(is_canceled == 0) %>%
  mutate(
    children = case_when(
      children + babies > 0 ~ "children",
      TRUE ~ "none"
    ),
    required_car_parking_spaces = case_when(
      required_car_parking_spaces > 0 ~ "parking",
      TRUE ~ "none"
    )
  ) %>%
  select(-is_canceled, -reservation_status, -babies)
hotel_stays %>%
  count(children)

hotel_stays
library(skimr)

skim(hotel_stays)
hotel_stays %>%
  mutate(arrival_date_month = factor(arrival_date_month,
                                     levels = month.name
  )) %>%
  count(hotel, arrival_date_month, children) %>%
  group_by(hotel, children) %>%
  mutate(proportion = n / sum(n)) %>%
  ggplot(aes(arrival_date_month, proportion, fill = children)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::percent_format()) +
  facet_wrap(~hotel, nrow = 2) +
  labs(
    x = NULL,
    y = "Proportion of hotel stays",
    fill = NULL
  )
hotel_stays %>%
  count(hotel, required_car_parking_spaces, children) %>%
  group_by(hotel, children) %>%
  mutate(proportion = n / sum(n)) %>%
  ggplot(aes(required_car_parking_spaces, proportion, fill = children)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::percent_format()) +
  facet_wrap(~hotel, nrow = 2) +
  labs(
    x = NULL,
    y = "Proportion of hotel stays",
    fill = NULL
  )
install.packages("GGally")

# Charger le package GGally
library(GGally)

hotel_stays %>%
  select(
    children, adr,
    required_car_parking_spaces,
    total_of_special_requests
  ) %>%
  ggpairs(mapping = aes(color = children))
hotels_df <- hotel_stays %>%
  select(
    children, hotel, arrival_date_month, meal, adr, adults,
    required_car_parking_spaces, total_of_special_requests,
    stays_in_week_nights, stays_in_weekend_nights
  ) %>%
  mutate_if(is.character, factor)
install.packages("caret")
library(caret)
library(tidymodels)

set.seed(1234)
hotel_split <- initial_split(hotels_df)

hotel_train <- training(hotel_split)
hotel_test <- testing(hotel_split)

hotel_rec <- recipe(children ~ ., data = hotel_train) %>%
 
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_zv(all_numeric()) %>%
  step_normalize(all_numeric()) %>%
  prep()

hotel_rec
test_proc <- bake(hotel_rec, new_data = hotel_test)
