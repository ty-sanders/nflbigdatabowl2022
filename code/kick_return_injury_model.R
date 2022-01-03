## Model Returner Injuries 
## Tyler Sanders
## 7-11-2021


# Setup -------------------------------------------------------------------
library(tidyverse)
library(tidymodels)
library(vip)
library(themis)
library(workflows)
library(janitor)
library(kableExtra)

# Load Data ---------------------------------------------------------------

## Load and wrangle initial modeling data
kick_returners <- read_csv(here::here("data/kick_returns.csv")) %>% 
  select(-special_teams_result, -c(week, play_result, min_speed, visitor_team_abbr, possession_team, jersey_number, play_description, game_date, team_name, returner_id, game_clock, yardline_number, quickest_slowdown, double_moves, kick_blocker_id, pass_result, above_average_snap_time, above_average_operation_time, is_gunner, is_punt_rusher, is_special_teams_safety, is_missed_tackler, is_assist_tackler, is_tackler, is_vises, is_returner, abbr_name, pre_snap_home_score, pre_snap_visitor_score, kicker_id, player_team, home_team_abbr, birth_date, college_name, position.y, snap_detail, snap_time, operation_time, hang_time, kick_direction_intended, kick_direction_actual, return_direction_intended, return_direction_actual, missed_tackler, assist_tackler, tackler, kickoff_return_formation, gunners, punt_rushers, special_teams_safeties, vises, kick_contact_type, yardline_side, penalty_codes, penalty_jersey_numbers, penalty_yards, kick_return_yardage)) %>% 
  mutate(injured = case_when(injured %in% 1 ~ "Injured", TRUE ~ "Not Injured")) %>% 
  mutate_if(is.character, as.factor) 
  
# Create Training/Testing Sets --------------------------------------------

## Set Seed by development start date 
set.seed(seed = 7112021)

# Initial Modeling Split with special injured player strata 
kick_returners_split <- initial_split(kick_returners, strata = injured, pool = .001, prop = .8)

# Store training split for review
training_df <- training(kick_returners_split)

## Model Recipe 
training_recipes <- recipe(injured ~ ., data = training_df) %>% 
  update_role(game_id, play_id, nfl_id, new_role = "ID") %>% 
  ## https://juliasilge.com/blog/sliced-aircraft/
  #step_smote() %>% 
  #step_dummy(all_nominal_predictors()) %>% 
  step_meanimpute(all_numeric_predictors()) %>% 
  step_modeimpute(all_nominal_predictors()) %>% 
 # step_zv(all_predictors()) %>% 
  prep()

## Pull prepped testing and training data
testing_df <- testing(kick_returners_split)
training_df <- juice(training_recipes)

# Model Returner Injury Likelihood ----------------------------------------

## Construct Engine 
ranger_engine <- rand_forest(trees = 100, mode = "classification", mtry = 4, min_n = 1000) %>%
  set_engine("ranger", importance = "impurity")

## Establish Workflow
return_wf <- workflow() %>% 
  add_model(ranger_engine) %>% 
  add_recipe(training_recipes)

## Fit prepped training data
ranger_fit <- return_wf %>% 
  fit(data = training_df)


## Final Predictions 
pred_results <- predict(ranger_fit, new_data = testing_df, type = "prob") %>% arrange(desc(.pred_Injured))


## Final Results DF
testing_results <- bind_cols(testing_df, pred_results) 


# Model Results Review  -----------------------------------------------------------

## Variable Importance Chart
ranger_fit %>% 
  extract_fit_parsnip()  %>% 
  vip(num_features = 20)

## Results Histogram
hist(pred_results$.pred_Injured)

## Injured Group Validation Test 
testing_results %>% 
  group_by(injured) %>% 
  summarise(mean(.pred_Injured))
 

## Results Table
testing_results %>% 
  group_by(season, display_name) %>% 
  summarise(total_injury_risk = sum(.pred_Injured),
            number_of_returns = n(),
            avg_risk = mean(.pred_Injured)) %>% 
  arrange(desc(total_injury_risk)) %>% 
  select(season, display_name, total_injury_risk, avg_risk, number_of_returns) %>% 
  mutate(number_of_returns = scales::comma(number_of_returns, accuracy = 1)) %>% 
  mutate(across(is.numeric, scales::percent, accuracy = .01)) %>% 
  filter(season %in% 2020) %>% 
  ungroup() %>% 
  select(-season) %>% 
  head(10) %>% 
  kbl(col.names = c("Returner", "Modeled Injury Risk", "Injury Risk Per Return", "Number of Returns"), align = c("r", "c", "c", "c")) %>% 
  kable_classic_2() %>% 
  add_header_above(c("Top 10 Most At-Risk Returners: 2020 Season" = 4), font = 12) %>% 
  add_header_above(c("Returner Injury Risk" = 4), line = F) 


