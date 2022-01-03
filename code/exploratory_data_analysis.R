## Exploratory Data Analysis & Cleaning 
## Tyler Sanders
## 2021-10-15

# Setup -------------------------------------------------------------------

library(tidyverse)
library(here)
library(janitor)
library(lubridate)


# Read in Data ------------------------------------------------------------

games <- here("data/games.csv") %>% 
  read_csv() %>% 
  clean_names()

pff_scouting_data <- here("data/PFFScoutingData.csv") %>% 
  read_csv() %>% 
  clean_names()


players <- here("data/players.csv") %>% 
  read_csv() %>% 
  clean_names()


plays <- here("data/plays.csv") %>% 
  read_csv() %>% 
  clean_names()


tracking_18 <- here("data/tracking2018.csv") %>% 
  read_csv() %>% 
  clean_names()


tracking_19 <- here("data/tracking2019.csv") %>% 
  read_csv() %>% 
  clean_names()


tracking_20 <- here("data/tracking2020.csv") %>% 
  read_csv() %>% 
  clean_names()

#loading data from Lee Sharpe's public GitHub repository. It includes info on field surface.
games_lee_sharpe <- read_csv("https://raw.githubusercontent.com/nflverse/nfldata/master/data/games.csv",
                              col_types = cols())



# Identify Injured Players ------------------------------------------------


injury_on_play <- plays %>% 
  filter(str_detect(play_description, "injured")) 


injured_player_names <- unlist(strsplit(injury_on_play$play_description, "(?<=[[:punct:]])\\s(?=[A-Z])", perl=T)) %>% 
  enframe() %>% 
  filter(str_detect(value, "was injured during the play.")) %>% 
  mutate(team_of_injured_player = str_extract(value, "[^-]+")) %>% 
  mutate(injured_player = str_extract(value,"(?<=-).+(?= )")) %>% 
  rowwise() %>% 
  mutate(injured_player = strsplit(injured_player, " ")[[1]][[1]]) %>% 
  select(team_of_injured_player, injured_player)
  
injury_stats <- injury_on_play %>% 
  bind_cols(injured_player_names) 

plays_with_injury_data <- left_join(plays, injury_stats) %>% 
  mutate(injury_on_play_flag = case_when(is.na(injured_player) ~ 0, TRUE ~ 1))

players_names_improved <- players %>% 
  mutate(first_name = str_extract(display_name, "[A-Za-z]+(?=\\s)"),
         last_name  = str_remove(display_name, "^\\S+\\s+"),
         first_initial = str_sub(first_name, start = 1L, end = 1L),
         abbr_name     = paste0(first_initial, ".", last_name))

injured_players <- plays_with_injury_data %>% 
  filter(injury_on_play_flag %in% 1) %>% 
  left_join(y = players_names_improved, by = c("injured_player" = "abbr_name"))


tracking_18 <- tracking_18 %>% 
  mutate(first_name    = str_extract(display_name, "[A-Za-z]+(?=\\s)"),
         last_name     = str_remove(display_name, "^\\S+\\s+"),
         first_initial = str_sub(first_name, start = 1L, end = 1L),
         abbr_name     = paste0(first_initial, ".", last_name))


tracking_19 <- tracking_19 %>%
  mutate(first_name = str_extract(display_name, "[A-Za-z]+(?=\\s)"),
         last_name  = str_remove(display_name, "^\\S+\\s+"),
         first_initial = str_sub(first_name, start = 1L, end = 1L),
         abbr_name     = paste0(first_initial, ".", last_name))

tracking_20 <- tracking_20 %>%
  mutate(first_name = str_extract(display_name, "[A-Za-z]+(?=\\s)"),
         last_name  = str_remove(display_name, "^\\S+\\s+"),
         first_initial = str_sub(first_name, start = 1L, end = 1L),
         abbr_name     = paste0(first_initial, ".", last_name))


# Create blank tibble for function data store
function_output_df <- tribble(~"nfl_id", ~"display_name", ~"game_id", ~"play_id", ~"abbr_name",
                              NA_real_,     "",         NA_real_,  NA_real_,       "")

locate_injured_player_id <- function(row_num){

  rm(data_search_18, data_search_19, data_search_20)
  
  data_search_18 <- tracking_18 %>% 
    filter(game_id %in% injured_players$game_id[[row_num]] & play_id %in% injured_players$play_id[[row_num]]) %>% 
    select(nfl_id, display_name, game_id, play_id, abbr_name) %>% 
    filter(abbr_name %in% injured_players$injured_player[[row_num]]) %>% 
    distinct()
  
  if(! nrow(data_search_18) %in% 1){
  data_search_19 <- tracking_19 %>% 
    filter(game_id %in% injured_players$game_id[[row_num]] & play_id %in% injured_players$play_id[[row_num]]) %>% 
    select(nfl_id, display_name, game_id, play_id, abbr_name) %>% 
    filter(abbr_name %in% injured_players$injured_player[[row_num]]) %>% 
    distinct()
  }
  
  if(exists("data_search_19")){
  if(! nrow(data_search_19) %in% 1){
  
  data_search_20 <- tracking_20 %>% 
    filter(game_id %in% injured_players$game_id[[row_num]] & play_id %in% injured_players$play_id[[row_num]]) %>% 
    select(nfl_id, display_name, game_id, play_id, abbr_name) %>% 
    filter(abbr_name %in% injured_players$injured_player[[row_num]]) %>% 
    distinct()
  }
  }
  if(nrow(data_search_18) %in% 1){
    temp <<- data_search_18
  }
  
  if(exists("data_search_19")){
    if(nrow(data_search_19) %in% 1){
    temp <<- data_search_19
  }
  }

  if(exists("data_search_20")){
    if(nrow(data_search_20) %in% 1){
      temp <<- data_search_20
    }
  }
  
  print(row_num)
  
  if(nrow(temp == 1)){
    
  function_output_df <<- bind_rows(function_output_df, temp)}
  
  

  
  
}

# Walk/Run Injured Player Iteration 
{
tictoc::tic()
injured_players$injured_player %>% 
  enframe() %>% 
  pull(name) %>% 
  walk(locate_injured_player_id)
tictoc::toc()
}

#Filter result to remove football 
injured_player_ids <- function_output_df %>% 
  filter(!is.na(nfl_id)) %>% 
  distinct()


# Plan --------------------------------------------------------------------
ffunction

# How Does This Work? 

# Goal, use a training set of special team injuries on kickoffs and punts
# to identify injury risk factors and model injury risk at the player level on a per-play basis

# What I Have
## Play tracking data from 2018-2020
## PFF Scouting Data for plays 2018-2020
## Dataset of all Special Teams NFL players 2018-2020
## Dataset of all plays 2018-2020

# How They Attach 
# The Plays dataset has a playDescription column which lists the last name and first initial of an injured player
# By iterating over tracking data I can use play and game Ids along with name info to find the nflId of each injured player
# I can use nflId to connect the plays, players, tracking, and scouting data 
# Create a dataframe where each row is a unique set of play and player where the columns are a combination of:
# -play data (repetitive for each player in on that play: quarter, game closeness, play type), 
# -player data (repetitive for each play the player was on the field for: position, age, weight)
# -player tracking data features (max acceleration, distance covered, number of pivots?, tackles?) 

# To Do

## Load in Data [+]
## Identify injuries [+]
## Connect injured players/plays to tracking [+]
## Feature selection for tracking and scouting []

# tracking_18 %>% head(100) %>% view()
# 
# avg_accel_pop <- mean(tracking_18_features$avg_acceleration)
# avg_speed_pop <- mean(tracking_18_features$avg_speed)
# 
# tracking_18 %>% 
#   filter(gameId %in% 2018123000, playId %in% 36, frameId %in% 12) %>% 
#   filter(displayName %in% "Maxx Williams") %>% 
#   pull(y)
# 

  
  

# Cleaning & Feature Selection --------------------------------------------

play_data_combined <- pff_scouting_data %>% 
  full_join(y = plays, by = c("game_id", "play_id")) %>% 
  mutate(kick_intended_achieved = case_when(kick_direction_intended %in% kick_direction_actual & ! is.na(kick_direction_actual) ~ 1, TRUE ~ 0),
         return_intended_achieved = case_when(return_direction_intended %in% return_direction_actual & ! is.na(return_direction_actual) ~ 1, TRUE ~ 0),
         above_average_snap_time  = case_when(snap_time > mean(snap_time) ~ 1, TRUE ~ 0),
         above_average_operation_time = case_when(operation_time > mean(operation_time) ~ 1, TRUE ~ 0),
         penalty_on_play              = case_when(! is.na(penalty_codes) ~ 1, TRUE ~ 0),
         home_team_pt_dif             = pre_snap_home_score - pre_snap_visitor_score,
         kick_returned                = case_when(! is.na(kick_return_yardage) ~ 1, TRUE ~ 0)
         
         
         #figure out adding player action/position flags by number
  )

create_data_combined <- function(year){

  if(year %in% 18){
  tracking <- tracking_18
  }
  
  if(year %in% 19){
    tracking <- tracking_19
  }
  
  if(year %in% 20){
    tracking <- tracking_20
  }
  
  
  
tracking_features <- tracking %>% 
  filter(! is.na(nfl_id)) %>% 
  group_by(game_id, play_id, nfl_id, display_name) %>% 
  mutate(change_direction = dir - lag(dir),
         change_acceleration = a - lag(a),
         quickest_slowdown = min(change_acceleration),
         pivots           = case_when(change_direction > abs(45) ~ 1, TRUE ~ 0),
         high_acceleration_pivots = case_when(pivots %in% 1 & a >= (5) ~ 1, TRUE ~ 0),
         high_speed_pivots = case_when(pivots %in% 1 & a >= (5) ~ 1, TRUE ~ 0),
         high_speed_frames = case_when(s >= (5) ~ 1, TRUE ~ 0), 
         double_moves     = case_when(pivots %in% 1 & lag(pivots) %in% 1 ~ 1, TRUE ~ 0),
         lateral_distance = max(y) - min(y),
         sideline_to_sideline = case_when(max(y) > (53.3-5) & min(y) < 5 ~ 1, TRUE ~ 0),
         cross_lateral_midfield = case_when(max(y) > (53.3 / 2) & min(y) > (53.2 / 2) |
                                            max(y) < (53.3 / 2) & min(y) < (53.2 / 2) ~ 1, TRUE ~ 0)) %>%
  #na.omit() %>% 
  summarise(avg_speed = mean(s),
            min_speed = min(s),
            max_speed = max(s),
            min_acceleration = min(a),
            avg_acceleration = mean(a),
            max_acceleration = max(a),
            quickest_slowdown = quickest_slowdown,
            total_distance = sum(dis),
            home = team,
            jersey_number = jersey_number, 
            position = position,
            pivots_total = sum(pivots), 
            high_acceleration_pivots_total = sum(high_acceleration_pivots),
            high_speed_pivots_total = sum(high_speed_pivots),
            high_speed_frames = sum(high_speed_frames),
            double_moves = double_moves,
            lateral_distance = lateral_distance,
            starting_x = first(x),
            starting_y = first(y),
            sideline_to_sideline = sideline_to_sideline,
            cross_lateral_midfield = cross_lateral_midfield
            
            #Initial sprint is 40 frames
            
            
            #distance from football
            #Number of players within x proximity 
            
            ) %>% 
  distinct(game_id, play_id, nfl_id, .keep_all = TRUE)
  
injured_eda_with_tracking <- tracking_features %>% 
  left_join(injured_player_ids, by = c("game_id", "play_id", "nfl_id", "display_name")) %>% 
  mutate(injured = case_when(! is.na(abbr_name) ~ 1, TRUE ~0)) %>%
  distinct()


data_combined <- injured_eda_with_tracking %>% 
  left_join(y = games, by = "game_id") %>% 
  left_join(y = players, by = c("nfl_id", "display_name")) %>% 
  left_join(y = play_data_combined, by = c("game_id", "play_id")) %>% 
  mutate(team_name               = case_when(home %in% "home" ~ home_team_abbr, TRUE ~ visitor_team_abbr),
         jersey_number           = case_when(jersey_number %in% c(0:9) ~ paste0(as.character(jersey_number), " "),
                                                                  TRUE ~ as.character(jersey_number)),
         player_team             = paste(team_name, jersey_number),
         is_gunner               = case_when(str_detect(gunners, player_team) ~ 1, TRUE ~ 0),
         is_punt_rusher          = case_when(str_detect(punt_rushers, player_team) ~ 1, TRUE ~ 0),
         is_punt_rusher          = case_when(str_detect(punt_rushers, player_team) ~ 1, TRUE ~ 0),
         is_special_teams_safety = case_when(str_detect(special_teams_safeties, player_team) ~ 1, TRUE ~ 0),
         is_missed_tackler       = case_when(str_detect(missed_tackler, player_team) ~ 1, TRUE ~ 0),
         is_assist_tackler       = case_when(str_detect(assist_tackler, player_team) ~ 1, TRUE ~ 0),
         is_tackler              = case_when(str_detect(tackler, player_team) ~ 1, TRUE ~ 0),
         is_vises                = case_when(str_detect(vises, player_team) ~ 1, TRUE ~ 0),
         is_returner             = case_when(nfl_id %in% returner_id ~ 1, TRUE ~ 0),
         called_for_penalty      = case_when(str_detect(penalty_jersey_numbers, player_team) ~ 1, TRUE ~ 0),
         season_quarter = case_when(week %in% c(1:4)   ~  "First",
                                    week %in% c(5:8)   ~ "Second", 
                                    week %in% c(9:12)  ~  "Third",
                                    week %in% c(13:17) ~  "Last"),
         game_time_slot = case_when(game_time_eastern < hms("16:05:00")                                     ~     "Early",
                                    game_time_eastern > hms("16:05:00") & game_time_eastern < hms("19:10:00") ~ "Afternoon",
                                    TRUE                                                                  ~    "Night")) %>% 
  ungroup()


}

{
tictoc::tic()
data_combined_18 <- create_data_combined(year = 18)
tictoc::toc()
} #14 minutes 20 seconds

{
tictoc::tic()
data_combined_19 <- create_data_combined(year = 19)
tictoc::toc() # 13 minutes 8 seconds
}

{
  tictoc::tic()
data_combined_20 <- create_data_combined(year = 20)
tictoc::toc() # 13 minutes 12 seconds
}

data_combined_final <- bind_rows(data_combined_18, data_combined_19, data_combined_20)

# Player Proximity --------------------------------------------------------


player_proximity <- tribble(~"game_id", ~"play_id", ~"nfl_id", ~"engaged_player_count", ~"close_player_count",
                                 ~"max_engaged", ~"max_close", ~"longest_engaged", ~"longest_close", ~"per_frames_engaged", ~"per_frames_close",
                              NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_)



calculate_player_proximity_18 <- function(row_num){

  primary_player_df <- primary_player_df_18
  
opposing_player_df <- tracking_18 %>%
  filter(game_id %in% primary_player_df$game_id[[row_num]] & play_id %in% primary_player_df$play_id[[row_num]] &
        !nfl_id %in% primary_player_df$nfl_id[[row_num]]) %>% 
  filter(!team %in% primary_player_df$home[[row_num]] & ! is.na(nfl_id)) %>%
  select(nfl_id, frame_id, x, y)

primary_player_frames <- tracking_18 %>% 
  filter(game_id %in% primary_player_df$game_id[[row_num]], play_id %in% primary_player_df$play_id[[row_num]],
         nfl_id %in% primary_player_df$nfl_id[[row_num]]) %>% 
  mutate(primary_nfl_id = nfl_id,
         primary_x = x,
         primary_y = y) %>% 
  select(-nfl_id, -x, -y)

temp <- opposing_player_df %>%
  left_join(y = primary_player_frames, by = c("frame_id")) %>%
  rowwise() %>%
  mutate(opponent_engaged = case_when(abs(primary_x - x) < .5 & abs(primary_y - y) < .5 ~ 1, TRUE ~ 0),
         opponent_close   = case_when(abs(primary_x - x) <  3 & abs(primary_y - y) <  3 ~ 1, TRUE ~ 0)) 



engaged_player_calc <- temp %>%
  group_by(primary_nfl_id, game_id, play_id, nfl_id) %>%
  summarise(engaged_player_count = case_when(sum(opponent_engaged) > 0 ~ 1, TRUE ~ 0)) %>%
  ungroup() %>%
  mutate(engaged_player_count = sum(engaged_player_count)) %>%
  select(game_id, play_id, primary_nfl_id, engaged_player_count) %>%
  slice(1)

max_engaged_calc <- temp %>%
  group_by(primary_nfl_id, frame_id) %>%
  summarise(max_engaged = sum(opponent_engaged)) %>%
  ungroup() %>%
  arrange(desc(max_engaged)) %>%
  select(primary_nfl_id, max_engaged) %>%
  slice(1)

longest_engaged_calc <- temp %>%
  group_by(primary_nfl_id, nfl_id) %>%
  summarise(longest_engaged = sum(opponent_engaged)) %>%
  ungroup() %>%
  arrange(desc(longest_engaged)) %>%
  select(primary_nfl_id, longest_engaged) %>%
  slice(1)

frames_engaged_calc <- temp %>%
  group_by(primary_nfl_id, frame_id) %>%
  summarise(engaged_frame = case_when(sum(opponent_engaged) > 0 ~ 1, TRUE ~ 0)) %>%
  summarise(per_frames_engaged = sum(engaged_frame) / max(temp$frame_id))

close_player_calc <- temp %>%
  group_by(primary_nfl_id, nfl_id) %>%
  summarise(close_player_count = case_when(sum(opponent_close) > 0 ~ 1, TRUE ~ 0)) %>%
  ungroup() %>%
  mutate(close_player_count = sum(close_player_count)) %>%
  select(primary_nfl_id, close_player_count) %>%
  slice(1)

max_close_calc <- temp %>%
  group_by(primary_nfl_id, frame_id) %>%
  summarise(max_close = sum(opponent_close)) %>%
  ungroup() %>%
  arrange(desc(max_close)) %>%
  select(primary_nfl_id, max_close) %>%
  slice(1)

longest_close_calc <- temp %>%
  group_by(primary_nfl_id, nfl_id) %>%
  summarise(longest_close = sum(opponent_close)) %>%
  ungroup() %>%
  arrange(desc(longest_close)) %>%
  select(primary_nfl_id, longest_close) %>%
  slice(1)

frames_close_calc <- temp %>%
  group_by(primary_nfl_id, frame_id) %>%
  summarise(close_frame = case_when(sum(opponent_close) > 0 ~ 1, TRUE ~ 0)) %>%
  summarise(per_frames_close = sum(close_frame) / max(temp$frame_id))

intermediate <<- engaged_player_calc %>%
  left_join(close_player_calc,    by = "primary_nfl_id") %>%
  left_join(max_engaged_calc,     by = "primary_nfl_id") %>%
  left_join(max_close_calc,       by = "primary_nfl_id") %>%
  left_join(longest_engaged_calc, by = "primary_nfl_id") %>%
  left_join(longest_close_calc,   by = "primary_nfl_id") %>%
  left_join(frames_engaged_calc,  by = "primary_nfl_id") %>%
  left_join(frames_close_calc,    by = "primary_nfl_id") %>%
  select(game_id, play_id, nfl_id = primary_nfl_id,  everything())
  

print(row_num)

  player_proximity <<- bind_rows(player_proximity, intermediate)
}

calculate_player_proximity_19 <- function(row_num){
  
  primary_player_df <- primary_player_df_19
  
  opposing_player_df <- tracking_19 %>%
    filter(game_id %in% primary_player_df$game_id[[row_num]] & play_id %in% primary_player_df$play_id[[row_num]] &
             !nfl_id %in% primary_player_df$nfl_id[[row_num]]) %>% 
    filter(!team %in% primary_player_df$home[[row_num]] & ! is.na(nfl_id)) %>%
    select(nfl_id, frame_id, x, y)
  
  primary_player_frames <- tracking_19 %>% 
    filter(game_id %in% primary_player_df$game_id[[row_num]], play_id %in% primary_player_df$play_id[[row_num]],
           nfl_id %in% primary_player_df$nfl_id[[row_num]]) %>% 
    mutate(primary_nfl_id = nfl_id,
           primary_x = x,
           primary_y = y) %>% 
    select(-nfl_id, -x, -y)
  
  temp <- opposing_player_df %>%
    left_join(y = primary_player_frames, by = c("frame_id")) %>%
    rowwise() %>%
    mutate(opponent_engaged = case_when(abs(primary_x - x) < .5 & abs(primary_y - y) < .5 ~ 1, TRUE ~ 0),
           opponent_close   = case_when(abs(primary_x - x) <  3 & abs(primary_y - y) <  3 ~ 1, TRUE ~ 0)) 
  
  
  
  engaged_player_calc <- temp %>%
    group_by(primary_nfl_id, game_id, play_id, nfl_id) %>%
    summarise(engaged_player_count = case_when(sum(opponent_engaged) > 0 ~ 1, TRUE ~ 0)) %>%
    ungroup() %>%
    mutate(engaged_player_count = sum(engaged_player_count)) %>%
    select(game_id, play_id, primary_nfl_id, engaged_player_count) %>%
    slice(1)
  
  max_engaged_calc <- temp %>%
    group_by(primary_nfl_id, frame_id) %>%
    summarise(max_engaged = sum(opponent_engaged)) %>%
    ungroup() %>%
    arrange(desc(max_engaged)) %>%
    select(primary_nfl_id, max_engaged) %>%
    slice(1)
  
  longest_engaged_calc <- temp %>%
    group_by(primary_nfl_id, nfl_id) %>%
    summarise(longest_engaged = sum(opponent_engaged)) %>%
    ungroup() %>%
    arrange(desc(longest_engaged)) %>%
    select(primary_nfl_id, longest_engaged) %>%
    slice(1)
  
  frames_engaged_calc <- temp %>%
    group_by(primary_nfl_id, frame_id) %>%
    summarise(engaged_frame = case_when(sum(opponent_engaged) > 0 ~ 1, TRUE ~ 0)) %>%
    summarise(per_frames_engaged = sum(engaged_frame) / max(temp$frame_id))
  
  close_player_calc <- temp %>%
    group_by(primary_nfl_id, nfl_id) %>%
    summarise(close_player_count = case_when(sum(opponent_close) > 0 ~ 1, TRUE ~ 0)) %>%
    ungroup() %>%
    mutate(close_player_count = sum(close_player_count)) %>%
    select(primary_nfl_id, close_player_count) %>%
    slice(1)
  
  max_close_calc <- temp %>%
    group_by(primary_nfl_id, frame_id) %>%
    summarise(max_close = sum(opponent_close)) %>%
    ungroup() %>%
    arrange(desc(max_close)) %>%
    select(primary_nfl_id, max_close) %>%
    slice(1)
  
  longest_close_calc <- temp %>%
    group_by(primary_nfl_id, nfl_id) %>%
    summarise(longest_close = sum(opponent_close)) %>%
    ungroup() %>%
    arrange(desc(longest_close)) %>%
    select(primary_nfl_id, longest_close) %>%
    slice(1)
  
  frames_close_calc <- temp %>%
    group_by(primary_nfl_id, frame_id) %>%
    summarise(close_frame = case_when(sum(opponent_close) > 0 ~ 1, TRUE ~ 0)) %>%
    summarise(per_frames_close = sum(close_frame) / max(temp$frame_id))
  
  intermediate <<- engaged_player_calc %>%
    left_join(close_player_calc,    by = "primary_nfl_id") %>%
    left_join(max_engaged_calc,     by = "primary_nfl_id") %>%
    left_join(max_close_calc,       by = "primary_nfl_id") %>%
    left_join(longest_engaged_calc, by = "primary_nfl_id") %>%
    left_join(longest_close_calc,   by = "primary_nfl_id") %>%
    left_join(frames_engaged_calc,  by = "primary_nfl_id") %>%
    left_join(frames_close_calc,    by = "primary_nfl_id") %>%
    select(game_id, play_id, nfl_id = primary_nfl_id,  everything())
  
  
  print(row_num)
  
  player_proximity <<- bind_rows(player_proximity, intermediate)
}

calculate_player_proximity_20 <- function(row_num){
  
  primary_player_df <- primary_player_df_20
  
  opposing_player_df <- tracking_20 %>%
    filter(game_id %in% primary_player_df$game_id[[row_num]] & play_id %in% primary_player_df$play_id[[row_num]] &
             !nfl_id %in% primary_player_df$nfl_id[[row_num]]) %>% 
    filter(!team %in% primary_player_df$home[[row_num]] & ! is.na(nfl_id)) %>%
    select(nfl_id, frame_id, x, y)
  
  primary_player_frames <- tracking_20 %>% 
    filter(game_id %in% primary_player_df$game_id[[row_num]], play_id %in% primary_player_df$play_id[[row_num]],
           nfl_id %in% primary_player_df$nfl_id[[row_num]]) %>% 
    mutate(primary_nfl_id = nfl_id,
           primary_x = x,
           primary_y = y) %>% 
    select(-nfl_id, -x, -y)
  
  temp <- opposing_player_df %>%
    left_join(y = primary_player_frames, by = c("frame_id")) %>%
    rowwise() %>%
    mutate(opponent_engaged = case_when(abs(primary_x - x) < .5 & abs(primary_y - y) < .5 ~ 1, TRUE ~ 0),
           opponent_close   = case_when(abs(primary_x - x) <  3 & abs(primary_y - y) <  3 ~ 1, TRUE ~ 0)) 
  
  
  
  engaged_player_calc <- temp %>%
    group_by(primary_nfl_id, game_id, play_id, nfl_id) %>%
    summarise(engaged_player_count = case_when(sum(opponent_engaged) > 0 ~ 1, TRUE ~ 0)) %>%
    ungroup() %>%
    mutate(engaged_player_count = sum(engaged_player_count)) %>%
    select(game_id, play_id, primary_nfl_id, engaged_player_count) %>%
    slice(1)
  
  max_engaged_calc <- temp %>%
    group_by(primary_nfl_id, frame_id) %>%
    summarise(max_engaged = sum(opponent_engaged)) %>%
    ungroup() %>%
    arrange(desc(max_engaged)) %>%
    select(primary_nfl_id, max_engaged) %>%
    slice(1)
  
  longest_engaged_calc <- temp %>%
    group_by(primary_nfl_id, nfl_id) %>%
    summarise(longest_engaged = sum(opponent_engaged)) %>%
    ungroup() %>%
    arrange(desc(longest_engaged)) %>%
    select(primary_nfl_id, longest_engaged) %>%
    slice(1)
  
  frames_engaged_calc <- temp %>%
    group_by(primary_nfl_id, frame_id) %>%
    summarise(engaged_frame = case_when(sum(opponent_engaged) > 0 ~ 1, TRUE ~ 0)) %>%
    summarise(per_frames_engaged = sum(engaged_frame) / max(temp$frame_id))
  
  close_player_calc <- temp %>%
    group_by(primary_nfl_id, nfl_id) %>%
    summarise(close_player_count = case_when(sum(opponent_close) > 0 ~ 1, TRUE ~ 0)) %>%
    ungroup() %>%
    mutate(close_player_count = sum(close_player_count)) %>%
    select(primary_nfl_id, close_player_count) %>%
    slice(1)
  
  max_close_calc <- temp %>%
    group_by(primary_nfl_id, frame_id) %>%
    summarise(max_close = sum(opponent_close)) %>%
    ungroup() %>%
    arrange(desc(max_close)) %>%
    select(primary_nfl_id, max_close) %>%
    slice(1)
  
  longest_close_calc <- temp %>%
    group_by(primary_nfl_id, nfl_id) %>%
    summarise(longest_close = sum(opponent_close)) %>%
    ungroup() %>%
    arrange(desc(longest_close)) %>%
    select(primary_nfl_id, longest_close) %>%
    slice(1)
  
  frames_close_calc <- temp %>%
    group_by(primary_nfl_id, frame_id) %>%
    summarise(close_frame = case_when(sum(opponent_close) > 0 ~ 1, TRUE ~ 0)) %>%
    summarise(per_frames_close = sum(close_frame) / max(temp$frame_id))
  
  intermediate <<- engaged_player_calc %>%
    left_join(close_player_calc,    by = "primary_nfl_id") %>%
    left_join(max_engaged_calc,     by = "primary_nfl_id") %>%
    left_join(max_close_calc,       by = "primary_nfl_id") %>%
    left_join(longest_engaged_calc, by = "primary_nfl_id") %>%
    left_join(longest_close_calc,   by = "primary_nfl_id") %>%
    left_join(frames_engaged_calc,  by = "primary_nfl_id") %>%
    left_join(frames_close_calc,    by = "primary_nfl_id") %>%
    select(game_id, play_id, nfl_id = primary_nfl_id,  everything())
  
  
  print(row_num)
  
  player_proximity <<- bind_rows(player_proximity, intermediate)
}

primary_player_df_18 <- data_combined_18 %>%
  filter(is_returner %in% 1 & special_teams_result %in% "Return") %>%
  distinct(game_id, play_id, nfl_id, home)

{
tictoc::tic()
primary_player_df_18$nfl_id %>% 
  enframe() %>% 
  pull(name) %>% 
  walk(calculate_player_proximity_18)
tictoc::toc()
} 

primary_player_df_19 <- data_combined_19 %>%
  filter(is_returner %in% 1 & special_teams_result %in% "Return") %>%
  distinct(game_id, play_id, nfl_id, home)

{
  tictoc::tic()
  primary_player_df_19$nfl_id %>% 
    enframe() %>% 
    pull(name) %>% 
    walk(calculate_player_proximity_19)
  tictoc::toc()
  }

primary_player_df_20 <- data_combined_20 %>%
  filter(is_returner %in% 1 & special_teams_result %in% "Return") %>%
  distinct(game_id, play_id, nfl_id, home)

{
  tictoc::tic()
  primary_player_df_20$nfl_id %>% 
    enframe() %>% 
    pull(name) %>% 
    walk(calculate_player_proximity_20)
  tictoc::toc()
  }


player_proximity_final <- player_proximity %>% 
  filter(! is.na(nfl_id)) 


full_sample_returns <- player_proximity_final %>% 
  left_join(y = data_combined_final, by = c("game_id", "play_id", "nfl_id"))

training_set_returns <- full_sample_returns %>% 
  filter(injured %in% 1)


full_sample_returns %>% 
   write_csv("data/kick_returns.csv")


## Create population modeling set [+]
## Complete first draft final modeling set []
## Expand population modeling set with feature selection [Never over]
## Model Research []
## Write Model Code [] 
## Review Model Quality []
## Visualize Results []
## Write-up Report [] 


