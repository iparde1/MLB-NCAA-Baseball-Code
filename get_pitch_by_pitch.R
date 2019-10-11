library(pitchRx)
library(dplyr)
library(devtools)
library(githubinstall)
install_github("keberwein/mlbgameday")
library(mlbgameday)

# Read in MLB PITCHf/x data
dat_2018 <- get_payload(start = "2018-03-29", end = "2018-09-30")
dat_2017 <- get_payload(start = "2017-04-02", end = "2017-10-01")

# Function to get data 
get_pitch_by_pitch <- function(df, INPUT_pitcher){
  require(dplyr)
  # Get pitching data
  datp <- df$pitch
  # split the date and time pitching dataframe to be able to work with pitcher pace
  datp <- datp %>% mutate(DateTime = str_replace(sv_id, "_", ""), RealDateTime = lubridate::ymd_hms(DateTime))
  
  # Add the game date
  datp <- datp %>% mutate(GAME_DATE = as.Date(substr(gameday_link, 5, 14), "%Y_%m_%d"))
  # Select the locations (pitch) with other variables, including game date
  locations <- datp %>% select_("GAME_DATE", "inning", "pitch_type", "start_speed", "px", "pz", "des", "num", "gameday_link", "start_speed",
                                 "end_speed", "break_length", "spin_rate", "zone", "RealDateTime", "count")
  
  # Get batting data
  datb <- df$atbat
  # Add the game date
  datb <- datb %>% mutate(GAME_DATE = as.Date(date))
  # Select the names of pitchers and batters with game date
  names <- datb %>% select_("GAME_DATE", "inning", "pitcher", "batter", "pitcher_name", "batter_name", "num", "gameday_link", "event", "stand")
  
  # Combine the batting and pitching data based on pitcher name, game date, pitch number, and game id
  data <- names %>% filter(pitcher_name == INPUT_pitcher) %>% inner_join(locations, ., by = c("GAME_DATE", "num", "gameday_link"))
  # add variable to determine how many innings pitcher went in their appearance
  data %>%
    group_by(GAME_DATE) %>% arrange(inning.x) %>%
    mutate(INN_NUM = last(inning.x) - first(inning.x) + 1) -> pitch_by_pitch_data   
  
  return(pitch_by_pitch_data)
}