################################# PART 1: PATTERNS AND PREDICTIONS #################################
# Main goal was to determine what factors or variables are the most important when it comes to
# regular season conference wins?

# Read in past years' data for conference statistics (all teams) game-by-game
df <- read.csv("/Users/**user_name**/Documents/Season_CAA_Stats_1.csv", header = TRUE, 
                 sep = ",", quote = "\"", dec = ".", 
                 fill = TRUE, comment.char = "", 
                 as.is = TRUE, na.strings = ".")
df <- as.data.frame(df)

# Package for the linear model variable importance
library(relaimpo)
# Utilize base variables provided in data
lmModel1 <- lm(Win ~ AB + R + H + RBI + X2B + X3B + HR + BB + SB + CS + HBP + 
              SH + SF + SO + KL + GDP + PO + A + AB_Opp + R_Opp + H_Opp + RBI_Opp + X2B_Opp + 
              X3B_Opp + HR_Opp + BB_Opp + SB_Opp + CS_Opp + HBP_Opp + 
              SH_Opp + SF_Opp + SO_Opp + KL_Opp + GDP_Opp + PO_Opp + A_Opp , data = df)

# Calculate relative importance of variables scaled to 100
relImportance <- calc.relimp(lmModel1, type = "lmg", rela = TRUE)
# Display variables according to their importance
sort(relImportance$lmg, decreasing=TRUE)

# Read in past years' data for conference statistics (all teams) season-by-season
totals <- read.csv("/Users/**user_name**/Documents/Total_CAA_Stats_FINAL.csv", header = TRUE, 
                   sep = ",", quote = "\"", dec = ".", 
                   fill = TRUE, comment.char = "", 
                   as.is = TRUE, na.strings = ".")
totals <- as.data.frame(totals)

# Random sampling
samplesize = 0.60 * nrow(totals)
set.seed(80)
index = sample(seq_len(nrow(totals)), size = samplesize)

# Create training and test set
datatrain = totals[ index, ]
datatest = totals[ -index, ]

# Now utilize linear model for total season wins as response variable
lmModel2 <- lm(CAA_Wins ~ BA + Hit_AB + Pitch_AB + RA + R + Hits_Given_Up + H + 
              X2B_Given_Up + X2B + X3B_Given_Up + X3B + HR_Given_Up + HR + RBI + SLG. + 
              Pitch_BB + Pitch_HBP + HBP + SO.I.+ SO + GDP + OB. + SF + SH + Stolen_Base., data = datatrain, singular.ok = FALSE)

# Calculate relative importance of variables scaled to 100
relImportance0 <- calc.relimp(lmModel2, type = c("lmg"), rela = TRUE)
# Display variables according to their importance
sort(relImportance0$lmg, decreasing=TRUE)

# Predict values with test set
predictions <- predict(lmModel2, datatest)
# Also look at residuals of model
res <- residuals(lmModel2)

# Use PCA for variance of data - how do the CAA teams differ from each other, and year-to-year?
# Use totals dataframe, which is total season data
library(devtools)
library(factoextra)
library(stat)
pca1 <- prcomp(totals, center = TRUE, scale. = TRUE)
pca_rotations <- pca1$rotation
# Compute standard deviation of each principal component
std_dev <- pca1$sdev
# Compute variance of each principal component
pca_var <- std_dev^2
# Proportion of variance explained
prop_varex <- pca_var/sum(pca_var)
# Scree plot
plot(prop_varex, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")
# Cumulative scree plot
plot(cumsum(prop_varex), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")
# Display % of variance explained by each component
fviz_eig(pca1)

################################# PART 2: PITCH-BY-PITCH DATA ANALYSIS #################################

library(dplyr)
# read in ALL season's iScoreSports HTML files
# the read_parse_iscore_pbp was worked out with Bill Petti (baseballR)

setwd("~/Desktop/TU_Baseball_Files/2019_HTML_Data_Files/html_files_2019")
Filelist <- list.files()
all_tu_files <- data.frame()
for(file in Filelist){
  new_df <- read_parse_iscore_pbp(file)
  new_df$game_id <- rep(file, nrow(new_df))
  all_tu_files <- rbind(all_tu_files, new_df)
}

# Add pitch number
all_tu_files <- all_tu_files %>% group_by(game_date, away_team, home_team, inning, inning_top_bot, pitcher_name, batter_name) %>% mutate(pitch = seq(n())) %>% ungroup()
# Clean up several columns and add some new variables for better identification
all_tu_files <- all_tu_files %>% group_by(game_date, away_team, home_team, inning, inning_top_bot, pitcher_name, batter_name, pitch) %>% mutate(count_correct = gsub(" ", "", case_when(grepl("\\d", count) ~ str_extract(count, "[0-9][ ][-][ ][0-9]"))),
                                                                                                                                                pitch_type = gsub("[^a-zA-Z]", "", count),
                                                                                                                                                clean_pitch_types = str_remove(pitch_type, "mph")) %>% ungroup()
all_tu_files <- all_tu_files %>% mutate(count_correct = ifelse(count != "" & is.na(count_correct), lag(count_correct), count_correct),
                                        batting = ifelse(inning_top_bot == "top", as.character(away_team), as.character(home_team)),
                                        pitching = ifelse(inning_top_bot == "bot", as.character(away_team), as.character(home_team)),
                                        home_text = ifelse(as.character(home_team) == batting, description,""),
                                        away_text = ifelse(as.character(away_team) == batting, description,""),
                                        year = lubridate::year(game_date))
# Re-order the file
all_tu_files <- all_tu_files %>% arrange(game_date, inning, match(inning_top_bot, c("top", "bot")), pitcher_name, batter_name, pitch)

# Function to mutate several new variables to help with classification and odentification
# Created from base of Dave Miller's NCAA parser function
# More variables to be added
ncaa_parse_adj <- function(pbp_data_frame){
  
  pbp_data_frame <- pbp_data_frame
  pbp_data_frame=pbp_data_frame%>%
    mutate(
      tmp_text=description,
      #  # 
      sub_fl=case_when(
        str_detect(tmp_text, '(singled|doubled|tripled|homered|walked|reached|struck out|grounded|flied|lined|popped| hit|infield fly|infield fly|out|double play|triple play)')==TRUE & str_detect(tmp_text, c('pinch hit'))==FALSE ~ 0,
        str_detect(tmp_text, c('to (p|c|1b|2b|3b|ss|lf|rf|cf|dh)'))==TRUE ~ 1,
        str_detect(tmp_text, c('pinch hit'))==TRUE ~ 1,
        str_detect(tmp_text, c('pinch ran'))==TRUE ~ 1,
        TRUE ~ 0),
      
      # Split the text up
      bat_text=gsub('(;|3a|:).*$','', tmp_text),
      
      r1_text=case_when(
        str_detect(tmp_text, '(;|3a|:)')==TRUE ~ stripwhite(gsub('^.*?(;|3a|:)','',tmp_text)),
        TRUE~''),
      
      r2_text=case_when(
        str_detect(r1_text, '(;|3a|:)')==TRUE ~ stripwhite(gsub('^.*?(;|3a|:)','',r1_text)),
        TRUE~''),
      
      r3_text=case_when(
        str_detect(r2_text, '(;|3a|:)')==TRUE ~ stripwhite(gsub('^.*?(;|3a|:)','',r2_text)),
        TRUE~''),
      
      r2_text=stripwhite(gsub('(;|3a|:).*$','',r2_text)),
      
      r1_text=stripwhite(gsub('(;|3a|:).*$','',r1_text)),
      
      # Event code: same as Retrosheet
      event_cd=case_when(
        sub_fl==1 ~ 1,
        str_sub(stripwhite(tmp_text),1,1)=='(' ~ 1,
        # str_detect(tmp_text, '(hitting out of turn| for |No play|halted|delay|postponed|ejected|suspended|coach|sunny|review|challenged|HC|\\*\\*)') == TRUE ~ 1,
        str_detect(tmp_text,'struck out|strikes out looking|strikes out') == TRUE ~ 3,
        str_detect(tmp_text,'stole') == TRUE ~ 4,
        (str_detect(tmp_text,'(caught stealing|out at second c to|out at third c to)') == TRUE) & (str_detect(tmp_text,'(bunt|grounded)') == FALSE) ~ 6,
        str_detect(tmp_text,'picked off') == TRUE ~ 8,
        str_detect(tmp_text,'wild pitch') == TRUE ~ 9,
        str_detect(tmp_text,'passed ball') == TRUE ~ 10,
        str_detect(tmp_text,'balk') == TRUE ~ 11,
        str_detect(tmp_text,'Dropped foul') == TRUE ~ 13,
        str_detect(tmp_text,'walked') == TRUE ~ 14,
        str_detect(tmp_text,'hit by pitch') == TRUE ~ 16,
        str_detect(tmp_text,'interference') == TRUE ~ 17,
        str_detect(tmp_text,'error') == TRUE ~ 18,
        str_detect(tmp_text,'muffed') == TRUE ~ 18,
        str_detect(tmp_text,'dropped') == TRUE ~ 18,
        str_detect(tmp_text,'fielder\'s choice') == TRUE ~ 19,
        str_detect(tmp_text,'singled|for a single') == TRUE ~ 20,
        str_detect(tmp_text,'doubled|double') == TRUE ~ 21,
        str_detect(tmp_text,'tripled|triple') == TRUE ~ 22,
        str_detect(tmp_text,'homered|homerun') == TRUE ~ 23,
        str_detect(tmp_text, '(flied out|grounded out|grounds out|lines out|flies out|pops up|popped|fouled out|lined out| infield fly|double play|triple play|out at (first|second|third|home))') == TRUE ~ 2,
        str_detect(tmp_text, 'advanced') == TRUE ~ 12,
        TRUE ~ 0),
      
      
      # Batter name
      bat_name= case_when(
        event_cd %in% c(0,1)~'',
        str_detect(bat_text, '(Batter|Runner\'s interference)')==TRUE ~'',
        str_detect(bat_text, '(walked|walks|singled|singles|doubled|doubles|tripled|triples|reached|reaches|struck out|strikes out|grounded out|grounds out)')==FALSE & str_detect(bat_text, '(advanced|caught stealing|stole|picked off|out at (first|second|third|home)|tagged out)')==TRUE ~ '',
        str_detect(bat_text, '(singled|singles|doubled|doubles|tripled|triples|homered|homerun|walked|walks|reached|reaches|struck out|strikes out|grounded|grounds out|flied|flies out|lined|lines out|popped|pops up|hit | out |fouled out|pinch hit|infield fly|intentionally walked|was intentionally walked|fouled into double play)')==TRUE ~ gsub('((singled|doubled|tripled|homered|walked|reached|struck out|grounded|flied|lined|popped|hit | out |fouled out|pinch hit|infield fly|intentionally walked|was intentionally walked|fouled into double play).*$)', '', bat_text),
        str_detect(stripwhite(r1_text), 'caught stealing  c to (2b|3b), double play.')==TRUE ~ bat_text,
        TRUE ~ ''),
      
      # Was a substitution made?
      sub_in= case_when(
        sub_fl==1&str_detect(bat_text, 'to (p|c|1b|2b|3b|ss|lf|rf|cf|dh)')==TRUE ~ stripwhite(gsub('(to (p|c|1b|2b|3b|ss|lf|rf|cf|dh).*$)', '', bat_text)),
        sub_fl==1&str_detect(bat_text, 'pinch ran for')==TRUE ~ stripwhite(gsub('pinch ran for.*$', '', bat_text)),
        sub_fl==1&str_detect(bat_text, 'pinch hit for')==TRUE ~ stripwhite(gsub('pinch hit for.*$', '', bat_text)),
        TRUE ~ ''),
      
      # Did a player sub out?
      sub_out= case_when(
        sub_fl==1&str_detect(bat_text, 'to (p|c|1b|2b|3b|ss|lf|rf|cf|dh) for')==TRUE ~ gsub('^.*to (p|c|1b|2b|3b|ss|lf|rf|cf|dh) for', '', bat_text),
        sub_fl==1&str_detect(bat_text, 'pinch ran for')==TRUE ~ gsub('^.*pinch ran for', '', bat_text),
        sub_fl==1&str_detect(bat_text, 'pinch hit')==TRUE ~ gsub('^.*pinch hit for', '', bat_text),
        TRUE ~ ''),
      # Clean up the "sub out" column
      sub_out=strip_punc(sub_out)
      )
}

