#load libraries
library(tidyverse)
library(paws)
library(elo)
library(shiny)
library(shinythemes)
library(shinyWidgets)

options(scipen=99999)
#connect to aws

readRenviron("/srv/shiny-server/rhs/.Renviron")

access_key <- Sys.getenv("AWS_ACCESS_KEY_ID")
secret_key <- Sys.getenv("AWS_SECRET_ACCESS_KEY")
region <- Sys.getenv("AWS_REGION")

s3 <- paws::s3()

bucket = "sehrlichhackathon2023"

#get schedule info
league_item <- s3$get_object(Bucket="sehrlichhackathon2023", Key="leagues_flat.csv")
player_item <- s3$get_object(Bucket="sehrlichhackathon2023", Key="players_flat.csv")
team_item <- s3$get_object(Bucket="sehrlichhackathon2023", Key="teams_flat.csv")
tournament_data_item <- s3$get_object(Bucket="sehrlichhackathon2023", Key="tournament_data_flat.csv")
mapping_data_item <- s3$get_object(Bucket="sehrlichhackathon2023", Key="mapping_data_flat.csv")
game_data_team_stat_item <- s3$get_object(Bucket="sehrlichhackathon2023", Key="games_data_team_stats.csv")
game_data_flat_item <- s3$get_object(Bucket="sehrlichhackathon2023", Key="games_data_flat.csv")
game_info_flat_item <- s3$get_object(Bucket="sehrlichhackathon2023", Key="games_info_flat.csv")


# #schedule info to df 
leagues <- league_item$Body %>% rawToChar() %>% read.csv(text = ., stringsAsFactors = FALSE, colClasses = c(leagueid = "character", tournamentid = "character"))
players <- player_item$Body %>% rawToChar() %>% read.csv(text = ., stringsAsFactors = FALSE, colClasses = c(player_id = "character", home_team_id = "character"))
teams <- team_item$Body %>% rawToChar() %>% read.csv(text = ., stringsAsFactors = FALSE, colClasses = c(team_id = "character"))
tournaments <- tournament_data_item$Body %>% rawToChar() %>% read.csv(text = ., stringsAsFactors = FALSE,
                                                                      colClasses = c(id = "character", leagueid = "character",matchid = "character", teamid = "character", gameid = "character"))
mapping_data <- mapping_data_item$Body %>% rawToChar() %>% read.csv(text = ., stringsAsFactors = FALSE, 
                                                                    colClasses = c(esportsgameid = "character", red_side_team_id = "character", blue_side_team_id = "character",
                                                                                                                           blue_side_top = "character", blue_side_jg = "character", blue_side_mid = "character",
                                                                                                                           blue_side_adc = "character", blue_side_sup = "character", red_side_top = "character",
                                                                                                                           red_side_jg = "character", red_side_mid = "character", red_side_adc = "character",
                                                                                                                           red_side_sup = "character"))
games_data_teams <-game_data_team_stat_item$Body %>% rawToChar() %>% read.csv(text = ., stringsAsFactors = FALSE)
games_data <- game_data_flat_item$Body %>% rawToChar() %>% read.csv(text = ., stringsAsFactors = FALSE, colClasses = c(accountid = "character"))
games_info <- game_info_flat_item$Body %>% rawToChar() %>% read.csv(text = ., stringsAsFactors = FALSE, colClasses = c(accountid = "character"))




# leagues <- read.csv('/Users/sam/Desktop/personal/RIOT/flattened_data/leagues_flat.csv', colClasses = c(leagueid = "character", tournamentid = "character"))
# players <- read.csv('/Users/sam/Desktop/personal/RIOT/flattened_data/players_flat.csv', colClasses = c(player_id = "character", home_team_id = "character"))
# teams <- read.csv('/Users/sam/Desktop/personal/RIOT/flattened_data/teams_flat.csv', colClasses = c(team_id = "character"))
# tournaments <- read.csv('/Users/sam/Desktop/personal/RIOT/flattened_data/tournament_data_flat.csv',
#                         colClasses = c(id = "character", leagueid = "character",matchid = "character", teamid = "character", gameid = "character"))
# mapping_data <- read.csv('/Users/sam/Desktop/personal/RIOT/flattened_data/mapping_data_flat.csv',
#                          colClasses = c(esportsgameid = "character", red_side_team_id = "character", blue_side_team_id = "character",
#                                         blue_side_top = "character", blue_side_jg = "character", blue_side_mid = "character",
#                                         blue_side_adc = "character", blue_side_sup = "character", red_side_top = "character",
#                                         red_side_jg = "character", red_side_mid = "character", red_side_adc = "character",
#                                         red_side_sup = "character"))
# games_data_teams <- read.csv('/Users/sam/Desktop/personal/RIOT/flattened_data/games_data_team_stats.csv')
# games_data <- read.csv('/Users/sam/Desktop/personal/RIOT/flattened_data/games_data_flat.csv',colClasses = c(accountid = "character"))
# games_info <- read.csv('/Users/sam/Desktop/personal/RIOT/flattened_data/games_info_flat.csv',colClasses = c(accountid = "character"))

#2 mapped games have a - in their roles which needs to be changed to null
mapping_data_ <- mapping_data %>%
  mutate(red_side_sup = case_when(
    red_side_sup == '-' ~ NA,
    red_side_sup == '' ~ NA,
    TRUE ~ red_side_sup
  ),
  blue_side_adc = case_when(
    blue_side_adc == '-' ~ NA,
    blue_side_adc == '' ~ NA,
    TRUE ~ blue_side_adc
  ))



#pivot the wide data from multiple columns to a long format 
mapping_data_ <- mapping_data_ %>%
  dplyr::select(-c(teammapping,participantmapping)) %>%
  pivot_longer(!c(esportsgameid,platformgameid,red_side_team_id,blue_side_team_id), names_to = 'role', values_to = 'playerid' )

mapping_data_clean <- mapping_data_ %>%
  mutate(participant = case_when(
    role == 'blue_side_top' ~ 1,
    role == 'blue_side_jg' ~ 2,
    role == 'blue_side_mid' ~ 3,
    role == 'blue_side_adc' ~ 4,
    role == 'blue_side_sup' ~ 5,
    role == 'red_side_top' ~ 6,
    role == 'red_side_jg' ~ 7,
    role == 'red_side_mid' ~ 8,
    role == 'red_side_adc' ~ 9,
    role == 'red_side_sup' ~ 10,
    TRUE ~ 11
  ),
  side = case_when(
    participant == 1 | participant == 2 | participant == 3 | participant == 4 | participant == 5 ~ 100,
    participant == 6 | participant == 7 | participant == 8 | participant == 9 | participant == 10 ~ 200,
    TRUE ~ 0
  ),
  role = case_when(
    role == 'blue_side_top' ~ 'top',
    role == 'blue_side_jg' ~ 'jg',
    role == 'blue_side_mid' ~ 'mid',
    role == 'blue_side_adc' ~ 'adc',
    role == 'blue_side_sup' ~ 'sup',
    role == 'red_side_top' ~ 'top',
    role == 'red_side_jg' ~ 'jg',
    role == 'red_side_mid' ~ 'mid',
    role == 'red_side_adc' ~ 'adc',
    role == 'red_side_sup' ~ 'sup',
    TRUE ~ 'no role'
  ),
  teamid = ifelse(side == 100,blue_side_team_id,red_side_team_id)) %>%
  dplyr::select(-c(blue_side_team_id,red_side_team_id))

#first try to join games data to mapped data

mapped_games <- inner_join(mapping_data_clean, games_data, by = c("platformgameid", "participant" = "participantid", "side" = "teamid"))

teams <- teams %>%
  dplyr::rename(team_name = name,
         team_acronym = acronym,
         team_slug = slug)

#remove the duplicate TSM
teams <- teams %>%
  distinct()

mapped_games_ <- left_join(mapped_games, teams, by = c('teamid' = 'team_id'))

mapped_games_ <- mapped_games_ %>%
  mutate(playerid = case_when(
    is.na(playerid) ~ 'unknown',
    TRUE ~ playerid
  ))

#adjust abbes name to always be the same
players <- players %>%
  mutate(first_name = case_when(
    player_id == '100174547541438669' ~ 'Felix',
    TRUE ~ first_name
  ))

#now remove the duplicate abbes
players <- players %>%
  distinct()

mapped_games_ <- left_join(mapped_games_, players, by = c('playerid' = 'player_id'))

#impute values for missing names and handles
mapped_games_ <- mapped_games_ %>%
  mutate(handle = case_when(
    is.na(handle) ~ 'no handle',
    TRUE ~ handle
  ),
  first_name = case_when(
    is.na(first_name) ~ 'no first name',
    TRUE ~ first_name
  ),
  last_name = case_when(
    is.na(last_name) ~ 'no last name',
    TRUE ~ last_name
  ))

#change tournaments side from red/blue to 100/200 to match
tournaments <- tournaments %>%
  mutate(side = case_when(
    team_side == 'blue' ~ 100,
    team_side == 'red' ~ 200,
    TRUE ~ 0
  )) %>%
  dplyr::rename(tournament_id = id,
         tournament_name = name,
         tournament_slug = slug)

mapped_games_tournament <- inner_join(mapped_games_, tournaments, by = c('esportsgameid' = 'gameid', 'side', 'teamid'))

#19168 games able to be fully joined
full_cnt<- mapped_games_tournament %>%
  group_by(esportsgameid) %>%
  summarise(cnt = n())

mapped_games_tournament_cnt <- inner_join(mapped_games_tournament,full_cnt, by = c('esportsgameid'))

#removing games with less than 10 participants
mapped_games_tournament_ <- mapped_games_tournament_cnt %>%
  filter(cnt == 10)

#selecting 1 of the 2 to keep
high_cnt <- mapped_games_tournament_cnt %>%
  filter(cnt > 10 & gametime == 2398958)

#join back to original
mapped_games_tournament <- rbind(mapped_games_tournament_,high_cnt)

#now all games have been cleaned and can be divided by role and modeled
games_data_teams <- games_data_teams %>%
  dplyr::rename(champions_killed_team = champions_killed)

#join team stats from each game to
mapped_games_tournament <- left_join(mapped_games_tournament, games_data_teams, by = c("eventtime","eventtype","platformgameid","side" = "team_side"))

mgt <- mapped_games_tournament %>%
  mutate(startdate = as.Date(startdate),
         enddate = as.Date(enddate),
         year = year(startdate)) %>%
  dplyr::select(-c(cnt)) %>%
  group_by(tournament_id) %>%
  arrange((startdate))


#add in champion names and playstyles 
assassins <- c('Ahri','Akali','Briar','Diana','Ekko','Evelynn','Fizz',
               'Elise','Kassadin','Katarina','Kayn','Khazix',
               'Leblanc','MasterYi','Naafiri','Nocturne','Pantheon',
               'Pyke','Qiyana','Rengar','RekSai',
               'Shaco','Talon','Zed')

fighters <- c('Aatrox','Belveth','Camille','Darius',
              'Fiora','Jax','LeeSin','Gangplank',
              'Garen','Gnar','Gragas','Graves','Gwen','Hecarim',
              'Illaoi','Irelia','JarvanIV','Jax','Jayce',
              'KSante','Kayle','Kled','Mordekaiser','Nasus',
              'Olaf','Renekton','Riven','Sett','Shyvana',
              'Singed','Trundle','Tryndamere','Udyr',
              'Urgot','Vi','Viego','Volibear','Warwick',
              'Wukong','XinZhao','Yasuo','Yone','Yorick')

mages <- c('Anivia','Annie','AurelionSol','Azir','Brand',
           'Cassiopeia','FiddleSticks','Heimerdinger','Kennen','Karthus','Lillia',
           'Lissandra','Lux','Malzahar','Morgana','Neeko',
           'Nidalee','Orianna','Rumble','Ryze','Swain','Sylas',
           'Syndra','Taliyah','TwistedFate','Veigar','Velkoz',
           'Vex','Viktor','Vladimir','Xerath','Ziggs','Zoe',
           'Zyra')

marksman <- c('Akshan','Aphelios','Ashe','Caitlyn','Corki',
              'Draven','Ezreal','Jhin','Jinx','Kaisa','Kalista',
              'Kindred','KogMaw','Lucian','MissFortune','Quinn',
              'Samira','Senna','Nilah','Sivir','Teemo','Tristana',
              'Twitch','Vayne','Varus','Xayah','Zeri')

tanks <- c('Alistar','Amumu','Blitzcrank','Braum','Chogath','DrMundo',
           'Galio','Leona','Malphite','Maokai','Nautilus','Nunu','Ornn',
           'Poppy','Rammus','Rell','Sejuani','Shen','Sion','Skarner',
           'TahmKench','Thresh','Zac')

enchanters <- c('Taric','Bard','Ivern','Janna','Karma','Lulu',
                'Nami','Soraka','Sona','Seraphine','Renata',
                'Yuumi','Zilean','Milio','Rakan')

games_info_ <- games_info %>%
  mutate(champion = ifelse(champion == 'MonkeyKing','Wukong',champion),
         playstyle = case_when(
           champion %in% assassins ~ 'assassin',
           champion %in% fighters ~ 'fighter',
           champion %in% mages ~ 'mage',
           champion %in% marksman ~ 'marksman',
           champion %in% tanks ~ 'tank',
           champion %in% enchanters ~ 'enchanter',
           TRUE ~ 'missing playstyle'
         ),
         playstyle = factor(playstyle, level = unique(playstyle))) %>%
  dplyr::select(platformgameid,teamid,participantid,champion,summonername,playstyle)

#2 games without champion names
mgt_ <- left_join(mgt,games_info_, by = c("platformgameid","side"="teamid","participant" = "participantid"))

#remove the 2 LPLB games that do not have champions recorded
mgt <- mgt_ %>%
  filter(!is.na(champion))

#5 dates that cant be converted - will manually convert to the correct date
mgt <- mgt %>%
  mutate(datetime = as.POSIXct(eventtime, format = "%Y-%m-%dT%H:%M:%OSZ"),
         datetime = case_when(
           eventtime == '2020-03-08T02:10:43.899Z' ~  as.POSIXct('2020-03-08', format = "%Y-%m-%d"),
           eventtime == '2021-03-14T02:24:07.521Z' ~  as.POSIXct('2021-03-14', format = "%Y-%m-%d"),
           eventtime == '2022-03-13T02:30:56.402Z' ~  as.POSIXct('2022-03-13', format = "%Y-%m-%d"),
           eventtime == '2023-03-12T02:29:58.015Z' ~  as.POSIXct('2023-03-12', format = "%Y-%m-%d"),
           eventtime == '2023-03-12T02:49:17.935Z' ~  as.POSIXct('2023-03-12', format = "%Y-%m-%d"),
           TRUE ~ datetime
         )) %>%
  arrange(datetime) %>%
  ungroup()

mgt <- mgt %>%
  filter(game_state != 'unstarted' & game_state != 'unneeded' & outcome != 'tie' & leagueid != '98767991295297326')

mgt_filt <- mgt %>%
  dplyr::select(-c(gamename,accountid,gameover,eventtype,sequenceindex,team_slug,home_team_id,
                   tournament_slug,sport,enddate,stage_slug,game_state,match_type,game_state,
                   game_number,eventtime,matchid,team_side))

mgt_elo_df <- mgt_filt %>%
  dplyr::select(leagueid,tournament_name,tournament_id,esportsgameid,
                platformgameid,datetime,teamid,team_name,side,playerid,role,
                participant,handle,first_name,
                last_name,outcome)

ties <- mgt_elo_df %>%
  filter(outcome == 'tie')

wide_elo_df <- pivot_wider(mgt_elo_df,
                           names_from  = participant,
                           names_sort  = TRUE,
                           values_from = c(team_name,side,handle,role,playerid,first_name,last_name, outcome, teamid))

#shrink the size of some columns
wide_elo_df <- wide_elo_df %>%
  dplyr::rename(home_team = team_name_1,
         away_team = team_name_6,
         home_team_side = side_1,
         away_team_side = side_6,
         home_team_outcome = outcome_1,
         away_team_outcome = outcome_6,
         home_team_id = teamid_1,
         away_team_id = teamid_6) %>%
  dplyr::select(-c(team_name_2,team_name_3,team_name_4,team_name_5,team_name_7,team_name_8,team_name_9,team_name_10,
                   side_2,side_3,side_4,side_5,side_7,side_8,side_9,side_10,
                   outcome_2,outcome_3,outcome_4,outcome_5,outcome_7,outcome_8,outcome_9,outcome_10,
                   teamid_2,teamid_3,teamid_4,teamid_5,teamid_7,teamid_8,teamid_9,teamid_10)) %>%
  mutate(home_team_win = ifelse(home_team_outcome == 'win',1,0),
         away_team_win = ifelse(home_team_win == 1,0,1))

#18413 games with all info available
wide_elo_df_filt <- wide_elo_df %>%
  filter(!is.na(home_team) & !is.na(away_team) & handle_1 != 'no handle'
         & handle_2 != 'no handle' & handle_3 != 'no handle' & handle_4 != 'no handle'
         & handle_5 != 'no handle' & handle_6 != 'no handle' & handle_7 != 'no handle'
         & handle_8 != 'no handle' & handle_9 != 'no handle' & handle_10 != 'no handle')


worlds <- c('98767975604431411')

# #missing id for OMG,TT,WE,RA,NIP,AL,UP
current_top_region_teams <- c('100725845018863243','99566404585387054','100205573495116443','100205573496804586',
                              '99566404579461230','99566404581868574','100725845022060229','102747101565183056',
                              '105505619546859895','98767991853197861','99566404852189289','98767991882270868',
                              '99566404850008779','99566404853058754','99566404853854212','99566404854685458',
                              '98767991892579754','99566404848691211','99566404846951820','99566404855553726',
                              '99294153828264740','98767991877340524','98926509884398584','98926509883054987',
                              '103461966951059521','98926509892121852','99294153824386385','98767991930907107',
                              '106972778172351142','98767991860392497','98926509885559666','101383793569248484',
                              '101383793572656373','98767991866488695','98767991926151025','101383793574360315',
                              '103461966965149786','101383793567806688','107563714667537640','109637393694097670',
                              '99322214695067838')

#give them slight bonus to account for their missing games played #missing id for OMG,TT,WE,RA,NIP,AL,UP
lpl_teams <- c('99566404853854212','98767991882270868','99566404855553726','99566404848691211',
               '99566404852189289','99566404846951820','98767991892579754','99566404853058754',
               '99566404854685458','99566404850008779')
lck_teams <- c('100725845018863243','99566404585387054','100205573495116443','100205573496804586',
               '99566404579461230','99566404581868574','100725845022060229','102747101565183056',
               '105505619546859895','98767991853197861')
lcs_teams <- c('99294153828264740','98767991877340524','98926509884398584','98926509883054987',
               '103461966951059521','98926509892121852','99294153824386385','98767991930907107',
               '106972778172351142','98767991860392497','98926509885559666')
lec_teams <- c('101383793569248484','101383793572656373','98767991866488695','98767991926151025','101383793574360315',
               '103461966965149786','101383793567806688','107563714667537640','109637393694097670',
               '99322214695067838')

#divide up roles summarise performances grade each metric then get total starting score
mgt_fe <- mgt %>%
  mutate(gametime_min = gametime/60000,
         xp_pm = xp/gametime_min,
         gold_pm = totalgold/gametime_min,
         cs_pm = minions_killed/gametime_min,
         kda = (champions_killed + assists)/num_deaths,
         damage_dealt_pm = total_damage_dealt_to_champions/gametime_min,
         damage_taken_pm = total_damage_taken/gametime_min,
         damage_mitigated_pm = total_damage_self_mitigated/gametime_min,
         tot_heal_pm = total_heal_on_teammates/gametime_min,
         tot_shield_pm = total_damage_shielded_on_teammates/gametime_min,
         tot_damage_buildings_pm = total_damage_dealt_to_buildings/gametime_min,
         tot_damage_to_turrets_pm = total_damage_dealt_to_turrets/gametime_min,
         tot_damage_to_objectives_pm = total_damage_dealt_to_objectives/gametime_min,
         kda = case_when(
           kda == Inf ~ champions_killed + assists,
           is.na(kda) ~ 0,
           TRUE ~ kda
         ),
         win = ifelse(outcome == 'win',1,0),
         blue_side = ifelse(side == 100,1,0),
         kp = champions_killed / champions_killed_team,
         kp = ifelse(is.na(kp),0,kp),
         top_region = ifelse(teamid %in% current_top_region_teams,1,0),
         east_region = ifelse(teamid %in% lpl_teams | teamid %in% lck_teams,1,0),
         west_region = ifelse(teamid %in% lec_teams | teamid %in% lcs_teams,1,0),
         international_appearance = ifelse(leagueid %in% worlds,1,0),
         lpl_region = ifelse(teamid %in% lpl_teams,1,0))


metrics_summary_table <- mgt_fe %>%
  group_by(playerid) %>%
  summarise(avg_gold = mean(gold_pm),
            avg_kda = mean(kda),
            avg_xp = mean(xp_pm),
            avg_cs = mean(cs_pm),
            worlds_appearances = sum(international_appearance),
            lpl_team_cnt = sum(lpl_region),
            east_cnt = sum(east_region),
            west_cnt = sum(west_region),
            count = n()) %>%
  filter(playerid != '') %>%
  left_join(mgt_fe %>%
              dplyr::select(playerid,handle) %>%
              distinct(), by = c('playerid')) %>%
  ungroup() %>%
  mutate(lpl_player = ifelse(lpl_team_cnt > 0 & east_cnt < 110 & (lpl_team_cnt >= 20 | worlds_appearances >= 20), 100,00),
         score = avg_gold * 0.1 + avg_kda * 0.8 + avg_xp * 0.1 + count * 0.2 + avg_cs + worlds_appearances*2 +
           east_cnt*0.5 + west_cnt*0.2 + lpl_player)

starting_elo_df_vec <- c(metrics_summary_table$score)
names(starting_elo_df_vec) <- metrics_summary_table$playerid


elo_players_frame <- elo.run(home_team_win ~ adjust(players(playerid_1,playerid_2,playerid_3,playerid_4,playerid_5, weights = c(0.2,0.2,0.2,0.2,0.2)),16) + 
                               players(playerid_6,playerid_7,playerid_8,playerid_9,playerid_10, weights = c(0.2,0.2,0.2,0.2,0.2)),
                             initial.elos = starting_elo_df_vec,
                             data = wide_elo_df_filt, k = 25)

summary(elo_players_frame)


elo_history <- elo_players_frame %>%
  as.data.frame() %>%
  dplyr::rename('playerid_1' = 1, 'playerid_2' = 2, 'playerid_3' = 3, 'playerid_4' = 4, 'playerid_5' = 5,
         'playerid_6' = 6, 'playerid_7' = 7, 'playerid_8' = 8, 'playerid_9' = 9, 'playerid_10' = 10,
         'home_win_prob' = 11, 'home_win' = 12, 'home_elo_change' = 13, 'away_elo_change' = 14,
         'player_1_updated_elo' = 15,
         'player_2_updated_elo' = 16, 'player_3_updated_elo' = 17, 'player_4_updated_elo' = 18,
         'player_5_updated_elo' = 19, 'player_6_updated_elo' = 20, 'player_7_updated_elo' = 21,
         'player_8_updated_elo' = 22, 'player_9_updated_elo' = 23, 'player_10_updated_elo' = 24)

elo_df <- wide_elo_df_filt %>%
  mutate(player_1_elo_before_game = elo_history$player_1_updated_elo - elo_history$home_elo_change*0.2,
         player_2_elo_before_game = elo_history$player_2_updated_elo - elo_history$home_elo_change*0.2,
         player_3_elo_before_game = elo_history$player_3_updated_elo - elo_history$home_elo_change*0.2,
         player_4_elo_before_game = elo_history$player_4_updated_elo - elo_history$home_elo_change*0.2,
         player_5_elo_before_game = elo_history$player_5_updated_elo - elo_history$home_elo_change*0.2,
         player_6_elo_before_game = elo_history$player_6_updated_elo - elo_history$away_elo_change*0.2,
         player_7_elo_before_game = elo_history$player_7_updated_elo - elo_history$away_elo_change*0.2,
         player_8_elo_before_game = elo_history$player_8_updated_elo - elo_history$away_elo_change*0.2,
         player_9_elo_before_game = elo_history$player_9_updated_elo - elo_history$away_elo_change*0.2,
         player_10_elo_before_game = elo_history$player_10_updated_elo - elo_history$away_elo_change*0.2,
         home_win_prob = elo_history$home_win_prob,
         away_win_prob = 1 - home_win_prob,
         home_elo_before_game = player_1_elo_before_game + player_2_elo_before_game + player_3_elo_before_game + player_4_elo_before_game + player_5_elo_before_game,
         home_elo_after_game = elo_history$player_1_updated_elo + elo_history$player_2_updated_elo + 
           elo_history$player_3_updated_elo + elo_history$player_4_updated_elo + elo_history$player_5_updated_elo,
         away_elo_before_game = player_6_elo_before_game + player_7_elo_before_game + player_8_elo_before_game + player_9_elo_before_game + player_10_elo_before_game,
         away_elo_after_game = elo_history$player_6_updated_elo + elo_history$player_7_updated_elo + 
           elo_history$player_8_updated_elo + elo_history$player_9_updated_elo + elo_history$player_10_updated_elo,
         home_elo_change = elo_history$home_elo_change,
         away_elo_change = elo_history$away_elo_change)

#join elo df together to orient data from home/away to school/opponent
all_team_schedule <- rbind(
  elo_df %>%
    dplyr::select(datetime, leagueid, tournament_name, tournament_id, 
           esportsgameid, platformgameid, side = home_team_side, teamid = home_team_id, team = home_team,
           opponent = away_team, result = home_team_win, team_win_prob = home_win_prob,
           team_elo = home_elo_after_game, player1 = playerid_1, player2 = playerid_2,
           player3 = playerid_3,player4 = playerid_4,player5 = playerid_5,
           handle1 = handle_1, handle2 = handle_2,
           handle3 = handle_3,handle4 = handle_4,handle5 = handle_5),
  elo_df %>%  #creating the inverse to display the opponent as the school
    dplyr::select(datetime, leagueid, tournament_name,tournament_id, 
           esportsgameid, platformgameid, side = away_team_side, teamid = away_team_id, team = away_team, 
           opponent = home_team, result = away_team_win, team_win_prob = away_win_prob,
           team_elo = away_elo_after_game, player1 = playerid_6, player2 = playerid_7,
           player3 = playerid_8,player4 = playerid_9,player5 = playerid_10,
           handle1 = handle_6, handle2 = handle_7,
           handle3 = handle_8,handle4 = handle_9,handle5 = handle_10)
)



all_team_schedule <- all_team_schedule %>%
  group_by(team) %>%
  arrange(team,datetime) %>%
  mutate(game_count = seq_along(team)) %>%
  ungroup() %>%
  left_join(leagues %>%
              dplyr::select(tournamentid,league_name) %>%
              distinct(), by = c('tournament_id' = 'tournamentid')) %>%
  mutate(tournament_name_id = ifelse(!is.na(league_name),paste0(league_name,' "',tournament_name,'"_',tournament_id),paste0(tournament_name,'_',tournament_id)),
         team_name_id = paste0(team,'_',teamid))

final_elos <- all_team_schedule %>%
  group_by(team) %>%
  dplyr::slice_max(game_count) %>%
  ungroup() %>%
  arrange(desc(team_elo)) %>%
  mutate(rank = row_number()) %>%
  dplyr::select(-opponent)


#build shiny app
ui <- fluidPage(
  theme = shinytheme('cosmo'),
  #page header
  headerPanel('Global Power Rankings Hackathon'),
  #navigation panel to different visualization tools
  navlistPanel(
    # "Name of tab panel", could add a title at the top of the tab panel here
    widths = c(2,10),
    #this tab has the final deliverables of the project
    tabPanel(
      "Rankings",
      column(6,
             h3('Type of Rank Selection'),
             selectInput(inputId = 'tournament_global_team',
                         label = 'Select Tournament, Global, or Team Ranking',
                         choices = c('Tournament','Global Rankings','Team Rankings'),
                         selected = 'Tournament'),
             
             #outputs additional selections or global ranking table
             uiOutput('display_additional_selections')
      ),
      fluidRow(
        column(12,
               #outputs selected tournament rankings when under the tournament ranking selection
               uiOutput("selected_tournament_rankings"),
               #outputs selected team rankings when under the team ranking selection
               uiOutput("selected_teams_rankings") 
        )
      )
    ),
    tabPanel(
      "Demo",
      column(4,
             h1("Youtube Demo")
             
      ),
      column(12,
             HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/C3GouGa0noM?si=20zS8fsqqbE6u6Px" title="YouTube video player" 
                  frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; 
                  web-share" allowfullscreen></iframe>')
      )
      
    ),
    tabPanel(
      "Methodology Write Up",
      column(4,
             h1("Methodology Write Up"),
             
             #####
             br(),
             p("The method I used for ranking teams was an Elo system. Elo is a rating system designed for head to head competition most notably in Chess.
             In this particular rating system, the Elo of each team is a sum of all 5 players that played in that game, and the difference between winning
             and losing is distributed across the participating players evenly in 20% shares. 
             The ratings displayed in the Tournament selection tab is the final Elo rating of each team at that given tournament. 
             Global rankings is done by taking the final Elo of each team and sorting them from highest to lowest, and the Team ranking is the same as 
             global rankings except filtered for selected teams. With all that covered, I will get into the reasoning behind some of my ideas."),
             br(),
             p("Strength of Competition is an important factor when accounting for how teams should be rated.,
             Elo accounts for individual team strength within the calculation. A team with a high rating has an expected win probability 
             just based on what their rating is. Beating a team they are expected to beat will result in a lower gain than if the underdog 
             were to upset the favored team. This way of gaining and losing Elo is important to account for strength of schedule, because if 
             teams are winning everything in low Elo regions, they will not be able to climb as high as teams facing better opponents."),
             br(),
             p("Roster strength is forever changing in league of legends, because players switch teams and get subbed in and out all the time. 
             I wanted to make sure my rating system could handle these kinds of changes, so this is why my rating system uses player level Elo. 
             This means that each team’s rating is a sum of the 5 players listed. When one player leaves and a new player joins, the team rating 
             is adjusted instantly to the new player without impacting the system at all. This is most notably seen in the LCK Summer 2023 season 
             when Faker got injured and Poby stepped in for 9 games. Faker’s rating was not changed, but the team’s rating dropped around 150 Elo 
             in rating when Faker stepped aside and Poby joined the roster. The team then proceeds to lose 7 of the next 9 games before Faker rejoins 
             the roster. T1 goes up about 200 Elo when Faker comes back, because the difference between Faker and Poby has changed with Poby losing 7 
             games and Faker staying the same. The team is not rated as highly as when Faker was first with the team, because Zeus, Oner, Gumayusi, 
             and Keria took 80% of the Elo rating they lost. This is a perfect representation of a team that went through roster changes and were 
             discredited as a team, but without a player level rating system could not account for the changes in talent. KT Rolster went on to win 
             1st team LCK all pro, and proceeded to not make the finals after choosing to play T1 over Hanwha Life in the Semis of the playoffs."),
             br(),
             plotOutput("t1_graph"),
             p("Cold start was the most difficult part of this rating system. Elo systems typically start at a flat 1000 or 1500 
             and adjust the rankings as more games are played over time. In the player rating case, this would result in every player 
             starting at 200/300 Elo and changing as they play more games throughout their career. The issue with a cold start like this 
             is that we have prior knowledge, and starting players like JDG Ruler at the same rating as ScaryJerry from Maryville University
             would not really be representative of the current state of the game. I did not want to artificially inflate some team’s ratings 
             and not other teams solely on my own bias, but keeping everyone the same would result in a rating system with teams in worse 
             regions but winning a lot at the top of the rankings. Anyone who has watched any regional or international events knows that 
             there are differences in level of play, and some regions are better than others. There is a reason LCK and LPL teams receive 4 
             worlds slots and LLA teams only have 1 slot. I tried giving players that played for teams in major regions a boost in their 
             starting Elo, but this failed when players play for a short period of time in a top region and then join a lower region. Their 
             rating would inflate the lower region team’s rating much higher to the point that the initial boost in Elo was not a viable option. 
             I then looked into using the game data provided to capture how each player individually performs. I first tried splitting the players 
             up by role and investigating what factors within a game provide the team with winning a game. Players that won more games would be 
             assigned a higher starting rating than players that do not win as much. This method took 20+ different features each engineered for 
             describing the impact the player provided for their team. Each model returned different features that gave more insight into how an 
             individual in each role can win a game. ADC players typically won when they dealt more damage to enemies, objectives and turrets. 
             This again led to players having higher ratings at lower levels of play than good players in tougher regions. Not to mention, each role, 
             champion, and player can be used differently depending on situations and metas. Sometimes Canyon is a supportive jungler that relies on 
             little to no gold, and sometimes Canyon is a carry that wants to funnel himself. The model cannot pick up on the nuances of the game 
             that is forever changing week to week. There is one underlying factor that prevailed through this trial, and that is that players across 
             every role win more when they have more gold, experience, cs, and a high kda. Good players are able to accumulate leads throughout a game.
             My final iteration of a cold start was to take these factors and create a scoring method that would fill my starting rating for each player. 
             This scoring method was done through many iterations changes, but my goal was to have a score that resulted in the high predictive accuracy 
             and a low mean squared error. First, I will show the scoring formula I used and then I will explain the reasoning behind why each part of 
             the formula was added."),
             br(),
             p(strong("Score = Average Gold * 0.10 + Average KDA * 0.80 + Average XP * 0.10 + Average CS + Game Count * 0.20 + 
                      Worlds Appearances * 2 + Eastern Region Games * 0.50  + Western Region Games * 0.2 + LPL Bonus")),
             br(),
             p("I will start with the easiest to explain features being gold, kda, experience, and cs. 
             These features gave insight into how each player was performing across all the games they played. 
             I added these points to how many games each player accumulated the stats in. This rewarded players that 
             had more games played, so good stats one time does not equate to good stats across 100 games. The next addition 
             was for players that qualified for international events, and even more rewarding for players that made deeper runs at 
             worlds. Good players typically qualify for worlds and the best players make it the furthest. Eastern and Western Region 
             games are divided by LCK and LPL for eastern regions and LEC and LCS for western regions. Every game played by a player 
             in the respective region awarded them a small amount of points. It is hard to compete at the highest level of league of legends, 
             and it is even harder to compete for a long time. Players that have lasted a long time were granted points for playing, and the 
             difference in regions accounts for imports. Players like Pyosik have played in both regions, but western regions typically 
             have a worse performance at international events, so they receive reduced points for competing in their region. The last feature 
             LPL Bonus is specifically for the lack of LPL data within this dataset. LPL teams were consistently rated at the bottom/middle of 
             the pack, so an addition of 100 bonus points was added to players in the LPL that had less than 110 games recorded and more than 20 
             games played in the LPL or a worlds event. With these conditions underrepresented LPL players with standout achievements were given a bonus. 
             The bonus applied for LPL players was not large enough to dominate the ranking system, but to put the top rated Chinese players near 
             the middle of the pack instead of the bottom. With all this being said, the highest rated initial player was Deft at 394 points and the 
             lowest rated player was Te Ka2 from Aguilas Doradas team at 44 points. This cold start system of rating the players in this method resulted 
             in an AUC(area under the curve) metric of 73.2%, and a mean squared error of 0.21."),
             br(),
             p("Accounting for recent performance is also important for a rating system. Sometimes a player is better in Summer Split than they were 
             in Winter Split. Similar to Jankos on Team Heretics, the players can have a shaky start, but finish strong. The benefit of an Elo rating 
             is that the volatility is adjustable through the K factor within the formula. A lower K factor means there is less volatility and a higher 
             K factor means more drastic swings in the ratings. For reference, sports like soccer that play very few games could be using a K factor of 
             anywhere from 20-100 where sports like baseball that play 160+ games in a season only need a K factor of 4-10. For this particular model, 
             the K factor was tested to match the highest predictive accuracy of the model without swinging the minor region teams into the top 10 of 
             the rankings. The K factor used in this model was 25 meaning a more volatile system than baseball, but more representative of a sport like soccer. 
             As for recent performances in the case of players like Jankos, the model is adaptable to increasing momentum. If Jankos is winning more games 
             against teams that have higher ratings than Team Heretics, he will be gaining Elo and be more representative of his skills in future games."),
             br(),
             p("Currently in the Elo system I have designed, I am not accounting regular season, playoffs, or international events with different match outcomes.
             There are ways of adjusting the formula to make certain games more important than others with custom formulas, but in my system each game is worth 
             the same no matter the situation."),
             br(),
             p("International competition is a way for players to compete across regions, and get an idea of how each region stacks up against the rest. 
             The games played at worlds gives the teams that qualified a chance to play additional games and potentially incur more rating. 
             Again, the values of the games are not worth more, but more games gives more opportunities for the model to better estimate ratings."),
             br(),
             p("Data selection for this project was limited to only complete and reliable data. There were about 25,000 games within the dataset, 
             but not every game had mapping data, and even less had in-game data. At the end of the filtering process, I was left with 18,262 games 
             that had everything listed. When I say everything I mean tournament, game, team, and player identification available. Each game needed 
             to have an outcome, win or loss, and each player within the game needed to have in-game stats. All other games missing these standards 
             were dropped from the dataset and not included in the final model. The most noticeable part of this would be the lack of LPL data. Only a 
             few teams had any complete data at all, so the LPL was difficult to accurately rate. When looking at my rating system, you may see RNG among 
             the top teams, but did not even qualify for worlds this year. This team being rated in my system is Breathe, Wei, Xiaohu, GALA and Ming. 
             A very good team, but not the current RNG team. Teams like NIP are not even present in the data, so creating global rankings without complete 
             data leaves the system lacking in information."),
             br(),
             p("Data Relevance from patch to patch and season to season is not necessary within my Elo system. The rating system created here only 
             looks at the sum of player ratings against the other team’s sum of player ratings. Just those ratings is enough for the model to work. 
             This makes changes in patch and season not relevant. As long as a team is increasing Elo during that patch, they probably have a good 
             read on the meta and that is mirrored in their increase in rating."),
             br(),
             p("Sensitivity to performance of an individual is accounted for in the player based ratings. If a player is under performing, 
               their team will suffer. Each player is taking 20% of each win or loss. If a team decides to swap out a player for not performing well, 
               the next game they will have the rating of the other player included in the calculation. I mentioned this earlier, but Faker was not 
               impacted by Poby’s poor performances. Zeus, Oner, Gumayusi and Keria were impacted, but they were a part of the losses, so they should 
               be docked rating for their individual performances. Once Faker returned, the rating of T1’s midlaner was swapped from Poby to Faker’s 
               rating, and the system continued to run just as normal."
               ),
             br(),
             br()
             
             #####
      ),
      column(12,
      )
    ),
    tabPanel(
      "Tooling",
      column(10,
             h1('Tooling: Include an explanation of the AWS services used to build the project.'),
             p("For this project I utilized Athena, Glue, S3, IAM, and EC2 from the AWS catalog. 
               I created tables in Athena using the ddl supplied from the starter guide. 
               This used Glue to parse the files and build the datatables. 
               Athena was used to query and unnest the data into usable csv files. 
               I stored the csv files in S3, and pulled them from S3 into my Rstudio environment. 
               All my data manipulation was done locally using Rstudio. Using EC2 
               I hosted my web application for this submission, and IAM to set my security restrictions 
               like read and write access for my S3 files.")
      ),
    ),
    tabPanel(
      'Code',
      column(4,
             h1('Code: Provide a URL to your code repository to show how your project was built. 
                Please provide access by sharing it with: testing@devpost.com, 
                riot-esports-hackathon-rg@riotgames.com, and aws-riotgames-hackathons@amazon.com '))
    )
  )
)


server <- function(input, output, session) {
  
  #turn data into reactive df
  all_team_schedule_reactive <- reactive({
    all_team_schedule
  })
  
  #reactive filtering based on selected deliverable
  observeEvent(input$tournament_global_team, {
    if (input$tournament_global_team == 'Tournament') {
      if (nrow(all_team_schedule_reactive()) > 0) {
        output$display_additional_selections <- renderUI({
          tagList(
            selectInput(inputId = 'tournament_selection',
                        label = 'Select Tournament',
                        choices = all_team_schedule_reactive() %>%
                          arrange(datetime) %>%
                          dplyr::select(tournament_name_id) %>%
                          unique(),
                        selected = NULL)
          )
        })
      }
    } else if (input$tournament_global_team == 'Global Rankings') {
      output$display_additional_selections <-  renderTable(
        all_team_schedule_reactive() %>%
          group_by(team) %>%
          dplyr::slice_max(game_count) %>%
          ungroup() %>%
          arrange(desc(team_elo)) %>%
          mutate(rank = row_number()) %>%
          dplyr::select(Rank = rank, Team = team, `Team Elo` = team_elo,
                        Top = handle1, Jungle = handle2, Mid = handle3,
                        ADC = handle4, Support = handle5))
      
    } else if (input$tournament_global_team == 'Team Rankings') {
      if (nrow(all_team_schedule_reactive()) > 0) {
        output$display_additional_selections <- renderUI({
          tagList(
            selectizeInput(
              inputId = "multiteam_select", 
              label = "Select Teams",
              choices = all_team_schedule_reactive() %>%
                arrange(team) %>%
                dplyr::select(team_name_id) %>%
                unique(),
              multiple = TRUE,
              selected = NULL
            )
          )
        })
      } else {
      }
      
    }
  })
  
  #tournament rankings table
  observeEvent(input$tournament_selection, {
    req(input$tournament_selection)
    
    filtered_tournament <- reactive({
      all_team_schedule_reactive() %>%
        filter(tournament_name_id == input$tournament_selection)
    })
    
    output$selected_tournament_rankings <- renderTable(
      if (input$tournament_global_team == 'Tournament') {
        filtered_tournament() %>%
          group_by(team) %>%
          dplyr::slice_max(game_count) %>%
          ungroup() %>%
          arrange(desc(team_elo)) %>%
          mutate(rank = row_number()) %>%
          dplyr::select(Rank = rank, Team = team, `Team Elo` = team_elo,
                        Top = handle1, Jungle = handle2, Mid = handle3,
                        ADC = handle4, Support = handle5)
      } else {
      }
    )
  })
  
  #team rankings table
  observeEvent(input$multiteam_select, {
    req(input$multiteam_select)
    
    filtered_teams <- reactive({
      all_team_schedule_reactive() %>%
        filter(team_name_id %in% input$multiteam_select)
    })
    
    output$selected_teams_rankings <- renderTable(
      if (input$tournament_global_team == 'Team Rankings') {
        filtered_teams() %>%
          group_by(team) %>%
          dplyr::slice_max(game_count) %>%
          ungroup() %>%
          arrange(desc(team_elo)) %>%
          mutate(rank = row_number()) %>%
          dplyr::select(Rank = rank, Team = team, `Team Elo` = team_elo,
                        Top = handle1, Jungle = handle2, Mid = handle3,
                        ADC = handle4, Support = handle5)
      } else {
      }
    )
  })
  
  output$t1_graph <- renderPlot({
    
    ggplot(all_team_schedule_reactive() %>%
             mutate(year = year(datetime)) %>%
             filter(team == 'T1' & year == 2023) %>%
             mutate(game_count_reset = seq(1:n()))) +
      geom_point(aes(x = game_count_reset, y = team_elo)) +
      geom_line(aes(x = game_count_reset, y = team_elo)) +
      theme_minimal() +
      labs(title = 'T1 Summer Split 2023', x = 'Game Number', y = 'Team Elo Rating') +
      annotate("text", label =  "Faker Leaves \n Poby Joins", x = 56, y = 1300) +
      annotate("text", label =  "Poby Leaves \n Faker Joins", x = 82, y = 1150)
    
  })
  
  
}


shinyApp(ui = ui, server = server)




























