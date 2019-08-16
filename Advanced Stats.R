################################################################################################
#### This code is for NCAA Women's 2019 Tournament
## Created: July 28, 2019
## Edited:
################################################################################################

rm(list = ls())

# set working directory
setwd("/Users/m/Desktop/ADS/Data Mining I/Project/Womens Tournament")

library(dplyr)
library(xgboost)
library(lme4)
library(caret)
library(corrplot)
library(car)
library(tidyverse)
library(magrittr)
library(data.table)
library(elo)
library(stringr)
library(gridExtra)
library(knitr)
library(magrittr)
library(ggExtra)




reg_season_stats <- read.csv('WRegularSeasonDetailedResults.csv', header=T)
tourney_stats <- read.csv("WNCAATourneyDetailedResults.csv", header=T)
teams <- read.csv("WTeams.csv", header=T)
tourney_stats_compact <- read.csv('WNCAAtourneyCompactResults.csv', header=T)
seeds <- read.csv('WNCAATourneySeeds.csv', header=T)
sub <- read.csv("WSampleSubmissionStage2.csv", header=T)

# convert the seeds
seeds$Seed = as.numeric(substring(seeds$Seed,2,4))

########################################################################################################
# Create useful functions
########################################################################################################
# create posession variables
# regular season
reg_season_stats <- reg_season_stats %>%
  mutate(WPos = WFGA + (WFTA*0.475) + WTO - WOR,
         LPos = LFGA + (LFTA*0.475) + LTO - LOR)
# tournament
tourney_stats <- tourney_stats %>%
  mutate(WPos = WFGA + (WFTA*0.475) + WTO - WOR,
         LPos = LFGA + (LFTA*0.475) + LTO - LOR)


# create a function to reshape the data to result in a data frame that encompasses regular season totals
reshape_detailed_results <- function(detailed_dataset){
  season_totals <- rbind(detailed_dataset %>%
                                 select(Season, DayNum, TeamID=WTeamID, Score=WScore, OScore=LScore, WLoc, NumOT, Pos=WPos,
                                        FGM=WFGM, FGA=WFGA, FGM3=WFGM3, FGA3=WFGA3, FTM=WFTM, FTA=WFTA, OR=WOR, DR=WDR, Ast=WAst,
                                        TO=WTO, Stl=WStl, Blk=WBlk, PF=WPF, OPos=LPos, OFGM=LFGM, OFGA=LFGA, OFGM3=LFGM3, OFGA3=LFGA3, OFTM=LFTM,
                                        OFTA=LFTA, O_OR=LOR, ODR=LDR, OAst=LAst, OTO=LTO, OStl=LStl, OBlk=LBlk,
                                        OPF=LPF) %>%
                                 mutate(Winner=1),
                               
                         detailed_dataset %>%
                                 select(Season, DayNum, TeamID=LTeamID, Score=LScore, OScore=WScore, WLoc, NumOT, Pos=LPos,
                                        FGM=LFGM, FGA=LFGA, FGM3=LFGM3, FGA3=LFGA3, FTM=LFTM, FTA=LFTA, OR=LOR, DR=LDR, Ast=LAst,
                                        TO=LTO, Stl=LStl, Blk=LBlk, PF=LPF, OPos=WPos, OFGM=WFGM, OFGA=WFGA, OFGM3=WFGM3, OFGA3=WFGA3,
                                        OFTM=WFTM, OFTA=WFTA, O_OR=WOR, ODR=WDR, OAst=WAst, OTO=WTO, OStl=WStl, OBlk=WBlk,
                                        OPF=WPF) %>%
                                 mutate(Winner=0)) %>%
    group_by(Season, TeamID) %>%
    summarise(GP = n(),
              wins = sum(Winner),
              TotPoints = sum(Score),
              TotPointsAllowed = sum(OScore),
              NumOT = sum(NumOT),
              TotPos = sum(Pos),
              TotFGM = sum(FGM),
              TotFGA = sum(FGA),
              TotFGM3 = sum(FGM3),
              TotFGA3 = sum(FGA3),
              TotFTM = sum(FTM),
              TotFTA = sum(FTA),
              TotOR = sum(OR),
              TotDR = sum(DR),
              TotAst = sum(Ast),
              TotTO = sum(TO),
              TotStl = sum(Stl),
              TotBlk = sum(Blk),
              TotPF = sum(PF),
              TotPosAllowed = sum(OPos),
              TotFGMAllowed = sum(OFGM),
              TotFGAAllowed = sum(OFGA),
              TotFGM3Allowed = sum(OFGM3),
              TotFGA3Allowed = sum(OFGA3),
              TotFTMAllowed = sum(OFTM),
              TotFTAAllowed = sum(OFTA),
              TotORAllowed = sum(O_OR),
              TotDRAllowed = sum(ODR),
              TotAstAllowed = sum(OAst),
              TotTOAllowed = sum(OTO),
              TotStlAllowed = sum(OStl),
              TotBlkAllowed = sum(OBlk),
              TotPFAllowed = sum(OPF))
}

# store the results to a data frame
season_totals <- reshape_detailed_results(reg_season_stats)

# calculate win percentage
season_totals$WinPerc <- season_totals$wins / season_totals$GP


# next, create a function that creates a data frame of season averages for each team
calc_detailed_averages <- function(totals_dataframe) {
  
  averages <- totals_dataframe
  
  cols <- names(averages[,c(5:35)])
  
  for (eachcol in cols) {
    averages[,eachcol] <- round(averages[,eachcol] / averages$GP,2)
    
  }
    
    averages <- averages %>%
      rename(AvgPoints=TotPoints, AvgPointsAllowed=TotPointsAllowed, AvgPT=NumOT, AvgFGM=TotFGM, AvgPos=TotPos,
             AvgFGA=TotFGA, AvgFGM3=TotFGM3, AvgFGA3=TotFGA3, AvgFTM=TotFTM, AvgFTA=TotFTA, AvgOR=TotOR, AvgDR=TotDR, 
             AvgAst=TotAst, AvgTO=TotTO, AvgStl=TotStl, AvgBlk=TotBlk, AvgPF=TotPF, AvgPosAllowed=TotPosAllowed, 
             AvgFGMAllowed=TotFGMAllowed, AvgFGAAllowed=TotFGAAllowed, AvgFGM3Allowed=TotFGM3Allowed, 
             AvgFGA3Allowed=TotFGA3Allowed, AvgFTMAllowed=TotFTMAllowed, AvgFTAAllowed=TotFTAAllowed, 
             AvgORAllowed=TotORAllowed, AvgDRAllowed=TotDRAllowed, AvgAstAllowed=TotAstAllowed, 
             AvgTOAllowed=TotTOAllowed, AvgStlAllowed=TotStlAllowed, AvgBlkAllowed=TotBlkAllowed, 
             AvgFPAllowed=TotPFAllowed) %>%
      mutate(PointsPerPos=AvgPoints/AvgPos,
             PointsPerPosAllowed=AvgPointsAllowed/AvgPosAllowed)
      return(averages)
}

# create an averages data frame
season_team_averages <- calc_detailed_averages(season_totals)












########################################################################################################
# Begin EDA
########################################################################################################
# join team names
tourney_stats_compact <- tourney_stats_compact %>%
  left_join(teams, by = c('WTeamID' = 'TeamID')) %>%
  left_join(teams, by=c('LTeamID' = 'TeamID'))

# rename the team variabels for ease of call-back
tourney_stats_compact <- tourney_stats_compact %>%
  rename(WTeamName = TeamName.x, LTeamName = TeamName.y)

# create new variable to be used in calling the ncaa champs
tourney_stats_compact$season_day <- paste(tourney_stats_compact$Season, tourney_stats_compact$DayNum, sep='_')

# get the champions
ncaa_champs <- tourney_stats_compact %>%
  group_by(Season) %>%
  summarise(max_days = max(DayNum)) %>%
  mutate(season_day = paste(Season, max_days, sep='_')) %>%
  left_join(tourney_stats_compact, by = 'season_day') %>% ungroup() %>%
  select(-Season.y) %>%
  rename(Season = Season.x)

# create plots of winners and losers of national championship games since 1998
# winners
wins_plot <- ncaa_champs %>%
  group_by(WTeamName) %>%
  summarise(n=n()) %>%
  ggplot(aes(x=reorder(WTeamName, n), y=n)) + 
           geom_bar(stat='identity', fill='blue', color='black') +
           theme_minimal() +
           labs(title="Most Tournament Wins", x= 'Winner', y='# of Titles') +
           coord_flip()
# losers
losses_plot <- ncaa_champs %>%
  group_by(LTeamName) %>%
  summarise(n=n()) %>%
  ggplot(aes(x=reorder(LTeamName, n), y=n)) + 
  geom_bar(stat='identity', fill='red', color='black') +
  theme_minimal() +
  labs(title="Most Tournament Second Places", x= 'Runner-Up', y='# of Times') +
  coord_flip()

# plot the two
grid.arrange(wins_plot, losses_plot, ncol=2)


# create the advanced stats data set to be further changed
  # swap the order of teams to double the data
advanced_stats_per_game <- rbind(reg_season_stats %>%
                                   select(Season, DayNum, TeamID=WTeamID, Score=WScore, OScore=LScore, WLoc, NumOT,
                                          FGM=WFGM, FGA=WFGA, FGM3=WFGM3, FGA3=WFGA3, FTM=WFTM, FTA=WFTA, OR=WOR, DR=WDR, Ast=WAst,
                                          TO=WTO, Stl=WStl, Blk=WBlk, PF=WPF, OFGM=LFGM, OFGM3=LFGM3, OFGA3=LFGA3, OFTM=LFTM,
                                          OFTA=LFTA, O_OR=LOR, ODR=LDR, OAst=LAst, OTO=LTO, OStl=LStl, OBlk=LBlk,
                                          OPF=LPF) %>%
                                   mutate(Winner=1),
                                 
                                 reg_season_stats %>%
                                   select(Season, DayNum, TeamID=LTeamID, Score=LScore, OScore=WScore, WLoc, NumOT,
                                          FGM=LFGM, FGA=LFGA, FGM3=LFGM3, FGA3=WFGA3, FTM=LFTM, FTA=LFTA, OR=LOR, DR=LDR, Ast=LAst,
                                          TO=LTO, Stl=LStl, Blk=LBlk, PF=LPF, OFGM=WFGM, OFGM3=WFGM3, OFGA3=LFGA3, OFTM=WFTM,
                                          OFTA=WFTA, O_OR=WOR, ODR=WDR, OAst=WAst, OTO=WTO, OStl=WStl, OBlk=WBlk,
                                          OPF=WPF) %>%
                                   mutate(Winner=0)) %>%
  mutate(PtsDiff = Score - OScore)

# create the advanced stats formulas and apply them to the data
advanced_stats_per_game <- advanced_stats_per_game %>%
  mutate(Poss = 0.96*(FGA + TO + (0.475*FTA) - OR),
          OffRtg = 100 * (Score / Poss),
          DefRtg = 100 * (OScore / Poss),
          NetRtg = OffRtg - DefRtg,
          AstRatio = 100 * Ast / (FGA + (0.475*FTA) + Ast + TO),
          TORatio = 100 * TO / (FGA *(0.475*FTA) + Ast + TO),
          TSPerc = 100 * Score / (2 * (FGA + (0.475 * FTA))),
          EFGPerc = FGM + (0.5 * FGM3) / FGA,
          FTRate = FTA / FGA) %>%
  select(Season, DayNum, TeamID, Score, OScore, WLoc, NumOT, Winner, Poss, OffRtg, DefRtg, NetRtg, AstRatio,
         TORatio, TSPerc, EFGPerc, FTRate)

# create plots to visualize all of the advanced stats
adv_plots <- function(variable, plot_name) {
  
  advanced_stats_per_game %>%
    ggplot(aes(fill = factor(Winner))) +
    geom_density(aes_string(x= variable), alpha = 0.5) +
    scale_fill_manual(values = c("orange", "steelblue")) +
    ggtitle(plot_name) +
    theme_minimal() +
    theme(legend.position = "none", axis.title.y = element_blank())
}

off_rat <- adv_plots("OffRtg", "Offensive Rating")
def_rat <- adv_plots("DefRtg", "Defensive Rating")
net_rat <- adv_plots("NetRtg", "Net Rating")
ast_ratio <- adv_plots("AstRatio", "Assist Ratio") 
to_ratio <- adv_plots("TORatio", "Turnover Ratio")
ts_perc <- adv_plots("TSPerc", "True Shooting Percentage")
eff_fg_perc <- adv_plots("EFGPerc", "Effective FG%")
ft_rate <- adv_plots("FTRate", "Free Throw Rate")
grid.arrange(off_rat, def_rat, net_rat, ast_ratio, to_ratio, ts_perc, eff_fg_perc, ft_rate, 
             ncol = 4, bottom = "Blue fill indicates winning team")



########################################################################################################
# Look at season totals per team
########################################################################################################
# look at the whole-season advanced stats 
season_totals <- reshape_detailed_results(reg_season_stats)

# create the advanced stats formulas and apply them to the whole-season data
advanced_stats_seasons <- season_totals %>%
  mutate(WinPerc = wins / GP,
         Poss = 0.96*(TotFGA + TotTO + (0.475*TotFTA) - TotOR),
         OffRtg = 100 * (TotPoints / TotPos),
         DefRtg = 100 * (TotPointsAllowed / TotPos),
         NetRtg = OffRtg - DefRtg,
         AstRatio = 100 * TotAst / (TotFGA + (0.475*TotFTA) + TotAst + TotTO),
         TORatio = 100 * TotTO / (TotFGA *(0.475*TotFTA) + TotAst + TotTO),
         TSPerc = 100 * TotPoints / (2 * (TotFGA + (0.475 * TotFTA))),
         EFGPerc = (TotFGM + (0.5 * TotFGM3) / TotFGA / GP),
         FTRate = TotFTA / TotFGA,
         ThreeShare = TotFGA3 / TotFGA) %>%
  select(Season, TeamID, TotPoints, TotPointsAllowed, wins, WinPerc, Poss, OffRtg, DefRtg, NetRtg, AstRatio,
         TORatio, TSPerc, EFGPerc, FTRate, ThreeShare) %>%
  left_join(teams %>% select(TeamID, TeamName), by='TeamID')

# identify each season's champ
ncaa_champs <- ncaa_champs %>%
  mutate(season_winner = paste(Season, WTeamID, sep='_'))
# create variable to identify the champs
advanced_stats_seasons <- advanced_stats_seasons %>%
  mutate(season_winner = paste(Season, TeamID, sep='_'),
         win_yes_no = ifelse(season_winner %in% ncaa_champs$season_winner, 'Champs', 'Not'))

# look for predictors of Win Percentage
reg <- glm(WinPerc~.-win_yes_no -TeamName -season_winner -Season -TeamID - NetRtg -wins, data=advanced_stats_seasons)
summary(reg)
best_reg <- step(reg)

# plot some of these
  # total points
advanced_stats_seasons %>%
  ggplot() +
  geom_point(data = subset(advanced_stats_seasons, win_yes_no == 'Not'), aes(x=TotPoints, y=wins),
             alpha=0.5, color='lightgrey') +
  geom_point(data = subset(advanced_stats_seasons, win_yes_no == 'Champs'), aes(x=TotPoints, y=wins),
             alpha=0.5, color='orange') +
  ggtitle('Total Points and Wins') +
  theme_minimal() +
  theme(legend.position = 'none') +
  facet_wrap(~Season)

  # total points allowed
advanced_stats_seasons %>%
  ggplot() +
  geom_point(data = subset(advanced_stats_seasons, win_yes_no == 'Not'), aes(x=TotPointsAllowed, y=wins),
             alpha=0.5, color='lightgrey') +
  geom_point(data = subset(advanced_stats_seasons, win_yes_no == 'Champs'), aes(x=TotPointsAllowed, y=wins),
             alpha=0.5, color='orange') +
  ggtitle('Total Points Allowed and Wins') +
  theme_minimal() +
  theme(legend.position = 'none') +
  facet_wrap(~Season)

  # offensive rating
advanced_stats_seasons %>%
  ggplot() +
  geom_point(data = subset(advanced_stats_seasons, win_yes_no == 'Not'), aes(x=OffRtg, y=wins),
             alpha=0.5, color='lightgrey') +
  geom_point(data = subset(advanced_stats_seasons, win_yes_no == 'Champs'), aes(x=OffRtg, y=wins),
             alpha=0.5, color='orange') +
  ggtitle('Offensive Rating and Wins') +
  theme_minimal() +
  theme(legend.position = 'none') +
  facet_wrap(~Season)

  # defensive rating
advanced_stats_seasons %>%
  ggplot() +
  geom_point(data = subset(advanced_stats_seasons, win_yes_no == 'Not'), aes(x=DefRtg, y=wins),
             alpha=0.5, color='lightgrey') +
  geom_point(data = subset(advanced_stats_seasons, win_yes_no == 'Champs'), aes(x=DefRtg, y=wins),
             alpha=0.5, color='orange') +
  ggtitle('Defensive Rating and Wins') +
  theme_minimal() +
  theme(legend.position = 'none') +
  facet_wrap(~Season)

  # assist ratio
advanced_stats_seasons %>%
  ggplot() +
  geom_point(data = subset(advanced_stats_seasons, win_yes_no == 'Not'), aes(x=AstRatio, y=wins),
             alpha=0.5, color='lightgrey') +
  geom_point(data = subset(advanced_stats_seasons, win_yes_no == 'Champs'), aes(x=AstRatio, y=wins),
             alpha=0.5, color='orange') +
  ggtitle('Assist Ratio and Wins') +
  theme_minimal() +
  theme(legend.position = 'none') +
  facet_wrap(~Season)

  # turnover ratio
advanced_stats_seasons %>%
  ggplot() +
  geom_point(data = subset(advanced_stats_seasons, win_yes_no == 'Not'), aes(x=TORatio, y=wins),
             alpha=0.5, color='lightgrey') +
  geom_point(data = subset(advanced_stats_seasons, win_yes_no == 'Champs'), aes(x=TORatio, y=wins),
             alpha=0.5, color='orange') +
  ggtitle('Turnover Ratio and Wins') +
  theme_minimal() +
  theme(legend.position = 'none') +
  facet_wrap(~Season)

  # effective field goal percentage
advanced_stats_seasons %>%
  ggplot() +
  geom_point(data = subset(advanced_stats_seasons, win_yes_no == 'Not'), aes(x=EFGPerc, y=wins),
             alpha=0.5, color='lightgrey') +
  geom_point(data = subset(advanced_stats_seasons, win_yes_no == 'Champs'), aes(x=EFGPerc, y=wins),
             alpha=0.5, color='orange') +
  ggtitle('Effective Field Goal Percentage and Wins') +
  theme_minimal() +
  theme(legend.position = 'none') +
  facet_wrap(~Season)

  # free throw rate
advanced_stats_seasons %>%
  ggplot() +
  geom_point(data = subset(advanced_stats_seasons, win_yes_no == 'Not'), aes(x=FTRate, y=wins),
             alpha=0.5, color='lightgrey') +
  geom_point(data = subset(advanced_stats_seasons, win_yes_no == 'Champs'), aes(x=FTRate, y=wins),
             alpha=0.5, color='orange') +
  ggtitle('Free Throw Rate and Wins') +
  theme_minimal() +
  theme(legend.position = 'none') +
  facet_wrap(~Season)

  # net rating
advanced_stats_seasons %>%
  ggplot() +
  geom_point(data = subset(advanced_stats_seasons, win_yes_no == 'Not'), aes(x=NetRtg, y=wins),
             alpha=0.5, color='lightgrey') +
  geom_point(data = subset(advanced_stats_seasons, win_yes_no == 'Champs'), aes(x=NetRtg, y=wins),
             alpha=0.5, color='orange') +
  ggtitle('Net Rating and Wins') +
  theme_minimal() +
  theme(legend.position = 'none') +
  facet_wrap(~Season)













########################################################################################################
# LogLoss for Seed Differential - baseline
########################################################################################################
set.seed(1234)

# amend the seed info
df_seeds <- seeds %>%
  mutate(Seed = substring(Seed, 2L)) %>%
  mutate(Seed = as.integer(Seed)) %>%
  rename(seed_int = Seed)

head(df_seeds)


## amend the tournament Compact results
df_tour <- tourney_stats_compact %>% 
  select(c(Season, WTeamID, LTeamID))

head(df_tour)


# Merge seed for each team
df_winseeds <- df_seeds %>%
  rename(WTeamID = TeamID) %>%
  rename(WSeed = seed_int)
df_lossseeds <- df_seeds %>%
  rename(LTeamID = TeamID) %>%
  rename(LSeed = seed_int)
df_dummy <- dplyr::left_join(x = df_tour,
                             y = df_winseeds,
                             by = c("Season", "WTeamID"))

df_concat <- dplyr::left_join(x = df_dummy,
                              y = df_lossseeds,
                              by = c("Season", "LTeamID")) %>%
  mutate(SeedDiff = WSeed - LSeed)
head(df_concat)



df_wins <- df_concat %>%
  select(SeedDiff) %>%
  mutate(Pred = 1)

df_losses <- df_concat %>%
  select(SeedDiff) %>%
  mutate(SeedDiff = -SeedDiff) %>%
  mutate(Pred = 0)

# Combine wins and losses and shuffle data
df_predictions <- rbind(df_wins, df_losses) %>%
  sample_frac(1L)

head(df_predictions)


# Fit model and estimate prediction metric
# Generalized linear model from `stats` package and estimated model `logloss`

glm_model <- glm(Pred ~ SeedDiff,
                 data = df_predictions,
                 family = binomial)

lglss <- logLoss(glm_model$data$Pred, glm_model$fitted.values)
paste("Logloss:", round(lglss, digits = 6)) # this score would have been good enough for 329th place


# Plot fitted function
X <- data.frame(SeedDiff = -10:10, Pred = NA)
X$Pred <- predict(glm_model, newdata = X, type = "response")

quartz()
ggplot(X, aes(x = SeedDiff, y = Pred)) +
  geom_line() +
  ggtitle("P(Team1 will win)") +
  xlab("Team1 seed - Team2 seed") +
  ylab("")
















########################################################################################################
# XGBoost
########################################################################################################
# convert the seeds
seeds$Seed = as.numeric(substring(seeds$Seed,2,4))

# create the advanced stats data set for the regular season data
# swap the order of teams to double the data
stats_man <- rbind(reg_season_stats %>%
                     select(Season, DayNum, T1=WTeamID, Score=WScore, OScore=LScore, T2=LTeamID, WLoc, NumOT,
                            FGM=WFGM, FGA=WFGA, FGM3=WFGM3, FGA3=WFGA3, FTM=WFTM, FTA=WFTA, OR=WOR, DR=WDR, Ast=WAst,
                            TO=WTO, Stl=WStl, Blk=WBlk, PF=WPF, OFGM=LFGM, OFGM3=LFGM3, OFGA3=LFGA3, OFTM=LFTM,
                            OFTA=LFTA, O_OR=LOR, ODR=LDR, OAst=LAst, OTO=LTO, OStl=LStl, OBlk=LBlk,
                            OPF=LPF) %>%
                     mutate(Winner=1),
                   
                   reg_season_stats %>%
                     select(Season, DayNum, T1=LTeamID, Score=LScore, T2=WTeamID, OScore=WScore, WLoc, NumOT,
                            FGM=LFGM, FGA=LFGA, FGM3=LFGM3, FGA3=WFGA3, FTM=LFTM, FTA=LFTA, OR=LOR, DR=LDR, Ast=LAst,
                            TO=LTO, Stl=LStl, Blk=LBlk, PF=LPF, OFGM=WFGM, OFGM3=WFGM3, OFGA3=LFGA3, OFTM=WFTM,
                            OFTA=WFTA, O_OR=WOR, ODR=WDR, OAst=WAst, OTO=WTO, OStl=WStl, OBlk=WBlk,
                            OPF=WPF) %>%
                     mutate(Winner=0)) %>%
  mutate(PtsDiff = Score - OScore)

# create the advanced stats formulas and apply them to the regular season data
stats_man <- stats_man %>%
  mutate(Poss = 0.96*(FGA + TO + (0.475*FTA) - OR),
         OffRtg = 100 * (Score / Poss),
         DefRtg = 100 * (OScore / Poss),
         NetRtg = OffRtg - DefRtg,
         AstRatio = 100 * Ast / (FGA + (0.475*FTA) + Ast + TO),
         TORatio = 100 * TO / (FGA *(0.475*FTA) + Ast + TO),
         TSPerc = 100 * Score / (2 * (FGA + (0.475 * FTA))),
         EFGPerc = FGM + (0.5 * FGM3) / FGA,
         FTRate = FTA / FGA) %>%
  select(Season, DayNum, T1, Score, T2, OScore, NumOT, Poss, OffRtg, DefRtg, NetRtg, AstRatio,
         TORatio, TSPerc, EFGPerc, FTRate, PtsDiff)

# create the advanced stats data set for the tournament data
# swap the order of teams to double the data
stats_tourn <- rbind(tourney_stats %>%
                       select(Season, DayNum, T1=WTeamID, Score=WScore, OScore=LScore, T2=LTeamID, WLoc, NumOT,
                              FGM=WFGM, FGA=WFGA, FGM3=WFGM3, FGA3=WFGA3, FTM=WFTM, FTA=WFTA, OR=WOR, DR=WDR, Ast=WAst,
                              TO=WTO, Stl=WStl, Blk=WBlk, PF=WPF, OFGM=LFGM, OFGM3=LFGM3, OFGA3=LFGA3, OFTM=LFTM,
                              OFTA=LFTA, O_OR=LOR, ODR=LDR, OAst=LAst, OTO=LTO, OStl=LStl, OBlk=LBlk,
                              OPF=LPF) %>%
                       mutate(Winner=1),
                     
                     reg_season_stats %>%
                       select(Season, DayNum, T1=LTeamID, Score=LScore, T2=WTeamID, OScore=WScore, WLoc, NumOT,
                              FGM=LFGM, FGA=LFGA, FGM3=LFGM3, FGA3=WFGA3, FTM=LFTM, FTA=LFTA, OR=LOR, DR=LDR, Ast=LAst,
                              TO=LTO, Stl=LStl, Blk=LBlk, PF=LPF, OFGM=WFGM, OFGM3=WFGM3, OFGA3=LFGA3, OFTM=WFTM,
                              OFTA=WFTA, O_OR=WOR, ODR=WDR, OAst=WAst, OTO=WTO, OStl=WStl, OBlk=WBlk,
                              OPF=WPF) %>%
                       mutate(Winner=0)) %>%
  mutate(PtsDiff = Score - OScore)

# create the advanced stats formulas and apply them to the tournament data
stats_tourn <- stats_tourn %>%
  mutate(Poss = 0.96*(FGA + TO + (0.475*FTA) - OR),
         OffRtg = 100 * (Score / Poss),
         DefRtg = 100 * (OScore / Poss),
         NetRtg = OffRtg - DefRtg,
         AstRatio = 100 * Ast / (FGA + (0.475*FTA) + Ast + TO),
         TORatio = 100 * TO / (FGA *(0.475*FTA) + Ast + TO),
         TSPerc = 100 * Score / (2 * (FGA + (0.475 * FTA))),
         EFGPerc = FGM + (0.5 * FGM3) / FGA,
         FTRate = FTA / FGA) %>%
  select(Season, DayNum, T1, Score, T2, OScore, NumOT, Poss, OffRtg, DefRtg, NetRtg, AstRatio,
         TORatio, TSPerc, EFGPerc, FTRate, PtsDiff)

# select march madness teams only and assign based on seeds per season
march_teams = select(seeds, Season, Team = TeamID)
X =  stats_man %>% 
  inner_join(march_teams, by = c("Season" = "Season", "T1" = "Team")) %>% 
  inner_join(march_teams, by = c("Season" = "Season", "T2" = "Team")) %>% 
  select(Season, T1, T2, Score, OScore, NumOT, Poss, OffRtg, DefRtg, NetRtg, AstRatio,
         TORatio, TSPerc, EFGPerc, FTRate) %>% distinct()
X$T1 = as.factor(X$T1)
X$T2 = as.factor(X$T2)

# fit GLMM on regular season data - extract random effects for each team
quality = list()
for (season in unique(X$Season)) {
  glmm = glmer(I(Score > OScore) ~  (1 | T1) + (1 | T2), data = X[X$Season == season & X$NumOT == 0, ], family = binomial) 
  random_effects = ranef(glmm)$T1
  quality[[season]] = data.frame(Season = season, Team_Id = as.numeric(row.names(random_effects)), quality = exp(random_effects[,"(Intercept)"]))
}
quality = do.call(rbind, quality)

# regular season statistics
season_summary = 
  stats_man %>%
  mutate(win14days = ifelse(DayNum > 118 & Score > OScore, 1, 0),
         last14days = ifelse(DayNum > 118, 1, 0)) %>% 
  group_by(Season, T1) %>%
  summarize(
    WinRatio14d = sum(win14days) / sum(last14days),
    PointsMean = mean(Score),
    PointsMedian = median(Score),
    PointsDiffMean = mean(Score - OScore),
    AvgPossessions = mean(Poss), 
    MedianPossessions = median(Poss),
    NetMean = mean(NetRtg), 
    AstRatioMean = mean(AstRatio), 
    TORatioMean = mean(TORatio), 
    DefRtgMean = mean(DefRtg), 
  )

season_summary_X1 = season_summary
season_summary_X2 = season_summary
names(season_summary_X1) = c("Season", "T1", paste0("X1_",names(season_summary_X1)[-c(1,2)]))
names(season_summary_X2) = c("Season", "T2", paste0("X2_",names(season_summary_X2)[-c(1,2)]))


# combine all features into a data frame
data_matrix =
  stats_tourn %>% 
  left_join(season_summary_X1, by = c("Season", "T1")) %>% 
  left_join(season_summary_X2, by = c("Season", "T2")) %>%
  left_join(select(seeds, Season, T1 = TeamID, Seed1 = Seed), by = c("Season", "T1")) %>% 
  left_join(select(seeds, Season, T2 = TeamID, Seed2 = Seed), by = c("Season", "T2")) %>% 
  mutate(SeedDiff = Seed1 - Seed2) %>%
  left_join(select(quality, Season, T1 = Team_Id, quality_march_T1 = quality), by = c("Season", "T1")) %>%
  left_join(select(quality, Season, T2 = Team_Id, quality_march_T2 = quality), by = c("Season", "T2"))


# prepare xgboost 
features = setdiff(names(data_matrix), c("Season", "DayNum", "T1", "T2", "Score", "OScore", "PtsDiff", "NumOT",
                                         "Poss", "OffRtg", "DefRtg", "NetRtg", "AstRatio", "TORatio",
                                         "TSPerc", "EFGPerc", "FTRate"))
dtrain = xgb.DMatrix(as.matrix(data_matrix[, features]), label = data_matrix$PtsDiff)

# create a Cauchy objective function in order to ensure optimization for MAE and not MSE
cauchyobj <- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  c <- 5000 
  x <-  preds-labels
  grad <- x / (x^2/c^2+1)
  hess <- -c^2*(x^2-c^2)/(x^2+c^2)^2
  return(list(grad = grad, hess = hess))
}

xgb_parameters = 
  list(objective = cauchyobj, 
       eval_metric = "mae",
       booster = "gbtree", 
       eta = 0.02,
       subsample = 0.35,
       colsample_bytree = 0.7,
       num_parallel_tree = 10,
       min_child_weight = 40,
       gamma = 10,
       max_depth = 3)

N = nrow(data_matrix)
fold5list = c(
  rep( 1, floor(N/5) ),
  rep( 2, floor(N/5) ),
  rep( 3, floor(N/5) ),
  rep( 4, floor(N/5) ),
  rep( 5, N - 4*floor(N/5) )
)

# build cross-validation model, repeated 10-times
iteration_count = c()
smooth_model = list()

for (i in 1:10) {
  
  # resample fold split
  set.seed(i)
  folds = list()  
  fold_list = sample(fold5list)
  for (k in 1:5) folds[[k]] = which(fold_list == k)
  
  set.seed(120)
  xgb_cv = 
    xgb.cv(
      params = xgb_parameters,
      data = dtrain,
      nrounds = 30,
      verbose = 0,
      nthread = 12,
      folds = folds,
      early_stopping_rounds = 25,
      maximize = FALSE,
      prediction = TRUE
    )
  iteration_count = c(iteration_count, xgb_cv$best_iteration)
  
  # fit a smoothed GAM model on predicted result point differential to get probabilities
  smooth_model[[i]] = smooth.spline(x = xgb_cv$pred, y = ifelse(data_matrix$PtsDiff > 0, 1, 0))
  
}

# build submission models
submission_model = list()

for (i in 1:10) {
  set.seed(i)
  submission_model[[i]] = 
    xgb.train(
      params = xgb_parameters,
      data = dtrain,
      nrounds = round(iteration_count[i]*1.05),
      verbose = 0,
      nthread = 12,
      maximize = FALSE,
      prediction = TRUE
    )
}


# run predictions
sub$Season = 2019
sub$T1 = as.numeric(substring(sub$ID,6,9))
sub$T2 = as.numeric(substring(sub$ID,11,14))

Z = sub %>% 
  left_join(season_summary_X1, by = c("Season", "T1")) %>% 
  left_join(season_summary_X2, by = c("Season", "T2")) %>%
  left_join(select(seeds, Season, T1 = TeamID, Seed1 = Seed), by = c("Season", "T1")) %>% 
  left_join(select(seeds, Season, T2 = TeamID, Seed2 = Seed), by = c("Season", "T2")) %>% 
  mutate(SeedDiff = Seed1 - Seed2) %>%
  left_join(select(quality, Season, T1 = Team_Id, quality_march_T1 = quality), by = c("Season", "T1")) %>%
  left_join(select(quality, Season, T2 = Team_Id, quality_march_T2 = quality), by = c("Season", "T2"))

a <- as.matrix(Z[, features])
dtest = xgb.DMatrix(as.matrix(Z[, features]))

probs = list()
for (i in 1:10) {
  preds = predict(submission_model[[i]], dtest)
  probs[[i]] = predict(smooth_model[[i]], preds)$y
}
Z$Pred = Reduce("+", probs) / 10

# better be safe than sorry
Z$Pred[Z$Pred <= 0.025] = 0.025
Z$Pred[Z$Pred >= 0.975] = 0.975

# anomaly event happened only once before
Z$Pred[Z$Seed1 == 16 & Z$Seed2 == 1] = 0
Z$Pred[Z$Seed1 == 15 & Z$Seed2 == 2] = 0
Z$Pred[Z$Seed1 == 14 & Z$Seed2 == 3] = 0
Z$Pred[Z$Seed1 == 13 & Z$Seed2 == 4] = 0
Z$Pred[Z$Seed1 == 1 & Z$Seed2 == 16] = 1
Z$Pred[Z$Seed1 == 2 & Z$Seed2 == 15] = 1
Z$Pred[Z$Seed1 == 3 & Z$Seed2 == 14] = 1
Z$Pred[Z$Seed1 == 4 & Z$Seed2 == 13] = 1

write.csv(select(Z, ID, Pred), "sub.csv", row.names = FALSE)







rm(list=ls())
devtools::install_github('zachmayer/kaggleNCAA')
setwd("/Users/m/Desktop/ADS/Data Mining I/Project/Womens Tournament")
file = ('sub.csv')
preds = read.csv(file = file, head=TRUE, sep=",")

set.seed(1)
library('kaggleNCAA')
dat <- parseBracket(file, w=1) 
sim <- simTourney(dat, 1000, year=2019, progress=TRUE)

bracket <- extractBracket(sim)
printableBracket(bracket) 


















