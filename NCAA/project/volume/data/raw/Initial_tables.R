rm(list = ls())

library(data.table)
library(caret)
library(reshape2)

d1 <- fread("project/volume/data/raw/NCAATourneyDetailedResults.csv")
d2 <- fread("project/volume/data/raw/RegularSeasonDetailedResults.csv")

All_Games_Table <- rbind(d1,d2)

W_stats <- All_Games_Table[,.(Season, DayNum, WTeamID, WScore, WLoc, NumOT, WFGM, WFGA, WFGM3, WFGA3, WFTM, WFTA ,WOR ,WDR , WAst, WTO, WStl, WBlk ,WPF)]
L_stats <- All_Games_Table[,.(Season, DayNum, LTeamID, LScore, WLoc, NumOT, LFGM, LFGA, LFGM3, LFGA3, LFTM, LFTA ,LOR ,LDR , LAst, LTO, LStl, LBlk ,LPF)]

colnames(W_stats) <- c("Season", "DayNum", "TeamID", "Score", "Loc", "NumOT", "FGM", "FGA", "FGM3", "FGA3", "FTM", "FTA" ,"OR" ,"DR" , "Ast", "TO", "Stl", "Blk" ,"PF")
colnames(L_stats) <- c("Season", "DayNum", "TeamID", "Score", "Loc", "NumOT", "FGM", "FGA", "FGM3", "FGA3", "FTM", "FTA" ,"OR" ,"DR" , "Ast", "TO", "Stl", "Blk" ,"PF")

master_stats <- rbind(W_stats,L_stats)

stats_by_day <- NULL
for (i in 1:max(master_stats$DayNum)){
  
  sub_master_stats <- master_stats[DayNum < i]
  team_stats_by_day <- dcast(sub_master_stats, TeamID+Season~., mean , value.var = c("FGM"))
  
  team_stats_by_dat$DayNum <- 1
  
  stats_by_day <- rbind(stats_by_day, team_stats_by_day)
  
}

