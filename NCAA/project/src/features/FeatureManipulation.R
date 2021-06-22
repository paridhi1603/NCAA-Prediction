rm(list = ls())
library(data.table)
library(caret)
library(reshape2)
library(tidyr)


test <- fread('./project/volume/data/raw/MSampleSubmissionStage2.csv')
season <- fread('./project/volume/data/raw/MRegularSeasonDetailedResults.csv')
tourney <- fread('./project/volume/data/raw/MNCAATourneyDetailedResults.csv')
ranks <- fread('./project/volume/data/raw/MMasseyOrdinals.csv')


All_Games_Table <- rbind(season,tourney)

W_stats <- All_Games_Table[,.(Season, DayNum, WTeamID, WScore, WFGM, WFGA, WFGM3, WFGA3, WFTM, WFTA ,WOR ,WDR , WAst, WTO, WStl, WBlk ,WPF)]
L_stats <- All_Games_Table[,.(Season, DayNum, LTeamID, LScore, LFGM, LFGA, LFGM3, LFGA3, LFTM, LFTA ,LOR ,LDR , LAst, LTO, LStl, LBlk ,LPF)]

colnames(W_stats) <- c("Season", "DayNum", "TeamID", "Score", "FGM", "FGA", "FGM3", "FGA3", "FTM", "FTA" ,"OR" ,"DR" , "Ast", "TO", "Stl", "Blk" ,"PF")
colnames(L_stats) <- c("Season", "DayNum", "TeamID", "Score", "FGM", "FGA", "FGM3", "FGA3", "FTM", "FTA" ,"OR" ,"DR" , "Ast", "TO", "Stl", "Blk" ,"PF")

master_stats <- rbind(W_stats,L_stats)

stats_by_day <- NULL
for (i in 1:max(master_stats$DayNum))
{
  sub_master_stats <- master_stats[DayNum < i]
  team_stats_by_day <- data.table::dcast(setDT(sub_master_stats), TeamID+Season~., mean , value.var = c("FGM", "Score", "FGA", "FTM", "FTA", "FGA3", "FTM", "FTA" ,"OR" ,"DR" , "Ast", "TO", "Stl", "Blk" ,"PF"))
  
  team_stats_by_day$DayNum <- i
  
  stats_by_day <- rbind(stats_by_day, team_stats_by_day)
}


stats_by_day$TeamID <- as.character(stats_by_day$TeamID)


#- Clean test

#test <- data.table(matrix(unlist(strsplit(test$id,"_")),ncol=2,byrow=T))
#setnames(test,c("V1","V2"),c("team_1","team_2"))

test<- data.table(matrix(unlist(strsplit(test$ID,"_")),ncol=3,byrow=T))
setnames(test,c("V1","V2","V3"),c("Season","team_1","team_2"))


#test$DayNum<-max(d2$Season==2021,d2$DayNum)+1


test$DayNum<-max(tourney$Season == 2021,tourney$DayNum)
#test$DayNum <- max(season[Season == 2021,DayNum]) + 1
test$Result <- 0.5

#- initializing train

train <- rbind(season,tourney)
train <- train[,.(WTeamID,LTeamID,Season,DayNum)]
setnames(train,c("WTeamID","LTeamID"),c("team_1","team_2"))

train$Result <- 1

#- make master data file

master <- rbind(train,test)

#- ensure my team ids are characters
master$team_1 <- as.character(master$team_1)
master$team_2 <- as.character(master$team_2)

master$Season <- as.integer(master$Season)


temp <- merge(master, stats_by_day, by.x = c("team_1", "Season", "DayNum"), by.y = c("TeamID", "Season", "DayNum"), all.x = T)
master <- merge(temp, stats_by_day, by.x = c("team_2", "Season", "DayNum"), by.y = c("TeamID", "Season", "DayNum"), all.x = T)

master$FGMdif<- master$FGM.x-master$FGM.y
master$Scoredif<- master$Score.x-master$Score.y
master$FGAdif<- master$FGA.x-master$FGA.y
master$FTMdif<- master$FTM.x-master$FTM.y
master$FTAdif<- master$FTA.x-master$FTA.y
master$ORdif<- master$OR.x-master$OR.y
master$FGA3dif<- master$FGA3.x-master$FGA3.y
master$DRdif<- master$DR.x-master$DR.y
master$Astdif<- master$Ast.x-master$Ast.y
master$TOdif<- master$TO.x-master$TO.y
master$Stldif<- master$Stl.x-master$Stl.y
master$Blkdif<- master$Blk.x-master$Blk.y
master$PFdif<- master$PF.x-master$PF.y

master<-na.omit(master)


#- teams' rank often change the day of a game so don't want to use the 'future'
# values. we ofset them by one.
ranks$Season<- as.integer(ranks$Season)
ranks$DayNum <- ranks$RankingDayNum+1

#- you should optimize the following by creating a list of the systems and 
#- creating a loop to add them into the table

#- following is the list of the five systems you should use. Your mission, should 
# you decide to accept it is to turn the big chunk of code starting at line #48ish
# into a for loop to incorporate the five rankings
which_system <- c("POM","SAG","MOR","DOK")
master$Season<-as.integer(master$Season)

#- start here
#- subset the ranks table
for (i in (which_system))
{
  
  #subset_ranks<- ranks[SystemName == which_system][,.(Season,DayNum,TeamID,OrdinalRank)]
  
  one_rank <- ranks[SystemName == i][,.(Season,DayNum,TeamID,OrdinalRank)]
  
  #- prep and join into the first team
  setnames(one_rank,"TeamID","team_1")
  one_rank$team_1 <- as.character(one_rank$team_1)
  setkey(master,Season,team_1,DayNum)
  setkey(one_rank,Season,team_1,DayNum)
  
  #- join here
  master <- one_rank[master,roll=T]
  setnames(master,"OrdinalRank","team_1_rank")
  
  
  #- prep and merge into the second team
  setnames(one_rank,"team_1","team_2")
  setkey(master,Season,team_2,DayNum)
  setkey(one_rank,Season,team_2,DayNum)
  
  master <- one_rank[master,roll=T]
  setnames(master,"OrdinalRank","team_2_rank")
  
  #subtract the rankings for a new variable
  master$rank_dif <- master$team_2_rank-master$team_1_rank
  
  master$team_1_rank <- NULL
  master$team_2_rank <- NULL
  setnames(master,"rank_dif",paste0(i,"_dif"))
  
  # end here
}

####INITIAL TAABLES CODE STARTS HERE

#- clean up the data
master <- master[order(Season,DayNum)]


#- get rid of id variables and nas ( you should keep the ids, Season and Day)
master <- master[,.(team_1,team_2,POM_dif, SAG_dif, MOR_dif, DOK_dif, Scoredif, FTMdif, FTAdif, PFdif, Blkdif, Stldif, TOdif, Astdif, DRdif, FGA3dif, ORdif, Result)]

master <- master[!is.na(master$POM_dif)]
#master <- master[!is.na(master$PIG_dif)]
master <- master[!is.na(master$SAG_dif)]
master <- master[!is.na(master$MOR_dif)]
master <- master[!is.na(master$DOK_dif)]
# #add all


#split back into train and test
test <- master[Result == 0.5]
train <- master[Result == 1]

#- divide so I have losses 
rand_inx <- sample(1:nrow(train),nrow(train)*0.5)
train_a <- train[rand_inx,]
train_b <- train[!rand_inx,]

#- train_b will encode the loses
train_b$Result <- 0
train_b$POM_dif <- train_b$POM_dif*-1
#train_b$PIG_dif <- train_b$PIG_dif*-1
train_b$SAG_dif <- train_b$SAG_dif*-1
train_b$MOR_dif <- train_b$MOR_dif*-1
train_b$DOK_dif <- train_b$DOK_dif*-1
#train_b$FGMdif<- NULL
train_b$Scoredif<- train_b$Scoredif*-1
#train_b$FGAdif<- NULL
train_b$FTMdif<- train_b$FTMdif*-1
train_b$FTAdif<- train_b$FTAdif*-1
train_b$ORdif<- train_b$ORdif*-1
train_b$FGA3dif<- train_b$FGA3dif*-1
train_b$DRdif<- train_b$DRdif*-1
train_b$Astdif<- train_b$Astdif*-1
train_b$TOdif<- train_b$TOdif*-1
train_b$Stldif<- train_b$Stldif*-1
train_b$Blkdif<- train_b$Blkdif*-1
train_b$PFdif<- train_b$PFdif*-1

setnames(train_b,c("team_1","team_2"),c("team_2","team_1"))

train <- rbind(train_a,train_b)


fwrite(test,'./project/volume/data/interim/test.csv')
fwrite(train,'./project/volume/data/interim/train.csv')













