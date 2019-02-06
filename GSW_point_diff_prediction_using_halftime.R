## Get last season data
# scraping package
install.packages("rvest")
library(rvest)

# pipe package
install.packages("magrittr")
library(magrittr)

install.packages("xml2")
library(xml2)

library(htmltab)
library(reshape2) ### Transform into one row

## Function of getting the third quarter data
#
scrap_home_team_half_stat<- function(x){
  row_start = which(grepl("Start of 1",x[,6]))
  row_end = which(grepl("3rd",x[,6]))
  FTM = sum(grepl("makes free", x[row_start:row_end,6]))
  FTA = sum(grepl("misses free", x[row_start:row_end,6]))+FTM
  FGM = sum(grepl("makes", x[row_start:row_end,6]))-FTM
  FGA = sum(grepl("misses", x[row_start:row_end,6])) - sum(grepl("misses free", x[row_start:row_end,6])) + FGM
  threePM = sum(grepl("makes 3", x[row_start:row_end,6]))
  threePA = sum(grepl("misses 3", x[row_start:row_end,6]))+threePM
  trb = sum(grepl("[Rr]ebound",x[row_start:row_end,6]))
  orb = sum(grepl("[Oo]ffensive rebound", x[row_start:row_end,6]))
  deb = sum(grepl("[Dd]efensive rebound", x[row_start:row_end,6]))
  ast = sum(grepl("[Aa]ssist", x[row_start:row_end,6]))
  stl = sum(grepl("[Ss]teal", x[row_start:row_end,2]))
  blk = sum(grepl("[Bb]lock", x[row_start:row_end,2]))
  tov = sum(grepl("[Tt]urnover", x[row_start:row_end,6]))
  pf = sum(grepl("[Ss]hooting foul", x[row_start:row_end,2]))+sum(grepl("[Pp]ersonal foul", x[row_start:row_end,6]))
  x1 <- data.frame("team_FGM_half" = FGM, "team_FGA_half" = FGA, "team_3PM_half" = threePM, "team_3PA_half" = threePA, 
                   "team_FTM_half" = FTM, "team_FTA_half" = FTA, "team_ORB_half" = orb, "team_DRB_half" = deb, 
                   "team_TRB_half" = trb, "team_AST_half" = ast, "team_STL_half" = stl, "team_BLK_half" = blk, 
                   "team_TOV_half" = tov, "team_PF_half" = pf)
  
  return(x1)
  
}
#
scrap_away_team_half_stat<- function(x){
  row_start = which(grepl("Start of 1",x[,2]))
  row_end = which(grepl("3rd",x[,2]))
  FTM = sum(grepl("makes free", x[row_start:row_end,2]))
  FTA = sum(grepl("misses free", x[row_start:row_end,2]))+FTM
  FGM = sum(grepl("makes", x[row_start:row_end,2]))-FTM
  FGA = sum(grepl("misses", x[row_start:row_end,2])) - sum(grepl("misses free", x[row_start:row_end,2])) + FGM
  threePM = sum(grepl("makes 3", x[row_start:row_end,2]))
  threePA = sum(grepl("misses 3", x[row_start:row_end,2]))+threePM
  trb = sum(grepl("[Rr]ebound",x[row_start:row_end,2]))
  orb = sum(grepl("[Oo]ffensive rebound", x[row_start:row_end,2]))
  deb = sum(grepl("[Dd]efensive rebound", x[row_start:row_end,2]))
  ast = sum(grepl("[Aa]ssist", x[row_start:row_end,2]))
  stl = sum(grepl("[Ss]teal", x[row_start:row_end,6]))
  blk = sum(grepl("[Bb]lock", x[row_start:row_end,6]))
  tov = sum(grepl("[Tt]urnover", x[row_start:row_end,2]))
  pf = sum(grepl("[Ss]hooting foul", x[row_start:row_end,6]))+sum(grepl("[Pp]ersonal foul", x[row_start:row_end,2]))
  x1 <- data.frame("team_FGM_half" = FGM, "team_FGA_half" = FGA, "team_3PM_half" = threePM, "team_3PA_half" = threePA, 
                   "team_FTM_half" = FTM, "team_FTA_half" = FTA, "team_ORB_half" = orb, "team_DRB_half" = deb, 
                   "team_TRB_half" = trb, "team_AST_half" = ast, "team_STL_half" = stl, "team_BLK_half" = blk, 
                   "team_TOV_half" = tov, "team_PF_half" = pf)
  
  return(x1)
  
}

#
scrap_home_opp_half_stat<- function(x){
  row_start = which(grepl("Start of 1",x[,6]))
  row_end = which(grepl("3rd",x[,6]))
  FTM = sum(grepl("makes free", x[row_start:row_end,6]))
  FTA = sum(grepl("misses free", x[row_start:row_end,6]))+FTM
  FGM = sum(grepl("makes", x[row_start:row_end,6]))-FTM
  FGA = sum(grepl("misses", x[row_start:row_end,6])) - sum(grepl("misses free", x[row_start:row_end,6])) + FGM
  threePM = sum(grepl("makes 3", x[row_start:row_end,6]))
  threePA = sum(grepl("misses 3", x[row_start:row_end,6]))+threePM
  trb = sum(grepl("[Rr]ebound",x[row_start:row_end,6]))
  orb = sum(grepl("[Oo]ffensive rebound", x[row_start:row_end,6]))
  deb = sum(grepl("[Dd]efensive rebound", x[row_start:row_end,6]))
  ast = sum(grepl("[Aa]ssist", x[row_start:row_end,6]))
  stl = sum(grepl("[Ss]teal", x[row_start:row_end,2]))
  blk = sum(grepl("[Bb]lock", x[row_start:row_end,2]))
  tov = sum(grepl("[Tt]urnover", x[row_start:row_end,6]))
  pf = sum(grepl("[Ss]hooting foul", x[row_start:row_end,2]))+sum(grepl("[Pp]ersonal foul", x[row_start:row_end,6]))
  x1 <- data.frame("opp_FGM_half" = FGM, "opp_FGA_half" = FGA, "opp_3PM_half" = threePM, "opp_3PA_half" = threePA, 
                   "opp_FTM_half" = FTM, "opp_FTA_half" = FTA, "opp_ORB_half" = orb, "opp_DRB_half" = deb, 
                   "opp_TRB_half" = trb, "opp_AST_half" = ast, "opp_STL_half" = stl, "opp_BLK_half" = blk, 
                   "opp_TOV_half" = tov, "opp_PF_half" = pf)
  
  return(x1)
  
}

#
scrap_away_opp_half_stat<- function(x){
  row_start = which(grepl("Start of 1",x[,2]))
  row_end = which(grepl("3rd",x[,2]))
  FTM = sum(grepl("makes free", x[row_start:row_end,2]))
  FTA = sum(grepl("misses free", x[row_start:row_end,2]))+FTM
  FGM = sum(grepl("makes", x[row_start:row_end,2]))-FTM
  FGA = sum(grepl("misses", x[row_start:row_end,2])) - sum(grepl("misses free", x[row_start:row_end,2])) + FGM
  threePM = sum(grepl("makes 3", x[row_start:row_end,2]))
  threePA = sum(grepl("misses 3", x[row_start:row_end,2]))+threePM
  trb = sum(grepl("[Rr]ebound",x[row_start:row_end,2]))
  orb = sum(grepl("[Oo]ffensive rebound", x[row_start:row_end,2]))
  deb = sum(grepl("[Dd]efensive rebound", x[row_start:row_end,2]))
  ast = sum(grepl("[Aa]ssist", x[row_start:row_end,2]))
  stl = sum(grepl("[Ss]teal", x[row_start:row_end,6]))
  blk = sum(grepl("[Bb]lock", x[row_start:row_end,6]))
  tov = sum(grepl("[Tt]urnover", x[row_start:row_end,2]))
  pf = sum(grepl("[Ss]hooting foul", x[row_start:row_end,6]))+sum(grepl("[Pp]ersonal foul", x[row_start:row_end,2]))
  x1 <- data.frame("opp_FGM_half" = FGM, "opp_FGA_half" = FGA, "opp_3PM_half" = threePM, "opp_3PA_half" = threePA, 
                   "opp_FTM_half" = FTM, "opp_FTA_half" = FTA, "opp_ORB_half" = orb, "opp_DRB_half" = deb, 
                   "opp_TRB_half" = trb, "opp_AST_half" = ast, "opp_STL_half" = stl, "opp_BLK_half" = blk, 
                   "opp_TOV_half" = tov, "opp_PF_half" = pf)
  
  return(x1)
  
}
#######################################################
#######################################################
# 2015
# Get each game team stats link, including team score and shot chart
team_stats_root <- read_html("https://www.basketball-reference.com/teams/GSW/2015_games.html")
each_game_team_stats_root <- team_stats_root %>% html_nodes("a") %>% html_attr('href') 
each_game_team_stats_root <- each_game_team_stats_root[grepl("/boxscores/",each_game_team_stats_root)]
each_game_team_stats <- each_game_team_stats_root[grepl(".html", each_game_team_stats_root)]
each_game_team_stats <- paste("https://www.basketball-reference.com",each_game_team_stats,sep = "")
each_game_team_stats <- gsub("/boxscores/","/boxscores/pbp/",each_game_team_stats)
each_game_team_stats = each_game_team_stats[-66]

##
schedule_1415_reg = htmltab("https://www.basketball-reference.com/teams/GSW/2015_games.html",which = 1)
schedule_1415_post = htmltab("https://www.basketball-reference.com/teams/GSW/2015_games.html",which = 2)
home = rbind(schedule_1415_reg, schedule_1415_post)
View(schedule_1415_reg)
home <- home[which(grepl("W|L", home[,7])),]
home <- gsub("@",0,home[,5])
home[is.na(home)] <- 1
View(home)


###########################################
## Get table
#
gsw_half_team_data_2015 = NULL
gsw_half_opp_data_2015 = NULL
for (x in 1:length(each_game_team_stats)){
  if(home[x] == 1){
    gsw_half_opp_data_2015<- rbind(gsw_half_opp_data_2015, scrap_away_opp_half_stat(htmltab(each_game_team_stats[x],which = 1, rm_nodata_cols = F)))
    gsw_half_team_data_2015 <- rbind(gsw_half_team_data_2015, scrap_home_team_half_stat(htmltab(each_game_team_stats[x],which = 1, rm_nodata_cols = F)))
  } else {
    gsw_half_opp_data_2015<- rbind(gsw_half_opp_data_2015, scrap_home_opp_half_stat(htmltab(each_game_team_stats[x],which = 1, rm_nodata_cols = F)))
    gsw_half_team_data_2015 <- rbind(gsw_half_team_data_2015, scrap_away_team_half_stat(htmltab(each_game_team_stats[x],which = 1, rm_nodata_cols = F)))
  }
  print(x)
}

row.names(gsw_half_team_data_2015) <- 1:nrow(gsw_half_team_data_2015)
row.names(gsw_half_opp_data_2015) <- 1:nrow(gsw_half_opp_data_2015)

View(gsw_half_team_data_2015)
View(gsw_half_opp_data_2015)



#######################################################
# 2016
# Get each game team stats link, including team score and shot chart
team_stats_root <- read_html("https://www.basketball-reference.com/teams/GSW/2016_games.html")
each_game_team_stats_root <- team_stats_root %>% html_nodes("a") %>% html_attr('href') 
each_game_team_stats_root <- each_game_team_stats_root[grepl("/boxscores/",each_game_team_stats_root)]
each_game_team_stats <- each_game_team_stats_root[grepl(".html", each_game_team_stats_root)]
each_game_team_stats <- paste("https://www.basketball-reference.com",each_game_team_stats,sep = "")
each_game_team_stats <- gsub("/boxscores/","/boxscores/pbp/",each_game_team_stats)
each_game_team_stats

##
schedule_1516_reg = htmltab("https://www.basketball-reference.com/teams/GSW/2016_games.html",which = 1)
schedule_1516_post = htmltab("https://www.basketball-reference.com/teams/GSW/2016_games.html",which = 2)
home = rbind(schedule_1516_reg, schedule_1516_post)
View(schedule_1516_reg)
home <- home[which(grepl("W|L", home[,7])),]
home <- gsub("@",0,home[,5])
home[is.na(home)] <- 1
View(home)


###########################################
## Get table
#
gsw_half_team_data_2016 = NULL
gsw_half_opp_data_2016 = NULL
for (x in 1:length(each_game_team_stats)){
  if(home[x] == 1){
    gsw_half_opp_data_2016<- rbind(gsw_half_opp_data_2016, scrap_away_opp_half_stat(htmltab(each_game_team_stats[x],which = 1, rm_nodata_cols = F)))
    gsw_half_team_data_2016 <- rbind(gsw_half_team_data_2016, scrap_home_team_half_stat(htmltab(each_game_team_stats[x],which = 1, rm_nodata_cols = F)))
  } else {
    gsw_half_opp_data_2016<- rbind(gsw_half_opp_data_2016, scrap_home_opp_half_stat(htmltab(each_game_team_stats[x],which = 1, rm_nodata_cols = F)))
    gsw_half_team_data_2016 <- rbind(gsw_half_team_data_2016, scrap_away_team_half_stat(htmltab(each_game_team_stats[x],which = 1, rm_nodata_cols = F)))
  }
}

row.names(gsw_half_team_data_2016) <- 1:nrow(gsw_half_team_data_2016)
row.names(gsw_half_opp_data_2016) <- 1:nrow(gsw_half_opp_data_2016)

View(gsw_half_team_data_2016)
View(gsw_half_opp_data_2016)

####################################################
# 2017
# Get each game team stats link, including team score and shot chart
team_stats_root <- read_html("https://www.basketball-reference.com/teams/GSW/2017_games.html")
each_game_team_stats_root <- team_stats_root %>% html_nodes("a") %>% html_attr('href') 
each_game_team_stats_root <- each_game_team_stats_root[grepl("/boxscores/",each_game_team_stats_root)]
each_game_team_stats <- each_game_team_stats_root[grepl(".html", each_game_team_stats_root)]
each_game_team_stats <- paste("https://www.basketball-reference.com",each_game_team_stats,sep = "")
each_game_team_stats <- gsub("/boxscores/","/boxscores/pbp/",each_game_team_stats)
each_game_team_stats

##
schedule_1617_reg = htmltab("https://www.basketball-reference.com/teams/GSW/2017_games.html",which = 1)
schedule_1617_post = htmltab("https://www.basketball-reference.com/teams/GSW/2017_games.html",which = 2)
home = rbind(schedule_1617_reg, schedule_1617_post)
View(schedule_1617_reg)
home <- home[which(grepl("W|L", home[,7])),]
home <- gsub("@",0,home[,5])
home[is.na(home)] <- 1
View(home)


###########################################
## Get table
#
gsw_half_team_data_2017 = NULL
gsw_half_opp_data_2017 = NULL
for (x in 1:length(each_game_team_stats)){
  if(home[x] == 1){
    gsw_half_opp_data_2017<- rbind(gsw_half_opp_data_2017, scrap_away_opp_half_stat(htmltab(each_game_team_stats[x],which = 1, rm_nodata_cols = F)))
    gsw_half_team_data_2017 <- rbind(gsw_half_team_data_2017, scrap_home_team_half_stat(htmltab(each_game_team_stats[x],which = 1, rm_nodata_cols = F)))
  } else {
    gsw_half_opp_data_2017<- rbind(gsw_half_opp_data_2017, scrap_home_opp_half_stat(htmltab(each_game_team_stats[x],which = 1, rm_nodata_cols = F)))
    gsw_half_team_data_2017 <- rbind(gsw_half_team_data_2017, scrap_away_team_half_stat(htmltab(each_game_team_stats[x],which = 1, rm_nodata_cols = F)))
  }
  print(x)
}

row.names(gsw_half_team_data_2017) <- 1:nrow(gsw_half_team_data_2017)
row.names(gsw_half_opp_data_2017) <- 1:nrow(gsw_half_opp_data_2017)

View(gsw_half_team_data_2017)
View(gsw_half_opp_data_2017)

#############################################
# 2018
# Get each game team stats link, including team score and shot chart
team_stats_root <- read_html("https://www.basketball-reference.com/teams/GSW/2018_games.html")
each_game_team_stats_root <- team_stats_root %>% html_nodes("a") %>% html_attr('href') 
each_game_team_stats_root <- each_game_team_stats_root[grepl("/boxscores/",each_game_team_stats_root)]
each_game_team_stats <- each_game_team_stats_root[grepl(".html", each_game_team_stats_root)]
each_game_team_stats <- paste("https://www.basketball-reference.com",each_game_team_stats,sep = "")
each_game_team_stats <- gsub("/boxscores/","/boxscores/pbp/",each_game_team_stats)
# each_game_team_stats <- each_game_team_stats[-1]
each_game_team_stats

##
schedule_1718_reg = htmltab("https://www.basketball-reference.com/teams/GSW/2018_games.html",which = 1)
schedule_1718_post = htmltab("https://www.basketball-reference.com/teams/GSW/2018_games.html",which = 2)
home = rbind(schedule_1718_reg, schedule_1718_post)
View(schedule_1718_reg)
home <- home[which(grepl("W|L", home[,7])),]
home <- gsub("@",0,home[,5])
home[is.na(home)] <- 1
View(home)


###########################################
## Get table
#
half_team_data = NULL
half_opp_data = NULL
for (x in 1:length(each_game_team_stats)){
  if(home[x] == 1){
    half_opp_data<- rbind(half_opp_data, scrap_away_opp_half_stat(htmltab(each_game_team_stats[x],which = 1, rm_nodata_cols = F)))
    half_team_data <- rbind(half_team_data, scrap_home_team_half_stat(htmltab(each_game_team_stats[x],which = 1, rm_nodata_cols = F)))
  } else {
    half_opp_data<- rbind(half_opp_data, scrap_home_opp_half_stat(htmltab(each_game_team_stats[x],which = 1, rm_nodata_cols = F)))
    half_team_data <- rbind(half_team_data, scrap_away_team_half_stat(htmltab(each_game_team_stats[x],which = 1, rm_nodata_cols = F)))
  }
}

row.names(half_team_data) <- 1:nrow(half_team_data)
row.names(half_opp_data) <- 1:nrow(half_opp_data)

View(half_team_data)
View(half_opp_data)

###########################################
## Get 2015 season 

library(htmltab)
schedule_1415_reg = htmltab("https://www.basketball-reference.com/teams/GSW/2015_games.html",which = 1)
schedule_1415_post = htmltab("https://www.basketball-reference.com/teams/GSW/2015_games.html",which = 2)
schedule_1415 = rbind(schedule_1415_reg, schedule_1415_post)

schedule_1415 <- schedule_1415[,-c(1,3,4)]
colnames(schedule_1415) <- c("Date", "home", "Opponent","W/L","Tm","Opp","W","L","Streak")
schedule_1415$home <- gsub("@",0,schedule_1415$home)
schedule_1415$home[is.na(schedule_1415$home)] <- 1
schedule_1415 <- schedule_1415[which(grepl("W|L", schedule_1415$`W/L`)),]
row.names(schedule_1415) <- 1:nrow(schedule_1415)
schedule_1415 = schedule_1415[-66,]
View(schedule_1415)

## Get 2016 season 

library(htmltab)
schedule_1516_reg = htmltab("https://www.basketball-reference.com/teams/GSW/2016_games.html",which = 1)
schedule_1516_post = htmltab("https://www.basketball-reference.com/teams/GSW/2016_games.html",which = 2)
schedule_1516 = rbind(schedule_1516_reg, schedule_1516_post)

schedule_1516 <- schedule_1516[,-c(1,3,4)]
colnames(schedule_1516) <- c("Date", "home", "Opponent","W/L","Tm","Opp","W","L","Streak")
schedule_1516$home <- gsub("@",0,schedule_1516$home)
schedule_1516$home[is.na(schedule_1516$home)] <- 1
schedule_1516 <- schedule_1516[which(grepl("W|L", schedule_1516$`W/L`)),]
row.names(schedule_1516) <- 1:nrow(schedule_1516)
View(schedule_1516)

## Get 2017 season 

library(htmltab)
schedule_1617_reg = htmltab("https://www.basketball-reference.com/teams/GSW/2017_games.html",which = 1)
schedule_1617_post = htmltab("https://www.basketball-reference.com/teams/GSW/2017_games.html",which = 2)
schedule_1617 = rbind(schedule_1617_reg, schedule_1617_post)

schedule_1617 <- schedule_1617[,-c(1,3,4)]
colnames(schedule_1617) <- c("Date", "home", "Opponent","W/L","Tm","Opp","W","L","Streak")
schedule_1617$home <- gsub("@",0,schedule_1617$home)
schedule_1617$home[is.na(schedule_1617$home)] <- 1
schedule_1617 <- schedule_1617[which(grepl("W|L", schedule_1617$`W/L`)),]
row.names(schedule_1617) <- 1:nrow(schedule_1617)
View(schedule_1617)

## Get 2018 season 

library(htmltab)
schedule_1718_reg = htmltab("https://www.basketball-reference.com/teams/GSW/2018_games.html",which = 1)
schedule_1718_post = htmltab("https://www.basketball-reference.com/teams/GSW/2018_games.html",which = 2)
schedule_1718 = rbind(schedule_1718_reg, schedule_1718_post)

schedule_1718 <- schedule_1718[,-c(1,3,4)]
colnames(schedule_1718) <- c("Date", "home", "Opponent","W/L","Tm","Opp","W","L","Streak")
schedule_1718$home <- gsub("@",0,schedule_1718$home)
schedule_1718$home[is.na(schedule_1718$home)] <- 1
schedule_1718 <- schedule_1718[which(grepl("W|L", schedule_1718$`W/L`)),]
row.names(schedule_1718) <- 1:nrow(schedule_1718)
View(schedule_1718)

#########################################
## Combine as one table
schedule_all = rbind(schedule_1415,schedule_1516,schedule_1617,schedule_1718)
team_half = rbind(gsw_half_team_data_2015,gsw_half_team_data_2016,gsw_half_team_data_2017,half_team_data)
opp_half = rbind(gsw_half_opp_data_2015,gsw_half_opp_data_2016,gsw_half_opp_data_2017,half_opp_data)
df_1418 = data.frame(schedule_all,team_half,opp_half)
df_1418$Tm = as.numeric(df_1418$Tm)
df_1418$Opp = as.numeric(df_1418$Opp)
df_1418$Dif = df_1418$Tm - df_1418$Opp
playoff_game = c(rep.int("0", 81),rep.int("1", 21),rep.int("0", 82),rep.int("1", 24),rep.int("0", 82) , rep.int("1", 17),rep.int("0", 82),rep.int("1", 21))
df_1418$playoff_game = playoff_game
df_1418$playoff_game = as.factor(df_1418$playoff_game)

# Save as df
write.csv(file="df_4season_half.csv", x=df_1418)




##################################################################################################
#######                     #########
#######      ANALYZE        #########
#######                     #########
###################################################################################################

###################################################################
install.packages("gbm")
library(gbm)
install.packages("randomForest")
library(randomForest)


## Get analysis result function
analyze <- function(var,df){
  set.seed(1000)
  test_instn= sample(nrow(df), 0.3*nrow(df)) 
  df_valid <- df [test_instn,] 
  df_train <- df [-test_instn,]
  ## Linear
  lm = lm(df_train$Dif ~ .,data = df_train[,var])
  print(summary(lm))
  linear_pred = predict(lm,newdata = df_valid[,var])
  ## Stepwise
  df_all <- lm(df_train$Dif~.,data = df_train[,var])
  df_null <- lm(df_train$Dif~1,data = df_train[,var])
  forward_model = step(df_null, scope=list(upper=df_all), direction="forward",trace = FALSE)
  backward_model = step(df_all,direction='backward',trace = FALSE)
  forward_model_both = step(df_null, scope=list(upper=df_all), direction="both",trace = FALSE)
  backward_model_both = step(df_all,direction='both',trace = FALSE)
  adj_r_sq_table = c(summary(forward_model)$adj.r.squared,summary(backward_model)$adj.r.squared,
                     summary(forward_model_both)$adj.r.squared,summary(backward_model_both)$adj.r.squared)
  
  print(paste("Forward R-square: ",summary(forward_model)$adj.r.squared))
  print(paste("Backward R-square: ",summary(backward_model)$adj.r.squared))
  print(paste("Forward both R-square: ",summary(forward_model_both)$adj.r.squared))
  print(paste("Forward both R-square: ",summary(backward_model_both)$adj.r.squared))
  ## Lasso/ridge
  library(glmnet)
  ridge = glmnet(as.matrix(df_train[,var]),df_train$Dif,alpha=0)
  ridge_cv <- cv.glmnet(as.matrix(df_train[,var]),df_train$Dif,alpha=0)
  best_lambda=ridge_cv$lambda.min
  ridge_pred = predict(ridge,s=best_lambda,newx=as.matrix(df_valid[,var])) 
  print(paste("Linear RMSE: ",sqrt(mean((df_valid$Dif-linear_pred)^2)),sep = ""))
  print(paste("Ridge RMSE: ",sqrt(mean((df_valid$Dif-ridge_pred)^2)),sep = ""))
  
  lasso = glmnet(as.matrix(df_train[,var]),df_train$Dif,alpha=1)
  
  lasso_cv <- cv.glmnet(as.matrix(df_train[,var]),df_train$Dif,alpha=1)
  best_lambda_lasso = lasso_cv$lambda.min
  
  lasso_pred = predict(lasso,s=best_lambda_lasso,newx=as.matrix(df_valid[,var])) 
  ## Get Lasso selected var
  lasso_best_lambda <- glmnet(as.matrix(df_train[,var]),df_train$Dif,alpha=1,lambda=best_lambda_lasso)
  coef_lasso_best_lambda <- coef(lasso_best_lambda) 
  coef_lasso_best_lambda_attr <- attributes(coef_lasso_best_lambda)
  var_idx_lasso_select <- coef_lasso_best_lambda_attr$i
  var_lasso_select <- coef_lasso_best_lambda_attr$Dimnames[[1]][var_idx_lasso_select]
  
  ##
  print(paste("Lasso RMSE: ",sqrt(mean((df_valid$Dif-lasso_pred)^2)),sep = ""))
  print(paste("Baseline RMSE: ",sqrt(mean((df_valid$Dif-mean(df$Dif))^2)),sep = ""))
  print("Lasso selected var: ")
  print(var_lasso_select)
  print(coef_lasso_best_lambda)
  
  ## 
  # random forest
  
  rf = randomForest(df_train$Dif ~ ., data=df_train[,var], ntree=500, importance = TRUE)
  rf_pred = predict(rf,newdata = df_valid[,var])
  rmse_rf = sqrt(mean((rf_pred - df_valid$Dif) ** 2)) 
  print(paste("Random Forest RMSE: ",rmse_rf,sep = ""))
  
  # GB
  
  gb = gbm(df_train$Dif ~ ., data=df_train[,var], distribution = "gaussian", n.trees = 10000, shrinkage = 0.01, interaction.depth = 3)
  n.trees = seq(from=100 ,to=10000, by=100)
  gb_pred = predict(gb,newdata = df_valid[,var],n.trees = n.trees)
  rmse_gb = sqrt(mean((gb_pred - df_valid$Dif) ** 2))
  print(paste("Gradient Boosting RMSE: ",rmse_gb,sep = ""))
  
}


############################
# test
###########################################
colnames(df_1418)
analyze(c(11,13,15:17,19:23,25,27,29:31,33:37),df_1418)
