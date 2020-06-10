library(tidyverse)
library(bigrquery)
library(DBI)
library(jsonlite)
library(httr)


### First star

base_nhl_url <- "https://records.nhl.com/site/api" 

franchise_url<-paste0(base_nhl_url,"/franchise")

franchise_GET<-GET(franchise_url)%>%
  content(as="text")%>%
  fromJSON(flatten = TRUE)

franchise_GET$data$teamCommonName

### Second star

team_total_url<-paste0(base_nhl_url,"/franchise-team-totals")

team_total_GET<-GET(team_total_url)%>%
  content(as="text")%>%
  fromJSON(flatten = TRUE)

### Third star

PIT_records_url<-paste0(base_nhl_url,"/franchise-season-records?cayenneExp=franchiseId=","17")

PIT_records_GET<-GET(PIT_records_url)%>%
  content(as="text")%>%
  fromJSON(flatten = TRUE)

### Fourth star

PIT_goalie_url<-paste0(base_nhl_url,"/franchise-goalie-records?cayenneExp=franchiseId=","17")

PIT_goalie_GET<-GET(PIT_goalie_url)%>%
  content(as="text")%>%
  fromJSON(flatten = TRUE)

### Fifth star

PIT_skater_url<-paste0(base_nhl_url,"/franchise-skater-records?cayenneExp=franchiseId=","17")

PIT_skater_GET<-GET(PIT_skater_url)%>%
  content(as="text")%>%
  fromJSON(flatten = TRUE)

### Timeline table------------------------------------------------------------------------

PIT_records_GET_df<-as.data.frame(PIT_records_GET)
names(PIT_records_GET_df)<-substring(names(PIT_records_GET_df),6)

### build accolades table

PIT_records_accolade<-PIT_records_GET_df%>%select(-ends_with("Seasons"), -ends_with("Streak"), -ends_with("Dates"),-"id")

accolades_table<-as.data.frame(t(PIT_records_accolade))%>%
  rownames_to_column("Accolade")%>%
  .[-18,]%>%
  rename("Metric"="1")

### build seasons table

PIT_records_season<-PIT_records_GET_df%>%select(ends_with("Seasons"))

names(PIT_records_season)=str_remove(names(PIT_records_season),"Seasons")

seasons_table<-as.data.frame(t(PIT_records_season))%>%
  rownames_to_column("Accolade")%>%
  rename("Season"="1")

### build streak table

PIT_records_streak<-PIT_records_GET_df%>%select(ends_with("Streak"))

names(PIT_records_streak)=str_remove(names(PIT_records_streak),"Streak")

streak_table<-as.data.frame(t(PIT_records_streak))%>%
  rownames_to_column("Accolade")%>%
  rename("Streak"="1")

### build dates table

PIT_records_dates<-PIT_records_GET_df%>%select(ends_with("StreakDates"))

names(PIT_records_dates)=str_remove(names(PIT_records_dates),"StreakDates")

dates_table<-as.data.frame(t(PIT_records_dates))%>%
  rownames_to_column("Accolade")%>%
  rename("Dates"="1")

### TABLE ONE

accolade_names<-c(accolades_table$Accolade)

first_nice_names<-toupper(gsub("([[:lower:]])([[:upper:]])", "\\1 \\2", accolade_names))

best_years<-full_join(accolades_table,seasons_table)%>%
  mutate("Accolade"=first_nice_names)%>%
  filter(Accolade != "FRANCHISE ID" & Accolade != "FRANCHISE NAME")

### TABLE TWO

streak_names<-c(streak_table$Accolade)

second_nice_names<-toupper(gsub("([[:lower:]])([[:upper:]])", "\\1 \\2", streak_names))

best_streaks<-full_join(streak_table,dates_table)%>%
  mutate("Accolade"=second_nice_names)

first_nice_names<-c("Fewest Goals","Fewest Goals Against","Fewest Losses","Fewest Points","Fewest Ties","Fewest Wins",
                    "Franchise ID","Franchise Name","Most Game Goals","Most Goals", "Most Goals Against","Most Losses",
                    "Most Penalty Minutes", "Most Points", "Most Shutouts","Most Ties","Most Wins")

### Timeline Diagram-----------------------------------

names(franchise_GET)<-substring(names(franchise_GET),6)%>%
  data[[1]]

franchise_table<-franchise_GET[[1]]  
  
franchise_years<-franchise_table%>%
  mutate("first_year"=as.numeric(substring(firstSeasonId,1,4)),
         "last_year"=as.numeric(ifelse(is.na(lastSeasonId),2020,substring(lastSeasonId,5,))),
         "full_name"=paste(teamPlaceName,teamCommonName))

franchise_years%>%
ggplot(aes(x=first_year, xend=last_year, y=reorder(full_name,desc(full_name)), yend=full_name))+
  geom_segment(size=6, color="blue")+
  xlab("Years Active")+
  scale_x_continuous(breaks=seq(1915, 2020, 5))+
  ylab("Franchise Name")+
  labs(title="NHL Franchises: Years Active")


help(scale_x_continuous)
  as.Date

### Team Total Digs------------------
team_total_table<-team_total_GET[[1]]
  
summed_games_penalties<-team_total_table%>%
  select(franchiseId,teamName,gamesPlayed,wins,losses,ties,penaltyMinutes)%>%
  group_by(franchiseId,teamName)%>%
  summarize_all(funs(sum))%>%
  mutate("win_percent"=wins/gamesPlayed,"pen_mins_game"=penaltyMinutes/gamesPlayed)

nhl_conferences<-read_csv("C:\\Users\\ocwag\\OneDrive\\Desktop\\Stat-for-Data-Science\\Project_1\\Project_1\\nhl_conferences.csv")

games_penalties_conferences<-nhl_conferences%>%
  select(-teamName)%>%
  full_join(summed_games_penalties,nhl_conferences,by="franchiseId")%>%
  filter(conference !="Defunct")

games_penalties_conferences%>%
ggplot(aes(x=conference,y=pen_mins_game,color=conference))+
  geom_boxplot()+
  geom_jitter()+
  ylab("Penalty Minutes Per Game")

games_penalties_conferences%>%
  group_by(conference)%>%
  summarize(Average=mean(pen_mins_game))



###Goalie Stats-----------------------------

### Goalie

PIT_goalie_table<-PIT_goalie_GET[[1]]

PIT_goalie_table

pit_goalie_game_season<-PIT_goalie_table%>%
  select(mostGoalsAgainstOneGame, mostSavesOneGame, mostShotsAgainstOneGame, 
         mostShutoutsOneSeason, mostWinsOneSeason)

goalie_tab_colnames<-c(colnames(pit_goalie_game_season))

goalie_nice_names<-toupper(gsub("([[:lower:]])([[:upper:]])", "\\1 \\2", goalie_tab_colnames))

help(rename_all)

colnames(pit_goalie_game_season)<-goalie_nice_names

pit_goalie_game_season%>%
  sapply(summary,digits=2)

### Goalie Diagram

tidy_goalie_table<-PIT_goalie_table%>%
  select(lastName,wins,losses,ties)%>%
  mutate("total_games"=rowSums(.[,c("wins","ties","losses")],na.rm =TRUE))%>%
  gather(key="game_outcome",value="games",wins,losses,ties)
  

tidy_goalie_table%>%
  ggplot(aes(x=reorder(lastName,desc(total_games)),y=games))+
  geom_col(aes(fill=game_outcome))+
  theme(axis.text.x = element_text(angle = 90))+
  xlab("Goalie")+
  ylab("Total Games")+
  labs(fill="Game Outcome")

###Skate stats------------------------------------


PIT_skater_table<-PIT_skater_GET[[1]]

View(PIT_skater_table)

pos_labels<-(c(C="Center",D ="Defense",L="Left Wing",R="Right Wing"))

PIT_skater_table%>%
  ggplot(aes(x=seasons, y=penaltyMinutes))+
  geom_point()+
  geom_smooth(method=lm,se=F)+
  ylab("Penalty Minutes")+
  facet_grid(~positionCode,labeller = labeller(positionCode=pos_labels))

