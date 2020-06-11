Project 1 - Accessing the NHL API for Penguins Data
================
Owen Wagner
6/5/2020

  - [Describing JSON Data](#describing-json-data)
  - [Reading in Data](#reading-in-data)
      - [Reading in Franchise data](#reading-in-franchise-data)
      - [Reading in total stats for every
        franchise](#reading-in-total-stats-for-every-franchise)
      - [Reading in Penguins Franchise Record
        Data](#reading-in-penguins-franchise-record-data)
      - [Reading in Penguins Goalie
        Records](#reading-in-penguins-goalie-records)
      - [Reading in Penguins Skater
        Records](#reading-in-penguins-skater-records)
  - [Building summary tables and
    diagrams](#building-summary-tables-and-diagrams)
      - [Timeline graphic for NHL
        franchises](#timeline-graphic-for-nhl-franchises)
      - [Which conferences are the least law
        abiding?](#which-conferences-are-the-least-law-abiding)
      - [A look at the Penguins best
        years](#a-look-at-the-penguins-best-years)
          - [Timeline table](#timeline-table)
          - [build accolades table](#build-accolades-table)
          - [build seasons table](#build-seasons-table)
          - [build streak table](#build-streak-table)
          - [build dates table](#build-dates-table)
          - [Interactive Accolades Kable](#interactive-accolades-kable)
          - [Interactive Streak Kable](#interactive-streak-kable)
      - [Some Pittsburgh Goalie Stats](#some-pittsburgh-goalie-stats)
          - [Goalie statistics summary](#goalie-statistics-summary)
          - [Goalie game outcome diagram](#goalie-game-outcome-diagram)
      - [Skater Records - which positions rack up the penalty minutes
        fastest?](#skater-records---which-positions-rack-up-the-penalty-minutes-fastest)

# Describing JSON Data

JSON stands for **J**ava**S**cript **O**bject **N**otation and is a data
format whereby data can be efficiently transfered from server to client
and vice versa.

Communication between server and client can take place in many formats.
Some like html provide design, which may already exist on client device
and is data intensive. In some cases, however, you may only want to send
data. For simple data, information could be sent as plain text. However,
for more complex objects, plain text would be limiting in the sense that
parsing the data would be difficult on the client device. **JSON** on
the other hand is lightweight, [easy for humans to read and write and
easy for machines to parse and
generate](https://www.json.org/json-en.html).

For the purposes of R, JSON data can be accessed from APIs
[(**A**pplication **P**rogramming
**I**nterface)](https://www.mulesoft.com/resources/api/what-is-an-api),
which is itself a software intermediary that allows two applications to
talk to eachother. Once JSON data has been accessed however, it must be
converted into an object format that is recognizeable by R.

There are three primary packages available in R to perform this
conversion, `jsonlite()`, `rjsonio()` and `rjson()`. While all three can
convert JSON data to/from R objects they differ in terms of 1) ease of
use, 2) availability of funcitons to stream, validate and prettifu JSON
data and 3) their performance. [A biased comparison of JSON packages in
R](https://rstudio-pubs-static.s3.amazonaws.com/31702_9c22e3d1a0c44968a4a1f9656f1800ab.html)
runs a series of excercises comparising behavior and performance of JSON
packages in R. The takeaway from this comparison seems to be that
`jsonlite()` offers the best balance of functionality and performance.
For this reason and based on the fact that `jsonlite()` is the sole
package we have been exposed to in prior assignments, I have opted to
use it for this vignette.

# Reading in Data

In this section we are reading in data from the NHL API. This
corresponds to the first five stars on the Project PDF. For franchise
specific queries, I have elected to look into statistics for the
Pittsburgh Penguins.

<img src="C:\Users\ocwag\OneDrive\Desktop\Stat-for-Data-Science\Project_1\pens_logo.png" width="50%" />

## Reading in Franchise data

A base nhl url was created to facilitate queries on the NHL API.

``` r
base_nhl_url <- "https://records.nhl.com/site/api" 

franchise_url<-paste0(base_nhl_url,"/franchise")

franchise_GET<-GET(franchise_url)%>%
  content(as="text")%>%
  fromJSON(flatten = TRUE)
```

    ## No encoding supplied: defaulting to UTF-8.

## Reading in total stats for every franchise

``` r
team_total_url<-paste0(base_nhl_url,"/franchise-team-totals")

team_total_GET<-GET(team_total_url)%>%
  content(as="text")%>%
  fromJSON(flatten = TRUE)
```

    ## No encoding supplied: defaulting to UTF-8.

## Reading in Penguins Franchise Record Data

``` r
PIT_records_url<-paste0(base_nhl_url,"/franchise-season-records?cayenneExp=franchiseId=","17")

PIT_records_GET<-GET(PIT_records_url)%>%
  content(as="text")%>%
  fromJSON(flatten = TRUE)
```

    ## No encoding supplied: defaulting to UTF-8.

## Reading in Penguins Goalie Records

``` r
PIT_goalie_url<-paste0(base_nhl_url,"/franchise-goalie-records?cayenneExp=franchiseId=","17")

PIT_goalie_GET<-GET(PIT_goalie_url)%>%
  content(as="text")%>%
  fromJSON(flatten = TRUE)
```

    ## No encoding supplied: defaulting to UTF-8.

## Reading in Penguins Skater Records

``` r
PIT_skater_url<-paste0(base_nhl_url,"/franchise-skater-records?cayenneExp=franchiseId=","17")

PIT_skater_GET<-GET(PIT_skater_url)%>%
  content(as="text")%>%
  fromJSON(flatten = TRUE)
```

    ## No encoding supplied: defaulting to UTF-8.

# Building summary tables and diagrams

Below we take the necessary steps to make data tables more visually
appealing. At least one summary table, statistical table and/or diagram
has been generated for each of the five data queries above.

## Timeline graphic for NHL franchises

There have been 38 franchises in the history of the NHL not counting
teams that moved and were then returned (Winnipeg Jets, for example). Of
the 38, 7 have been permanently disbanded. While some teams have been
around since the early 1900s, most active teams, including the Penguins,
came into existance in the late 60s/ early 70s.

``` r
names(franchise_GET)<-substring(names(franchise_GET),6)

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
```

![](Project_1_Markdown_files/figure-gfm/timeline%20graphic-1.png)<!-- -->

## Which conferences are the least law abiding?

A new column has been created to show penalty minues per game to provide
a basis of comparison for older and newer teams. All teams have been
joined with their appropriate conferences to see which conference have
been most abiding of the rules over time.

The Answer? Atlantic conference teams rack up the fewest penalty minutes
per game, on average, and also show the tightest distribution around the
mean. Teams in the Metropolitan conference, by contrast rack up, on
average, an additional 1.6 minutes of penalty time per game.

``` r
team_total_table<-team_total_GET[[1]]
  
summed_games_penalties<-team_total_table%>%
  select(franchiseId,teamName,gamesPlayed,wins,losses,ties,penaltyMinutes)%>%
  group_by(franchiseId,teamName)%>%
  summarize_all(funs(sum))%>%
  mutate("win_percent"=wins/gamesPlayed,"pen_mins_game"=penaltyMinutes/gamesPlayed)
```

    ## Warning: funs() is soft deprecated as of dplyr 0.8.0
    ## Please use a list of either functions or lambdas: 
    ## 
    ##   # Simple named list: 
    ##   list(mean = mean, median = median)
    ## 
    ##   # Auto named with `tibble::lst()`: 
    ##   tibble::lst(mean, median)
    ## 
    ##   # Using lambdas
    ##   list(~ mean(., trim = .2), ~ median(., na.rm = TRUE))
    ## This warning is displayed once per session.

``` r
nhl_conferences<-read_csv("C:\\Users\\ocwag\\OneDrive\\Desktop\\Stat-for-Data-Science\\Project_1\\Project_1\\nhl_conferences.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   franchiseId = col_double(),
    ##   teamName = col_character(),
    ##   conference = col_character()
    ## )

``` r
games_penalties_conferences<-nhl_conferences%>%
  select(-teamName)%>%
  full_join(summed_games_penalties,nhl_conferences,by="franchiseId")%>%
  filter(conference !="Defunct")

games_penalties_conferences%>%
ggplot(aes(x=conference,y=pen_mins_game,color=conference))+
  geom_boxplot()+
  geom_jitter()+
  ylab("Penalty Minutes Per Game")
```

![](Project_1_Markdown_files/figure-gfm/penalty%20mins%20graphic-1.png)<!-- -->

``` r
kable(games_penalties_conferences%>%
  group_by(conference)%>%
  summarize("Average Penalty Minutes Per Game"=round(mean(pen_mins_game),1)))
```

| conference   | Average Penalty Minutes Per Game |
| :----------- | -------------------------------: |
| Atlantic     |                             13.3 |
| Metropolitan |                             14.9 |
| Pacific      |                             14.7 |
| Western      |                             14.5 |

## A look at the Penguins best years

### Timeline table

``` r
PIT_records_GET_df<-as.data.frame(PIT_records_GET)
names(PIT_records_GET_df)<-substring(names(PIT_records_GET_df),6)
```

### build accolades table

``` r
PIT_records_accolade<-PIT_records_GET_df%>%select(-ends_with("Seasons"), -ends_with("Streak"), -ends_with("Dates"),-"id")

accolades_table<-as.data.frame(t(PIT_records_accolade))%>%
  rownames_to_column("Accolade")%>%
  .[-18,]%>%
  rename("Metric"="1")
```

### build seasons table

``` r
PIT_records_season<-PIT_records_GET_df%>%select(ends_with("Seasons"))

names(PIT_records_season)=str_remove(names(PIT_records_season),"Seasons")

seasons_table<-as.data.frame(t(PIT_records_season))%>%
  rownames_to_column("Accolade")%>%
  rename("Season"="1")
```

### build streak table

``` r
PIT_records_streak<-PIT_records_GET_df%>%select(ends_with("Streak"))

names(PIT_records_streak)=str_remove(names(PIT_records_streak),"Streak")

streak_table<-as.data.frame(t(PIT_records_streak))%>%
  rownames_to_column("Accolade")%>%
  rename("Streak"="1")
```

### build dates table

``` r
PIT_records_dates<-PIT_records_GET_df%>%select(ends_with("StreakDates"))

names(PIT_records_dates)=str_remove(names(PIT_records_dates),"StreakDates")

dates_table<-as.data.frame(t(PIT_records_dates))%>%
  rownames_to_column("Accolade")%>%
  rename("Dates"="1")
```

### Interactive Accolades Kable

``` r
accolade_names<-c(accolades_table$Accolade)

first_nice_names<-toupper(gsub("([[:lower:]])([[:upper:]])", "\\1 \\2", accolade_names))

best_years<-full_join(accolades_table,seasons_table)%>%
  mutate("Accolade"=first_nice_names)%>%
  filter(Accolade != "FRANCHISE ID" & Accolade !="FRANCHISE NAME")
```

    ## Joining, by = "Accolade"

``` r
kable(best_years)
```

| Accolade             | Metric | Season                     |
| :------------------- | :----- | :------------------------- |
| FEWEST GOALS         | 182    | 1969-70 (76)               |
| FEWEST GOALS AGAINST | 188    | 1997-98 (82)               |
| FEWEST LOSSES        | 21     | 1992-93 (84), 2016-17 (82) |
| FEWEST POINTS        | 38     | 1983-84 (80)               |
| FEWEST TIES          | 4      | 1995-96 (82)               |
| FEWEST WINS          | 16     | 1983-84 (80)               |
| MOST GAME GOALS      | 12     | NA                         |
| MOST GOALS           | 367    | 1992-93 (84)               |
| MOST GOALS AGAINST   | 394    | 1982-83 (80)               |
| MOST LOSSES          | 58     | 1983-84 (80)               |
| MOST PENALTY MINUTES | 2674   | 1988-89 (80)               |
| MOST POINTS          | 119    | 1992-93 (84)               |
| MOST SHUTOUTS        | 10     | 2014-15 (82)               |
| MOST TIES            | 20     | 1970-71 (78)               |
| MOST WINS            | 56     | 1992-93 (84)               |

### Interactive Streak Kable

``` r
streak_names<-c(streak_table$Accolade)

second_nice_names<-toupper(gsub("([[:lower:]])([[:upper:]])", "\\1 \\2", streak_names))

best_streaks<-full_join(streak_table,dates_table)%>%
  mutate("Accolade"=second_nice_names)
```

    ## Joining, by = "Accolade"

``` r
kable(best_streaks)
```

| Accolade     | Streak | Dates                                                |
| :----------- | -----: | :--------------------------------------------------- |
| HOME LOSS    |     14 | Dec 31 2003 - Feb 22 2004                            |
| HOME POINT   |     20 | Nov 30 1974 - Feb 22 1975                            |
| HOME WIN     |     13 | Nov 15 2013 - Jan 15 2014                            |
| HOME WINLESS |     16 | Dec 31 2003 - Mar 04 2004                            |
| LOSS         |     13 | Jan 13 2004 - Feb 12 2004                            |
| POINT        |     18 | Mar 09 1993 - Apr 14 1993                            |
| ROAD LOSS    |     18 | Dec 23 1982 - Mar 04 1983                            |
| ROAD POINT   |      8 | Jan 13 2007 - Feb 16 2007, Mar 11 2016 - Apr 07 2016 |
| ROAD WIN     |      8 | Mar 11 2016 - Apr 07 2016                            |
| ROAD WINLESS |     18 | Oct 25 1970 - Jan 14 1971, Dec 23 1982 - Mar 04 1983 |
| WIN          |     17 | Mar 09 1993 - Apr 10 1993                            |
| WINLESS      |      6 | Feb 20 2020 - Feb 29 2020                            |

## Some Pittsburgh Goalie Stats

Below we present two code chunks. The first produces a statistical
summary of the “Mosts per Game” and “Mosts per Season” for Pittsburgh’s
goalies. The Second illustrates games played and game outcomes for
Pittsburgh’s goalies over the years.

### Goalie statistics summary

Distribution of most shutouts and most wins per seasons are skewed
heavily to the right.

``` r
PIT_goalie_table<-PIT_goalie_GET[[1]]

pit_goalie_game_season<-PIT_goalie_table%>%
  select(mostGoalsAgainstOneGame, mostSavesOneGame, mostShotsAgainstOneGame, 
         mostShutoutsOneSeason, mostWinsOneSeason)

goalie_tab_colnames<-c(colnames(pit_goalie_game_season))

goalie_nice_names<-toupper(gsub("([[:lower:]])([[:upper:]])", "\\1 \\2", goalie_tab_colnames))

colnames(pit_goalie_game_season)<-goalie_nice_names

pit_goalie_game_season%>%
  sapply(summary,digits=2)%>%
  kable()
```

|         | MOST GOALS AGAINST ONE GAME | MOST SAVES ONE GAME | MOST SHOTS AGAINST ONE GAME | MOST SHUTOUTS ONE SEASON | MOST WINS ONE SEASON |
| ------- | --------------------------: | ------------------: | --------------------------: | -----------------------: | -------------------: |
| Min.    |                         0.0 |                   7 |                           7 |                      0.0 |                    0 |
| 1st Qu. |                         6.0 |                  33 |                          38 |                      0.0 |                    2 |
| Median  |                         7.0 |                  42 |                          46 |                      1.0 |                   13 |
| Mean    |                         6.6 |                  39 |                          43 |                      1.8 |                   13 |
| 3rd Qu. |                         8.0 |                  48 |                          51 |                      3.0 |                   20 |
| Max.    |                        13.0 |                  57 |                          62 |                     10.0 |                   43 |

### Goalie game outcome diagram

Keith Fleury has logged more games at goalie than any other person in
history of the franchise and roughly 2/3rds of the games he played in
ended in a win.

``` r
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
```

    ## Warning: Removed 8 rows containing missing values (position_stack).

![](Project_1_Markdown_files/figure-gfm/goalie%20game%20outcome-1.png)<!-- -->

## Skater Records - which positions rack up the penalty minutes fastest?

Pittsburgh’s Defenders and Left Wingers rack up penalty minutes at the
fastest clip while right wingers accumulate them more slowly.

``` r
PIT_skater_table<-PIT_skater_GET[[1]]

pos_labels<-(c(C="Center",D ="Defense",L="Left Wing",R="Right Wing"))

PIT_skater_table%>%
  ggplot(aes(x=seasons, y=penaltyMinutes))+
  geom_point()+
  geom_smooth(method=lm,se=F)+
  ylab("Penalty Minutes")+
  facet_grid(~positionCode,labeller = labeller(positionCode=pos_labels))
```

![](Project_1_Markdown_files/figure-gfm/skater%20penalty%20minutes-1.png)<!-- -->
