library(tidyverse)
library(ggplot2) 
library(dplyr)
library(rvest)
library(caret)
library(dbplyr)
library(corrplot)


### Training Data ###
RegSeasonData <- read_csv("RegSeasonData.csv")
TeamData <- read_csv("MNCAATourneyTeams.csv")
OffenseDefense <- read_csv("OffenseDefense.csv")

GroupedData <- RegSeasonData %>% 
  filter(season < 2020) %>% 
  group_by(WTeamID, season) %>% 
  summarize(total3made = sum(WFGM3), total3attempt = sum(WFGA3), totalftmade = sum(WFTM), totalftattempt = sum(WFTA), defensiveRebounds = sum(WDR), blocks = sum(WBlk), steals = sum(WStl), totalAssists = sum(WAst)) %>% 
  mutate(threePointAccuracy = total3made/total3attempt) %>% 
  mutate(freeThrowAccuracy = totalftmade/totalftattempt) %>% 
  subset(select = -c(total3made,total3attempt,totalftmade,totalftattempt))%>% 
  rename(teamID = WTeamID)

Joined <- right_join(GroupedData, TeamData, by = c("season","teamID")) %>% 
  arrange(season, tournamentWins)

Joined <- left_join(Joined, OffenseDefense, by = c("season", "teamID")) %>% 
  subset(select = -c(TEAM)) %>% 
  mutate(winPercentage = gamesWon/gamesPlayed) %>% 
  select(season, teamID, teamName, conference, seed,gamesPlayed, gamesWon, winPercentage, tempo,defensiveRebounds, blocks, defensiveEfficiency, steals, totalAssists, threePointAccuracy, freeThrowAccuracy, fieldGoalPercentShot, turnoverPercentAllowed, offensiveEfficiency, tournamentWins) 

write_csv(Joined, "TrainingData.csv")


### Testing Data ###
RegSeason21 <- read_csv("21RegSeasonData.csv")
TeamData21 <- read_csv("21Teams.csv")
OffenseDefense21 <- read_csv("OffenseDefense21.csv")

GroupedData21 <- RegSeason21 %>% 
  group_by(WTeamID, season) %>% summarize(total3made = sum(WFGM3), total3attempt = sum(WFGA3), totalftmade = sum(WFTM), totalftattempt = sum(WFTA), defensiveRebounds = sum(WDR), blocks = sum(WBlk), steals = sum(WStl), totalAssists = sum(WAst)) %>% 
  mutate(threePointAccuracy = total3made/total3attempt) %>% 
  mutate(freeThrowAccuracy = totalftmade/totalftattempt) %>%
  subset(select = -c(total3made,total3attempt,totalftmade,totalftattempt)) %>% 
  rename(teamID = WTeamID)

Joined21 <- right_join(GroupedData21, TeamData21, by = c("season","teamID")) %>% 
  arrange(season)

Joined21 <- left_join(Joined21, OffenseDefense21, by = c("season","teamID")) %>% 
  subset(select = -c(TEAM)) %>% 
  mutate(winPercentage = gamesWon/gamesPlayed) %>% 
  select(season, teamID, teamName, conference, seed,gamesPlayed, gamesWon, winPercentage, tempo,defensiveRebounds, blocks, defensiveEfficiency, steals, totalAssists, threePointAccuracy, freeThrowAccuracy, fieldGoalPercentShot, turnoverPercentAllowed, offensiveEfficiency, tournamentWins) 

write_csv(Joined21, "TestingData.csv")


### Visualizations ###
# Figure 4
options(scipen=999) 
Joined <- read.csv("TrainingData.csv") 
Joined$season <- as.numeric(Joined$season) 
ProjectA <- Joined %>% 
  mutate(count = 1) %>% 
  filter(tournamentWins >1)

ProjectB <- ProjectA %>% 
  group_by(conference, season) %>% 
  summarise(participatingTeams = sum(count)) %>% 
  arrange(season, desc(participatingTeams))

ProjectC <- ProjectB %>% 
  mutate(conAb = ifelse(conference == "American Athletic Conference", "ACC", ifelse(conference == "Big East Conference", "Big East", ifelse(conference == "Big Ten Conference", "Big 10", ifelse(conference == "Pacific-10 Conference", "Pac-12", ifelse(conference == "Atlantic 10 Conference", "A10", ifelse(conference == "Big 12 Conference", "Big 12", ifelse(conference == "Southeastern Conference", "SEC", conference))))))))

ProjectD <- ProjectC %>% 
  mutate(other = ifelse(((season == 2015) & (conAb != "ACC") & (conAb != "Big East") & (conAb != "Big 10")), "Other", ifelse(((season == 2016) & (conAb != "ACC") & (conAb != "Big East") & (conAb != "Big 12")), "Other", ifelse(((season == 2017) & (conAb != "SEC") & (conAb != "Big 10") & (conAb != "Pac-12")), "Other", ifelse(((season == 2018) & (conAb != "ACC") & (conAb != "Big East") & (conAb != "Big 12")), "Other", ifelse(((season == 2019) & (conAb != "ACC") & (conAb != "SEC") & (conAb != "Big 10")), "Other", ifelse(((season ==2021) & (conAb != "ACC") & (conAb != "SEC") & (conAb != "Big 10")), "Other", conAb)))))))

ProjectE <- ProjectD %>% 
  group_by(other, season) %>% 
  summarise(participatingTeams = sum(participatingTeams)) %>% 
  arrange(season, participatingTeams)

ggplot() + geom_bar(aes(y = participatingTeams, x = season, fill = factor(other), label = other), data = ProjectE, stat="identity") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.position = "none", axis.title.y=element_blank(), axis.title.x=element_blank()) + 
  geom_text(aes(x = season, y = participatingTeams, label = other, group = other), data=ProjectE,position = position_stack(vjust = .5)) + 
  coord_flip() + 
  scale_y_continuous(labels = function(participatingTeams) paste0(participatingTeams)) + 
  scale_fill_manual(values = c("Other" = "#ababab", "ACC" = "#006ba4", "SEC" = "#5f9ed1", "Big 10" = "#c85300", "Big 12" = "#a2c8ec", "Big East" = "#ffb60e", "Conference USA" = "#006ba4", "A10" = "#ffb60e", "Pac-12" = "#ff7e0e"))

# Figure 5
allmmData <- read_csv("cbb.csv")

mmData <- allmmData %>% 
  na.omit(mmData) %>% 
  filter(YEAR > 2014) %>% 
  group_by(YEAR) 

highlight_df <- mmData %>% 
  filter(POSTSEASON == "Champions") 

ggplot(mmData, mapping = aes(x = SEED, y = POSTSEASON)) + 
  geom_point(color = '#56B4E9') + geom_point(data = highlight_df, aes(x = SEED, y = POSTSEASON), color = '#E69F00', size = 2) + 
  facet_wrap(~ YEAR) + 
  theme_classic() + 
  scale_y_discrete(limits = c("R68","R64","R32","S16","E8","F4","2ND","Champions")) + 
  labs(x = "Tournament Seeding", y = "Tournament Placing", title = "Tournament Outcome by Seed (2015-2019)")


### Modeling ###
TrainingData <- read.csv("TrainingData.csv") 

model <- lm(tournamentWins ~ seed+ winPercentage+tempo+defensiveRebounds+blocks+defensiveEfficiency+steals+totalAssists+threePointAccuracy+freeThrowAccuracy+fieldGoalPercentage+turnoverPercentage+offensiveEfficiency, data=TrainingData) 

summary(model)

# Machine Learning Approach
TrainingData <- read.csv("TrainingData.csv") 
TestingData <- read.csv("TestingData.csv")

lmmodel <- train(tournamentWins ~seed+ winPercentage+tempo+defensiveRebounds+blocks+defensiveEfficiency+steals+totalAssists+threePointAccuracy+freeThrowAccuracy+fieldGoalPercentage+turnoverPercentage+offensiveEfficiency, data=TrainingData, method= "lm", na.action = na.exclude) 
predictedlm <- predict(lmmodel, TestingData) 
postResample(pred =predictedlm, obs = TestingData$tournamentWins)

correlation <- cor(TrainingData[,8:20],use = "complete.obs") 
findCorrelation(correlation, cutoff = .7, names = TRUE)

reducedmodel <- train(tournamentWins ~seed+ winPercentage+tempo+blocks+defensiveEfficiency+steals+threePointAccuracy+freeThrowAccuracy+fieldGoalPercentage+turnoverPercentage+offensiveEfficiency, data=TrainingData, method= "lm", na.action = na.exclude) 
predictedreduced <- predict(reducedmodel, TestingData) 
postResample(pred =predictedreduced, obs = TestingData$tournamentWins)

rangermodel <- train(tournamentWins ~seed+ winPercentage+tempo+defensiveRebounds+blocks+defensiveEfficiency+steals+totalAssists+threePointAccuracy+freeThrowAccuracy+fieldGoalPercentage+turnoverPercentage+offensiveEfficiency, data=TrainingData, method= "ranger", na.action = na.exclude) 
predictedranger <- predict(rangermodel, TestingData) 
postResample(pred =predictedranger, obs = TestingData$tournamentWins)

modelPredictions <- cbind(TestingData, lmModel1 = predictedlm, lmModel2 = predictedreduced, rangerModel= predictedranger) %>% 
  arrange(predictedranger) 

write_csv(modelPredictions, "modelPredictions.csv")












