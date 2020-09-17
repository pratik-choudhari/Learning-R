#Questions that we will answer in this analysis
#Q1. Which is the most played match type?
#Q2. If I kill more players, will I win?
#Q3. Are most players skilled or unskilled?
#Q4. How long does a player play the match for?
#Q5. If I play for longer, will I win?  
#Q6. If I drive more, will I kill more?

##installing necessary packages
#install.packages(c("ggplot2", "visdat","DataExplorer","dplyr","moments"))

##importing necessary libraries

library(ggplot2)
library(moments)

#set project file's source as working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

##loading the dataset (csv file)
##The dataset has data about restaurant's name, location,price, ratings
pubg<- read.csv("PUBG.csv")
pubg

## INTRODUCTION to our data

dim(pubg) #we see that our data has 29 columns

str(pubg)
  
#Checking head of loaded dataset in order to understand structure of data
head(pubg)

#Graphical representation of data
plot_intro(pubg)

#Column names
colnames(pubg) 

# we observe that the first 3 columns are 
#not of any analytical importance, they can be dropped.

##FEATURE ENGINEERING
pubg<-pubg[,-c(1,2,3,18)] # delete first 3 and 18th col
colnames(pubg) # the first 3 columnsare deleted.

#DATA CLEANING

#checking data types of each feature
plot_str(pubg)

#check for na values
colSums(is.na(pubg)) #display na value in each column ie no NA values in data.

summary(pubg) # we can see that there 992 NA values in each col.

##removing columns with na values
##replacing na values with median of the column
#ifelse(condition, true, false)

pubg$assists = ifelse(is.na(pubg$assists),median(pubg$assists,na.rm = TRUE),pubg$assists)
pubg$boosts = ifelse(is.na(pubg$boosts),median(pubg$boosts,na.rm = TRUE),pubg$boosts)
pubg$damageDealt = ifelse(is.na(pubg$damageDealt),median(pubg$damageDealt,na.rm = TRUE),pubg$damageDealt)
pubg$DBNOs = ifelse(is.na(pubg$DBNOs),median(pubg$DBNOs,na.rm = TRUE),pubg$DBNOs)
pubg$headshotKills = ifelse(is.na(pubg$headshotKills),median(pubg$headshotKills,na.rm = TRUE),pubg$headshotKills)
pubg$heals = ifelse(is.na(pubg$heals),median(pubg$heals,na.rm = TRUE),pubg$heals)
pubg$killPlace = ifelse(is.na(pubg$killPlace),median(pubg$killPlace,na.rm = TRUE),pubg$killPlace)
pubg$killStreaks = ifelse(is.na(pubg$killStreaks),median(pubg$killStreaks,na.rm = TRUE),pubg$killStreaks)
pubg$killPoints = ifelse(is.na(pubg$killPoints),median(pubg$killPoints,na.rm = TRUE),pubg$killPoints)
pubg$kills = ifelse(is.na(pubg$kills),median(pubg$kills,na.rm = TRUE),pubg$kills)
pubg$longestKill = ifelse(is.na(pubg$longestKill),median(pubg$longestKill,na.rm = TRUE),pubg$longestKill)
pubg$matchDuration = ifelse(is.na(pubg$matchDuration),median(pubg$matchDuration,na.rm = TRUE),pubg$matchDuration)
pubg$maxPlace = ifelse(is.na(pubg$maxPlace),median(pubg$maxPlace,na.rm = TRUE),pubg$maxPlace)
pubg$rankPoints = ifelse(is.na(pubg$rankPoints),median(pubg$rankPoints,na.rm = TRUE),pubg$rankPoints)
pubg$revives = ifelse(is.na(pubg$revives),median(pubg$revives,na.rm = TRUE),pubg$revives)
pubg$rideDistance = ifelse(is.na(pubg$rideDistance),median(pubg$rideDistance,na.rm = TRUE),pubg$rideDistance)
pubg$roadKills = ifelse(is.na(pubg$roadKills),median(pubg$roadKills,na.rm = TRUE),pubg$roadKills)
pubg$swimDistance = ifelse(is.na(pubg$swimDistance),median(pubg$swimDistance,na.rm = TRUE),pubg$swimDistance)
pubg$teamKills = ifelse(is.na(pubg$teamKills),median(pubg$teamKills,na.rm = TRUE),pubg$teamKills)
pubg$vehicleDestroys = ifelse(is.na(pubg$vehicleDestroys),median(pubg$vehicleDestroys,na.rm = TRUE),pubg$vehicleDestroys)
pubg$walkDistance = ifelse(is.na(pubg$walkDistance),median(pubg$walkDistance,na.rm = TRUE),pubg$walkDistance)
pubg$weaponsAcquired = ifelse(is.na(pubg$weaponsAcquired),median(pubg$weaponsAcquired,na.rm = TRUE),pubg$weaponsAcquired)
pubg$winPoints = ifelse(is.na(pubg$winPoints),median(pubg$winPoints,na.rm = TRUE),pubg$winPoints)
pubg$winPlacePerc = ifelse(is.na(pubg$winPlacePerc),median(pubg$winPlacePerc,na.rm = TRUE),pubg$winPlacePerc)

#confirming that there are no NA values
summary(pubg)

plot_intro(pubg)#we can conclude that there are no missing observations.

##positive kurtosis indicates a fat-tailed distribution
kurtosis(pubg$kills)#ideal value is 3. we observe 13. therefore it is Leptokurtic.

skewness(pubg$kills)# this feature should have been 0. ie it is positively skewed.

##DATA VISUALISATION
#Q1 Which is the most played match type?
#understanding our match Types
ggplot(pubg) + geom_bar(aes(x = matchType), fill='olivedrab1') + 
  labs(title = "Types of Match",
       y = 'Number of Matches', x='Type of Match')+
  theme(plot.title = element_text(hjust = 0.5, face="bold") )+
  theme(panel.background = element_rect(fill = "aliceblue"))
  
#Q2 If I kill more players, will I win?
#plotting a Faceted point graph of kills vs win-percentage
#alpha adds transperancy
#colouring based on match type.
ggplot(pubg, aes(x = kills, y = winPlacePerc*100, col = matchType)) +  
  geom_point(alpha = 1/2) +
  labs(title = "Winning chances based on number of kills",x='Numbers of Kills', y='Chances of Winning') +
  theme(plot.title = element_text(hjust = 0.5, face="bold") )+
  theme(panel.background = element_rect(fill = "aliceblue"))+
  facet_grid(~kills)

#Q3 Are most players skilled or unskilled?
#Q3a Kills
#plotting a bar graph of number of kills in a match
#we observe that maximum players have no kills.
ggplot(pubg,
       aes(x = kills, fill = cut(kills, 100))) + 
  geom_histogram(binwidth = 0.5, show.legend = FALSE) + 
  scale_fill_discrete(h = c(300, 100), c = 120, l = 70)+
  theme(axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black")) + 
  labs(title = "Number of Kills in a Match",
       y = 'Number of Players')+
  theme(plot.title = element_text(hjust = 0.5, face="bold") )+
  theme(panel.background = element_rect(fill = "ivory"))

#Q3b Headshots
#plotting a histogram of headshots in a match
#we observe that maximum players have no headshots. 
ggplot(pubg,
       aes(x = headshotKills, fill = cut(damageDealt, 100))) + 
  geom_histogram(binwidth = 0.5, show.legend = FALSE) + 
  scale_fill_discrete(h = c(220, 98), c = 150, l = 20)+
  theme(axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black")) + 
  labs(title = "Number of Headshots in a Match",
       y = 'Number of Players', x='Headshots')+
  theme(plot.title = element_text(hjust = 0.5, face="bold") )+
  theme(panel.background = element_rect(fill = "honeydew"))

#Q3c Damage
# plot of damage dealt in a match
ggplot(pubg,
       aes(x = damageDealt, fill = cut(damageDealt, 100))) + 
  geom_histogram(binwidth = 100, show.legend = FALSE) + 
  scale_fill_discrete(h = c(240, 10), c = 120, l = 70)+
  theme(axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black")) + 
  labs(title = "Damage Done in a Match",x='Damage Done(hp)',
       y = 'Number of Players')+
  theme(plot.title = element_text(hjust = 0.5, face="bold") )+
  theme(panel.background = element_rect(fill = "lavenderblush"))

#Q4 How long does a player play the match for?
#a plot of match duration
#most matches are played for 1300 seconds(21 minutes)
ggplot(pubg,
       aes(x = matchDuration, fill = cut(matchDuration, 100))) + 
  geom_histogram(binwidth = 50, show.legend = FALSE) + 
  scale_fill_discrete(h = c(300, 100), c = 120, l = 70)+
  theme(axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black")) + 
  labs(title = "Distribution of Match Duration",
       y = 'Number of Players', x= 'Time(sec)')+
  theme(plot.title = element_text(hjust = 0.5, face="bold") )+
  theme(panel.background = element_rect(fill = "azure2"))

#Q5 If I play for longer, will I win?  
#match duration vs winnning chances
#as we see there is no correlation between the two.
ggplot(pubg, aes(x = matchDuration, y = winPlacePerc*100, col = killPoints)) +  
  geom_point(alpha = 1/2) +
  labs(title = "Match Duration vs Winnning Chances", x='Match Duration', y="Chances of Winning")+
  theme(plot.title = element_text(hjust = 0.5, face="bold") )+
  theme(panel.background = element_rect(fill = "lemonchiffon2"))


table(pubg$roadKills)
#Q6 If I drive more, will I kill more?
#roadkill vs ride distance
# we see that maximum people ride their vehical without killing anybody.
ggplot(pubg, aes(x = rideDistance, y = roadKills, col = killPoints)) +  
  geom_point(alpha = 1/2) +
  labs(title = "Roadkill vs Ride Distance", x='Ride Distance', y='Road Kills')+
  theme(plot.title = element_text(hjust = 0.5, face="bold") )+
  theme(panel.background = element_rect(fill = "mintcream"))

