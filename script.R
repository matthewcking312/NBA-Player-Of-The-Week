library(readr)
library(dplyr)
library(ggplot2)
library(stringi)
library(reshape2)
library(viridis)
library(searcher)
library(lubridate)

# Reading in the NBA Player of The Week Data
nbaData = read_csv("~/Desktop/Projects/NBA-Player-Of-The-Week/Data/NBA_player_of_the_week.csv")

# Below we can see all of the unique teams that are listed without a conference.
# We will add in the conference manually
unique(nbaData[is.na(nbaData$Conference), ]$Team)

# We need to first handle the Conference Columns that have many NA values...
eastern = c("New Jersey Nets", "Boston Celtics", "Atlanta Hawks", "Washington Bullets", "Chicago Bulls", "Cleveland Cavaliers",
            "Detroit Pistons", "Milwaukee Bucks", "New York Knicks", "Philadelphia Sixers",
            "Indiana Pacers", "Miami Heat", "Orlando Magic", "Charlotte Hornets", "Toronto Raptors")
western = c("Los Angeles Clippers", "Denver Nuggets", "Los Angeles Lakers", "Utah Jazz", "Golden State Warriors",
            "Dallas Mavericks", "Phoenix Suns", "Portland Trail Blazers", "San Antonio Spurs", "Sacramento Kings",
            "Seattle SuperSonics", "Houston Rockets", "Minnesota Timberwolves")
# If the teams are in the eastern or western objects, accordingly
# Eastern
nbaData[is.na(nbaData$Conference) & (nbaData$Team %in% eastern), ]$Conference = "East"
# Western
nbaData[is.na(nbaData$Conference) & (nbaData$Team %in% western), ]$Conference = "West"



# Calculate the BMI of all players

# How many NAs exist in the weight column?
sum(is.na(nbaData$Weight))

# Need to clean up the Players who have missing weights.
# Some of the players that are missing might have previously showed up in the list and won't have missing weights.
# Let's see if we can fill in some of those missing weights before incorporating any outside data.

for(i in 1:dim(nbaData)[1]) {
  if(is.na(nbaData$Weight[i])) {
    if(nbaData$Player[i] %in% nbaData[!(is.na(nbaData$Weight)), ]$Player) {
      nbaData$Weight[i] = nbaData[!(is.na(nbaData$Weight)) & nbaData$Player == nbaData$Player[i], ]$Weight[1]
    }
  } 
}
# How many NAs still exist?
sum(is.na(nbaData$Weight))
# We have seen a drastic improvement, and now there are only 17 NAs.
# For the players that are remaining, we will perform manual entry for their weights since it is a small number.
missing_weights = c(265, 216, 249, 209, 181, 238, 249, 214, 249, 234, 249, 207, 214, 227, 249, 240, 214)
nbaData[is.na(nbaData$Weight), ]$Weight = missing_weights

# After manual input, how many are missing still? 
sum(is.na(nbaData$Weight))
# 0 are missing. Now we can continue with calculating BMIs


# First we will want to convert Height to inches, and change any heights that re in feet-inches
convertHeight = function(height) {
  if(grepl('cm', height)) {
    return(as.numeric(gsub('cm', '', height)) * 0.394)
  } else {
    feet = as.numeric(strsplit(height, "-")[[1]][1])
    inches = as.numeric(strsplit(height, "-")[[1]][2])
    return((feet * 12 + inches))
   }
}

calculateBMI = function(height, weigth) {
  return(weight / ((height)^2 * 703))
}

nbaData$HeightIN = sapply(nbaData$Height, convertHeight)
nbaData$BMI = (nbaData$Weight / ((nbaData$HeightIN)^2)) * 703


# Rename the positions for those who may not know the specific acronyms of positions. 
nbaData[nbaData$Position == "PG", ]$Position = "Point Guard"
nbaData[nbaData$Position == "SG", ]$Position = "Shooting Guard"
nbaData[nbaData$Position == "G", ]$Position = "Guard"
nbaData[nbaData$Position == "G-F" | nbaData$Position == "GF", ]$Position = "Guard-Forward"
nbaData[nbaData$Position == "SF", ]$Position = "Small Forward"
nbaData[nbaData$Position == "F", ]$Position = "Forward"
nbaData[nbaData$Position == "FC" | nbaData$Position == "F-C", ]$Position = "Forward-Center"
nbaData[nbaData$Position == "PF", ]$Position = "Power Forward"
nbaData[nbaData$Position == "C", ]$Position = "Center"


# Colors for different positions in graph
colors = viridis_pal(option = "D")(length(unique(nbaData$Position)))
cols_df = data_frame(colors, unique(nbaData$Position))
names(cols_df) = c("Colors", "Position")


# What physical attributes correspond to players who tend to get elected to the player of the week?
left_join(nbaData %>% 
            select(Position, HeightIN, Weight, BMI) %>%
            group_by(Position) %>%
            melt(id = 'Position'),
          cols_df, by = 'Position')

# Let's adjust the dates into specific columns. 
dateFun = function(x) {
  oldDate = strsplit(x, ',')[[1]]
  year = trimws(oldDate[2])
  month = match(substr(oldDate[1], 0, 3), month.abb)
  day = strsplit(oldDate[1], ' ')[[1]][2]
  return(paste0(day))
}




qplot(nbaData$Weight,
      geom="histogram",
      binwidth = 5,  
      main = "Histogram for Weight", 
      xlab = "Weight",  
      fill=I("blue"), 
      col=I("red"), 
      alpha=I(.2))

qplot(nbaData$Age,
      geom = "histogram",
      binwidth = 1,
      main = "Histogram for Height",
      xlab = "Height",
      fill = I("blue"),
      col = I("red"),
      alpha = I(.2))



library(ballr)
players <- NBAPerGameAdvStatistics(season = 2018)
players
