library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(openair)
library(summarytools)
library(shinythemes)
library(DT)
library(shinyWidgets)
library(dygraphs)
library(xts)

#load the data file
boston_weather <- read.csv('boston_weather.csv')

#rename the columns by replacing the dot with whitespace
names(boston_weather) <- gsub('\\.', ' ', names(boston_weather))

#Parse Date column using dmy() assuming day-month-year format
boston_weather$Date <- dmy(boston_weather$Date)

#Extract Year, Month, and Day components
boston_weather$Year <- year(boston_weather$Date)
boston_weather$Month <- month(boston_weather$Date, label = TRUE)
boston_weather$Day <- day(boston_weather$Date)

#Convert Year, Month, and Day to factors with specified levels
boston_weather$Year <- factor(boston_weather$Year, levels = 2013:2023, ordered = TRUE)
boston_weather$Month <- factor(month.name[boston_weather$Month], levels = month.name, ordered = TRUE)
boston_weather$Day <- factor(boston_weather$Day, levels = as.character(1:31), ordered = TRUE)

#define a function to categorise the months of the year into seasons
get_season <- function(date) {
  month <- month(date)
  if (month %in% c(3, 4, 5)) {
    return("Spring")
  } else if (month %in% c(6, 7, 8)) {
    return("Summer")
  } else if (month %in% c(9, 10, 11)) {
    return("Autumn")
  } else {
    return("Winter")
  }
}


#Add a season column to the data
boston_weather$Season <- factor(sapply(boston_weather$Date, get_season))