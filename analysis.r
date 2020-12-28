# load packages

library(tidyverse)
library(maps)
library(mapproj)
library(patchwork)
library(ggplot2)
library(plotly)
library(reshape2)

# load national data
national <- read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us.csv")
# configure for date
national$configured_date <- as.Date(national$date)


# load state data
#states <- read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")

# load county data 
counties <- read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")

########## Graph 1; number of new and average cases, number of new and average deaths (toggle widget) ##########

# create seven day index for death and cases and take the median
national$seven_day_index <- c(0, rep(1:(nrow(national)-1)%/%7))

# create column for new cases
national$new_cases = national$cases - lag(national$cases)

# create column for new deaths
national$new_deaths = national$deaths - lag(national$deaths)

# find 7 day averages, create table with summary of data
national_with_average_and_new <- group_by(national, seven_day_index) %>% summarize(
	"Date" = configured_date,
	# new cases
	"Cases (new)" = new_cases,
	# median cases
	"Cases (7-day average)" = median(new_cases),
	# new deaths
	"Deaths (new)" = new_deaths,
	# median deaths
	"Deaths (7-day average)" = median(new_deaths)
)

##### Cases Graph #####

# graph1cases
graph1cases <- ggplot(data = national_with_average_and_new) +
	geom_col(mapping = aes(x = `Date`, y = `Cases (new)`), color = "lightpink1") +
	geom_smooth(mapping = aes(x = `Date`, y = `Cases (7-day average)`), span = 0.1) +
	labs(title = "Trends in Coronavirus Cases and Deaths")

# add date as x-axis
graph1cases <- graph1cases + scale_x_date(date_labels = "%Y %b %d")

# make y-axis readable
require(scales)
graph1cases <- graph1cases + scale_y_continuous(labels = comma)

# make it interactive
graph1cases <- ggplotly(graph1cases)
graph1cases

##### Deaths Graph #####

# graph1deaths
graph1deaths <- ggplot(data = national_with_average_and_new) +
	geom_col(mapping = aes(x = `Date`, y = `Deaths (new)`), color = "indianred2") +
	geom_smooth(mapping = aes(x = `Date`, y = `Deaths (7-day average)`), span = 0.1) +
	labs(title = "Trends in Coronavirus Cases and Deaths")

# add date as x-axis
graph1deaths <- graph1deaths + scale_x_date(date_labels = "%Y %b %d")

# make y-axis readable
require(scales)
graph1deaths <- graph1deaths + scale_y_continuous(labels = comma)

# make it interactive
graph1deaths <- ggplotly(graph1deaths)
graph1deaths

########## Table 1; Summary Statistics on Cases and Deaths ##########

totalcases <- filter(national, date == max(date))$cases

totaldeaths <- filter(national, date == max(date))$deaths

mostrecentcases <- filter(national, date == max(date))$new_cases

mostrecentdeaths <- filter(national, date == max(date))$new_deaths

table1_cases_and_deaths <- matrix(c(totalcases, mostrecentcases, totaldeaths, mostrecentdeaths), ncol = 2, byrow = TRUE)

rownames(table1_cases_and_deaths) <- c("Cases", "Deaths")
colnames(table1_cases_and_deaths) <- c("Total Reported", paste("On ", toString(max(national$date))))

########## Map 1; Covid data by state and county ##########

sumCountyData <- group_by(counties, state) %>% group_by(county)

##### "hot spots" -- cases per capita in the past 7 days (default)
cases_per_capita_average <- sumCountyData %>% summarize(

) 

## ATTENTION SAM, WE NEED TO FIND POPULATION DATA FIRST!

##### total cases by county

##### total deaths by county 

##### deaths per capita