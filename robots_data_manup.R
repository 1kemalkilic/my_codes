rm(list = ls())
setwd("~/Desktop/LMU/Prof. Marin/Project/ifr")

library("readxl");
library("data.table");
library("dplyr");
library("stringr");

##--------------------------------------------------------------------##

## First, I create a list to save different excel sheets.

continents <- c("Africa", "America", "AustralAsia", "CEEurope", "WesternEurope");
my_list <- list() 

## This is a for loop to extract all the excel sheets.

for (i in continents) {
  page <- paste("OperationalStock_", i, sep = "")
  raw_data <- read_excel("robotics_data.xlsx", sheet = page)
  data_name <- raw_data
  data_name$`industry description` <- NULL
  setDT(data_name)
  data_name <- melt(data_name, id = c("industry code", "year"), ##
    variable.name = "country", value.name = "robotstock")
  my_list[[i]] <- data_name
}

## Then, I bind all the elements in my list.

robotics_temp <- rbindlist(my_list)

##-----------Industry classification adjustements.--------------##

setnames(robotics_temp, "industry code", "cons_IFRcode")
robotics_temp$cons_IFRcode <- gsub("-", "to", robotics_temp$cons_IFRcode)

## I delete unrelated industries, i.e. three or more digit levels, and unrelated countries, i.e. Africa, Europe etc.

robotics_temp$a <- nchar(robotics_temp$cons_IFRcode)
robotics_temp$b <- str_detect(robotics_temp$cons_IFRcode, "to")
robotics_temp$country <- as.character(robotics_temp$country)
robotics_temp$c <- nchar(robotics_temp$country) 
robotics_temp$d <- ifelse(robotics_temp$a < 3 | robotics_temp$b == TRUE, 1, 0)
robotics_temp$e <- ifelse(robotics_temp$c < 4 & robotics_temp$d == 1, 1, 0)
robotics_temp <- robotics_temp[robotics_temp$e == 1]

## Keep only related variables.

robotics_temp <- select(robotics_temp, cons_IFRcode:robotstock)

## Industry classification manipulation.

robotics_temp <- dcast(robotics_temp, year + country ~ cons_IFRcode, value.var = "robotstock")

robotics_temp$'16to18' <- robotics_temp$'16' + robotics_temp$'17to18'
robotics_temp$'19to21' <- robotics_temp$'19to22' - robotics_temp$'22'
robotics_temp$'22to23' <- robotics_temp$'22' + robotics_temp$'23'
robotics_temp$'24to25' <- robotics_temp$'24' + robotics_temp$'25'
robotics_temp$'29to30' <- robotics_temp$'29' + robotics_temp$'30'

for (j in c("16", "17to18", "20to21", "19to22", "19", "22", "23", ## 
  "24", "24to28", "25", "26to27", "29", "30")) {
  robotics_temp <- select(robotics_temp, -all_of(j))
  }

robotics_temp <- melt(robotics_temp, id = c("year", "country"), ##
  variable.name = "cons_IFRcode", value.name = "robotstock")

##--------------------------------------------------------------------##

## I define rich and developing countries.

rich_countries <- c("USA", "CAN", "CHE", "DEU", "GBR", "AUT", "BEL", "DNK", ##
  "ESP", "FRA", "FIN", "GRC", "IRL", "ITA", "JPN", "KOR", "NLD", ##
  "NOR", "PRT", "SWE", "AUS")
dev_countries <- c("BGR", "BRA", "CHN", "CZE", "HRV", "HUN", ##
  "IDN", "IND", "LTU", "LVA", "MEX", "POL", "ROU", "RUS", "SVK", "SVN", "TUR")

setnames(robotics_temp, "country", "cons_country")

##--------------------------------------------------------------------##

## I create a new dataset that only includes rich and developing countries. ##

robotics_use <- filter(robotics_temp, robotics_temp$cons_country %in% c(rich_countries, dev_countries))
robotics_use$prod_country <- robotics_use$cons_country

##--------------------------------------------------------------------##

rm(list=setdiff(ls(), c("robotics_use", "robotics_temp")))

save(robotics_use, file = "robotics_use.RData")

