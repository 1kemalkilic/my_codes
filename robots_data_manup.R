rm(list = ls())
setwd("~/Desktop/LMU/Prof. Marin/Project/ifr")

library("readxl");
library("data.table");
library("dplyr");
library("stringr");

continents <- c("Africa", "America", "AustralAsia", "CEEurope", "WesternEurope");
my_list <- list()

for (i in continents) {
  page <- paste("OperationalStock_", i, sep = "")
  raw_data <- read_excel("robotics_data.xlsx", sheet = page)
  raw_data$`industry description` <- NULL
  setDT(raw_data)
  data_name <- melt(raw_data, id = c("industry code", "year"), ##
    variable.name = "country", value.name = "robotstock")
  my_list[[i]] <- data_name
}

robotics <- rbindlist(my_list)

setnames(robotics, "industry code", "cons_IFRcode")
rm(i, continents, page, data_name, my_list, raw_data)

robotics$cons_IFRcode <- gsub("-", "to", robotics$cons_IFRcode)
robotics$a <- nchar(robotics$cons_IFRcode)
robotics$b <- str_detect(robotics$cons_IFRcode, "to")
robotics$country <- as.character(robotics$country)
robotics$c <- nchar(robotics$country) 
robotics$d <- ifelse(robotics$a < 3 | robotics$b == TRUE, 1, 0)
robotics$e <- ifelse(robotics$c < 4 & robotics$d == 1, 1, 0)

robotics <- robotics[robotics$e == 1]

robotics <- select(robotics, cons_IFRcode:robotstock)
save(robotics, file = "robotics.RData")