rm(list = ls())
setwd("~/Desktop/LMU/Prof. Marin/Project/WIOD/SEA")

library("readxl");
library("data.table");
library("dplyr");
library("stringr");

##--------------------------------------------------------------------##

wiod_sea_temp <- read_excel("WIOD_SEA_Nov16.xlsx", sheet = "Data") ## I import the excel file.
setDT(wiod_sea_temp)
wiod_sea_temp$description <- NULL
wiod_sea_use <- melt(wiod_sea_temp, ##
  id = c("country", "variable", "code"), ##
  variable.name = "year", value.name = "value") ## I create the year variable by melting
wiod_sea_use <- dcast(wiod_sea_use, country + code + year ~ variable, value.var = "value")

##--------------------------------------------------------------------##

wiod_sea <- select(wiod_sea_use, country:year, "COMP") ## I only select COMP variable along with country, year and industry variables to initialize a dataframe to be merged.


wiod_sea <- dcast(wiod_sea, country + year ~ code, value.var = "COMP") ## To begin the industry reclassification, I cast the dataframe to have columns with industry codes.
wiod_sea[, ':='(`10to12` = `C10-C12`, `13to15` = `C13-C15`, `16to18` = C16 + C17 + C18, ##
             `19to21` = C19 + C20 + C21, `22to23` = C22 + C23, `24to25` = C24 + C25, ##
             `26to27` = C26 + C27, `28` = C28, `29to30` = C29 + C30, `91` = `C31_C32` + C33)] ## I create submanufacturing sectors.
wiod_sea <- select(wiod_sea, -starts_with("C"), "country") ## I drop old submanufacturing columns.

wiod_sea$D = rowSums(select(wiod_sea, contains("to"), `28`, `91`)) ## I create manufacturing sector by summing up all submanufacturing sectors.
wiod_sea$`90` <- rowSums(select(wiod_sea, G45:O84, Q:U)) ## I create all other nonmanufacturing sector.
wiod_sea$`P` <- wiod_sea$`P85` ## Research and education sector
wiod_sea$AtoB <- rowSums(select(wiod_sea, A01:A03)) ## Agriulture sector
wiod_sea$C <- wiod_sea$B ## Mining sector
wiod_sea$E <- rowSums(select(wiod_sea, D35:`E37-E39`)) ## Water and gas supply sector
wiod_sea <- select(wiod_sea, year, `F`, `10to12`:`E`) ## I drop old industry classes by selecting only the new ones plus year and country variables.
wiod_sea[, ':='(`0` = `AtoB` + `C` + `D` + `E` + `F` + `P` + `90`)] ## I create "total" industry class.
wiod_sea <- wiod_sea[, c("year", "country", "AtoB", "C", "D", "E", "F", "P", "90", ##
    "10to12", "13to15", "16to18", "19to21", "22to23", ##
    "24to25", "26to27", "28", "29to30", "91", "0")] ## I reorder the columns.

wiod_sea <- melt(wiod_sea, id = c("year", "country"), variable.name = "cons_IFRcode", ##
  value.name = "COMP") ## I finalize the construction of the new dataframe by melting.

for (i in c("EMP", "EMPE", "H_EMPE", "LAB", "VA")) {
  
  wiod_sea1 <- select(wiod_sea_use, country:year, i)
  wiod_sea1 <- dcast(wiod_sea1, country + year ~ code, value.var = i)
  wiod_sea1[, ':='(`10to12` = `C10-C12`, `13to15` = `C13-C15`, `16to18` = C16 + C17 + C18, ##
    `19to21` = C19 + C20 + C21, `22to23` = C22 + C23, `24to25` = C24 + C25, ##
    `26to27` = C26 + C27, `28` = C28, `29to30` = C29 + C30, `91` = `C31_C32` + C33)]
  wiod_sea1 <- select(wiod_sea1, -starts_with("C"), "country")
  wiod_sea1$D = rowSums(select(wiod_sea1, contains("to"), `28`, `91`))
  wiod_sea1$`90` <- rowSums(select(wiod_sea1, G45:O84, Q:U))
  wiod_sea1$`P` <- wiod_sea1$`P85`
  wiod_sea1$AtoB <- rowSums(select(wiod_sea1, A01:A03))
  wiod_sea1$C <- wiod_sea1$B
  wiod_sea1$E <- rowSums(select(wiod_sea1, D35:`E37-E39`))
  wiod_sea1 <- select(wiod_sea1, year, `F`, `10to12`:`E`)
  wiod_sea1[, ':='(`0` = AtoB + C + D + E + F + P + `90`)]
  wiod_sea1 <- wiod_sea1[, c("year", "country", "AtoB", "C", "D", "E", "F", "P", "90", ##
                       "10to12", "13to15", "16to18", "19to21", "22to23", ##
                       "24to25", "26to27", "28", "29to30", "91", "0")]
  wiod_sea1 <- melt(wiod_sea1, id = c("year", "country"), variable.name = "cons_IFRcode", ##
    value.name = i)

  wiod_sea <- merge(wiod_sea, wiod_sea1, by = c("year", "country", "cons_IFRcode"))  
  
}

rm(list = setdiff(ls(), c("wiod_sea", "wiod_sea_temp")))

save(wiod_sea, file = "wiod_sea.RData")


