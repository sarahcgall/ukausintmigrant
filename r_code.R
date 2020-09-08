#=========================LIBRARY============================#
library(tidyverse)
library(rvest)
library(readxl)
library(lubridate)
library(janitor)
library(RColorBrewer)
library(extrafont)

library(ggtext)
library(ggpmisc)

#============================================================#


#============================================================#
#                DOWNLOAD & CLEAN DATA
#============================================================#
#Download:
download.file(url = "https://www.un.org/en/development/desa/population/migration/data/estimates2/data/UN_MigrantStockByOriginAndDestination_2019.xlsx", 
              destfile = "C:/Users/Sarah/RProjects/Personal Projects/ukausmigrantstock/Data/origindestination.xlsx", mode = "wb")

rawdata <- read_xlsx("C:/Users/Sarah/RProjects/Personal Projects/ukausmigrantstock/Data/origindestination.xlsx", sheet = 2)


#Clean:
#1) Remove additional space at top of sheet and make row 11 column names
data <- as.data.frame(rawdata) %>% row_to_names(row_number = 11) %>% clean_names()

#2) Manually adjust names, amend class, and filter columns
data <- data %>%
  mutate(Year = as.numeric(na), `Sort Order` = na_2, `Major area, region, country or area of destination` = na_3, `Region Type` = na_4, Code = na_5, `Type of data(a)` = na_6, Total = as.numeric(total), Australia = as.numeric(australia), `United Kingdom` = as.numeric(united_kingdom)) %>%
  select(Year, `Sort Order`, `Major area, region, country or area of destination`, `Region Type`, Code, `Type of data(a)`, Total, Australia, `United Kingdom`)

#3) Filter out unnecessary rows
data <- data %>%
  filter(!`Major area, region, country or area of destination` %in% c("UN development groups","More developed regions","Less developed regions","Least developed countries","Less developed regions, excluding least developed countries","	World Bank income groups","High-income countries","Middle-income countries","Upper-middle-income countries","Lower-middle-income countries","Low-income countries","No income group available","Geographic regions"))

#4) Amend `Region Type`
data <- data %>%
  mutate(`Major area, region, country or area of destination` = ifelse(`Major area, region, country or area of destination` == "EUROPE AND NORTHERN AMERICA", "Northern America", `Major area, region, country or area of destination`),
         Total = ifelse(`Major area, region, country or area of destination` == "EUROPE AND NORTHERN AMERICA", Total[`Major area, region, country or area of destination` == "EUROPE AND NORTHERN AMERICA"]-Total[`Major area, region, country or area of destination` == "EUROPE"], Total),
         Australia = ifelse(`Major area, region, country or area of destination` == "EUROPE AND NORTHERN AMERICA", Australia[`Major area, region, country or area of destination` == "EUROPE AND NORTHERN AMERICA"]-Australia[`Major area, region, country or area of destination` == "EUROPE"], Australia),
         `United Kingdom` = ifelse(`Major area, region, country or area of destination` == "EUROPE AND NORTHERN AMERICA", `United Kingdom`[`Major area, region, country or area of destination` == "EUROPE AND NORTHERN AMERICA"]-`United Kingdom`[`Major area, region, country or area of destination` == "EUROPE"], `United Kingdom`),
         `Region Type` = ifelse(`Major area, region, country or area of destination` == "WORLD", "Global",
                                ifelse(`Major area, region, country or area of destination` %in% c("Africa","Asia","Europe","Latin America and the Caribbean","Northern America","Oceania"), "Area",
                                       ifelse(`Major area, region, country or area of destination` %in% c("SUB-SAHARAN AFRICA","NORTHERN AFRICA AND WESTERN ASIA","CENTRAL AND SOUTHERN ASIA","EASTERN AND SOUTH-EASTERN ASIA","LATIN AMERICA AND THE CARIBBEAN","OCEANIA", "EUROPE", "NORTHERN AMERICA"), "Major Region",
                                              ifelse(`Major area, region, country or area of destination` %in% c("Eastern Africa","Middle Africa","Southern Africa","Western Africa","Northern Africa","Western Asia", "Central Asia", "Southern Asia", "Eastern Asia", "South-Eastern Asia", "Caribbean", "Central America", "South America", "Australia / New Zealand", "Melanesia", "Micronesia", "Polynesia", "Eastern Europe", "Northern Europe", "Southern Europe", "Western Europe", "Northern America"), "Region", "Country"
                                                                                 )))))

#5) Filter out unnecessary rows
data <- data %>%
  filter(!`Major area, region, country or area of destination` %in% c("World Bank income groups","Sustainable Development Goal (SDG) regions"))

#6) Convert uppercase to lowercase and capitalise first letter
cap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2), sep="", collapse=" ")
}
data <- data %>%
  mutate(`Major area, region, country or area of destination` = ifelse(`Region Type` %in% c("Global", "Major Region"), tolower(`Major area, region, country or area of destination`), `Major area, region, country or area of destination`),
         `Major area, region, country or area of destination` = ifelse(`Region Type` %in% c("Global", "Major Region"), sapply(`Major area, region, country or area of destination`, cap), `Major area, region, country or area of destination`))


#============================================================#
#                  VISUALISE THE DATA
#============================================================#


      