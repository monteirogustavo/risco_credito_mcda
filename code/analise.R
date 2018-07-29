library(dplyr)
library(readr)
library(RMCriteria)
library(readxl)
library(tidyr)

balance_gdp <- read.csv("data/balance_gdp.csv", skip = 4)
debt <- read.csv("data/debt.csv", skip = 4)
exports <- read.csv("data/exports_growth.csv", skip = 4)
gdp <- read.csv("data/gdp.csv", skip = 4)
gross_savings <- read.csv("data/gross_savings.csv", skip = 4)

countries <- rbind(balance_gdp, debt, exports, gdp, gross_savings)
countries <- select(countries, Country.Name, Country.Code, Indicator.Name, Indicator.Code, X2016)

countries$Indicator.Code <- as.character(countries$Indicator.Code)
countries[countries$Indicator.Code == "BN.CAB.XOKA.GD.ZS", ]$Indicator.Code <- "balance"
countries[countries$Indicator.Code == "DT.TDS.DECT.CD", ]$Indicator.Code <- "debt"
countries[countries$Indicator.Code == "NE.EXP.GNFS.KD.ZG", ]$Indicator.Code <- "exports"
countries[countries$Indicator.Code == "NY.GDP.PCAP.CD", ]$Indicator.Code <- "gdp"
countries[countries$Indicator.Code == "NY.GDS.TOTL.ZS", ]$Indicator.Code <- "savings"

names(countries)[5] <- "Value"

countries_list <- c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Ecuador", "Paraguay", "Peru", "Uruguay")

countries <- filter(countries, Country.Name %in% countries_list)


balance_gdp <- select(balance_gdp, Country.Name, Country.Code, Indicator.Name, Indicator.Code, X2016)


balance_gdp %>%
    group_by(Country.Name) %>%
    summarize(na_number = mean(is.na(X2014))) %>% 
    filter(na_number == 0)

