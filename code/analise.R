library(dplyr)
library(readr)
library(RMCriteria)
library(readxl)
library(tidyr)

# Import datasets
balance_gdp <- read.csv("data/balance_gdp.csv", skip = 4)
debt <- read.csv("data/debt.csv", skip = 4)
exports <- read.csv("data/exports_growth.csv", skip = 4)
gdp <- read.csv("data/gdp.csv", skip = 4)
gross_savings <- read.csv("data/gross_savings.csv", skip = 4)

# Merge different datasets to make the manipulation easier
countries <- rbind(balance_gdp, debt, exports, gdp, gross_savings)
countries <- select(countries, Country.Name, Country.Code, Indicator.Name, Indicator.Code, X2016)

# Change indicator code to a more readable format
countries$Indicator.Code <- as.character(countries$Indicator.Code)
countries$Country.Name <- as.character(countries$Country.Name)
countries[countries$Indicator.Code == "BN.CAB.XOKA.GD.ZS", ]$Indicator.Code <- "balance"
countries[countries$Indicator.Code == "DT.TDS.DECT.CD", ]$Indicator.Code <- "debt"
countries[countries$Indicator.Code == "NE.EXP.GNFS.KD.ZG", ]$Indicator.Code <- "exports"
countries[countries$Indicator.Code == "NY.GDP.PCAP.CD", ]$Indicator.Code <- "gdp"
countries[countries$Indicator.Code == "NY.GDS.TOTL.ZS", ]$Indicator.Code <- "savings"

names(countries)[5] <- "Value"

# Countries from South America selected to the analysis
countries_list <- c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Ecuador", "Paraguay", "Peru", "Uruguay")

countries <- filter(countries, Country.Name %in% countries_list)
countries_sub <- countries
countries_sub <- countries_test[, -c(1, 3)]
countries_sub <- spread(countries_test, Indicator.Code, Value)

datMat <- matrix(as.numeric(unlist(countries_sub[,c(2:6)])), nrow = 9)
rownames(datMat) <- names
parms <- matrix(c(rep(NA, 5)), byrow = TRUE, ncol = 1, nrow = 5)
vecWeights <- c(rep(1/5, 5))
vecMaximiz <- c(TRUE, FALSE, TRUE, TRUE, TRUE)
prefFunction <- c(rep(1, 5))
names <- as.character(countries_sub[,1])

PromObj <- RPrometheeConstructor(datMat, vecWeights, vecMaximiz, prefFunction, parms, normalize = FALSE, alternatives = names)

results <- RPrometheeII(PromObj)

final <- data.frame(Country = results@alternatives, Phi = results@Phi)
