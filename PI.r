# All archives downloaded and used in this base was founded here: 
# https://dados.gov.br/dataset/serie-historica-de-precos-de-combustiveis-por-revenda

# Installing necessary packages
install.packages('ggplot2')
install.packages('ggthemes')
install.packages('lme4')
install.packages("dplyr")
install.packages("gapminder")

library(ggplot2)
library(ggthemes)
library(dplyr)
library(gapminder)

# Function for get MODE
getmode <- function(v) {
   uniqv <- unique(v)
   uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Entering in dataset directory
setwd('C:\\Users\\danie\\Documents\\R Projects\\USJT\\P.I\\dataset')

# Defining dataset and removing unecessary columns
firstC_2017  <-  read.csv(file="2017-1_CA.csv", sep=";", encoding="ISO-8859-1")
firstC_2017  <- firstC_2017[-c(1,10:45)]

# Converting column valor de venda for numeric
firstC_2017$Valor.de.Venda <- as.numeric(sub(",", ".", firstC_2017$Valor.de.Venda))
firstC_2017 <- firstC_2017[!is.na(as.numeric(as.character(firstC_2017$Valor.de.Venda))),]

#Cleaning data
na.omit(firstC_2017)
View(firstC_2017)

# I'm using it to see what's the line number that there is a problem
print(which(is.na(as.numeric(sub(",", ".", firstC_2017$Valor.de.Venda)))))

# Creating vector with central averages
mean_2017_1   <- mean(firstC_2017$Valor.de.Venda)
median_2017_1 <- median(firstC_2017$Valor.de.Venda)
mode_2017_1   <- getmode(firstC_2017$Valor.de.Venda)

secondC_2017 <-  read.csv(file="2017-2_CA.csv", sep=";", encoding="ISO-8859-1")

firstC_2018  <-  read.csv(file="2018-1_CA.csv", sep=";", encoding="ISO-8859-1")
secondC_2018 <-  read.csv(file="2018-2_CA.csv", sep=";", encoding="ISO-8859-1")

firstC_2019  <-  read.csv(file="2019-1_CA.csv", sep=";", encoding="ISO-8859-1")



#Creating dataframe with farmed informations
infos <- data.frame(
   mean   = c(mean_2017_1),
   median = c(median_2017_1),
   mode   = c(mode_2017_1),
   data   = c('2017/1')
)