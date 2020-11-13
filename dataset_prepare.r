# All archives downloaded and used in this base was founded here: 
# https://dados.gov.br/dataset/serie-historica-de-precos-de-combustiveis-por-revenda

# Installing necessary packages
install.packages('ggplot2')
install.packages('ggthemes')
install.packages('hrbrthemes')
install.packages('lme4')
install.packages("dplyr")
install.packages("gapminder")

library(lubridate)
library(ggplot2)
library(ggthemes)
library(hrbrthemes)
library(dplyr)
library(gapminder)


# Function for get MODE
getmode <- function(v) {
   uniqv <- unique(v)
   uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Entering in dataset directory
setwd('C:\\Users\\danie\\Documents\\R Projects\\USJT\\P.I\\dataset')

#--------------------------------------------------------2017/1----------------------------------#

# Defining dataset and removing unecessary columns
firstC_2017  <-  read.csv(file="2017-1_CA.csv", sep=";", encoding="ISO-8859-1")
firstC_2017  <- firstC_2017[-c(1,10:45)]

# Converting column valor de venda for numeric
firstC_2017$Valor.de.Venda <- as.numeric(sub(",", ".", firstC_2017$Valor.de.Venda))
firstC_2017$Data.da.Coleta <- as.Date(firstC_2017$Data.da.Coleta, "%d/%m/%Y")
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

#--------------------------------------------------------2017/2----------------------------------#
secondC_2017  <-  read.csv(file="2017-2_CA.csv", sep=";", encoding="ISO-8859-1")
secondC_2017  <- secondC_2017[-c(1,10:45)]

secondC_2017$Valor.de.Venda <- as.numeric(sub(",", ".", secondC_2017$Valor.de.Venda))
secondC_2017$Data.da.Coleta <- as.Date(secondC_2017$Data.da.Coleta, "%d/%m/%Y")
secondC_2017 <- secondC_2017[!is.na(as.numeric(as.character(secondC_2017$Valor.de.Venda))),]

na.omit(secondC_2017)
print(which(is.na(as.numeric(sub(",", ".", secondC_2017$Valor.de.Venda)))))

mean_2017_2   <- mean(secondC_2017$Valor.de.Venda)
median_2017_2 <- median(secondC_2017$Valor.de.Venda)
mode_2017_2   <- getmode(secondC_2017$Valor.de.Venda)

#--------------------------------------------------------2018/1----------------------------------#
firstC_2018  <-  read.csv(file="2018-1_CA.csv", sep=";", encoding="ISO-8859-1")
firstC_2018  <- firstC_2018[-c(1,10:45)]

firstC_2018$Valor.de.Venda <- as.numeric(sub(",", ".", firstC_2018$Valor.de.Venda))
firstC_2018$Data.da.Coleta <- as.Date(firstC_2018$Data.da.Coleta, "%d/%m/%Y")
firstC_2018 <- firstC_2018[!is.na(as.numeric(as.character(firstC_2018$Valor.de.Venda))),]

na.omit(firstC_2018)
print(which(is.na(as.numeric(sub(",", ".", firstC_2018$Valor.de.Venda)))))

mean_2018_1   <- mean(firstC_2018$Valor.de.Venda)
median_2018_1 <- median(firstC_2018$Valor.de.Venda)
mode_2018_1   <- getmode(firstC_2018$Valor.de.Venda)

#--------------------------------------------------------2018/2----------------------------------#
secondC_2018  <-  read.csv(file="2018-2_CA.csv", sep=";", encoding="ISO-8859-1")
secondC_2018  <- secondC_2018[-c(1,10:45)]

secondC_2018$Valor.de.Venda <- as.numeric(sub(",", ".", secondC_2018$Valor.de.Venda))
secondC_2018$Data.da.Coleta <- as.Date(secondC_2018$Data.da.Coleta, "%d/%m/%Y")
secondC_2018 <- secondC_2018[!is.na(as.numeric(as.character(secondC_2018$Valor.de.Venda))),]

na.omit(secondC_2018)
print(which(is.na(as.numeric(sub(",", ".", secondC_2018$Valor.de.Venda)))))

mean_2018_2   <- mean(secondC_2018$Valor.de.Venda)
median_2018_2 <- median(secondC_2018$Valor.de.Venda)
mode_2018_2   <- getmode(secondC_2018$Valor.de.Venda)

#--------------------------------------------------------2019/1----------------------------------#
firstC_2019  <-  read.csv(file="2019-1_CA.csv", sep=";", encoding="ISO-8859-1")
firstC_2019  <- firstC_2019[-c(1,10:45)]

firstC_2019$Valor.de.Venda <- as.numeric(sub(",", ".", firstC_2019$Valor.de.Venda))
firstC_2019$Data.da.Coleta <- as.Date(firstC_2019$Data.da.Coleta, "%d/%m/%Y")
firstC_2019 <- firstC_2019[!is.na(as.numeric(as.character(firstC_2019$Valor.de.Venda))),]

na.omit(firstC_2019)
print(which(is.na(as.numeric(sub(",", ".", firstC_2019$Valor.de.Venda)))))

mean_2019_1   <- mean(firstC_2019$Valor.de.Venda)
median_2019_1 <- median(firstC_2019$Valor.de.Venda)
mode_2019_1   <- getmode(firstC_2019$Valor.de.Venda)

#-------------------------------------------------------FIM--------------------------------------#


#Creating dataframe with farmed informations
infos <- data.frame(
   mean   = c(mean_2017_1,   mean_2017_2,   mean_2018_1,   mean_2018_2,   mean_2019_1),
   median = c(median_2017_1, median_2017_2, median_2018_1, median_2018_2, median_2019_1),
   mode   = c(mode_2017_1,   median_2017_2, mode_2018_1,   mode_2018_2,    mode_2019_1),
   data   = c('2017/1', '2017/2', '2018/1', '2018/2', '2019/1')
)
infos
# Line chart upgrade fuel price in 2017-1 2017-2
infos %>%
   filter(data %in% c('2017/1','2017/2','2018/1','2018/2','2019/1')) %>%
      ggplot(., aes(x = data, y= median, group = 2 ))                            + 
         geom_line(color="#69b3a2", size=1, alpha=0.9)                           +
         labs(x="Semestre", y="Valor em R$")                                     +
         theme_ipsum()                                                           +
         theme(plot.title = element_text(hjust = 0.5))                           +
         ggtitle("Evolução do preço mediano do combustível de 2017/1 á 2017/2")
                                             

# Bar chart with mean median and mode of 3 years
ggplot(infos)                                                                        +
   geom_bar(stat = "identity", width=.75, aes(x = data, y= mean,   fill="Média"))    +
   geom_bar(stat = "identity", width=.75, aes(x = data, y= median, fill="Mediana"))  +
   labs(x="Semestre", y="Média")                                                     +
   scale_y_continuous(breaks = seq(0,3.5,.5))                                        +
   theme_ipsum()                                                                     +
   ggtitle("Valores de tedência central do valor do combustivel de 2017/1 á 2019/1")

# Chart with fuel price by fuel type
View(firstYear)