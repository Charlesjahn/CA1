getwd()
setwd("C:/Users/charl/Desktop/classes/Data Exploration and Preparation/CA1")
library(tidyr)
library(dplyr)
# read the file
dataCrimes <- read.csv("C:/Users/charl/Desktop/classes/Data Exploration and Preparation/CA1/Crimes_in_Brazil_2015_2022.csv")

# Show 20 first
head(dataCrimes, 20)

# Reorganize the data
dataReorganized <- dataCrimes %>%
  pivot_wider(names_from = `Tipo.Crime`, values_from = Ocorrências)

head(dataReorganized, 20)

# Reorganize os dados para de crime em linhas e remova a coluna de ano
dataLong <- dataReorganized %>%
  select(-Ano) %>%
  group_by(UF, Mês) %>%
  summarise(Mean = round(mean(Estupro, na.rm = TRUE)))

head(dataLong)


