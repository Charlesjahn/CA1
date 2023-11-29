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

#Translate the database titles from Portugues to English
dataReorganized <- dataReorganized %>%
  rename(
    State = UF,
    Year = Ano,
    Month = Mês,
    Rape = Estupro,
    Vehicle_Theft = `Furto de veículo`,
    Homicide = `Homicídio doloso`,
    Bodily_injury_followed_by_death = `Lesão corporal seguida de morte`,
    Robbery_Institution = `Roubo a instituição financeira`,
    Cargo_Theft = `Roubo de carga`,
    Vehicle_Robbery = `Roubo de veículo`,
    Robbery_Followed_by_Death = `Roubo seguido de morte (latrocínio)`,
    Attempted_Homicide = `Tentativa de homicídio`
  )

head(dataReorganized, 20)

# Add a column  "Total Crimes"
dataReorganized <- dataReorganized %>%
  mutate(Total_Crimes = rowSums(select(., -State, -Year, -Month), na.rm = TRUE))

head(dataReorganized, 20)

# Find the mean of all crimes based in the states and month of a specific crime 
means_crimes <- dataReorganized %>%
  group_by(State, Month) %>%
  summarise(across(c(Rape, Vehicle_Theft, Homicide, Bodily_injury_followed_by_death, Robbery_Institution, Cargo_Theft, Vehicle_Robbery, Robbery_Followed_by_Death, Attempted_Homicide), ~mean(., na.rm = TRUE), .names = "Mean_{.col}"))
        
# Replace NA values in the crime columns with the corresponding mean values, keeping only the columns with the results.
dataReorganized <- dataReorganized %>%
  left_join(means_crimes, by = c("State", "Month")) %>%
  mutate(across(c(Rape, Vehicle_Theft, Homicide, Bodily_injury_followed_by_death, Robbery_Institution, Cargo_Theft, Vehicle_Robbery, Robbery_Followed_by_Death, Attempted_Homicide), ~ifelse(is.na(.x), get(paste0("Mean_", cur_column())), .x)),
         across(c(Rape, Vehicle_Theft, Homicide, Bodily_injury_followed_by_death, Robbery_Institution, Cargo_Theft, Vehicle_Robbery, Robbery_Followed_by_Death, Attempted_Homicide), ~as.integer(round(.)))) %>%
  select(State, Year, Month, Rape, Vehicle_Theft, Homicide, Bodily_injury_followed_by_death, Robbery_Institution, Cargo_Theft, Vehicle_Robbery, Robbery_Followed_by_Death, Attempted_Homicide)

# View updated data
View(dataReorganized)





