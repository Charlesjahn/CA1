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



#criar um array com os nomes de crime.
exmep [a,b,c]
        
colocar um loop para cada item em exempla array

        # Find a mean based in the States and Month of a specific Crime 
        means <- dataReorganized %>%
          group_by(State, Month) %>%
          summarise(Mean = round(mean(Rape, na.rm = TRUE)))
        
        # Substituir valores NA na coluna Rape pelo valor médio correspondente
        dataReorganized <- dataReorganized %>%
          left_join(means, by = c("State", "Month")) %>%
          mutate(Rape = ifelse(is.na(Rape), Mean, Rape))





