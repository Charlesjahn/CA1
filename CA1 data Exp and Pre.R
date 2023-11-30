getwd()
setwd("C:/Users/charl/Desktop/classes/Data Exploration and Preparation/CA1")
library(tidyr)
library(dplyr)

library(ggplot2)
library(gridExtra)

# read the file
dataCrimes <- read.csv("C:/Users/charl/Desktop/classes/Data Exploration and Preparation/CA1/Crimes_in_Brazil_2015_2022.csv")

# Show 20 first
head(dataCrimes, 20)
dim(dataCrimes)
summary(dataCrimes)

# Reorganize the data
dataReorganized <- dataCrimes %>%
  pivot_wider(names_from = `Tipo.Crime`, values_from = Ocorrências)

head(dataReorganized, 20)

#Translate the database titles from Portuguese to English
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

# months translations 
translation_dict <- c("janeiro" = "january",
                      "fevereiro" = "february",
                      "março" = "march",
                      "abril" = "april",
                      "maio" = "may",
                      "junho" = "june",
                      "julho" = "july",
                      "agosto" = "august",
                      "setembro" = "september",
                      "outubro" = "october",
                      "novembro" = "november",
                      "dezembro" = "december")

# replace the months from Portuguese to English
dataReorganized$Month <- translation_dict[dataReorganized$Month]


head(dataReorganized, 20)

# Find the mean of all crimes based in the states and month of a specific crime 
means_crimes <- dataReorganized %>%
  group_by(State, Month) %>%
  summarise(across(c(Rape, 
                     Vehicle_Theft, Homicide, Bodily_injury_followed_by_death, 
                     Robbery_Institution, Cargo_Theft,Vehicle_Robbery, 
                     Robbery_Followed_by_Death, 
                     Attempted_Homicide), ~mean(., na.rm = TRUE), .names = "Mean_{.col}"))
        
# Replace NA values in the crime columns with the corresponding mean values, keeping only the columns with the results.
dataReorganized <- dataReorganized %>%
  left_join(means_crimes, by = c("State", "Month")) %>%
  mutate(across(c(Rape, 
                  Vehicle_Theft, Homicide, Bodily_injury_followed_by_death, 
                  Robbery_Institution, Cargo_Theft,Vehicle_Robbery, 
                  Robbery_Followed_by_Death, 
                  Attempted_Homicide), ~ifelse(is.na(.x), get(paste0("Mean_", cur_column())), .x)),
         across(c(Rape, 
                  Vehicle_Theft, Homicide, Bodily_injury_followed_by_death, 
                  Robbery_Institution, Cargo_Theft, Vehicle_Robbery, 
                  Robbery_Followed_by_Death, 
                  Attempted_Homicide), ~as.integer(round(.)))) %>%
  select(State, Year, Month, Rape, Vehicle_Theft, Homicide, Bodily_injury_followed_by_death, Robbery_Institution, Cargo_Theft, Vehicle_Robbery, Robbery_Followed_by_Death, Attempted_Homicide)

# Add a column  "Total Crimes"
dataReorganized <- dataReorganized %>%
  mutate(Total_Crimes = rowSums(select(., -State, -Year, -Month), na.rm = TRUE))

head(dataReorganized[, c('State', 'Year', 'Month', 'Total_Crimes')], 20)



# Remove the column 'Year'
dataReorganized_no_year <- dataReorganized[, !names(dataReorganized) %in% "Year"]

# Select only numeric columns
numeric_cols <- sapply(dataReorganized_no_year, is.numeric)
numeric_data <- dataReorganized_no_year[, numeric_cols]

# Calculate MEAN MEDIAN MIN MAX SD of all columns that is calculable, Limit to 3 decimal
statistics <- sapply(numeric_data, function(x) {
  c(Mean = round(mean(x, na.rm = TRUE), digits = 3),
    Median = round(median(x, na.rm = TRUE), digits = 3),
    Minimum = round(min(x, na.rm = TRUE), digits = 3),
    Maximum = round(max(x, na.rm = TRUE), digits = 3),
    SD = round(sd(x, na.rm = TRUE), digits = 3))
})

# Transpose the results for display
statistics <- t(statistics)

# Name lines and columns
rownames(statistics) <- names(dataReorganized_no_year)[numeric_cols]
colnames(statistics) <- c("Mean", "Median", "Minimum", "Maximum", "SD")

# show new table with all statistics. 
statistics


# --- PLOT EXAMPLES WITH ORIGINAL DATA ---
# Plot (Homicide x Bodily_injury_followed_by_death)
ggplot(dataReorganized, aes(x = Homicide, y = Bodily_injury_followed_by_death)) + geom_point()

# Plot (between Vehicle_Theft x Vehicle_Robbery)
ggplot(dataReorganized, aes(x = Vehicle_Theft, y = Vehicle_Robbery)) + geom_point()

# Plot (between Homicide x Attempted_Homicide)
ggplot(dataReorganized, aes(x = Homicide, y = Attempted_Homicide)) + geom_point()

# Plot (between Robbery_Institution x Cargo_Theft)
ggplot(dataReorganized, aes(x = Robbery_Institution, y = Cargo_Theft)) + geom_point()


# MinMax Normalization
normalized_MinMax <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# MinMax normalization of Homicide and Bodily_injury_followed_by_death
Homicide_norm<-normalized_MinMax(dataReorganized$Homicide)
Bodily_injury_followed_by_death_norm<-normalized_MinMax(dataReorganized$Bodily_injury_followed_by_death)
# Normalized plot (Homicide x Bodily_injury_followed_by_death)
ggplot(dataReorganized, aes(x = Homicide_norm, y = Bodily_injury_followed_by_death_norm)) + geom_point()

# MinMax normalization of Vehicle_Theft and Vehicle_Robbery
Vehicle_Theft_norm<-normalized_MinMax(dataReorganized$Vehicle_Theft)
Vehicle_Robbery_norm<-normalized_MinMax(dataReorganized$Vehicle_Robbery)
# Normalized plot (Vehicle_Theft x Vehicle_Robbery)
ggplot(dataReorganized, aes(x = Vehicle_Theft_norm, y = Vehicle_Robbery_norm)) + geom_point()

# MinMax normalization of Homicide and Attempted_Homicide
Homicide_norm<-normalized_MinMax(dataReorganized$Homicide)
Attempted_Homicide_norm<-normalized_MinMax(dataReorganized$Attempted_Homicide)
# Normalized plot (Homicide x Attempted_Homicide)
ggplot(dataReorganized, aes(x = Homicide_norm, y = Attempted_Homicide_norm)) + geom_point()

# MinMax normalization of Robbery_Institution and Cargo_Theft
Robbery_Institution_norm<-normalized_MinMax(dataReorganized$Robbery_Institution)
Cargo_Theft_norm<-normalized_MinMax(dataReorganized$Cargo_Theft)
# Normalized plot (Homicide x Attempted_Homicide)
ggplot(dataReorganized, aes(x = Robbery_Institution_norm, y = Cargo_Theft_norm)) + geom_point()



# Z-score Standardization  
Homicide_zscaled<-scale(dataReorganized$Homicide)
Bodily_injury_followed_by_death_zscaled<-scale(dataReorganized$Bodily_injury_followed_by_death)
# Z-score plot (Homicide x Bodily_injury_followed_by_death)
ggplot(dataReorganized, aes(x = Homicide_zscaled, y = Bodily_injury_followed_by_death_zscaled)) + geom_point()



# Robust Scaler
install.packages("robustbase")
library(robustbase)

# Normalization with Robust Scaler
Homicide_robust <- (dataReorganized$Homicide - median(dataReorganized$Homicide)) / mad(dataReorganized$Homicide)
Bodily_injury_followed_by_death_robust <- (dataReorganized$Bodily_injury_followed_by_death - median(dataReorganized$Bodily_injury_followed_by_death)) / mad(dataReorganized$Bodily_injury_followed_by_death)
# Robust Scaler plot (Homicide x Bodily_injury_followed_by_death)
ggplot(dataReorganized, aes(x = Homicide_robust, y = Bodily_injury_followed_by_death_robust)) + geom_point()