getwd()
setwd("C:/Users/charl/Desktop/classes/Data Exploration and Preparation/CA1")
library(tidyr)
library(dplyr)
library(scales)
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
  select(State, Year, Month, Rape, Vehicle_Theft, Homicide, Bodily_injury_followed_by_death, 
         Robbery_Institution, Cargo_Theft, Vehicle_Robbery, Robbery_Followed_by_Death, Attempted_Homicide)

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

dataReorganized$Date <- as.Date(paste(dataReorganized$Year, dataReorganized$Month, "01"), format = "%Y %B %d")

# Aggregating total crimes by date
total_crimes_by_date <- dataReorganized %>%
  group_by(Date) %>%
  summarise(Total_Crimes = sum(Total_Crimes, na.rm = TRUE))

# Graph
ggplot(total_crimes_by_date, aes(x = Date, y = Total_Crimes)) +
  geom_line() +
  labs(title = "Total Crimes vs Year", x = "Date", y = "Total Crimes")

# Aggregating total crimes by states
total_crimes_by_state <- aggregate(Total_Crimes ~ State, data = dataReorganized, sum)

# arrange in descending  order
total_crimes_by_state <- total_crimes_by_state %>%
  mutate(Mean = Total_Crimes / length(unique(dataReorganized$Year))) %>%
  arrange(desc(Mean))

# Select tpo 10 states of total crimes
top_10_states <- head(total_crimes_by_state, 10)

# add % column 
top_10_states <- top_10_states %>%
  mutate(Percentage = Mean / sum(Mean) * 100)

# Create a pie plot showing the top 10 mean of crimes
pie_chart <- ggplot(top_10_states, aes(x = "", y = Percentage, fill = State)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Top 10 States with high mean") +
  geom_text(aes(label = paste0(round(Mean))), position = position_stack(vjust = 0.5))

print(pie_chart)


# --- PLOT OF CORRELATION BETWEEN TWO CRIMES OVER TIME WITH ORIGINAL DATA ---
# Plot - rates over time (Homicide x Bodily_injury_followed_by_death)
ggplot(dataReorganized, aes(x = Year, y = Homicide, color = "Homicide")) +
  geom_point() +
  geom_point(aes(y = Bodily_injury_followed_by_death, color = "Bodily Injury followed by death")) +
  labs(title = "Correlation Over Time",
       x = "Year",
       y = "Rates",
       color = "Crime") +
  scale_color_manual(values = c("Homicide" = "red", "Bodily Injury followed by death" = "blue"))


# MinMax Normalization function
normalized_MinMax <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# MinMax Normalization of crimes
Rape_norm<-normalized_MinMax(dataReorganized$Rape)
Vehicle_Theft_norm<-normalized_MinMax(dataReorganized$Vehicle_Theft)
Homicide_norm<-normalized_MinMax(dataReorganized$Homicide)
Bodily_injury_followed_by_death_norm<-normalized_MinMax(dataReorganized$Bodily_injury_followed_by_death)
Robbery_Institution_norm<-normalized_MinMax(dataReorganized$Robbery_Institution)
Cargo_Theft_norm<-normalized_MinMax(dataReorganized$Cargo_Theft)
Vehicle_Robbery_norm<-normalized_MinMax(dataReorganized$Vehicle_Robbery)
Robbery_Followed_by_Death_norm<-normalized_MinMax(dataReorganized$Robbery_Followed_by_Death)
Attempted_Homicide_norm<-normalized_MinMax(dataReorganized$Attempted_Homicide)

# Normalize rates over time (Homicide x Bodily_injury_followed_by_death)
dataReorganized <- dataReorganized %>%
  mutate(Year = as.factor(Year)) %>%
  group_by(Year) %>%
  mutate(
    Homicide_norm = normalized_MinMax(Homicide),
    Bodily_injury_followed_by_death_norm = normalized_MinMax(Bodily_injury_followed_by_death)
  ) %>%
  ungroup()

# Normalized plot - rates over time (Homicide x Bodily_injury_followed_by_death)
ggplot(dataReorganized, aes(x = Year, y = Homicide_norm, color = "Homicide")) +
  geom_point() +
  geom_point(aes(y = Bodily_injury_followed_by_death_norm, color = "Bodily Injury followed by death")) +
  labs(title = "Correlation Over Time",
       x = "Year",
       y = "Normalized Rates",
       color = "Crime") +
  scale_color_manual(values = c("Homicide" = "red", "Bodily Injury followed by death" = "blue"))


# Calculate Z-score
standardize_zscore <- function(x) {
  return ((x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE))
}

# Z-scores of crimes
dataZscaled <- dataReorganized %>%
  mutate(
    Rape_zscaled = standardize_zscore(Rape),
    Vehicle_Theft_zscaled = standardize_zscore(Vehicle_Theft),
    Homicide_zscaled = standardize_zscore(Homicide),
    Bodily_injury_followed_by_death_zscaled = standardize_zscore(Bodily_injury_followed_by_death),
    Robbery_Institution_zscaled = standardize_zscore(Robbery_Institution),
    Cargo_Theft_zscaled = standardize_zscore(Cargo_Theft),
    Vehicle_Robbery_zscaled = standardize_zscore(Vehicle_Robbery),
    Robbery_Followed_by_Death_zscaled = standardize_zscore(Robbery_Followed_by_Death),
    Attempted_Homicide_zscaled = standardize_zscore(Attempted_Homicide)
  ) %>%
  select(State, Year, Month, Rape_zscaled, Vehicle_Theft_zscaled, Homicide_zscaled, Bodily_injury_followed_by_death_zscaled, Robbery_Institution_zscaled, Cargo_Theft_zscaled, Vehicle_Robbery_zscaled,Robbery_Followed_by_Death_zscaled, Attempted_Homicide_zscaled)

View(dataZscaled)

# Melt data 
dataMelt <- dataZscaled %>%
  select(State, Year, Month, Rape_zscaled, Vehicle_Theft_zscaled, Homicide_zscaled, Bodily_injury_followed_by_death_zscaled, Robbery_Institution_zscaled, Cargo_Theft_zscaled, Vehicle_Robbery_zscaled,Robbery_Followed_by_Death_zscaled, Attempted_Homicide_zscaled) %>%
  pivot_longer(cols = c(Vehicle_Theft_zscaled, Vehicle_Robbery_zscaled),
               names_to = "Crime", values_to = "Z_Score")

View(dataMelt)

# Plot - comparison between states (Vehicle_Theft_zscaled x Vehicle_Robbery_zscaled)
ggplot(dataMelt, aes(x = State, y = Z_Score, fill = Crime)) +
  geom_boxplot() +
  labs(title = "Comparison between states",
       x = "State",
       y = "Z-Score",
       fill = "Crime") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Robust Scaler
install.packages("robustbase")
library(robustbase)

# Normalization with Robust Scaler
Rape_robust <- (dataReorganized$Rape - median(dataReorganized$Rape)) / mad(dataReorganized$Rape)
Vehicle_Theft_robust <- (dataReorganized$Vehicle_Theft - median(dataReorganized$Vehicle_Theft)) / mad(dataReorganized$Vehicle_Theft)
Homicide_robust <- (dataReorganized$Homicide - median(dataReorganized$Homicide)) / mad(dataReorganized$Homicide)
Bodily_injury_followed_by_death_robust <- (dataReorganized$Bodily_injury_followed_by_death - median(dataReorganized$Bodily_injury_followed_by_death)) / mad(dataReorganized$Bodily_injury_followed_by_death)

Robbery_Institution_robust <- (dataReorganized$Robbery_Institution - median(dataReorganized$Robbery_Institution)) / mad(dataReorganized$Robbery_Institution)
Cargo_Theft_robust <- (dataReorganized$Cargo_Theft - median(dataReorganized$Cargo_Theft)) / mad(dataReorganized$Cargo_Theft)
Vehicle_Robbery_robust <- (dataReorganized$Vehicle_Robbery - median(dataReorganized$Vehicle_Robbery)) / mad(dataReorganized$Vehicle_Robbery)
Robbery_Followed_by_Death_robust <- (dataReorganized$Robbery_Followed_by_Death - median(dataReorganized$Robbery_Followed_by_Death)) / mad(dataReorganized$Robbery_Followed_by_Death)
Attempted_Homicide_robust <- (dataReorganized$Attempted_Homicide - median(dataReorganized$Attempted_Homicide)) / mad(dataReorganized$Attempted_Homicide)

# Boxplot showing outliers
boxplot(Robbery_Institution_robust, main="Robbery_Institution - Robust Scaled", col="lightgreen", border="black")

# Robust Scaler plot (Homicide x Bodily_injury_followed_by_death)
ggplot(dataReorganized, aes(x = Homicide_robust, y = Bodily_injury_followed_by_death_robust)) + geom_point()


# Reorganize months in order for plotting and analysis
dataReorganized$Month <- factor(dataReorganized$Month, levels = c("january", "february", "march", "april", "may", "june", "july", "august", "september", "october", "november", "december"))


# MONTHLY NUMBERS OF A CRIMES OVER THE YEARS (NUMBERS PER MONTH IN DIFFERENT YEARS)
# Specific crime (To view other types of crimes, just change the variable y)
ggplot(dataReorganized, aes(x = Month, y = Homicide, fill = as.factor(Year))) +
  geom_bar(stat = "sum") +
  labs(title = "Fluctuation of Homicide Numbers",
       x = "Month",
       y = "Number of Homicides",
       fill = "Year") +
  scale_x_discrete(labels = translation_dict) 
scale_y_continuous(labels = scales::comma_format()) 

# Total number of crimes
ggplot(dataReorganized, aes(x = Month, y = Total_Crimes, fill = as.factor(Year))) +
  geom_bar(stat = "sum") +
  labs(title = "Fluctuation of Crime Numbers",
       x = "Month",
       y = "Number of Crimes",
       fill = "Year") +
  scale_x_discrete(labels = translation_dict) +  
  scale_y_continuous(labels = scales::comma_format())


# ANUAL NUMBERS OF A CRIMES OVER THE YEARS (NUMBERS PER YEAR BETWEEN 2025 AND 2022)
# Specific crime (To view other types of crimes, just change the variable y)
ggplot(dataReorganized, aes(x = Year, y = Homicide, fill = as.factor(Month))) +
  geom_bar(stat = "sum") +
  labs(title = "Fluctuation of Homicide Numbers",
       x = "Year",
       y = "Number of Homicides",
       fill = "Month") +
  scale_x_discrete(labels = translation_dict)  
scale_y_continuous(labels = scales::comma_format())  

# Total number of crimes
ggplot(dataReorganized, aes(x = Year, y = Total_Crimes, fill = as.factor(Month))) +
  geom_bar(stat = "sum") +
  labs(title = "Fluctuation of Crime Numbers",
       x = "Year",
       y = "Number of Crimes",
       fill = "Month") +
  scale_x_discrete(labels = translation_dict) +  
  scale_y_continuous(labels = scales::comma_format()) 



# Heatmap of total number of crimes between 2015 and 2022
heatmap_total_crimes <- ggplot(dataReorganized, aes(x = Month, y = as.factor(Year), fill = Total_Crimes)) +
  geom_tile(stat = "sum") +
  labs(title = "Crimes Per Month",
       x = "Month",
       y = "Year",
       fill = "Number of Crimes") +
  scale_x_discrete(labels = translation_dict) +
  scale_fill_gradient2(low = "blue", mid = "yellow", high = "red", midpoint = 0) + 
  theme_minimal()

print(heatmap_total_crimes)


# Heatmap for comparison between months (To view other types of crimes, just change the variable fill)
heatmap_month <- ggplot(dataReorganized, aes(x = Month, y = State, fill = Homicide)) +
  geom_tile(stat = "sum", width = 0.9, height = 0.9) +
  labs(title = "Comparison of Homicide Numbers Between States (Month)",
       x = "Month",
       y = "State",
       fill = "Homicide") +
  scale_x_discrete(labels = translation_dict) +
  scale_fill_gradient2(low = "yellow", mid = "orange", high = "red", midpoint = 250) + 
  theme_minimal()

print(heatmap_month)

# Heatmap for comparison between years (To view other types of crimes, just change the variable fill)
heatmap_year <- ggplot(dataReorganized, aes(x = Year, y = State, fill = Rape)) +
  geom_tile(width = 0.9, height = 0.9) +
  labs(title = "Comparison of Rape Numbers Between States (Year)",
       x = "Year",
       y = "State",
       fill = "Rape") +
  scale_fill_gradient2(low = "yellow", mid = "orange", high = "red", midpoint = 250) +  
  theme_minimal()

print(heatmap_year)



# Scatter plot to compare the two types of crime
ggplot(dataReorganized, aes(x = Homicide_norm, y = Attempted_Homicide_norm)) +
  geom_point() +
  labs(title = "Correlation between Homicide and Attempted Homicide (Normalized)",
       x = "Homicide",
       y = "Attempted Homicide") +
  theme_minimal() 



# Create Dummy Encoding for State
install.packages('fastDummies')
library('fastDummies')

dataReorganized <- dummy_cols(dataReorganized, select_columns = 'State')

head(dataReorganized)
View(dataReorganized)
