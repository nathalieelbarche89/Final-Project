


# code to clean and prepare data
# Load necessary packages
library(readr)
library(dplyr)
library(tidyr)


# Load the dataset from GitHub
url = "https://raw.githubusercontent.com/nathalieelbarche89/Final-Project/c7f4f15b65668fa22315dd9574f27e2da8e9fea1/lifestyle_sustainability_data.csv"
sustainability_data = read_csv(url)


# Inspect the data
str(sustainability_data)
head(sustainability_data)
summary(sustainability_data)

# Check for missing values
missing_values=sustainability_data %>% summarise_all(~sum(is.na(.)))
print(missing_values)

# Remove rows with missing values
sustainability_data=sustainability_data %>% drop_na()

# Handling categorical data
# Convert relevant columns to factors
sustainability_data=sustainability_data %>%
  mutate(
    DietType = as.factor(DietType),
    TransportationMode = as.factor(TransportationMode),
    EnergySource = as.factor(EnergySource),
    DisposalMethods = as.factor(DisposalMethods),
    UsingPlasticProducts = factor(UsingPlasticProducts, levels = c("Rarely", "Sometimes", "Often")),
    LocalFoodFrequency = factor(LocalFoodFrequency, levels = c("Rarely", "Sometimes", "Often")),
    CommunityInvolvement = factor(CommunityInvolvement, levels = c("Low","Moderate", "High"))
  )

# Boxplot to check for outliers in numeric columns
boxplot(sustainability_data$MonthlyElectricityConsumption, main= "Boxplot of Monthly Electricity Consumption", col= "lightgreen")

#Create sustainability score based on actual columns
sustainability_data=sustainability_data %>%
  mutate(
    energy_score = case_when(
      EnergySource == "Renewable" ~3,
      EnergySource == "Mixed" ~2,
      EnergySource == "Non-Renewable" ~1
    ),
    transportation_score = case_when(
      TransportationMode == "Bike" | TransportationMode == "Walk" ~3,
      TransportationMode == "Public Transit" ~2,
      TransportationMode == "Car" ~1
    ),
    disposal_score = case_when(
      DisposalMethods == "Composting" ~3,
      DisposalMethods == "Recycling" ~2,
      DisposalMethods == "Landfill" ~1
    ),
    electricity_score = 1 / (MonthlyElectricityConsumption / max(MonthlyElectricityConsumption)),
    water_score = 1 / (MonthlyWaterConsumption / max(MonthlyWaterConsumption))
  ) 

#Calculate overall sustainability score
sustainability_data = sustainability_data %>%
  mutate(
    sustainability_score = (energy_score * 0.3) +
      (transportation_score * 0.3) +
      (disposal_score * 0.2) +
      (electricity_score * 0.1) +
      (water_score * 0.1)
  )
# Save the cleaned dataset
write.csv(sustainability_data, "cleaned_lifestyle_sustainability_data.csv", row.names= FALSE)

