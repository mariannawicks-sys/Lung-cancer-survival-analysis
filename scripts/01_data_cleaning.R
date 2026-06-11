# Load libraries
library(survival)
library(survminer)
library(dplyr)

# Load data
lung <- read.csv("data/lung_cancer_dataset.csv", stringsAsFactors = FALSE)

# Check it loaded
dim(lung)
head(lung)

# Create survival status column (1 = survived, 0 = did not)
lung$status <- ifelse(lung$Survived == "Yes", 1, 0)

# Recode smoking status
lung$Smoking_Status <- factor(lung$Smoking_Status,
  levels = c("Never Smoked", "Former Smoker", "Current Smoker"))

# Recode cancer stage
lung$Cancer_Stage <- factor(lung$Cancer_Stage,
  levels = c("Stage I", "Stage II", "Stage III", "Stage IV"))

# Recode gender and cancer type
lung$Gender <- factor(lung$Gender)
lung$Cancer_Type <- factor(lung$Cancer_Type)

# Recode air pollution
lung$Air_Pollution_Exposure <- factor(lung$Air_Pollution_Exposure,
  levels = c("Low", "Moderate", "High"))

# Recode all Yes/No columns to 1/0
yes_no_vars <- c("Secondhand_Smoke", "Family_History", "Occupational_Hazard",
  "Chronic_Lung_Disease", "Asbestos_Exposure", "Radon_Exposure",
  "Previous_Cancer_History", "Metastasis", "Coughing",
  "Shortness_of_Breath", "Chest_Pain", "Coughing_Blood",
  "Fatigue", "Weight_Loss", "Wheezing", "Recurrent_Infections",
  "Swallowing_Difficulty", "Finger_Clubbing")

for (var in yes_no_vars) {
  lung[[var]] <- ifelse(lung[[var]] == "Yes", 1, 0)
}

# Check for missing values
missing_summary <- colSums(is.na(lung))
print(missing_summary[missing_summary > 0])

# Save cleaned data
write.csv(lung, "data/lung_cancer_cleaned.csv", row.names = FALSE)
cat("Done! Cleaned data saved.\n")
cat("Total records:", nrow(lung), "\n")
cat("Total columns:", ncol(lung), "\n")
