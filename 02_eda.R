# =============================================================================
# 02_eda.R
# Lung Cancer Survival Analysis — Exploratory Data Analysis
# Author: mariannawicks-sys
# =============================================================================

# --- Load Libraries ----------------------------------------------------------
library(dplyr)
library(ggplot2)
library(survival)

# --- Load Cleaned Data -------------------------------------------------------
lung <- read.csv("data/lung_cancer_cleaned.csv", stringsAsFactors = FALSE)

# Re-apply factor levels after reading CSV
lung$Cancer_Stage <- factor(lung$Cancer_Stage,
                            levels = c("Stage I", "Stage II",
                                       "Stage III", "Stage IV"))
lung$Smoking_Status <- factor(lung$Smoking_Status,
                              levels = c("Never Smoked",
                                         "Former Smoker",
                                         "Current Smoker"))

# --- Summary Statistics ------------------------------------------------------
cat("=== Survival Status Distribution ===\n")
table(lung$Survived)
prop.table(table(lung$Survived))

cat("\n=== Cancer Stage Distribution ===\n")
table(lung$Cancer_Stage)

cat("\n=== Smoking Status Distribution ===\n")
table(lung$Smoking_Status)

cat("\n=== Age Summary ===\n")
summary(lung$Age)

# --- Plot 1: Age Distribution by Survival Status -----------------------------
p1 <- ggplot(lung, aes(x = Age, fill = Survived)) +
  geom_histogram(bins = 30, alpha = 0.7, position = "dodge") +
  labs(title = "Age Distribution by Survival Status",
       x = "Age", y = "Count", fill = "Survived") +
  theme_minimal()

ggsave("figures/age_by_survival.png", p1, width = 8, height = 5, dpi = 300)
cat("Saved: figures/age_by_survival.png\n")

# --- Plot 2: Survival by Cancer Stage ----------------------------------------
stage_surv <- lung %>%
  group_by(Cancer_Stage) %>%
  summarise(survival_rate = mean(status), .groups = "drop")

p2 <- ggplot(stage_surv, aes(x = Cancer_Stage, y = survival_rate,
                              fill = Cancer_Stage)) +
  geom_col(alpha = 0.8) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Survival Rate by Cancer Stage",
       x = "Stage", y = "Survival Rate") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("figures/survival_by_stage.png", p2, width = 8, height = 5, dpi = 300)
cat("Saved: figures/survival_by_stage.png\n")

# --- Plot 3: Survival by Treatment -------------------------------------------
treatment_surv <- lung %>%
  group_by(Treatment) %>%
  summarise(survival_rate = mean(status), n = n(), .groups = "drop") %>%
  arrange(desc(survival_rate))

p3 <- ggplot(treatment_surv, aes(x = reorder(Treatment, survival_rate),
                                  y = survival_rate, fill = survival_rate)) +
  geom_col(alpha = 0.8) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_gradient(low = "#f28c8c", high = "#4caf7d") +
  labs(title = "Survival Rate by Treatment Type",
       x = "Treatment", y = "Survival Rate") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("figures/survival_by_treatment.png", p3, width = 9, height = 6, dpi = 300)
cat("Saved: figures/survival_by_treatment.png\n")

cat("\nEDA complete. All figures saved to figures/\n")
