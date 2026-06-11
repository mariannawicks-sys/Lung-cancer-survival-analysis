# =============================================================================
# 04_cox_model.R
# Lung Cancer Survival Analysis — Cox PH Model & Diagnostics
# Author: mariannawicks-sys
# =============================================================================

# --- Load Libraries ----------------------------------------------------------
library(survival)
library(survminer)
library(dplyr)

# --- Load Cleaned Data -------------------------------------------------------
lung <- read.csv("data/lung_cancer_cleaned.csv", stringsAsFactors = FALSE)

lung$Cancer_Stage <- factor(lung$Cancer_Stage,
                            levels = c("Stage I", "Stage II",
                                       "Stage III", "Stage IV"))
lung$Smoking_Status <- factor(lung$Smoking_Status,
                              levels = c("Never Smoked",
                                         "Former Smoker",
                                         "Current Smoker"))

# =============================================================================
# SECTION 1: Fit Cox Model
# =============================================================================
cox_model <- coxph(
  Surv(Survival_Months, status) ~
    Age + Gender + Smoking_Status + Cancer_Stage +
    Metastasis + Tumor_Size_cm + Family_History +
    Chronic_Lung_Disease + Air_Pollution_Exposure,
  data = lung
)

cat("=== Cox Model Summary ===\n")
summary(cox_model)

# Concordance (C-statistic)
cat("\nConcordance (C-statistic):",
    summary(cox_model)$concordance["C"], "\n")

# =============================================================================
# SECTION 2: Test Proportional Hazards Assumption
# =============================================================================
cat("\n=== Proportional Hazards Assumption (Schoenfeld Residuals) ===\n")
ph_test <- cox.zph(cox_model, transform = "km")
print(ph_test)

# Plot Schoenfeld residuals
png("figures/adjusted_survival_curves.png", width = 1000, height = 700)
plot(ph_test)
dev.off()
cat("Saved: figures/adjusted_survival_curves.png\n")

# =============================================================================
# SECTION 3: Goodness-of-Fit (Gronnesby-Borgan style)
# =============================================================================
cat("\n=== Goodness-of-Fit: Decile Risk Group LRT ===\n")

lung$lin_pred <- predict(cox_model, type = "lp")
lung$risk_group <- cut(
  lung$lin_pred,
  breaks = quantile(lung$lin_pred, probs = seq(0, 1, by = 0.1), na.rm = TRUE),
  include.lowest = TRUE,
  labels = FALSE
)
lung$risk_group <- factor(lung$risk_group)

cox_full <- coxph(
  Surv(Survival_Months, status) ~
    Age + Gender + Smoking_Status + Cancer_Stage +
    Metastasis + Tumor_Size_cm + Family_History +
    Chronic_Lung_Disease + Air_Pollution_Exposure + risk_group,
  data = lung
)

lrt_result <- anova(cox_model, cox_full, test = "LRT")
print(lrt_result)

# =============================================================================
# SECTION 4: Outlier Detection
# =============================================================================
cat("\n=== Outlier Detection ===\n")

mart_resid <- resid(cox_model, type = "martingale")
dev_resid  <- resid(cox_model, type = "deviance")

mart_outliers    <- which(abs(mart_resid) > 2)
dev_outliers     <- which(abs(dev_resid)  > 2)
combined_outliers <- intersect(mart_outliers, dev_outliers)

cat("Observations flagged by BOTH martingale & deviance residuals:\n")
print(combined_outliers)

lung$predicted_risk <- predict(cox_model, type = "risk")

if (length(combined_outliers) > 0) {
  cat("\nOutlier details:\n")
  print(lung[combined_outliers,
             c("Age", "Cancer_Stage", "Smoking_Status",
               "Survival_Months", "status", "predicted_risk")])
}

# =============================================================================
# SECTION 5: Influential Observations
# =============================================================================
cat("\n=== Influential Observations ===\n")

dfbetas_values  <- residuals(cox_model, type = "dfbeta")
score_resid     <- residuals(cox_model, type = "score")
std_score_resid <- apply(score_resid, 2, function(x) x / sd(x))
dffits_scores   <- apply(std_score_resid, 1, max)
cooks_d         <- dffits_scores^2 /
                   (ncol(dfbetas_values) * mean(dffits_scores^2))

influence_measures <- list(
  DFBETAS    = apply(abs(dfbetas_values), 1, max),
  Score_Resid = apply(abs(score_resid),  1, max),
  Cooks_D    = cooks_d,
  DFFITS     = dffits_scores
)

top_1_percent <- lapply(influence_measures, function(x) {
  threshold <- quantile(x, 0.99)
  which(x > threshold)
})

obs_counts      <- table(unlist(top_1_percent))
highly_infl_obs <- as.numeric(names(obs_counts[obs_counts >= 2]))
moderate_infl_obs <- as.numeric(names(obs_counts[obs_counts == 1]))

cat("Highly influential observations (flagged by 2+ measures):\n")
print(lung[highly_infl_obs,
           c("Age", "Cancer_Stage", "Smoking_Status",
             "Survival_Months", "status", "predicted_risk")])

# =============================================================================
# SECTION 6: Forest Plot
# =============================================================================
p_forest <- ggforest(cox_model, data = lung,
                     main = "Cox Model Hazard Ratios — Lung Cancer Survival")

ggsave("figures/cox_forest_plot.png", p_forest,
       width = 10, height = 8, dpi = 300)
cat("Saved: figures/cox_forest_plot.png\n")

# =============================================================================
# SECTION 7: Save Results
# =============================================================================
dir.create("results", showWarnings = FALSE)

# Save model summary
sink("results/cox_model_summary.txt")
summary(cox_model)
cat("\n\nProportional Hazards Test:\n")
print(ph_test)
cat("\n\nLRT Goodness-of-Fit:\n")
print(lrt_result)
sink()

cat("\nResults saved to results/cox_model_summary.txt\n")
cat("Cox model analysis complete.\n")
