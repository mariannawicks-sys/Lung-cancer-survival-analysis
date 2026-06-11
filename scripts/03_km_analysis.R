# =============================================================================
# 03_km_analysis.R
# Lung Cancer Survival Analysis — Kaplan-Meier Curves
# Author: mariannawicks-sys
# =============================================================================

# --- Load Libraries ----------------------------------------------------------
library(survival)
library(survminer)

# --- Load Cleaned Data -------------------------------------------------------
lung <- read.csv("data/lung_cancer_cleaned.csv", stringsAsFactors = FALSE)

lung$Cancer_Stage <- factor(lung$Cancer_Stage,
                            levels = c("Stage I", "Stage II",
                                       "Stage III", "Stage IV"))
lung$Smoking_Status <- factor(lung$Smoking_Status,
                              levels = c("Never Smoked",
                                         "Former Smoker",
                                         "Current Smoker"))

# --- Overall KM Curve --------------------------------------------------------
km_overall <- survfit(Surv(Survival_Months, status) ~ 1, data = lung)

cat("=== Overall Survival Summary ===\n")
print(km_overall)
cat("Median survival:", summary(km_overall)$table["median"], "months\n")

p_overall <- ggsurvplot(km_overall,
                        data = lung,
                        conf.int = TRUE,
                        risk.table = TRUE,
                        title = "Overall Kaplan-Meier Survival Curve",
                        xlab = "Time (Months)",
                        ylab = "Survival Probability",
                        palette = "#2196F3",
                        ggtheme = theme_minimal())

ggsave("figures/km_survival_plot.png",
       print(p_overall), width = 10, height = 7, dpi = 300)
cat("Saved: figures/km_survival_plot.png\n")

# --- KM by Smoking Status ----------------------------------------------------
km_smoking <- survfit(Surv(Survival_Months, status) ~ Smoking_Status,
                      data = lung)

cat("\n=== Log-rank Test: Smoking Status ===\n")
print(survdiff(Surv(Survival_Months, status) ~ Smoking_Status, data = lung))

p_smoking <- ggsurvplot(km_smoking,
                        data = lung,
                        conf.int = FALSE,
                        risk.table = TRUE,
                        pval = TRUE,
                        title = "Survival by Smoking Status",
                        xlab = "Time (Months)",
                        ylab = "Survival Probability",
                        legend.title = "Smoking Status",
                        palette = c("#4caf7d", "#ff9800", "#f44336"),
                        ggtheme = theme_minimal())

ggsave("figures/km_smoking_survival_plot.png",
       print(p_smoking), width = 10, height = 7, dpi = 300)
cat("Saved: figures/km_smoking_survival_plot.png\n")

# --- KM by Cancer Stage ------------------------------------------------------
km_stage <- survfit(Surv(Survival_Months, status) ~ Cancer_Stage, data = lung)

cat("\n=== Log-rank Test: Cancer Stage ===\n")
print(survdiff(Surv(Survival_Months, status) ~ Cancer_Stage, data = lung))

p_stage <- ggsurvplot(km_stage,
                      data = lung,
                      conf.int = FALSE,
                      risk.table = TRUE,
                      pval = TRUE,
                      title = "Survival by Cancer Stage",
                      xlab = "Time (Months)",
                      ylab = "Survival Probability",
                      legend.title = "Stage",
                      palette = c("#4caf7d", "#2196F3", "#ff9800", "#f44336"),
                      ggtheme = theme_minimal())

ggsave("figures/survival_by_stage_km.png",
       print(p_stage), width = 10, height = 7, dpi = 300)
cat("Saved: figures/survival_by_stage_km.png\n")

cat("\nKaplan-Meier analysis complete.\n")
