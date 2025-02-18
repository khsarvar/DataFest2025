# ---------------------------
# Setup and Data Preparation
# ---------------------------

# Install and load packages
install.packages(c("ranger", "survey", "caret", "pROC", "PRROC"))
library(ranger)
library(survey)
library(caret)
library(pROC)
library(PRROC)

# Load your dataset (replace with your data loading code)
# nhanes_df <- ...

# Extract survey weights from the NHANES survey design object
nhanes_df$survey_weights <- weights(nhanesComplete, "sampling")

# Create interaction term
nhanes_df$med_depression_interaction <-
 as.numeric(nhanes_df$bp_med_use == 1) * nhanes_df$depressionScore

# Handle missing data
nhanes_clean <- na.omit(nhanes_df)

# Convert outcome to factor with explicit levels
nhanes_clean$bp_uncontrolled_140_90 <- factor(
 nhanes_clean$bp_uncontrolled_140_90,
 levels = c("No", "Yes")
)

# Stratified train-test split (preserves class balance)
set.seed(123)
train_idx <- createDataPartition(
 nhanes_clean$bp_uncontrolled_140_90,
 p = 0.8,
 list = FALSE
)
train_data <- nhanes_clean[train_idx, ]
test_data <- nhanes_clean[-train_idx, ]

# ---------------------------
# Model 1: Logistic Regression (Survey-Weighted)
# ---------------------------

logit_model <- svyglm(
 bp_uncontrolled_140_90 ~ factor(demo_age_cat) + factor(demo_race) + depressionScore +
  factor(bp_med_use) + factor(cc_smoke) + factor(cc_cvd_any) + factor(htn_aware) +
  factor(bp_med_use)*depressionScore + factor(cc_bmi),
 family = quasibinomial,
 design = nhanesComplete,
 na.action = na.omit
)

summary(logit_model)
# ---------------------------
# Model 2: Random Forest (Survey-Weighted)
# ---------------------------

rf_model <- ranger(
 formula = bp_uncontrolled_140_90 ~ demo_age_cat + demo_race + depressionScore +
  bp_med_use + cc_smoke + cc_cvd_any + htn_aware + med_depression_interaction + cc_bmi,
 data = train_data,
 case.weights = train_data$survey_weights,  # Critical: Include survey weights
 importance = "permutation",
 probability = TRUE,
 seed = 123
)

# ---------------------------
# Model Evaluation
# ---------------------------

# Get predictions
logit_preds <- predict(logit_model, test_data, type = "response")
rf_preds <- predict(rf_model, test_data)$predictions[, "Yes"]

# Convert to classes with matching factor levels
logit_class <- factor(
 ifelse(logit_preds > 0.5, "Yes", "No"),
 levels = c("No", "Yes")
)

rf_class <- factor(
 ifelse(rf_preds > 0.5, "Yes", "No"),
 levels = c("No", "Yes")
)

# Ground truth (ensure factor levels match)
actual <- factor(
 test_data$bp_uncontrolled_140_90,
 levels = c("No", "Yes")
)

# Confusion matrices
logit_cm <- confusionMatrix(logit_class, actual)
rf_cm <- confusionMatrix(rf_class, actual)

print("Logistic Regression Results:")
print(logit_cm)

print("Random Forest Results:")
print(rf_cm)

# ROC-AUC Analysis
logit_roc <- roc(actual, logit_preds, levels = c("No", "Yes"))
rf_roc <- roc(actual, rf_preds, levels = c("No", "Yes"))

# Plot ROC curves
plot(logit_roc, col = "blue", main = "ROC Curve Comparison")
lines(rf_roc, col = "red")
legend("bottomright",
       legend = c(paste0("Logistic Regression (AUC = ", round(auc(logit_roc), 2)),
                  paste0("Random Forest (AUC = ", round(auc(rf_roc), 2))),
       col = c("blue", "red"), lwd = 2)

# Feature Importance Plot (Random Forest)
importance_df <- data.frame(
 Feature = names(rf_model$variable.importance),
 Importance = rf_model$variable.importance
) |>
 dplyr::arrange(-Importance)

ggplot(importance_df, aes(x = reorder(Feature, Importance), y = Importance)) +
 geom_bar(stat = "identity", fill = "steelblue") +
 coord_flip() +
 theme_minimal() +
 labs(title = "Random Forest Feature Importance", x = "", y = "Importance Score")

# Model Comparison Table
model_comparison <- data.frame(
 Model = c("Logistic Regression", "Random Forest"),
 Accuracy = c(logit_cm$overall["Accuracy"], rf_cm$overall["Accuracy"]),
 AUC = c(auc(logit_roc), auc(rf_roc)),
 Sensitivity = c(logit_cm$byClass["Sensitivity"], rf_cm$byClass["Sensitivity"]),
 Specificity = c(logit_cm$byClass["Specificity"], rf_cm$byClass["Specificity"])
)
install.packages("knitr")  # If not already installed
library(knitr)



# Print a clean summary table
kable(model_comparison, caption = "Model Performance Comparison")
install.packages("flextable")
library(flextable)

#GLM summary
# Install and load required packages
install.packages("survey")
install.packages("broom")
install.packages("flextable")
install.packages("officer")

library(survey)
library(broom)
library(flextable)
library(officer)

# Extract GLM results in a tidy format
glm_summary <- tidy(logit_model, conf.int = TRUE)

# Format the table correctly
glm_table <- glm_summary %>%
 select(term, estimate, std.error, conf.low, conf.high, p.value) %>%
 rename(
  "Variable" = term,
  "Estimate" = estimate,
  "Std. Error" = std.error,
  "Lower CI" = conf.low,
  "Upper CI" = conf.high,
  "P-value" = p.value  # Corrected column name
 ) %>%

 mutate(`P-value` = format.pval(`P-value`, digits = 3, eps = 0.001)) %>%  # Corrected column reference
 flextable() %>%
 set_caption("Survey-Weighted Logistic Regression Results") %>%
 theme_vanilla() %>%
 autofit() %>%
 align(j = 2:6, align = "center", part = "all")

# Print the table
glm_table

# Create a styled table
print(model_comparison)
summary(model_comparison)

#comparison Table in  slides PPT
flextable(model_comparison) %>%
 set_caption("Model Performance Comparison") %>%
 theme_vanilla()
#
#Table 2

# Load required libraries
library(survey)
library(broom)
library(dplyr)
library(flextable)

# Create a mapping of NHANES variable names to their corresponding labels
variable_labels <- c(
 "(Intercept)" = "Intercept",
 "factor(demo_age_cat)45 to 64" = "Age 45 to 64",
 "factor(demo_age_cat)65 to 74" = "Age 65 to 74",
 "factor(demo_age_cat)75+" = "Age 75+",
 "factor(demo_race)Non-Hispanic Black" = "Non-Hispanic Black",
 "factor(demo_race)Non-Hispanic Asian" = "Non-Hispanic Asian",
 "factor(demo_race)Hispanic" = "Hispanic",
 "factor(demo_race)Other" = "Other Race",
 "depressionScore" = "Depression Score",
 "factor(bp_med_use)Yes" = "Antihypertensive Medication Use",
 "factor(cc_smoke)Former" = "Former Smoker",
 "factor(cc_smoke)Current" = "Current Smoker",
 "factor(cc_cvd_any)Yes" = "History of Cardiovascular Disease",
 "factor(htn_aware)Yes" = "Hypertension Awareness",
 "factor(cc_bmi)25 to <30" = "BMI 25 to <30",
 "factor(cc_bmi)30 to <35" = "BMI 30 to <35",
 "factor(cc_bmi)35+" = "BMI 35+",
 "depressionScore:factor(bp_med_use)Yes" = "Depression Score x Medication Use"
)

# Extract GLM results in a tidy format
glm_summary <- tidy(logit_model, conf.int = TRUE)

# Apply correct labels to variables
glm_summary$term <- recode(glm_summary$term, !!!variable_labels)

# Format the table correctly
glm_table <- glm_summary %>%
 select(term, estimate, std.error, conf.low, conf.high, p.value) %>%
 rename(
  "Variable" = term,
  "Estimate" = estimate,
  "Std. Error" = std.error,
  "Lower CI" = conf.low,
  "Upper CI" = conf.high,
  "P-value" = p.value
 ) %>%
 mutate(`P-value` = format.pval(`P-value`, digits = 3, eps = 0.001)) %>%
 flextable() %>%
 set_caption("Table 2. Adjusted logistic regression model for assessing moderator effect on antihypertensive medication on depression score and uncontrolled hypertension using 2005-2020 NHANES Data") %>%
 theme_vanilla() %>%
 autofit() %>%
 align(j = 2:6, align = "center", part = "all")

# Print the formatted table
glm_table



#Graph 2
# Install Required Packages (if not already installed)
install.packages(c("survey", "broom", "dplyr", "ggplot2", "ggthemes"))

# Load Libraries
library(survey)
library(broom)
library(dplyr)
library(ggplot2)
library(ggthemes)

# Fit Survey-Weighted Logistic Regression Model
logit_model <- svyglm(
 bp_uncontrolled_140_90 ~ factor(demo_age_cat) + factor(demo_race) + depressionScore +
  factor(bp_med_use) + factor(cc_smoke) + factor(cc_cvd_any) + factor(htn_aware) +
  factor(bp_med_use) * depressionScore + factor(cc_bmi),
 family = quasibinomial,
 design = nhanesComplete,  # Uses survey weights
 na.action = na.omit
)

# Extract Wald Test Statistics as a Proxy for Importance
glm_importance <- tidy(logit_model) %>%
 select(term, estimate, std.error, statistic, p.value) %>%
 rename(
  Feature = term,
  Estimate = estimate,
  StdError = std.error,
  Importance = statistic  # Use Wald Test statistic as importance score
 ) %>%
 mutate(Model = "Survey-Weighted GLM (Wald Test)") %>%
 arrange(desc(abs(Importance)))



# Install Required Packages
install.packages(c("ranger", "DALEX", "shapper"))

# Load Libraries
library(ranger)
library(DALEX)
library(shapper)

# Fit Random Forest Model
rf_model <- ranger(
 formula = bp_uncontrolled_140_90 ~ demo_age_cat + demo_race + depressionScore +
  bp_med_use + cc_smoke + cc_cvd_any + htn_aware + med_depression_interaction + cc_bmi,
 data = train_data,
 case.weights = train_data$survey_weights,  # Use survey weights
 importance = "impurity",  # Gini Impurity for feature importance
 probability = TRUE,
 seed = 123,
 num.threads = parallel::detectCores()  # Use all CPU cores
)

# Extract Gini Importance
rf_importance <- data.frame(
 Feature = names(rf_model$variable.importance),
 Importance = rf_model$variable.importance
) %>%
 arrange(desc(Importance)) %>%
 mutate(Model = "Random Forest (Gini)")



# Combine Feature Importance from Both Models
importance_df <- bind_rows(glm_importance, rf_importance)

# # Plot Feature Importance
# ggplot(importance_df, aes(x = reorder(Feature, Importance), y = Importance, fill = Model)) +
#  geom_bar(stat = "identity", position = "dodge") +
#  coord_flip() +
#  theme_minimal(base_size = 8) +
#  theme(
#   panel.background = element_rect(fill = "white"),
#   panel.grid.major = element_line(color = "gray90"),
#   text = element_text(family = "Arial"),
#   legend.position = "top"
#  ) +
#  scale_fill_manual(values = c("Survey-Weighted GLM (Wald Test)" = "blue",
#                               "Random Forest (Gini)" = "red")) +
#  labs(
#   title = "Feature Importance: Survey-Weighted GLM vs. Random Forest",
#   subtitle = "GLM Importance (Wald Test Statistic) vs. RF (Gini Impurity)",
#   x = "",
#   y = "Importance Score",
#   fill = "Model"
#  ) +
#  theme_economist()

library(stringr)

library(ggplot2)
library(ggthemes)
library(dplyr)
library(stringr)

# Adjust variable names (mapping from raw to readable labels)

variable_labels <- c(
 "(Intercept)" = "Intercept",
 "factor(demo_age_cat)45 to 64" = "Age 45 to 64",
 "factor(demo_age_cat)65 to 74" = "Age 65 to 74",
 "factor(demo_age_cat)75+" = "Age 75+",
 "factor(demo_race)Non-Hispanic Black" = "Non-Hispanic Black",
 "factor(demo_race)Non-Hispanic Asian" = "Non-Hispanic Asian",
 "factor(demo_race)Hispanic" = "Hispanic",
 "factor(demo_race)Other" = "Other Race",
 "depressionScore" = "Depression Score",
 "factor(bp_med_use)Yes" = "Antihypertensive Medication Use",
 "factor(cc_smoke)Former" = "Former Smoker",
 "factor(cc_smoke)Current" = "Current Smoker",
 "factor(cc_cvd_any)Yes" = "History of Cardiovascular Disease",
 "factor(htn_aware)Yes" = "Hypertension Awareness",
 "factor(cc_bmi)25 to <30" = "BMI 25 to <30",
 "factor(cc_bmi)30 to <35" = "BMI 30 to <35",
 "factor(cc_bmi)35+" = "BMI 35+",
 "depressionScore:factor(bp_med_use)Yes" = "Depression Score x Medication Use"
)
# Apply new variable labels
importance_df$Feature <- factor(importance_df$Feature,
                                levels = names(variable_labels),
                                labels = variable_labels)

# Ensure NAs are not removed (they will appear as "NA" in the graph)
importance_df$Feature <- forcats::fct_explicit_na(importance_df$Feature, na_level = "Missing Feature")




# Fix text cutoff, adjust aspect ratio
plot <- ggplot(importance_df, aes(x = reorder(Feature, Importance), y = Importance, fill = Model)) +
 geom_bar(stat = "identity", position = "dodge", width = 0.6) +  # Reduce bar width
 coord_flip() +  # Flip for better readability

 # Adjust label placement and ensure full visibility
 geom_text(aes(label = round(Importance, 2)),
           hjust = -0.2, size = 4, fontface = "bold") +

 # Ensure labels do not get cut off
 scale_x_discrete(labels = function(x) str_wrap(x, width = 30)) +

 # Academic formatting
 theme_minimal(base_size = 10) +  # Set font size to be readable
 theme(
  panel.background = element_rect(fill = "white"),
  panel.grid.major = element_line(color = "gray90"),
  text = element_text(family = "Arial"),
  legend.position = "top",
  plot.title = element_text(size = 14, face = "bold", hjust = 0.5),  # Centered title
  plot.subtitle = element_text(size = 12, hjust = 0.5),
  plot.caption = element_text(size = 10, hjust = 0.5, face = "italic"),  # Center caption
  axis.text.y = element_text(size = 9, hjust = 1),  # Ensure y-axis labels are fully visible
  axis.text.x = element_text(size = 9),
  axis.title = element_text(size = 10, face = "bold"),
  aspect.ratio = 1.5  # Prevent the graph from being too tall
 ) +

 # Custom Colors
 scale_fill_manual(values = c("Survey-Weighted GLM (Wald Test)" = "blue",
                              "Random Forest (Gini)" = "red")) +

 # Labels
 labs(
  title = "Comparison of Feature Importance Between Survey-Weighted GLM and Random Forest",
  subtitle = "GLM Importance (Wald Test Statistic) vs. RF (Gini Impurity)",
  x = "",
  y = "Importance Score",
  fill = "Model Type",
  caption = "Figure 1. Feature Importance Comparison: GLM uses Wald Test statistics, while Random Forest uses Gini Impurity scores.\n
               Numeric labels indicate actual importance values for each feature."
 ) +

 theme_economist()  # Apply a clean academic theme

# Display the plot
print(plot)
