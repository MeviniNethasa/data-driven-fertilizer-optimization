# ==============================================================================
# IT3081 Statistical Modelling
# Task 4: Statistical Inference Evidence
# ==============================================================================
#
# Save as:
# scripts/04_statistical_inference.R
#
# Project root:
# ~/Documents/Fertilizer_Consultancy_Project/
#
# Purpose:
#   Conduct formal statistical inference for four business hypotheses:
#   H1 - One-Way ANOVA: Soil pH across Agro-Ecological Zones
#   H2 - One-Way ANOVA: Available Phosphorus across Agro-Ecological Zones
#   H3 - Welch Two-Sample t-test: Total Nitrogen across Maha vs Yala
#   H4 - Levene's Test: Homogeneity of Soil pH variance across Agro-Zones
#
# Outputs:
#   output/figures/04_anova_soil_ph.png
#   output/figures/04_anova_available_phosphorus.png
#   output/figures/04_ttest_nitrogen_season.png
#   output/figures/04_levene_soil_ph.png
#
#   output/reports/04_hypothesis_tests_summary.csv
#   output/reports/04_anova_group_confidence_intervals.csv
#   output/reports/04_ttest_confidence_interval.csv
#   output/reports/04_statistical_inference_report.txt
#
# Statistical significance level:
#   alpha = 0.05
#
# Important:
#   Statistical conclusions are based on the supplied dataset and the
#   specified hypotheses. Statistical significance does not by itself
#   establish practical or agricultural importance.
# ==============================================================================


# ------------------------------------------------------------------------------
# 1. DIRECTORY SETUP
# ------------------------------------------------------------------------------

if(!dir.exists("output/figures")) dir.create("output/figures", recursive = TRUE)
if(!dir.exists("output/reports")) dir.create("output/reports", recursive = TRUE)


# ------------------------------------------------------------------------------
# 2. REQUIRED PACKAGES
# ------------------------------------------------------------------------------

required_packages <- c(
  "tidyverse",
  "car",
  "carData",
  "Formula"
)

missing_packages <- required_packages[
  !required_packages %in% rownames(installed.packages())
]

if(length(missing_packages) > 0){
  
  stop(
    paste0(
      "Missing required package(s): ",
      paste(missing_packages, collapse = ", "),
      "\n\nPlease install them using:\n\n",
      "install.packages(c(",
      paste0(
        "\"",
        missing_packages,
        "\"",
        collapse = ", "
      ),
      "))"
    )
  )
  
}

library(tidyverse)
library(car)



# ------------------------------------------------------------------------------
# 3. PROJECT SETTINGS
# ------------------------------------------------------------------------------

alpha <- 0.05

figure_dir <- "output/figures"
report_dir <- "output/reports"

dataset_path <- "Fertilizer_Dataset.csv"


# ------------------------------------------------------------------------------
# 4. LOAD DATA
# ------------------------------------------------------------------------------

fertilizer_data <- read.csv(
  dataset_path,
  stringsAsFactors = FALSE
)

cat(
  "\n============================================================\n",
  "TASK 4: STATISTICAL INFERENCE EVIDENCE\n",
  "============================================================\n",
  "\nDataset loaded successfully.\n",
  "Rows: ", nrow(fertilizer_data),
  "\nColumns: ", ncol(fertilizer_data),
  "\n",
  sep = ""
)


# ------------------------------------------------------------------------------
# 5. DATA STRUCTURE AND REQUIRED VARIABLE CHECK
# ------------------------------------------------------------------------------

required_variables <- c(
  "District",
  "Agro_Zone",
  "Season",
  "Soil_pH",
  "Available_P_mgkg",
  "Total_N_percent"
)

missing_required_variables <- setdiff(
  required_variables,
  names(fertilizer_data)
)

if(length(missing_required_variables) > 0){

  stop(
    paste0(
      "The dataset is missing the following required variable(s): ",
      paste(missing_required_variables, collapse = ", ")
    )
  )

}


# ------------------------------------------------------------------------------
# 6. DATA CLEANING PRE-CHECK
# ------------------------------------------------------------------------------

# 6.1 Count exact duplicate rows before removing them.

duplicate_rows <- sum(
  duplicated(fertilizer_data)
)

cat(
  "\nDuplicate rows detected: ",
  duplicate_rows,
  "\n",
  sep = ""
)


# 6.2 Remove exact duplicate rows.

fertilizer_data_clean <- fertilizer_data %>%
  distinct()


# 6.3 Record the number of rows removed.

rows_removed_duplicates <- nrow(fertilizer_data) -
  nrow(fertilizer_data_clean)

cat(
  "Rows removed as exact duplicates: ",
  rows_removed_duplicates,
  "\n",
  sep = ""
)


# ------------------------------------------------------------------------------
# 7. VARIABLE TYPE PREPARATION
# ------------------------------------------------------------------------------

# Convert grouping variables to factors.
# This ensures that ANOVA, t-test and Levene's test treat them correctly.

fertilizer_data_clean <- fertilizer_data_clean %>%
  mutate(
    Agro_Zone = as.factor(Agro_Zone),
    Season = as.factor(Season)
  )


# Display observed factor levels.

cat(
  "\nAgro-Ecological Zone levels:\n",
  paste(
    levels(fertilizer_data_clean$Agro_Zone),
    collapse = ", "
  ),
  "\n",
  sep = ""
)

cat(
  "\nSeason levels:\n",
  paste(
    levels(fertilizer_data_clean$Season),
    collapse = ", "
  ),
  "\n",
  sep = ""
)


# ------------------------------------------------------------------------------
# 8. DATASETS FOR EACH HYPOTHESIS
# ------------------------------------------------------------------------------

# Each hypothesis receives its own complete-case dataset.
#
# This is important because statistical tests calculate degrees of freedom
# from the observations actually available for the variables involved.
#
# We therefore remove missing values only for the variables required by
# each individual test rather than deleting observations unnecessarily
# from the entire dataset.


# H1: Soil pH ~ Agro-Zone

h1_data <- fertilizer_data_clean %>%
  select(
    Soil_pH,
    Agro_Zone
  ) %>%
  drop_na()


# H2: Available P ~ Agro-Zone

h2_data <- fertilizer_data_clean %>%
  select(
    Available_P_mgkg,
    Agro_Zone
  ) %>%
  drop_na()


# H3: Total N ~ Season

h3_data <- fertilizer_data_clean %>%
  select(
    Total_N_percent,
    Season
  ) %>%
  drop_na()


# H4: Soil pH ~ Agro-Zone

h4_data <- fertilizer_data_clean %>%
  select(
    Soil_pH,
    Agro_Zone
  ) %>%
  drop_na()


# ------------------------------------------------------------------------------
# 9. CHECK GROUP COUNTS
# ------------------------------------------------------------------------------

h1_group_counts <- h1_data %>%
  count(Agro_Zone, name = "n")

h2_group_counts <- h2_data %>%
  count(Agro_Zone, name = "n")

h3_group_counts <- h3_data %>%
  count(Season, name = "n")

h4_group_counts <- h4_data %>%
  count(Agro_Zone, name = "n")


cat(
  "\n============================================================\n",
  "HYPOTHESIS DATASET SIZES\n",
  "============================================================\n",
  "\nH1 observations: ", nrow(h1_data),
  "\nH2 observations: ", nrow(h2_data),
  "\nH3 observations: ", nrow(h3_data),
  "\nH4 observations: ", nrow(h4_data),
  "\n",
  sep = ""
)

cat(
  "\nH1/H4 Agro-Zone group counts:\n"
)

print(h1_group_counts)

cat(
  "\nH3 Season group counts:\n"
)

print(h3_group_counts)


# ------------------------------------------------------------------------------
# 10. HYPOTHESIS DEFINITIONS
# ------------------------------------------------------------------------------

hypotheses <- tibble(
  hypothesis_id = c("H1", "H2", "H3", "H4"),

  method = c(
    "One-Way ANOVA",
    "One-Way ANOVA",
    "Welch Two-Sample t-test",
    "Levene's Test"
  ),

  null_hypothesis = c(
    "Soil pH does not differ significantly across Agro-Ecological Zones.",
    "Available Phosphorus does not differ significantly across Agro-Ecological Zones.",
    "Total Nitrogen levels are equal across Maha and Yala cultivation seasons.",
    "Variances of Soil pH are equal across Agro-Ecological Zones."
  ),

  alternative_hypothesis = c(
    "At least one Agro-Ecological Zone has a different mean Soil pH.",
    "At least one Agro-Ecological Zone has a different mean Available Phosphorus.",
    "Mean Total Nitrogen differs between Maha and Yala cultivation seasons.",
    "At least one Agro-Ecological Zone has a different Soil pH variance."
  )
)

write_csv(
  hypotheses,
  file.path(
    report_dir,
    "04_hypothesis_definitions.csv"
  )
)


# ==============================================================================
# HYPOTHESIS 1
# ONE-WAY ANOVA: SOIL pH ACROSS AGRO-ECOLOGICAL ZONES
# ==============================================================================

cat(
  "\n\n============================================================\n",
  "HYPOTHESIS 1: ONE-WAY ANOVA - SOIL pH\n",
  "============================================================\n"
)


# ------------------------------------------------------------------------------
# 11. H1 DESCRIPTIVE GROUP SUMMARY
# ------------------------------------------------------------------------------

h1_group_summary <- h1_data %>%
  group_by(Agro_Zone) %>%
  summarise(
    n = n(),
    mean = mean(Soil_pH),
    sd = sd(Soil_pH),
    se = sd / sqrt(n),
    ci_lower = mean - qt(
      1 - alpha / 2,
      df = n - 1
    ) * se,
    ci_upper = mean + qt(
      1 - alpha / 2,
      df = n - 1
    ) * se,
    median = median(Soil_pH),
    min = min(Soil_pH),
    max = max(Soil_pH),
    .groups = "drop"
  )

print(h1_group_summary)


# ------------------------------------------------------------------------------
# 12. H1 ANOVA MODEL
# ------------------------------------------------------------------------------

h1_anova_model <- aov(
  Soil_pH ~ Agro_Zone,
  data = h1_data
)

h1_anova_table <- summary(
  h1_anova_model
)[[1]]

print(h1_anova_table)


# Extract ANOVA statistics.

h1_f_statistic <- unname(
  h1_anova_table["Agro_Zone", "F value"]
)

h1_df_between <- unname(
  h1_anova_table["Agro_Zone", "Df"]
)

h1_df_within <- unname(
  h1_anova_table["Residuals", "Df"]
)

h1_p_value <- unname(
  h1_anova_table["Agro_Zone", "Pr(>F)"]
)


# ------------------------------------------------------------------------------
# 13. H1 EFFECT SIZE: ETA-SQUARED
# ------------------------------------------------------------------------------

h1_ss_between <- unname(
  h1_anova_table["Agro_Zone", "Sum Sq"]
)

h1_ss_total <- sum(
  h1_anova_table[, "Sum Sq"],
  na.rm = TRUE
)

h1_eta_squared <- h1_ss_between / h1_ss_total


# Interpretation note:
#
# Eta-squared represents the proportion of total variation in Soil pH
# explained by Agro-Ecological Zone in this one-way ANOVA.
#
# It is a descriptive effect-size measure. A large effect size does not
# automatically establish causality.


# ------------------------------------------------------------------------------
# 14. H1 95% CONFIDENCE INTERVALS FOR GROUP MEANS
# ------------------------------------------------------------------------------

write_csv(
  h1_group_summary,
  file.path(
    report_dir,
    "04_h1_soil_ph_group_confidence_intervals.csv"
  )
)


# ------------------------------------------------------------------------------
# 15. H1 ANOVA BOXplot
# ------------------------------------------------------------------------------

h1_p_label <- ifelse(
  h1_p_value < 0.001,
  "ANOVA p < 0.001",
  paste0(
    "ANOVA p = ",
    format.pval(
      h1_p_value,
      digits = 3,
      eps = 0.001
    )
  )
)

h1_plot_y <- max(
  h1_data$Soil_pH,
  na.rm = TRUE
) +
  0.08 * diff(
    range(
      h1_data$Soil_pH,
      na.rm = TRUE
    )
  )

p_h1 <- ggplot(
  h1_data,
  aes(
    x = Agro_Zone,
    y = Soil_pH
  )
) +
  geom_boxplot(
    width = 0.65,
    outlier.alpha = 0.35
  ) +
  stat_summary(
    fun = mean,
    geom = "point",
    shape = 18,
    size = 3
  ) +
  annotate(
    "text",
    x = Inf,
    y = h1_plot_y,
    label = h1_p_label,
    hjust = 1.05,
    vjust = 1,
    size = 4
  ) +
  labs(
    title = "Soil pH Across Agro-Ecological Zones",
    subtitle = paste0(
      "One-Way ANOVA | F(",
      h1_df_between,
      ", ",
      h1_df_within,
      ") = ",
      round(h1_f_statistic, 3),
      " | Eta-squared = ",
      round(h1_eta_squared, 4)
    ),
    x = "Agro-Ecological Zone",
    y = "Soil pH"
  ) +
  theme_minimal(
    base_size = 12
  )

ggsave(
  filename = file.path(
    figure_dir,
    "04_anova_soil_ph.png"
  ),
  plot = p_h1,
  width = 10,
  height = 7,
  dpi = 300
)


# ==============================================================================
# HYPOTHESIS 2
# ONE-WAY ANOVA: AVAILABLE PHOSPHORUS ACROSS AGRO-ECOLOGICAL ZONES
# ==============================================================================

cat(
  "\n\n============================================================\n",
  "HYPOTHESIS 2: ONE-WAY ANOVA - AVAILABLE PHOSPHORUS\n",
  "============================================================\n"
)


# ------------------------------------------------------------------------------
# 16. H2 DESCRIPTIVE GROUP SUMMARY
# ------------------------------------------------------------------------------

h2_group_summary <- h2_data %>%
  group_by(Agro_Zone) %>%
  summarise(
    n = n(),
    mean = mean(Available_P_mgkg),
    sd = sd(Available_P_mgkg),
    se = sd / sqrt(n),
    ci_lower = mean - qt(
      1 - alpha / 2,
      df = n - 1
    ) * se,
    ci_upper = mean + qt(
      1 - alpha / 2,
      df = n - 1
    ) * se,
    median = median(Available_P_mgkg),
    min = min(Available_P_mgkg),
    max = max(Available_P_mgkg),
    .groups = "drop"
  )

print(h2_group_summary)


# ------------------------------------------------------------------------------
# 17. H2 ANOVA MODEL
# ------------------------------------------------------------------------------

h2_anova_model <- aov(
  Available_P_mgkg ~ Agro_Zone,
  data = h2_data
)

h2_anova_table <- summary(
  h2_anova_model
)[[1]]

print(h2_anova_table)


# Extract statistics.

h2_f_statistic <- unname(
  h2_anova_table["Agro_Zone", "F value"]
)

h2_df_between <- unname(
  h2_anova_table["Agro_Zone", "Df"]
)

h2_df_within <- unname(
  h2_anova_table["Residuals", "Df"]
)

h2_p_value <- unname(
  h2_anova_table["Agro_Zone", "Pr(>F)"]
)


# ------------------------------------------------------------------------------
# 18. H2 EFFECT SIZE: ETA-SQUARED
# ------------------------------------------------------------------------------

h2_ss_between <- unname(
  h2_anova_table["Agro_Zone", "Sum Sq"]
)

h2_ss_total <- sum(
  h2_anova_table[, "Sum Sq"],
  na.rm = TRUE
)

h2_eta_squared <- h2_ss_between / h2_ss_total


# ------------------------------------------------------------------------------
# 19. H2 95% CONFIDENCE INTERVALS
# ------------------------------------------------------------------------------

write_csv(
  h2_group_summary,
  file.path(
    report_dir,
    "04_h2_available_phosphorus_group_confidence_intervals.csv"
  )
)


# ------------------------------------------------------------------------------
# 20. H2 ANOVA BOXPLOT
# ------------------------------------------------------------------------------

h2_p_label <- ifelse(
  h2_p_value < 0.001,
  "ANOVA p < 0.001",
  paste0(
    "ANOVA p = ",
    format.pval(
      h2_p_value,
      digits = 3,
      eps = 0.001
    )
  )
)

h2_plot_y <- max(
  h2_data$Available_P_mgkg,
  na.rm = TRUE
) +
  0.08 * diff(
    range(
      h2_data$Available_P_mgkg,
      na.rm = TRUE
    )
  )

p_h2 <- ggplot(
  h2_data,
  aes(
    x = Agro_Zone,
    y = Available_P_mgkg
  )
) +
  geom_boxplot(
    width = 0.65,
    outlier.alpha = 0.35
  ) +
  stat_summary(
    fun = mean,
    geom = "point",
    shape = 18,
    size = 3
  ) +
  annotate(
    "text",
    x = Inf,
    y = h2_plot_y,
    label = h2_p_label,
    hjust = 1.05,
    vjust = 1,
    size = 4
  ) +
  labs(
    title = "Available Phosphorus Across Agro-Ecological Zones",
    subtitle = paste0(
      "One-Way ANOVA | F(",
      h2_df_between,
      ", ",
      h2_df_within,
      ") = ",
      round(h2_f_statistic, 3),
      " | Eta-squared = ",
      round(h2_eta_squared, 4)
    ),
    x = "Agro-Ecological Zone",
    y = "Available P (mg/kg)"
  ) +
  theme_minimal(
    base_size = 12
  )

ggsave(
  filename = file.path(
    figure_dir,
    "04_anova_available_phosphorus.png"
  ),
  plot = p_h2,
  width = 10,
  height = 7,
  dpi = 300
)


# ==============================================================================
# HYPOTHESIS 3
# WELCH TWO-SAMPLE T-TEST: TOTAL NITROGEN BY SEASON
# ==============================================================================

cat(
  "\n\n============================================================\n",
  "HYPOTHESIS 3: WELCH TWO-SAMPLE T-TEST - TOTAL NITROGEN\n",
  "============================================================\n"
)


# ------------------------------------------------------------------------------
# 21. STANDARDISE SEASON LABELS FOR TESTING
# ------------------------------------------------------------------------------

# The project hypothesis specifically compares Maha and Yala.
#
# Matching is performed case-insensitively and with surrounding whitespace
# removed so that minor text-formatting differences do not create artificial
# group mismatches.

h3_data <- h3_data %>%
  mutate(
    Season_clean = str_to_lower(
      str_trim(
        as.character(Season)
      )
    )
  ) %>%
  filter(
    Season_clean %in% c(
      "maha",
      "yala"
    )
  ) %>%
  mutate(
    Season = factor(
      str_to_title(Season_clean),
      levels = c(
        "Maha",
        "Yala"
      )
    )
  )


# Check that both seasons are available.

h3_season_levels <- levels(
  droplevels(h3_data$Season)
)

if(!all(c("Maha", "Yala") %in% h3_season_levels)){

  stop(
    paste0(
      "H3 requires both Maha and Yala observations. ",
      "Available groups after cleaning: ",
      paste(h3_season_levels, collapse = ", ")
    )
  )

}


# ------------------------------------------------------------------------------
# 22. H3 DESCRIPTIVE GROUP SUMMARY
# ------------------------------------------------------------------------------

h3_group_summary <- h3_data %>%
  group_by(Season) %>%
  summarise(
    n = n(),
    mean = mean(Total_N_percent),
    sd = sd(Total_N_percent),
    se = sd / sqrt(n),
    ci_lower = mean - qt(
      1 - alpha / 2,
      df = n - 1
    ) * se,
    ci_upper = mean + qt(
      1 - alpha / 2,
      df = n - 1
    ) * se,
    median = median(Total_N_percent),
    min = min(Total_N_percent),
    max = max(Total_N_percent),
    .groups = "drop"
  )

print(h3_group_summary)


# ------------------------------------------------------------------------------
# 23. H3 WELCH TWO-SAMPLE T-TEST
# ------------------------------------------------------------------------------

h3_ttest <- t.test(
  Total_N_percent ~ Season,
  data = h3_data,
  var.equal = FALSE,
  conf.level = 0.95
)

print(h3_ttest)


# Extract statistics.

h3_t_statistic <- unname(
  h3_ttest$statistic
)

h3_df <- unname(
  h3_ttest$parameter
)

h3_p_value <- h3_ttest$p.value

h3_ci_lower <- h3_ttest$conf.int[1]

h3_ci_upper <- h3_ttest$conf.int[2]

h3_mean_difference <- unname(
  diff(
    h3_group_summary$mean
  )
)


# ------------------------------------------------------------------------------
# 24. H3 COHEN'S d
# ------------------------------------------------------------------------------

maha_values <- h3_data %>%
  filter(Season == "Maha") %>%
  pull(Total_N_percent)

yala_values <- h3_data %>%
  filter(Season == "Yala") %>%
  pull(Total_N_percent)

n_maha <- length(maha_values)
n_yala <- length(yala_values)

sd_maha <- sd(maha_values)
sd_yala <- sd(yala_values)

pooled_sd <- sqrt(
  (
    (n_maha - 1) * sd_maha^2 +
      (n_yala - 1) * sd_yala^2
  ) /
    (
      n_maha + n_yala - 2
    )
)

h3_cohens_d <- (
  mean(maha_values) -
    mean(yala_values)
) / pooled_sd


# Cohen's d expresses the difference between the two seasonal means
# relative to the pooled within-group standard deviation.
#
# The sign depends on the order of the groups. Here:
#   d = Maha mean - Yala mean
#
# Therefore, the sign indicates the direction of the difference,
# while the absolute magnitude indicates standardized separation.


# ------------------------------------------------------------------------------
# 25. H3 T-TEST CONFIDENCE INTERVAL REPORT
# ------------------------------------------------------------------------------

h3_ci_report <- tibble(
  comparison = "Maha - Yala",
  maha_mean = mean(maha_values),
  yala_mean = mean(yala_values),
  mean_difference = h3_mean_difference,
  ci_lower = h3_ci_lower,
  ci_upper = h3_ci_upper,
  confidence_level = 0.95,
  t_statistic = h3_t_statistic,
  degrees_of_freedom = h3_df,
  p_value = h3_p_value,
  cohens_d = h3_cohens_d
)

write_csv(
  h3_ci_report,
  file.path(
    report_dir,
    "04_ttest_confidence_interval.csv"
  )
)


# ------------------------------------------------------------------------------
# 26. H3 BOXPLOT
# ------------------------------------------------------------------------------

h3_p_label <- ifelse(
  h3_p_value < 0.001,
  "Welch t-test p < 0.001",
  paste0(
    "Welch t-test p = ",
    format.pval(
      h3_p_value,
      digits = 3,
      eps = 0.001
    )
  )
)

h3_plot_y <- max(
  h3_data$Total_N_percent,
  na.rm = TRUE
) +
  0.08 * diff(
    range(
      h3_data$Total_N_percent,
      na.rm = TRUE
    )
  )

p_h3 <- ggplot(
  h3_data,
  aes(
    x = Season,
    y = Total_N_percent
  )
) +
  geom_boxplot(
    width = 0.55,
    outlier.alpha = 0.35
  ) +
  stat_summary(
    fun = mean,
    geom = "point",
    shape = 18,
    size = 3
  ) +
  annotate(
    "text",
    x = 1.5,
    y = h3_plot_y,
    label = h3_p_label,
    hjust = 0.5,
    vjust = 1,
    size = 4
  ) +
  labs(
    title = "Total Nitrogen Across Maha and Yala Seasons",
    subtitle = paste0(
      "Welch Two-Sample t-test | t = ",
      round(h3_t_statistic, 3),
      " | df = ",
      round(h3_df, 2),
      " | Cohen's d = ",
      round(h3_cohens_d, 4)
    ),
    x = "Cultivation Season",
    y = "Total Nitrogen (%)"
  ) +
  theme_minimal(
    base_size = 12
  )

ggsave(
  filename = file.path(
    figure_dir,
    "04_ttest_nitrogen_season.png"
  ),
  plot = p_h3,
  width = 10,
  height = 7,
  dpi = 300
)


# ==============================================================================
# HYPOTHESIS 4
# LEVENE'S TEST: HOMOGENEITY OF SOIL pH VARIANCE
# ==============================================================================

cat(
  "\n\n============================================================\n",
  "HYPOTHESIS 4: LEVENE'S TEST - SOIL pH VARIANCES\n",
  "============================================================\n"
)


# ------------------------------------------------------------------------------
# 27. LEVENE'S TEST
# ------------------------------------------------------------------------------

# car::leveneTest() performs Levene's test.
#
# center = median specifies the median-centered version of Levene's test,
# which is generally more robust to departures from normality than the
# mean-centered version.

h4_levene <- car::leveneTest(
  Soil_pH ~ Agro_Zone,
  data = h4_data,
  center = median
)

print(h4_levene)


# Extract test statistics.

h4_f_statistic <- unname(
  h4_levene[1, "F value"]
)

h4_df_between <- unname(
  h4_levene[1, "Df"]
)

h4_df_within <- unname(
  h4_levene[2, "Df"]
)

h4_p_value <- unname(
  h4_levene[1, "Pr(>F)"]
)


# ------------------------------------------------------------------------------
# 28. H4 GROUP VARIANCE SUMMARY
# ------------------------------------------------------------------------------

h4_variance_summary <- h4_data %>%
  group_by(Agro_Zone) %>%
  summarise(
    n = n(),
    mean = mean(Soil_pH),
    variance = var(Soil_pH),
    standard_deviation = sd(Soil_pH),
    .groups = "drop"
  )

print(h4_variance_summary)

write_csv(
  h4_variance_summary,
  file.path(
    report_dir,
    "04_h4_soil_ph_variance_summary.csv"
  )
)


# ------------------------------------------------------------------------------
# 29. H4 LEVENE VISUALISATION
# ------------------------------------------------------------------------------

h4_p_label <- ifelse(
  h4_p_value < 0.001,
  "Levene's test p < 0.001",
  paste0(
    "Levene's test p = ",
    format.pval(
      h4_p_value,
      digits = 3,
      eps = 0.001
    )
  )
)

h4_plot_y <- max(
  h4_data$Soil_pH,
  na.rm = TRUE
) +
  0.08 * diff(
    range(
      h4_data$Soil_pH,
      na.rm = TRUE
    )
  )

p_h4 <- ggplot(
  h4_data,
  aes(
    x = Agro_Zone,
    y = Soil_pH
  )
) +
  geom_boxplot(
    width = 0.65,
    outlier.alpha = 0.35
  ) +
  annotate(
    "text",
    x = Inf,
    y = h4_plot_y,
    label = h4_p_label,
    hjust = 1.05,
    vjust = 1,
    size = 4
  ) +
  labs(
    title = "Soil pH Variability Across Agro-Ecological Zones",
    subtitle = paste0(
      "Median-centered Levene's Test | F(",
      h4_df_between,
      ", ",
      h4_df_within,
      ") = ",
      round(h4_f_statistic, 3)
    ),
    x = "Agro-Ecological Zone",
    y = "Soil pH"
  ) +
  theme_minimal(
    base_size = 12
  )

ggsave(
  filename = file.path(
    figure_dir,
    "04_levene_soil_ph.png"
  ),
  plot = p_h4,
  width = 10,
  height = 7,
  dpi = 300
)


# ==============================================================================
# 30. STATISTICAL DECISION LOGIC
# ==============================================================================

h1_decision <- ifelse(
  h1_p_value < alpha,
  "Reject H0",
  "Fail to reject H0"
)

h2_decision <- ifelse(
  h2_p_value < alpha,
  "Reject H0",
  "Fail to reject H0"
)

h3_decision <- ifelse(
  h3_p_value < alpha,
  "Reject H0",
  "Fail to reject H0"
)

h4_decision <- ifelse(
  h4_p_value < alpha,
  "Reject H0",
  "Fail to reject H0"
)


# ------------------------------------------------------------------------------
# Statistical interpretation:
#
# p < 0.05:
#   Evidence is statistically significant at the 5% significance level,
#   leading to rejection of the stated null hypothesis.
#
# p >= 0.05:
#   The available evidence is insufficient to reject the stated null
#   hypothesis at the 5% significance level.
#
# "Fail to reject H0" does NOT mean that H0 has been proven true.
# It means the observed data do not provide sufficient evidence against H0
# under the specified statistical test.
# ------------------------------------------------------------------------------


# ==============================================================================
# 31. MASTER HYPOTHESIS TEST SUMMARY
# ==============================================================================

hypothesis_summary <- tibble(

  hypothesis_id = c(
    "H1",
    "H2",
    "H3",
    "H4"
  ),

  test = c(
    "One-Way ANOVA",
    "One-Way ANOVA",
    "Welch Two-Sample t-test",
    "Levene's Test"
  ),

  dependent_variable = c(
    "Soil_pH",
    "Available_P_mgkg",
    "Total_N_percent",
    "Soil_pH"
  ),

  grouping_variable = c(
    "Agro_Zone",
    "Agro_Zone",
    "Season: Maha vs Yala",
    "Agro_Zone"
  ),

  n = c(
    nrow(h1_data),
    nrow(h2_data),
    nrow(h3_data),
    nrow(h4_data)
  ),

  statistic = c(
    h1_f_statistic,
    h2_f_statistic,
    h3_t_statistic,
    h4_f_statistic
  ),

  statistic_type = c(
    "F",
    "F",
    "t",
    "F"
  ),

  df_1 = c(
    h1_df_between,
    h2_df_between,
    NA_real_,
    h4_df_between
  ),

  df_2 = c(
    h1_df_within,
    h2_df_within,
    h3_df,
    h4_df_within
  ),

  p_value = c(
    h1_p_value,
    h2_p_value,
    h3_p_value,
    h4_p_value
  ),

  effect_size = c(
    h1_eta_squared,
    h2_eta_squared,
    h3_cohens_d,
    NA_real_
  ),

  effect_size_type = c(
    "Eta-squared",
    "Eta-squared",
    "Cohen's d",
    "Not applicable for primary Levene output"
  ),

  ci_lower = c(
    NA_real_,
    NA_real_,
    h3_ci_lower,
    NA_real_
  ),

  ci_upper = c(
    NA_real_,
    NA_real_,
    h3_ci_upper,
    NA_real_
  ),

  confidence_level = c(
    NA_real_,
    NA_real_,
    0.95,
    NA_real_
  ),

  alpha = alpha,

  decision = c(
    h1_decision,
    h2_decision,
    h3_decision,
    h4_decision
  ),

  stringsAsFactors = FALSE
)


# ------------------------------------------------------------------------------
# 32. EXPORT MASTER SUMMARY
# ------------------------------------------------------------------------------

write_csv(
  hypothesis_summary,
  file.path(
    report_dir,
    "04_hypothesis_tests_summary.csv"
  )
)


# ==============================================================================
# 33. CONSOLIDATED CONFIDENCE INTERVAL TABLE
# ==============================================================================

# For the ANOVA hypotheses, confidence intervals are provided for each
# individual Agro-Zone mean.
#
# For the Welch t-test, the confidence interval is for the difference
# between the Maha and Yala means.
#
# Levene's test is an omnibus variance test and does not naturally produce
# a single confidence interval for the null hypothesis. Group variances
# are therefore reported separately.

anova_ci_combined <- bind_rows(

  h1_group_summary %>%
    transmute(
      hypothesis_id = "H1",
      variable = "Soil_pH",
      group = as.character(Agro_Zone),
      n = n,
      estimate = mean,
      estimate_type = "Group mean",
      ci_lower = ci_lower,
      ci_upper = ci_upper,
      confidence_level = 0.95
    ),

  h2_group_summary %>%
    transmute(
      hypothesis_id = "H2",
      variable = "Available_P_mgkg",
      group = as.character(Agro_Zone),
      n = n,
      estimate = mean,
      estimate_type = "Group mean",
      ci_lower = ci_lower,
      ci_upper = ci_upper,
      confidence_level = 0.95
    )

)

write_csv(
  anova_ci_combined,
  file.path(
    report_dir,
    "04_anova_group_confidence_intervals.csv"
  )
)


# ==============================================================================
# 34. WRITE FULL STATISTICAL INFERENCE REPORT
# ==============================================================================

report_file <- file.path(
  report_dir,
  "04_statistical_inference_report.txt"
)

report_connection <- file(
  report_file,
  open = "wt"
)


# ------------------------------------------------------------------------------
# Report Header
# ------------------------------------------------------------------------------

cat(
  "============================================================\n",
  "IT3081 STATISTICAL MODELLING\n",
  "TASK 4: STATISTICAL INFERENCE EVIDENCE\n",
  "============================================================\n\n",
  sep = "",
  file = report_connection
)

cat(
  "Dataset: Fertilizer_Dataset.csv\n",
  "Significance level: alpha = 0.05\n",
  "Confidence level: 95%\n\n",
  sep = "",
  file = report_connection
)


# ------------------------------------------------------------------------------
# Data Cleaning
# ------------------------------------------------------------------------------

cat(
  "------------------------------------------------------------\n",
  "1. DATA CLEANING PRE-CHECK\n",
  "------------------------------------------------------------\n\n",
  sep = "",
  file = report_connection
)

cat(
  "Original observations: ",
  nrow(fertilizer_data),
  "\n",
  sep = "",
  file = report_connection
)

cat(
  "Exact duplicate rows detected: ",
  duplicate_rows,
  "\n",
  sep = "",
  file = report_connection
)

cat(
  "Exact duplicate rows removed: ",
  rows_removed_duplicates,
  "\n",
  sep = "",
  file = report_connection
)

cat(
  "Observations remaining after duplicate removal: ",
  nrow(fertilizer_data_clean),
  "\n\n",
  sep = "",
  file = report_connection
)

cat(
  "Each hypothesis used complete observations for its required variables.\n",
  "This preserves valid degrees of freedom for each individual test.\n\n",
  sep = "",
  file = report_connection
)


# ------------------------------------------------------------------------------
# H1 Report
# ------------------------------------------------------------------------------

cat(
  "------------------------------------------------------------\n",
  "2. HYPOTHESIS 1 - ONE-WAY ANOVA: SOIL pH\n",
  "------------------------------------------------------------\n\n",
  sep = "",
  file = report_connection
)

cat(
  "H0: Soil pH does not differ significantly across Agro-Ecological Zones.\n",
  "H1: At least one Agro-Ecological Zone has a different mean Soil pH.\n\n",
  sep = "",
  file = report_connection
)

cat(
  "Test: One-Way ANOVA\n",
  "N = ", nrow(h1_data), "\n",
  "F-statistic = ", round(h1_f_statistic, 6), "\n",
  "Degrees of freedom = (", h1_df_between, ", ", h1_df_within, ")\n",
  "p-value = ", format.pval(h1_p_value, digits = 6), "\n",
  "Eta-squared = ", round(h1_eta_squared, 6), "\n",
  "Decision at alpha = 0.05: ", h1_decision, "\n\n",
  sep = "",
  file = report_connection
)

cat(
  "Interpretation:\n",
  "Eta-squared represents the proportion of total observed Soil pH ",
  "variation attributable to Agro-Ecological Zone in this ANOVA model.\n",
  "The p-value determines whether the observed between-zone variation ",
  "provides sufficient statistical evidence against the null hypothesis ",
  "at the 5% significance level.\n\n",
  sep = "",
  file = report_connection
)


# ------------------------------------------------------------------------------
# H2 Report
# ------------------------------------------------------------------------------

cat(
  "------------------------------------------------------------\n",
  "3. HYPOTHESIS 2 - ONE-WAY ANOVA: AVAILABLE PHOSPHORUS\n",
  "------------------------------------------------------------\n\n",
  sep = "",
  file = report_connection
)

cat(
  "H0: Available Phosphorus does not differ across Agro-Ecological Zones.\n",
  "H1: At least one Agro-Ecological Zone has a different mean Available Phosphorus.\n\n",
  sep = "",
  file = report_connection
)

cat(
  "Test: One-Way ANOVA\n",
  "N = ", nrow(h2_data), "\n",
  "F-statistic = ", round(h2_f_statistic, 6), "\n",
  "Degrees of freedom = (", h2_df_between, ", ", h2_df_within, ")\n",
  "p-value = ", format.pval(h2_p_value, digits = 6), "\n",
  "Eta-squared = ", round(h2_eta_squared, 6), "\n",
  "Decision at alpha = 0.05: ", h2_decision, "\n\n",
  sep = "",
  file = report_connection
)

cat(
  "Interpretation:\n",
  "Eta-squared represents the proportion of total observed Available ",
  "Phosphorus variation attributable to Agro-Ecological Zone in this ",
  "ANOVA model.\n",
  "The omnibus ANOVA tests whether all zone means can reasonably be ",
  "treated as equal under the specified model.\n\n",
  sep = "",
  file = report_connection
)


# ------------------------------------------------------------------------------
# H3 Report
# ------------------------------------------------------------------------------

cat(
  "------------------------------------------------------------\n",
  "4. HYPOTHESIS 3 - WELCH TWO-SAMPLE T-TEST: TOTAL NITROGEN\n",
  "------------------------------------------------------------\n\n",
  sep = "",
  file = report_connection
)

cat(
  "H0: Total Nitrogen levels are equal across Maha and Yala seasons.\n",
  "H1: Mean Total Nitrogen differs between Maha and Yala seasons.\n\n",
  sep = "",
  file = report_connection
)

cat(
  "Test: Welch Two-Sample t-test\n",
  "N = ", nrow(h3_data), "\n",
  "Maha n = ", n_maha, "\n",
  "Yala n = ", n_yala, "\n",
  "t-statistic = ", round(h3_t_statistic, 6), "\n",
  "Welch degrees of freedom = ", round(h3_df, 6), "\n",
  "p-value = ", format.pval(h3_p_value, digits = 6), "\n",
  "Mean difference (Maha - Yala) = ",
  round(h3_mean_difference, 6),
  "\n",
  "95% CI for mean difference = [",
  round(h3_ci_lower, 6),
  ", ",
  round(h3_ci_upper, 6),
  "]\n",
  "Cohen's d = ", round(h3_cohens_d, 6), "\n",
  "Decision at alpha = 0.05: ", h3_decision, "\n\n",
  sep = "",
  file = report_connection
)

cat(
  "Interpretation:\n",
  "Welch's t-test does not assume equal population variances between ",
  "Maha and Yala. The reported confidence interval describes the ",
  "plausible range for the population mean difference (Maha - Yala) ",
  "under the test assumptions.\n",
  "Cohen's d expresses the standardized difference between the two ",
  "seasonal means.\n\n",
  sep = "",
  file = report_connection
)


# ------------------------------------------------------------------------------
# H4 Report
# ------------------------------------------------------------------------------

cat(
  "------------------------------------------------------------\n",
  "5. HYPOTHESIS 4 - LEVENE'S TEST: SOIL pH VARIANCE\n",
  "------------------------------------------------------------\n\n",
  sep = "",
  file = report_connection
)

cat(
  "H0: Variances of Soil pH are equal across Agro-Ecological Zones.\n",
  "H1: At least one Agro-Ecological Zone has a different Soil pH variance.\n\n",
  sep = "",
  file = report_connection
)

cat(
  "Test: Median-centered Levene's Test\n",
  "N = ", nrow(h4_data), "\n",
  "F-statistic = ", round(h4_f_statistic, 6), "\n",
  "Degrees of freedom = (", h4_df_between, ", ", h4_df_within, ")\n",
  "p-value = ", format.pval(h4_p_value, digits = 6), "\n",
  "Decision at alpha = 0.05: ", h4_decision, "\n\n",
  sep = "",
  file = report_connection
)

cat(
  "Interpretation:\n",
  "Levene's test evaluates whether the variability of Soil pH differs ",
  "across Agro-Ecological Zones.\n",
  "The median-centered implementation is used because it provides a ",
  "more robust assessment of variance homogeneity when distributions ",
  "are not perfectly normal.\n\n",
  sep = "",
  file = report_connection
)


# ------------------------------------------------------------------------------
# Group Confidence Intervals
# ------------------------------------------------------------------------------

cat(
  "------------------------------------------------------------\n",
  "6. 95% CONFIDENCE INTERVALS FOR GROUP MEANS\n",
  "------------------------------------------------------------\n\n",
  sep = "",
  file = report_connection
)

cat(
  "H1 - Soil pH:\n",
  sep = "",
  file = report_connection
)

capture.output(
  print(h1_group_summary),
  file = report_connection,
  append = TRUE
)

cat(
  "\nH2 - Available Phosphorus:\n",
  sep = "",
  file = report_connection
)

capture.output(
  print(h2_group_summary),
  file = report_connection,
  append = TRUE
)

cat(
  "\nH3 - Welch t-test mean difference:\n",
  sep = "",
  file = report_connection
)

capture.output(
  print(h3_ci_report),
  file = report_connection,
  append = TRUE
)


# ------------------------------------------------------------------------------
# Statistical Assumptions and Limitations
# ------------------------------------------------------------------------------

cat(
  "\n------------------------------------------------------------\n",
  "7. STATISTICAL INTERPRETATION NOTES\n",
  "------------------------------------------------------------\n\n",
  sep = "",
  file = report_connection
)

cat(
  "1. ANOVA tests whether the population means are equal across the ",
  "specified Agro-Ecological Zone groups under the model assumptions.\n\n",
  sep = "",
  file = report_connection
)

cat(
  "2. A statistically significant ANOVA result indicates that at least ",
  "one group mean differs, but the omnibus ANOVA alone does not identify ",
  "which specific pairs of groups differ.\n\n",
  sep = "",
  file = report_connection
)

cat(
  "3. If pairwise zone comparisons are required for the consultancy, ",
  "post-hoc procedures such as Tukey's HSD can be conducted as a ",
  "follow-up analysis after a significant ANOVA.\n\n",
  sep = "",
  file = report_connection
)

cat(
  "4. Welch's t-test was deliberately used for the seasonal comparison ",
  "because it does not require equal population variances.\n\n",
  sep = "",
  file = report_connection
)

cat(
  "5. Levene's test evaluates variance homogeneity. A non-significant ",
  "result does not prove that all variances are identical; it indicates ",
  "that the sample provides insufficient evidence of variance differences ",
  "at the selected significance level.\n\n",
  sep = "",
  file = report_connection
)

cat(
  "6. Statistical significance should be considered alongside effect ",
  "sizes, confidence intervals, sample sizes, agricultural context, ",
  "and practical relevance.\n\n",
  sep = "",
  file = report_connection
)


# ------------------------------------------------------------------------------
# File Outputs
# ------------------------------------------------------------------------------

cat(
  "------------------------------------------------------------\n",
  "8. GENERATED OUTPUTS\n",
  "------------------------------------------------------------\n\n",
  sep = "",
  file = report_connection
)

cat(
  "Figures:\n",
  "- output/figures/04_anova_soil_ph.png\n",
  "- output/figures/04_anova_available_phosphorus.png\n",
  "- output/figures/04_ttest_nitrogen_season.png\n",
  "- output/figures/04_levene_soil_ph.png\n\n",
  sep = "",
  file = report_connection
)

cat(
  "Reports:\n",
  "- output/reports/04_hypothesis_tests_summary.csv\n",
  "- output/reports/04_anova_group_confidence_intervals.csv\n",
  "- output/reports/04_ttest_confidence_interval.csv\n",
  "- output/reports/04_h1_soil_ph_group_confidence_intervals.csv\n",
  "- output/reports/04_h2_available_phosphorus_group_confidence_intervals.csv\n",
  "- output/reports/04_h4_soil_ph_variance_summary.csv\n",
  "- output/reports/04_hypothesis_definitions.csv\n",
  "- output/reports/04_statistical_inference_report.txt\n\n",
  sep = "",
  file = report_connection
)


# ------------------------------------------------------------------------------
# Session Information
# ------------------------------------------------------------------------------

cat(
  "------------------------------------------------------------\n",
  "9. SESSION INFORMATION\n",
  "------------------------------------------------------------\n\n",
  sep = "",
  file = report_connection
)

capture.output(
  sessionInfo(),
  file = report_connection,
  append = TRUE
)

close(
  report_connection
)


# ==============================================================================
# 35. CONSOLE SUMMARY
# ==============================================================================

cat(
  "\n\n============================================================\n",
  "TASK 4 COMPLETED\n",
  "============================================================\n\n",
  sep = ""
)

cat(
  "H1 - Soil pH ANOVA:\n",
  "  F = ", round(h1_f_statistic, 4),
  "\n  p = ", format.pval(h1_p_value, digits = 4),
  "\n  Eta-squared = ", round(h1_eta_squared, 4),
  "\n  Decision = ", h1_decision,
  "\n\n",
  sep = ""
)

cat(
  "H2 - Available Phosphorus ANOVA:\n",
  "  F = ", round(h2_f_statistic, 4),
  "\n  p = ", format.pval(h2_p_value, digits = 4),
  "\n  Eta-squared = ", round(h2_eta_squared, 4),
  "\n  Decision = ", h2_decision,
  "\n\n",
  sep = ""
)

cat(
  "H3 - Total Nitrogen Welch t-test:\n",
  "  t = ", round(h3_t_statistic, 4),
  "\n  df = ", round(h3_df, 4),
  "\n  p = ", format.pval(h3_p_value, digits = 4),
  "\n  Cohen's d = ", round(h3_cohens_d, 4),
  "\n  95% CI = [",
  round(h3_ci_lower, 4),
  ", ",
  round(h3_ci_upper, 4),
  "]\n",
  "  Decision = ", h3_decision,
  "\n\n",
  sep = ""
)

cat(
  "H4 - Soil pH Levene's test:\n",
  "  F = ", round(h4_f_statistic, 4),
  "\n  p = ", format.pval(h4_p_value, digits = 4),
  "\n  Decision = ", h4_decision,
  "\n\n",
  sep = ""
)

cat(
  "All Task 4 figures and statistical reports have been exported.\n",
  "============================================================\n",
  sep = ""
)
