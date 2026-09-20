# ==============================================================================
# Task 3: Data Quality, Exploratory Data Analysis (EDA) & Initial Insights
# Project: Data-Driven Precision Fertilizer Advisory & Strategic Agriculture Framework
# Course: IT3081 Statistical Modelling
#
# Save as:
# scripts/03_eda_and_quality.R
#
# IMPORTANT:
# Run this script with the project root as the working directory:
# Fertilizer_Consultancy_Project/
#
# Expected project structure:
# Fertilizer_Consultancy_Project/
# ├── Fertilizer_Dataset.csv
# ├── PRD.pdf
# ├── ref/
# ├── scripts/
# │   └── 03_eda_and_quality.R
# └── output/
#     ├── figures/
#     └── reports/
#
# Task 3 Scope:
#   1. Data loading and structural quality checks
#   2. Missing-value analysis using naniar
#   3. IQR-based outlier detection
#   4. Distribution analysis
#   5. Boxplot analysis
#   6. Correlation analysis using corrplot
#   7. Agro-ecological zone breakdowns
#   8. District-level breakdowns
#   9. Initial descriptive insights
#
# Outputs:
#   output/figures/  -> PNG visualizations
#   output/reports/  -> CSV/TXT analytical summaries
# ==============================================================================


# ==============================================================================
# 0. DIRECTORY SETUP
# ==============================================================================

if(!dir.exists("output/figures")) dir.create("output/figures", recursive = TRUE)
if(!dir.exists("output/reports")) dir.create("output/reports", recursive = TRUE)


# ==============================================================================
# 1. REQUIRED PACKAGES
# ==============================================================================

required_packages <- c(
  "tidyverse",
  "naniar",
  "corrplot",
  "ggplotify"
)

missing_packages <- required_packages[
  !(required_packages %in% rownames(installed.packages()))
]

if(length(missing_packages) > 0) {

  stop(
    paste0(
      "\nThe following required R packages are not installed:\n",
      paste(missing_packages, collapse = ", "),
      "\n\nPlease install them before running this script using:\n\n",
      "install.packages(c(",
      paste0("\"", missing_packages, "\"", collapse = ", "),
      "))\n"
    )
  )
}

library(tidyverse)
library(naniar)
library(corrplot)
library(ggplotify)


# ==============================================================================
# 2. PROJECT CONFIGURATION
# ==============================================================================

options(stringsAsFactors = FALSE)

dataset_path <- "Fertilizer_Dataset.csv"

figure_dir <- "output/figures"
report_dir <- "output/reports"


# ==============================================================================
# 3. LOAD DATA
# ==============================================================================

if(!file.exists(dataset_path)) {

  stop(
    paste0(
      "\nDataset not found: ", dataset_path,
      "\n\nPlease make sure the RStudio working directory is the project root:\n",
      "Fertilizer_Consultancy_Project/\n"
    )
  )
}

fertilizer_data <- read.csv(
  "Fertilizer_Dataset.csv",
  stringsAsFactors = FALSE
)


# ==============================================================================
# 4. BASIC DATA STRUCTURE AND QUALITY CHECKS
# ==============================================================================

cat(
  "\n============================================================\n",
  "TASK 3: DATA QUALITY, EDA & INITIAL INSIGHTS\n",
  "============================================================\n\n"
)

cat("Dataset loaded successfully.\n")
cat("Number of observations:", nrow(fertilizer_data), "\n")
cat("Number of variables:", ncol(fertilizer_data), "\n\n")


# ------------------------------------------------------------------------------
# 4.1 Dataset dimensions
# ------------------------------------------------------------------------------

dataset_dimensions <- data.frame(
  Metric = c(
    "Number of observations",
    "Number of variables"
  ),
  Value = c(
    nrow(fertilizer_data),
    ncol(fertilizer_data)
  )
)

write.csv(
  dataset_dimensions,
  file.path(report_dir, "03_dataset_dimensions.csv"),
  row.names = FALSE
)


# ------------------------------------------------------------------------------
# 4.2 Variable names and data types
# ------------------------------------------------------------------------------

variable_structure <- data.frame(
  Variable = names(fertilizer_data),
  Data_Type = sapply(fertilizer_data, function(x) class(x)[1]),
  Missing_Count = sapply(
    fertilizer_data,
    function(x) sum(is.na(x))
  ),
  Missing_Percent = round(
    sapply(
      fertilizer_data,
      function(x) mean(is.na(x)) * 100
    ),
    3
  ),
  Unique_Values = sapply(
    fertilizer_data,
    function(x) length(unique(x))
  )
)

write.csv(
  variable_structure,
  file.path(report_dir, "03_variable_structure.csv"),
  row.names = FALSE
)


# ------------------------------------------------------------------------------
# 4.3 Duplicate row check
# ------------------------------------------------------------------------------

duplicate_count <- sum(duplicated(fertilizer_data))

duplicate_summary <- data.frame(
  Metric = c(
    "Total rows",
    "Duplicate rows",
    "Duplicate percentage"
  ),
  Value = c(
    nrow(fertilizer_data),
    duplicate_count,
    round(duplicate_count / nrow(fertilizer_data) * 100, 3)
  )
)

write.csv(
  duplicate_summary,
  file.path(report_dir, "03_duplicate_summary.csv"),
  row.names = FALSE
)


# ==============================================================================
# 5. VARIABLE REGISTRY
# ==============================================================================

# Variables required by the Task 3 analysis.

required_variables <- c(
  "Sample_ID",
  "District",
  "Agro_Zone",
  "Season",
  "Soil_pH",
  "Total_N_percent",
  "Available_P_mgkg",
  "Available_K_mgkg",
  "Organic_Carbon_percent",
  "Rainfall_30d_mm",
  "Temperature_mean_C",
  "Recommended_N_kg_ha",
  "Recommended_P2O5_kg_ha",
  "Recommended_K2O_kg_ha",
  "Urea_kg_ha",
  "TSP_kg_ha",
  "MOP_kg_ha"
)

missing_required_variables <- setdiff(
  required_variables,
  names(fertilizer_data)
)

if(length(missing_required_variables) > 0) {

  stop(
    paste0(
      "\nThe following expected dataset variables are missing:\n",
      paste(missing_required_variables, collapse = ", "),
      "\n\nPlease verify the dataset structure before running Task 3.\n"
    )
  )
}


# ------------------------------------------------------------------------------
# Key continuous variables
# ------------------------------------------------------------------------------

soil_variables <- c(
  "Soil_pH",
  "Total_N_percent",
  "Available_P_mgkg",
  "Available_K_mgkg",
  "Organic_Carbon_percent"
)

weather_variables <- c(
  "Rainfall_30d_mm",
  "Temperature_mean_C"
)

requested_distribution_variables <- c(
  soil_variables,
  weather_variables
)

fertilizer_variables <- c(
  "Recommended_N_kg_ha",
  "Recommended_P2O5_kg_ha",
  "Recommended_K2O_kg_ha",
  "Urea_kg_ha",
  "TSP_kg_ha",
  "MOP_kg_ha"
)

categorical_variables <- c(
  "District",
  "Agro_Zone",
  "Season"
)


# ==============================================================================
# 6. MISSING VALUE ANALYSIS USING naniar
# ==============================================================================

cat("\n------------------------------------------------------------\n")
cat("6. Missing Value Analysis\n")
cat("------------------------------------------------------------\n")


# ------------------------------------------------------------------------------
# 6.1 Overall missing-value count
# ------------------------------------------------------------------------------

total_missing_values <- sum(is.na(fertilizer_data))

total_cells <- nrow(fertilizer_data) * ncol(fertilizer_data)

overall_missing_percentage <- (
  total_missing_values / total_cells
) * 100

overall_missing_summary <- data.frame(
  Metric = c(
    "Total cells",
    "Total missing values",
    "Overall missing percentage"
  ),
  Value = c(
    total_cells,
    total_missing_values,
    round(overall_missing_percentage, 4)
  )
)

write.csv(
  overall_missing_summary,
  file.path(report_dir, "03_missing_overall_summary.csv"),
  row.names = FALSE
)


# ------------------------------------------------------------------------------
# 6.2 Variable-level missingness using naniar
# ------------------------------------------------------------------------------

missing_by_variable <- naniar::miss_var_summary(
  fertilizer_data
)

# Convert to a standard data.frame and remove special tibble/pillar classes.
missing_by_variable <- as.data.frame(
  missing_by_variable,
  stringsAsFactors = FALSE
)

missing_by_variable <- data.frame(
  variable = as.character(
    missing_by_variable$variable
  ),
  n_miss = as.numeric(
    missing_by_variable$n_miss
  ),
  pct_miss = as.numeric(
    missing_by_variable$pct_miss
  ),
  stringsAsFactors = FALSE
)

write.csv(
  missing_by_variable,
  file.path(
    report_dir,
    "03_missing_by_variable.csv"
  ),
  row.names = FALSE
)


# ------------------------------------------------------------------------------
# 6.3 Observation-level missingness
# ------------------------------------------------------------------------------

missing_by_case <- naniar::miss_case_summary(
  fertilizer_data
)

# Convert naniar output to a standard data.frame.
missing_by_case <- as.data.frame(
  missing_by_case,
  stringsAsFactors = FALSE
)

# Keep the expected missingness summary columns and convert them
# to standard R types.
missing_by_case <- data.frame(
  case = as.numeric(
    missing_by_case$case
  ),
  n_miss = as.numeric(
    missing_by_case$n_miss
  ),
  pct_miss = as.numeric(
    missing_by_case$pct_miss
  ),
  stringsAsFactors = FALSE
)

write.csv(
  missing_by_case,
  file.path(
    report_dir,
    "03_missing_by_observation.csv"
  ),
  row.names = FALSE
)

# ------------------------------------------------------------------------------
# 6.4 Missingness visualization: variable level
# ------------------------------------------------------------------------------

p_missing_variables <- ggplot(
  missing_by_variable,
  aes(
    x = reorder(variable, n_miss),
    y = n_miss
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Missing Values by Variable",
    x = "Variable",
    y = "Number of Missing Values"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  filename = file.path(
    figure_dir,
    "03_missing_values_by_variable.png"
  ),
  plot = p_missing_variables,
  width = 10,
  height = 7,
  dpi = 300
)


# ------------------------------------------------------------------------------
# 6.5 Missingness visualization: naniar vis_miss
# ------------------------------------------------------------------------------

p_missing_matrix <- naniar::vis_miss(
  fertilizer_data,
  cluster = TRUE
) +
  labs(
    title = "Missingness Pattern Across the Dataset"
  ) +
  theme_minimal(base_size = 11)

ggsave(
  filename = file.path(
    figure_dir,
    "03_missingness_pattern.png"
  ),
  plot = p_missing_matrix,
  width = 12,
  height = 8,
  dpi = 300
)


# ------------------------------------------------------------------------------
# 6.6 Missingness report
# ------------------------------------------------------------------------------

missing_report_text <- paste0(
  "TASK 3 - MISSING VALUE ANALYSIS\n",
  "========================================\n\n",
  "Total observations: ", nrow(fertilizer_data), "\n",
  "Total variables: ", ncol(fertilizer_data), "\n",
  "Total data cells: ", total_cells, "\n",
  "Total missing values: ", total_missing_values, "\n",
  "Overall missing percentage: ",
  round(overall_missing_percentage, 4), "%\n\n"
)

if(total_missing_values == 0) {

  missing_report_text <- paste0(
    missing_report_text,
    "Interpretation:\n",
    "No missing values were detected in the dataset.\n",
    "Therefore, missing-value imputation is not required for the\n",
    "Task 3 exploratory analysis.\n"
  )

} else {

  missing_report_text <- paste0(
    missing_report_text,
    "Interpretation:\n",
    "Missing values are present and should be considered when\n",
    "interpreting descriptive statistics, correlations and models.\n",
    "Variable-level missingness is reported in:\n",
    "03_missing_by_variable.csv\n"
  )
}

cat(
  missing_report_text,
  file = file.path(
    report_dir,
    "03_missingness_interpretation.txt"
  )
)


# ==============================================================================
# 7. DESCRIPTIVE STATISTICS
# ==============================================================================

cat("\n------------------------------------------------------------\n")
cat("7. Descriptive Statistics\n")
cat("------------------------------------------------------------\n")


numeric_variables <- names(
  fertilizer_data
)[
  sapply(
    fertilizer_data,
    is.numeric
  )
]

# Sample_ID is an identifier rather than a meaningful continuous measurement.
numeric_analysis_variables <- setdiff(
  numeric_variables,
  "Sample_ID"
)


# ------------------------------------------------------------------------------
# 7.1 Comprehensive numeric summary
# ------------------------------------------------------------------------------

numeric_summary <- fertilizer_data %>%
  select(all_of(numeric_analysis_variables)) %>%
  summarise(
    across(
      everything(),
      list(
        n = ~sum(!is.na(.)),
        missing = ~sum(is.na(.)),
        mean = ~mean(., na.rm = TRUE),
        median = ~median(., na.rm = TRUE),
        sd = ~sd(., na.rm = TRUE),
        min = ~min(., na.rm = TRUE),
        q1 = ~quantile(., 0.25, na.rm = TRUE),
        q3 = ~quantile(., 0.75, na.rm = TRUE),
        max = ~max(., na.rm = TRUE)
      )
    )
  )

write.csv(
  numeric_summary,
  file.path(report_dir, "03_numeric_descriptive_statistics.csv"),
  row.names = FALSE
)


# ------------------------------------------------------------------------------
# 7.2 Tidy descriptive statistics table
# ------------------------------------------------------------------------------

tidy_descriptive_statistics <- fertilizer_data %>%
  select(all_of(numeric_analysis_variables)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Variable",
    values_to = "Value"
  ) %>%
  group_by(Variable) %>%
  summarise(
    N = sum(!is.na(Value)),
    Missing = sum(is.na(Value)),
    Mean = mean(Value, na.rm = TRUE),
    Median = median(Value, na.rm = TRUE),
    SD = sd(Value, na.rm = TRUE),
    Minimum = min(Value, na.rm = TRUE),
    Q1 = quantile(Value, 0.25, na.rm = TRUE),
    Q3 = quantile(Value, 0.75, na.rm = TRUE),
    Maximum = max(Value, na.rm = TRUE),
    .groups = "drop"
  )

write.csv(
  tidy_descriptive_statistics,
  file.path(
    report_dir,
    "03_tidy_descriptive_statistics.csv"
  ),
  row.names = FALSE
)


# ==============================================================================
# 8. IQR-BASED OUTLIER DETECTION
# ==============================================================================

cat("\n------------------------------------------------------------\n")
cat("8. IQR-Based Outlier Detection\n")
cat("------------------------------------------------------------\n")


# ------------------------------------------------------------------------------
# Statistical method
#
# For each continuous variable:
#
#   IQR = Q3 - Q1
#
#   Lower fence = Q1 - 1.5 * IQR
#   Upper fence = Q3 + 1.5 * IQR
#
# Observations below the lower fence or above the upper fence are
# flagged as potential statistical outliers.
#
# IMPORTANT:
# An IQR outlier is not automatically an erroneous observation.
# Agricultural measurements can legitimately contain extreme values.
# Outliers therefore require domain interpretation before removal.
# ------------------------------------------------------------------------------


outlier_summary_list <- list()

for(variable in numeric_analysis_variables) {

  x <- fertilizer_data[[variable]]

  q1 <- quantile(
    x,
    0.25,
    na.rm = TRUE,
    names = FALSE
  )

  q3 <- quantile(
    x,
    0.75,
    na.rm = TRUE,
    names = FALSE
  )

  iqr_value <- q3 - q1

  lower_fence <- q1 - (1.5 * iqr_value)

  upper_fence <- q3 + (1.5 * iqr_value)

  outlier_flag <- (
    x < lower_fence |
      x > upper_fence
  )

  outlier_flag[is.na(outlier_flag)] <- FALSE

  outlier_count <- sum(outlier_flag)

  valid_count <- sum(!is.na(x))

  outlier_percentage <- ifelse(
    valid_count > 0,
    outlier_count / valid_count * 100,
    NA
  )

  outlier_summary_list[[variable]] <- data.frame(
    Variable = variable,
    Q1 = q1,
    Q3 = q3,
    IQR = iqr_value,
    Lower_Fence = lower_fence,
    Upper_Fence = upper_fence,
    Valid_Observations = valid_count,
    Outlier_Count = outlier_count,
    Outlier_Percentage = outlier_percentage
  )
}

outlier_summary <- bind_rows(
  outlier_summary_list
)

write.csv(
  outlier_summary,
  file.path(
    report_dir,
    "03_iqr_outlier_summary.csv"
  ),
  row.names = FALSE
)


# ------------------------------------------------------------------------------
# 8.1 Create row-level outlier flags
# ------------------------------------------------------------------------------

outlier_flag_data <- fertilizer_data %>%
  select(
    Sample_ID,
    District,
    Agro_Zone,
    Season
  )

for(variable in numeric_analysis_variables) {

  x <- fertilizer_data[[variable]]

  q1 <- quantile(
    x,
    0.25,
    na.rm = TRUE,
    names = FALSE
  )

  q3 <- quantile(
    x,
    0.75,
    na.rm = TRUE,
    names = FALSE
  )

  iqr_value <- q3 - q1

  lower_fence <- q1 - (1.5 * iqr_value)
  upper_fence <- q3 + (1.5 * iqr_value)

  flag_name <- paste0(
    variable,
    "_IQR_Outlier"
  )

  outlier_flag_data[[flag_name]] <- (
    x < lower_fence |
      x > upper_fence
  )

  outlier_flag_data[[flag_name]][
    is.na(outlier_flag_data[[flag_name]])
  ] <- FALSE
}

# Number of variables for which each observation is flagged.
outlier_flag_columns <- grep(
  "_IQR_Outlier$",
  names(outlier_flag_data),
  value = TRUE
)

outlier_flag_data$Total_IQR_Outlier_Flags <- rowSums(
  outlier_flag_data[
    outlier_flag_columns
  ]
)

write.csv(
  outlier_flag_data,
  file.path(
    report_dir,
    "03_observation_level_outlier_flags.csv"
  ),
  row.names = FALSE
)


# ------------------------------------------------------------------------------
# 8.2 Outlier boxplots for requested continuous variables
# ------------------------------------------------------------------------------

boxplot_data <- fertilizer_data %>%
  select(
    all_of(requested_distribution_variables)
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Variable",
    values_to = "Value"
  )

p_all_boxplots <- ggplot(
  boxplot_data,
  aes(
    x = Variable,
    y = Value
  )
) +
  geom_boxplot(
    na.rm = TRUE
  ) +
  facet_wrap(
    ~ Variable,
    scales = "free",
    ncol = 3
  ) +
  labs(
    title = "Boxplots of Soil and Weather Variables",
    x = NULL,
    y = "Observed Value"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    strip.text = element_text(face = "bold")
  )

ggsave(
  filename = file.path(
    figure_dir,
    "03_boxplots_soil_weather_variables.png"
  ),
  plot = p_all_boxplots,
  width = 12,
  height = 9,
  dpi = 300
)


# ------------------------------------------------------------------------------
# 8.3 Fertilizer variable boxplots
# ------------------------------------------------------------------------------

fertilizer_boxplot_data <- fertilizer_data %>%
  select(
    all_of(fertilizer_variables)
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Variable",
    values_to = "Value"
  )

p_fertilizer_boxplots <- ggplot(
  fertilizer_boxplot_data,
  aes(
    x = Variable,
    y = Value
  )
) +
  geom_boxplot(
    na.rm = TRUE
  ) +
  facet_wrap(
    ~ Variable,
    scales = "free",
    ncol = 3
  ) +
  labs(
    title = "Boxplots of Fertilizer Recommendation and Application Variables",
    x = NULL,
    y = "Rate (kg/ha)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    strip.text = element_text(face = "bold")
  )

ggsave(
  filename = file.path(
    figure_dir,
    "03_boxplots_fertilizer_variables.png"
  ),
  plot = p_fertilizer_boxplots,
  width = 12,
  height = 9,
  dpi = 300
)


# ==============================================================================
# 9. DISTRIBUTION ANALYSIS
# ==============================================================================

cat("\n------------------------------------------------------------\n")
cat("9. Distribution Analysis\n")
cat("------------------------------------------------------------\n")


# ------------------------------------------------------------------------------
# Distribution statistics
# ------------------------------------------------------------------------------

distribution_summary <- fertilizer_data %>%
  select(
    all_of(requested_distribution_variables)
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Variable",
    values_to = "Value"
  ) %>%
  group_by(Variable) %>%
  summarise(
    N = sum(!is.na(Value)),
    Mean = mean(Value, na.rm = TRUE),
    Median = median(Value, na.rm = TRUE),
    SD = sd(Value, na.rm = TRUE),
    Minimum = min(Value, na.rm = TRUE),
    Q1 = quantile(Value, 0.25, na.rm = TRUE),
    Q3 = quantile(Value, 0.75, na.rm = TRUE),
    Maximum = max(Value, na.rm = TRUE),
    .groups = "drop"
  )

write.csv(
  distribution_summary,
  file.path(
    report_dir,
    "03_distribution_summary.csv"
  ),
  row.names = FALSE
)


# ------------------------------------------------------------------------------
# Individual distribution plots
# ------------------------------------------------------------------------------

for(variable in requested_distribution_variables) {

  plot_data <- fertilizer_data %>%
    select(
      Value = all_of(variable)
    ) %>%
    filter(
      !is.na(Value)
    )

  p_distribution <- ggplot(
    plot_data,
    aes(
      x = Value
    )
  ) +
    geom_histogram(
      aes(
        y = after_stat(density)
      ),
      bins = 30,
      na.rm = TRUE
    ) +
    geom_density(
      na.rm = TRUE,
      linewidth = 1
    ) +
    labs(
      title = paste(
        "Distribution of",
        variable
      ),
      x = variable,
      y = "Density"
    ) +
    theme_minimal(base_size = 12)

  safe_filename <- paste0(
    "03_distribution_",
    variable,
    ".png"
  )

  ggsave(
    filename = file.path(
      figure_dir,
      safe_filename
    ),
    plot = p_distribution,
    width = 9,
    height = 6,
    dpi = 300
  )
}


# ------------------------------------------------------------------------------
# Combined distribution plot
# ------------------------------------------------------------------------------

p_combined_distributions <- ggplot(
  fertilizer_data %>%
    select(
      all_of(requested_distribution_variables)
    ) %>%
    pivot_longer(
      cols = everything(),
      names_to = "Variable",
      values_to = "Value"
    ),
  aes(
    x = Value
  )
) +
  geom_histogram(
    aes(
      y = after_stat(density)
    ),
    bins = 30,
    na.rm = TRUE
  ) +
  geom_density(
    na.rm = TRUE,
    linewidth = 0.8
  ) +
  facet_wrap(
    ~ Variable,
    scales = "free",
    ncol = 2
  ) +
  labs(
    title = "Distributions of Key Soil and Weather Variables",
    x = "Observed Value",
    y = "Density"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    strip.text = element_text(face = "bold")
  )

ggsave(
  filename = file.path(
    figure_dir,
    "03_combined_distributions_soil_weather.png"
  ),
  plot = p_combined_distributions,
  width = 12,
  height = 14,
  dpi = 300
)


# ==============================================================================
# 10. CORRELATION ANALYSIS
# ==============================================================================

cat("\n------------------------------------------------------------\n")
cat("10. Correlation Analysis\n")
cat("------------------------------------------------------------\n")


# ------------------------------------------------------------------------------
# 10.1 Pearson correlation matrix
#
# Pairwise complete observations are used so that correlations can still be
# calculated when individual variables contain missing values.
# ------------------------------------------------------------------------------

correlation_variables <- numeric_analysis_variables

correlation_data <- fertilizer_data %>%
  select(
    all_of(correlation_variables)
  )

correlation_matrix <- cor(
  correlation_data,
  use = "pairwise.complete.obs",
  method = "pearson"
)

write.csv(
  correlation_matrix,
  file.path(
    report_dir,
    "03_pearson_correlation_matrix.csv"
  )
)


# ------------------------------------------------------------------------------
# 10.2 Corrplot
#
# corrplot creates a base-R graphics object. ggplotify is used to convert the
# resulting base plot into a ggplot-compatible object so that the final PNG
# is exported consistently using ggsave().
# ------------------------------------------------------------------------------

corrplot_gg <- ggplotify::as.ggplot(
  function() {

    corrplot::corrplot(
      correlation_matrix,
      method = "color",
      type = "upper",
      order = "hclust",
      tl.col = "black",
      tl.cex = 0.7,
      tl.srt = 45,
      addCoef.col = "black",
      number.cex = 0.5,
      diag = FALSE
    )
  }
)

ggsave(
  filename = file.path(
    figure_dir,
    "03_correlation_matrix_corrplot.png"
  ),
  plot = corrplot_gg,
  width = 14,
  height = 12,
  dpi = 300
)


# ------------------------------------------------------------------------------
# 10.3 Target-specific correlations
#
# These tables identify the Pearson correlation between each numerical
# predictor and each major fertilizer outcome.
#
# Correlation does not establish causation.
# ------------------------------------------------------------------------------

target_variables <- c(
  "Urea_kg_ha",
  "TSP_kg_ha",
  "MOP_kg_ha"
)

predictor_variables <- setdiff(
  correlation_variables,
  target_variables
)

target_correlation_results <- list()

for(target in target_variables) {

  for(predictor in predictor_variables) {

    complete_rows <- complete.cases(
      fertilizer_data[[target]],
      fertilizer_data[[predictor]]
    )

    if(sum(complete_rows) >= 3) {

      correlation_value <- cor(
        fertilizer_data[[target]][complete_rows],
        fertilizer_data[[predictor]][complete_rows],
        method = "pearson"
      )

    } else {

      correlation_value <- NA_real_
    }

    target_correlation_results[[paste(
      target,
      predictor,
      sep = "_"
    )]] <- data.frame(
      Target = target,
      Predictor = predictor,
      Pearson_Correlation = correlation_value,
      Absolute_Correlation = abs(correlation_value)
    )
  }
}

target_correlation_table <- bind_rows(
  target_correlation_results
) %>%
  arrange(
    Target,
    desc(Absolute_Correlation)
  )

write.csv(
  target_correlation_table,
  file.path(
    report_dir,
    "03_target_correlation_summary.csv"
  ),
  row.names = FALSE
)


# ------------------------------------------------------------------------------
# 10.4 Soil-variable correlation matrix
# ------------------------------------------------------------------------------

soil_correlation_matrix <- cor(
  fertilizer_data %>%
    select(all_of(soil_variables)),
  use = "pairwise.complete.obs",
  method = "pearson"
)

write.csv(
  soil_correlation_matrix,
  file.path(
    report_dir,
    "03_soil_correlation_matrix.csv"
  )
)

soil_corrplot_gg <- ggplotify::as.ggplot(
  function() {

    corrplot::corrplot(
      soil_correlation_matrix,
      method = "color",
      type = "upper",
      order = "hclust",
      tl.col = "black",
      tl.cex = 0.8,
      tl.srt = 45,
      addCoef.col = "black",
      number.cex = 0.6,
      diag = FALSE
    )
  }
)

ggsave(
  filename = file.path(
    figure_dir,
    "03_soil_correlation_matrix_corrplot.png"
  ),
  plot = soil_corrplot_gg,
  width = 10,
  height = 8,
  dpi = 300
)


# ==============================================================================
# 11. AGRO-ECOLOGICAL ZONE BREAKDOWN
# ==============================================================================

cat("\n------------------------------------------------------------\n")
cat("11. Agro-Ecological Zone Breakdown\n")
cat("------------------------------------------------------------\n")


# ------------------------------------------------------------------------------
# 11.1 Observation count by agro-ecological zone
# ------------------------------------------------------------------------------

agro_zone_counts <- fertilizer_data %>%
  count(
    Agro_Zone,
    name = "Observations"
  ) %>%
  arrange(
    desc(Observations)
  )

write.csv(
  agro_zone_counts,
  file.path(
    report_dir,
    "03_agro_zone_observation_counts.csv"
  ),
  row.names = FALSE
)


# ------------------------------------------------------------------------------
# 11.2 Agro-zone descriptive statistics
# ------------------------------------------------------------------------------

agro_zone_summary <- fertilizer_data %>%
  group_by(
    Agro_Zone
  ) %>%
  summarise(
    Observations = n(),

    Mean_Soil_pH = mean(
      Soil_pH,
      na.rm = TRUE
    ),

    Mean_Total_N_percent = mean(
      Total_N_percent,
      na.rm = TRUE
    ),

    Mean_Available_P_mgkg = mean(
      Available_P_mgkg,
      na.rm = TRUE
    ),

    Mean_Available_K_mgkg = mean(
      Available_K_mgkg,
      na.rm = TRUE
    ),

    Mean_Organic_Carbon_percent = mean(
      Organic_Carbon_percent,
      na.rm = TRUE
    ),

    Mean_Rainfall_30d_mm = mean(
      Rainfall_30d_mm,
      na.rm = TRUE
    ),

    Mean_Temperature_mean_C = mean(
      Temperature_mean_C,
      na.rm = TRUE
    ),

    Mean_Urea_kg_ha = mean(
      Urea_kg_ha,
      na.rm = TRUE
    ),

    Mean_TSP_kg_ha = mean(
      TSP_kg_ha,
      na.rm = TRUE
    ),

    Mean_MOP_kg_ha = mean(
      MOP_kg_ha,
      na.rm = TRUE
    ),

    .groups = "drop"
  ) %>%
  arrange(
    desc(Observations)
  )

write.csv(
  agro_zone_summary,
  file.path(
    report_dir,
    "03_agro_zone_summary.csv"
  ),
  row.names = FALSE
)


# ------------------------------------------------------------------------------
# 11.3 Agro-zone soil profile
# ------------------------------------------------------------------------------

agro_zone_soil_long <- fertilizer_data %>%
  group_by(
    Agro_Zone
  ) %>%
  summarise(
    Soil_pH = mean(
      Soil_pH,
      na.rm = TRUE
    ),

    Total_N_percent = mean(
      Total_N_percent,
      na.rm = TRUE
    ),

    Available_P_mgkg = mean(
      Available_P_mgkg,
      na.rm = TRUE
    ),

    Available_K_mgkg = mean(
      Available_K_mgkg,
      na.rm = TRUE
    ),

    Organic_Carbon_percent = mean(
      Organic_Carbon_percent,
      na.rm = TRUE
    ),

    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = -Agro_Zone,
    names_to = "Variable",
    values_to = "Mean_Value"
  )

write.csv(
  agro_zone_soil_long,
  file.path(
    report_dir,
    "03_agro_zone_soil_profile.csv"
  ),
  row.names = FALSE
)


# ------------------------------------------------------------------------------
# 11.4 Agro-zone observation plot
# ------------------------------------------------------------------------------

p_agro_zone_counts <- ggplot(
  agro_zone_counts,
  aes(
    x = reorder(
      Agro_Zone,
      Observations
    ),
    y = Observations
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Observations by Agro-Ecological Zone",
    x = "Agro-Ecological Zone",
    y = "Number of Observations"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  filename = file.path(
    figure_dir,
    "03_agro_zone_observation_counts.png"
  ),
  plot = p_agro_zone_counts,
  width = 10,
  height = 7,
  dpi = 300
)


# ------------------------------------------------------------------------------
# 11.5 Agro-zone fertilizer requirement comparison
# ------------------------------------------------------------------------------

agro_zone_fertilizer_long <- fertilizer_data %>%
  group_by(
    Agro_Zone
  ) %>%
  summarise(
    Urea_kg_ha = mean(
      Urea_kg_ha,
      na.rm = TRUE
    ),

    TSP_kg_ha = mean(
      TSP_kg_ha,
      na.rm = TRUE
    ),

    MOP_kg_ha = mean(
      MOP_kg_ha,
      na.rm = TRUE
    ),

    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(
      Urea_kg_ha,
      TSP_kg_ha,
      MOP_kg_ha
    ),
    names_to = "Fertilizer",
    values_to = "Mean_Rate_kg_ha"
  )

write.csv(
  agro_zone_fertilizer_long,
  file.path(
    report_dir,
    "03_agro_zone_fertilizer_summary.csv"
  ),
  row.names = FALSE
)

p_agro_zone_fertilizer <- ggplot(
  agro_zone_fertilizer_long,
  aes(
    x = Agro_Zone,
    y = Mean_Rate_kg_ha,
    fill = Fertilizer
  )
) +
  geom_col(
    position = "dodge"
  ) +
  labs(
    title = "Mean Fertilizer Application Rates by Agro-Ecological Zone",
    x = "Agro-Ecological Zone",
    y = "Mean Rate (kg/ha)",
    fill = "Fertilizer"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )

ggsave(
  filename = file.path(
    figure_dir,
    "03_agro_zone_fertilizer_comparison.png"
  ),
  plot = p_agro_zone_fertilizer,
  width = 11,
  height = 7,
  dpi = 300
)


# ==============================================================================
# 12. DISTRICT-LEVEL BREAKDOWN
# ==============================================================================

cat("\n------------------------------------------------------------\n")
cat("12. District-Level Breakdown\n")
cat("------------------------------------------------------------\n")


# ------------------------------------------------------------------------------
# 12.1 Observation count by district
# ------------------------------------------------------------------------------

district_counts <- fertilizer_data %>%
  count(
    District,
    name = "Observations"
  ) %>%
  arrange(
    desc(Observations)
  )

write.csv(
  district_counts,
  file.path(
    report_dir,
    "03_district_observation_counts.csv"
  ),
  row.names = FALSE
)


# ------------------------------------------------------------------------------
# 12.2 District descriptive summary
# ------------------------------------------------------------------------------

district_summary <- fertilizer_data %>%
  group_by(
    District
  ) %>%
  summarise(
    Observations = n(),

    Mean_Soil_pH = mean(
      Soil_pH,
      na.rm = TRUE
    ),

    Mean_Total_N_percent = mean(
      Total_N_percent,
      na.rm = TRUE
    ),

    Mean_Available_P_mgkg = mean(
      Available_P_mgkg,
      na.rm = TRUE
    ),

    Mean_Available_K_mgkg = mean(
      Available_K_mgkg,
      na.rm = TRUE
    ),

    Mean_Organic_Carbon_percent = mean(
      Organic_Carbon_percent,
      na.rm = TRUE
    ),

    Mean_Rainfall_30d_mm = mean(
      Rainfall_30d_mm,
      na.rm = TRUE
    ),

    Mean_Temperature_mean_C = mean(
      Temperature_mean_C,
      na.rm = TRUE
    ),

    Mean_Urea_kg_ha = mean(
      Urea_kg_ha,
      na.rm = TRUE
    ),

    Mean_TSP_kg_ha = mean(
      TSP_kg_ha,
      na.rm = TRUE
    ),

    Mean_MOP_kg_ha = mean(
      MOP_kg_ha,
      na.rm = TRUE
    ),

    .groups = "drop"
  ) %>%
  arrange(
    desc(Observations)
  )

write.csv(
  district_summary,
  file.path(
    report_dir,
    "03_district_summary.csv"
  ),
  row.names = FALSE
)


# ------------------------------------------------------------------------------
# 12.3 District observation count plot
# ------------------------------------------------------------------------------

p_district_counts <- ggplot(
  district_counts,
  aes(
    x = reorder(
      District,
      Observations
    ),
    y = Observations
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Observations by District",
    x = "District",
    y = "Number of Observations"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  filename = file.path(
    figure_dir,
    "03_district_observation_counts.png"
  ),
  plot = p_district_counts,
  width = 11,
  height = 9,
  dpi = 300
)


# ------------------------------------------------------------------------------
# 12.4 District-level mean fertilizer rates
# ------------------------------------------------------------------------------

district_fertilizer_long <- fertilizer_data %>%
  group_by(
    District
  ) %>%
  summarise(
    Urea_kg_ha = mean(
      Urea_kg_ha,
      na.rm = TRUE
    ),

    TSP_kg_ha = mean(
      TSP_kg_ha,
      na.rm = TRUE
    ),

    MOP_kg_ha = mean(
      MOP_kg_ha,
      na.rm = TRUE
    ),

    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(
      Urea_kg_ha,
      TSP_kg_ha,
      MOP_kg_ha
    ),
    names_to = "Fertilizer",
    values_to = "Mean_Rate_kg_ha"
  )

write.csv(
  district_fertilizer_long,
  file.path(
    report_dir,
    "03_district_fertilizer_summary.csv"
  ),
  row.names = FALSE
)

p_district_fertilizer <- ggplot(
  district_fertilizer_long,
  aes(
    x = District,
    y = Mean_Rate_kg_ha,
    fill = Fertilizer
  )
) +
  geom_col(
    position = "dodge"
  ) +
  labs(
    title = "Mean Fertilizer Application Rates by District",
    x = "District",
    y = "Mean Rate (kg/ha)",
    fill = "Fertilizer"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )

ggsave(
  filename = file.path(
    figure_dir,
    "03_district_fertilizer_comparison.png"
  ),
  plot = p_district_fertilizer,
  width = 13,
  height = 8,
  dpi = 300
)


# ==============================================================================
# 13. SEASONAL BREAKDOWN
# ==============================================================================

cat("\n------------------------------------------------------------\n")
cat("13. Seasonal Breakdown\n")
cat("------------------------------------------------------------\n")


season_summary <- fertilizer_data %>%
  group_by(
    Season
  ) %>%
  summarise(
    Observations = n(),

    Mean_Soil_pH = mean(
      Soil_pH,
      na.rm = TRUE
    ),

    Mean_Rainfall_30d_mm = mean(
      Rainfall_30d_mm,
      na.rm = TRUE
    ),

    Mean_Temperature_mean_C = mean(
      Temperature_mean_C,
      na.rm = TRUE
    ),

    Mean_Urea_kg_ha = mean(
      Urea_kg_ha,
      na.rm = TRUE
    ),

    Mean_TSP_kg_ha = mean(
      TSP_kg_ha,
      na.rm = TRUE
    ),

    Mean_MOP_kg_ha = mean(
      MOP_kg_ha,
      na.rm = TRUE
    ),

    .groups = "drop"
  )

write.csv(
  season_summary,
  file.path(
    report_dir,
    "03_season_summary.csv"
  ),
  row.names = FALSE
)


# ------------------------------------------------------------------------------
# 13.1 Seasonal observation plot
# ------------------------------------------------------------------------------

season_counts <- fertilizer_data %>%
  count(
    Season,
    name = "Observations"
  )

p_season_counts <- ggplot(
  season_counts,
  aes(
    x = Season,
    y = Observations
  )
) +
  geom_col() +
  labs(
    title = "Observations by Season",
    x = "Season",
    y = "Number of Observations"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  filename = file.path(
    figure_dir,
    "03_season_observation_counts.png"
  ),
  plot = p_season_counts,
  width = 9,
  height = 6,
  dpi = 300
)


# ==============================================================================
# 14. AGRO-ZONE × SEASON BREAKDOWN
# ==============================================================================

agro_zone_season_summary <- fertilizer_data %>%
  count(
    Agro_Zone,
    Season,
    name = "Observations"
  ) %>%
  arrange(
    Agro_Zone,
    Season
  )

write.csv(
  agro_zone_season_summary,
  file.path(
    report_dir,
    "03_agro_zone_season_breakdown.csv"
  ),
  row.names = FALSE
)


p_agro_zone_season <- ggplot(
  agro_zone_season_summary,
  aes(
    x = Agro_Zone,
    y = Observations,
    fill = Season
  )
) +
  geom_col(
    position = "dodge"
  ) +
  labs(
    title = "Observation Distribution Across Agro-Ecological Zones and Seasons",
    x = "Agro-Ecological Zone",
    y = "Number of Observations",
    fill = "Season"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )

ggsave(
  filename = file.path(
    figure_dir,
    "03_agro_zone_season_breakdown.png"
  ),
  plot = p_agro_zone_season,
  width = 12,
  height = 7,
  dpi = 300
)


# ==============================================================================
# 15. AGRO-ZONE DISTRIBUTION OF KEY SOIL VARIABLES
# ==============================================================================

# These plots provide an initial visual assessment of whether soil conditions
# differ across agro-ecological zones.
#
# This is descriptive EDA only. Formal statistical inference between groups
# belongs to Task 4.


for(variable in soil_variables) {

  p_zone_soil <- ggplot(
    fertilizer_data,
    aes(
      x = Agro_Zone,
      y = .data[[variable]]
    )
  ) +
    geom_boxplot(
      na.rm = TRUE
    ) +
    labs(
      title = paste(
        variable,
        "Across Agro-Ecological Zones"
      ),
      x = "Agro-Ecological Zone",
      y = variable
    ) +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.x = element_text(
        angle = 45,
        hjust = 1
      )
    )

  safe_filename <- paste0(
    "03_",
    variable,
    "_by_agro_zone.png"
  )

  ggsave(
    filename = file.path(
      figure_dir,
      safe_filename
    ),
    plot = p_zone_soil,
    width = 10,
    height = 7,
    dpi = 300
  )
}


# ==============================================================================
# 16. AGRO-ZONE DISTRIBUTION OF FERTILIZER VARIABLES
# ==============================================================================

for(variable in fertilizer_variables) {

  p_zone_fertilizer <- ggplot(
    fertilizer_data,
    aes(
      x = Agro_Zone,
      y = .data[[variable]]
    )
  ) +
    geom_boxplot(
      na.rm = TRUE
    ) +
    labs(
      title = paste(
        variable,
        "Across Agro-Ecological Zones"
      ),
      x = "Agro-Ecological Zone",
      y = variable
    ) +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.x = element_text(
        angle = 45,
        hjust = 1
      )
    )

  safe_filename <- paste0(
    "03_",
    variable,
    "_by_agro_zone.png"
  )

  ggsave(
    filename = file.path(
      figure_dir,
      safe_filename
    ),
    plot = p_zone_fertilizer,
    width = 10,
    height = 7,
    dpi = 300
  )
}


# ==============================================================================
# 17. AUTOMATED INITIAL INSIGHTS
# ==============================================================================

cat("\n------------------------------------------------------------\n")
cat("17. Generating Initial Insights\n")
cat("------------------------------------------------------------\n")


# ------------------------------------------------------------------------------
# 17.1 Missingness insight
# ------------------------------------------------------------------------------

if(total_missing_values == 0) {

  missing_insight <- paste(
    "No missing values were detected across the dataset."
  )

} else {

  highest_missing_variable <- missing_by_variable %>%
    arrange(
      desc(n_miss)
    ) %>%
    slice(1)

  missing_insight <- paste0(
    "Missing values were detected. The variable with the highest ",
    "number of missing observations was ",
    highest_missing_variable$variable,
    " with ",
    highest_missing_variable$n_miss,
    " missing observations."
  )
}


# ------------------------------------------------------------------------------
# 17.2 Outlier insight
# ------------------------------------------------------------------------------

highest_outlier_variable <- outlier_summary %>%
  arrange(
    desc(Outlier_Count)
  ) %>%
  slice(1)

outlier_insight <- paste0(
  "Using the 1.5 × IQR rule, the highest number of statistical ",
  "outlier observations was detected for ",
  highest_outlier_variable$Variable,
  " with ",
  highest_outlier_variable$Outlier_Count,
  " observations flagged (",
  round(
    highest_outlier_variable$Outlier_Percentage,
    2
  ),
  "% of valid observations)."
)


# ------------------------------------------------------------------------------
# 17.3 Agro-zone observation insight
# ------------------------------------------------------------------------------

largest_agro_zone <- agro_zone_counts %>%
  slice_max(
    order_by = Observations,
    n = 1,
    with_ties = FALSE
  )

smallest_agro_zone <- agro_zone_counts %>%
  slice_min(
    order_by = Observations,
    n = 1,
    with_ties = FALSE
  )

agro_zone_insight <- paste0(
  "The largest agro-ecological-zone sample contains ",
  largest_agro_zone$Observations,
  " observations (",
  largest_agro_zone$Agro_Zone,
  "). The smallest zone contains ",
  smallest_agro_zone$Observations,
  " observations (",
  smallest_agro_zone$Agro_Zone,
  ")."
)


# ------------------------------------------------------------------------------
# 17.4 District observation insight
# ------------------------------------------------------------------------------

largest_district <- district_counts %>%
  slice_max(
    order_by = Observations,
    n = 1,
    with_ties = FALSE
  )

smallest_district <- district_counts %>%
  slice_min(
    order_by = Observations,
    n = 1,
    with_ties = FALSE
  )

district_insight <- paste0(
  "The district with the largest number of observations is ",
  largest_district$District,
  " with ",
  largest_district$Observations,
  " observations. The district with the smallest number is ",
  smallest_district$District,
  " with ",
  smallest_district$Observations,
  " observations."
)


# ------------------------------------------------------------------------------
# 17.5 Strongest target correlation
# ------------------------------------------------------------------------------

strongest_target_correlations <- target_correlation_table %>%
  filter(
    !is.na(Absolute_Correlation)
  ) %>%
  group_by(
    Target
  ) %>%
  slice_max(
    order_by = Absolute_Correlation,
    n = 1,
    with_ties = FALSE
  ) %>%
  ungroup()

correlation_insights <- paste0(
  "Strongest absolute Pearson correlations with the main fertilizer ",
  "outcomes were identified as follows:\n"
)

for(i in seq_len(nrow(strongest_target_correlations))) {

  correlation_insights <- paste0(
    correlation_insights,
    "  - ",
    strongest_target_correlations$Target[i],
    " vs ",
    strongest_target_correlations$Predictor[i],
    ": r = ",
    round(
      strongest_target_correlations$Pearson_Correlation[i],
      3
    ),
    "\n"
  )
}


# ------------------------------------------------------------------------------
# 17.6 Mean soil conditions by agro-zone
# ------------------------------------------------------------------------------

highest_mean_ph_zone <- agro_zone_summary %>%
  slice_max(
    order_by = Mean_Soil_pH,
    n = 1,
    with_ties = FALSE
  )

lowest_mean_ph_zone <- agro_zone_summary %>%
  slice_min(
    order_by = Mean_Soil_pH,
    n = 1,
    with_ties = FALSE
  )

soil_zone_insight <- paste0(
  "Among agro-ecological zones, the highest mean soil pH was observed ",
  "in ",
  highest_mean_ph_zone$Agro_Zone,
  " (mean pH = ",
  round(
    highest_mean_ph_zone$Mean_Soil_pH,
    3
  ),
  "), while the lowest mean soil pH was observed in ",
  lowest_mean_ph_zone$Agro_Zone,
  " (mean pH = ",
  round(
    lowest_mean_ph_zone$Mean_Soil_pH,
    3
  ),
  ")."
)


# ------------------------------------------------------------------------------
# 17.7 Rainfall variation by agro-zone
# ------------------------------------------------------------------------------

highest_rainfall_zone <- agro_zone_summary %>%
  slice_max(
    order_by = Mean_Rainfall_30d_mm,
    n = 1,
    with_ties = FALSE
  )

lowest_rainfall_zone <- agro_zone_summary %>%
  slice_min(
    order_by = Mean_Rainfall_30d_mm,
    n = 1,
    with_ties = FALSE
  )

rainfall_zone_insight <- paste0(
  "Mean 30-day rainfall varied across agro-ecological zones. ",
  "The highest mean rainfall was observed in ",
  highest_rainfall_zone$Agro_Zone,
  " (",
  round(
    highest_rainfall_zone$Mean_Rainfall_30d_mm,
    2
  ),
  " mm), while the lowest was observed in ",
  lowest_rainfall_zone$Agro_Zone,
  " (",
  round(
    lowest_rainfall_zone$Mean_Rainfall_30d_mm,
    2
  ),
  " mm)."
)


# ==============================================================================
# 18. WRITE INITIAL INSIGHTS REPORT
# ==============================================================================

initial_insights <- paste0(
  "TASK 3 - INITIAL EDA INSIGHTS\n",
  "==============================================\n\n",

  "Dataset Overview\n",
  "-----------------\n",
  "Observations: ",
  nrow(fertilizer_data),
  "\n",
  "Variables: ",
  ncol(fertilizer_data),
  "\n",
  "Duplicate rows: ",
  duplicate_count,
  "\n\n",

  "1. Missingness\n",
  "--------------\n",
  missing_insight,
  "\n\n",

  "2. IQR Outlier Detection\n",
  "------------------------\n",
  outlier_insight,
  "\n",
  "The IQR rule identifies observations that are statistically extreme ",
  "relative to the distribution. These observations should not be ",
  "automatically removed because extreme agricultural conditions may ",
  "represent genuine field observations.\n\n",

  "3. Agro-Ecological Zone Distribution\n",
  "------------------------------------\n",
  agro_zone_insight,
  "\n\n",

  "4. District Distribution\n",
  "------------------------\n",
  district_insight,
  "\n\n",

  "5. Soil pH Variation Across Agro-Zones\n",
  "--------------------------------------\n",
  soil_zone_insight,
  "\n\n",

  "6. Rainfall Variation Across Agro-Zones\n",
  "---------------------------------------\n",
  rainfall_zone_insight,
  "\n\n",

  "7. Correlation Analysis\n",
  "-----------------------\n",
  correlation_insights,
  "\n",
  "Correlation measures statistical association and does not establish ",
  "causation. The observed relationships should therefore be considered ",
  "as exploratory evidence for subsequent statistical modelling.\n\n",

  "8. Next Statistical Steps\n",
  "-------------------------\n",
  "The descriptive patterns identified in Task 3 provide the basis for ",
  "formal statistical inference in Task 4. In particular, observed ",
  "differences between agro-ecological zones and other groups can be ",
  "evaluated using the planned ANOVA, Welch two-sample t-tests and ",
  "Levene's test procedures.\n"
)

cat(
  initial_insights,
  file = file.path(
    report_dir,
    "03_initial_insights.txt"
  )
)


# ==============================================================================
# 19. EDA COMPLETION SUMMARY
# ==============================================================================

completion_summary <- data.frame(
  Analysis = c(
    "Dataset structure",
    "Duplicate check",
    "Missing-value analysis",
    "naniar missingness visualization",
    "IQR outlier detection",
    "Boxplot analysis",
    "Distribution analysis",
    "Pearson correlation matrix",
    "corrplot visualization",
    "Agro-ecological zone breakdown",
    "District breakdown",
    "Seasonal breakdown",
    "Agro-zone × season breakdown",
    "Initial automated insights"
  ),
  Status = rep(
    "Completed",
    14
  )
)

write.csv(
  completion_summary,
  file.path(
    report_dir,
    "03_eda_completion_summary.csv"
  ),
  row.names = FALSE
)


# ==============================================================================
# 20. CONSOLE SUMMARY
# ==============================================================================

cat(
  "\n\n============================================================\n",
  "TASK 3 COMPLETED SUCCESSFULLY\n",
  "============================================================\n\n"
)

cat(
  "Dataset:\n",
  "  Observations: ",
  nrow(fertilizer_data),
  "\n",
  "  Variables: ",
  ncol(fertilizer_data),
  "\n",
  "  Missing values: ",
  total_missing_values,
  "\n",
  "  Duplicate rows: ",
  duplicate_count,
  "\n\n"
)

cat(
  "Output directories:\n",
  "  Figures: ",
  figure_dir,
  "\n",
  "  Reports: ",
  report_dir,
  "\n\n"
)

cat(
  "Major outputs generated:\n",
  "  - Missing-value summaries and naniar visualizations\n",
  "  - IQR outlier summary and observation-level flags\n",
  "  - Boxplots for soil, weather and fertilizer variables\n",
  "  - Distribution plots for requested soil/weather variables\n",
  "  - Pearson correlation matrices\n",
  "  - corrplot correlation visualizations\n",
  "  - Agro-ecological zone summaries\n",
  "  - District-level summaries\n",
  "  - Seasonal summaries\n",
  "  - Agro-zone × season breakdown\n",
  "  - Automated initial insights report\n\n"
)

cat(
  "Task 3 analysis is complete.\n",
  "The outputs can now be used as evidence for Task 4: Statistical Inference.\n\n"
)


# ==============================================================================
# 21. SESSION INFORMATION
# ==============================================================================

session_info_text <- capture.output(
  sessionInfo()
)

writeLines(
  session_info_text,
  con = file.path(
    report_dir,
    "03_session_info.txt"
  )
)


# ==============================================================================
# END OF TASK 3 SCRIPT
# ==============================================================================
