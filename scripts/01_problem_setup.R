# ==============================================================================
# IT3081 Statistical Modelling
# Data-Driven Precision Fertilizer Advisory & Strategic Agriculture Framework
# Task 1: Problem Understanding & Project Setup
#
# Save as:
#   scripts/01_problem_setup.R
#
# IMPORTANT:
#   Run this script with the RStudio working directory set to the project root:
#   Fertilizer_Consultancy_Project/
#
# Purpose:
#   1. Establish the consultancy problem and project scope.
#   2. Document stakeholders, objectives, target outcomes, predictors and
#      categorical dimensions.
#   3. Load and validate the supplied Fertilizer_Dataset.csv.
#   4. Generate a reproducible data dictionary and dataset inventory.
#   5. Record the statistical modelling roadmap for Tasks 1-12.
#
# This script intentionally does NOT perform EDA, hypothesis testing or
# predictive modelling. Those analyses belong to later project tasks.
# ==============================================================================


# ------------------------------------------------------------------------------
# 0. PROJECT DIRECTORY SAFETY
# ------------------------------------------------------------------------------

if(!dir.exists("output/figures")) dir.create("output/figures", recursive = TRUE)
if(!dir.exists("output/reports")) dir.create("output/reports", recursive = TRUE)

# The script is designed to be run from the project root. This check prevents
# accidental execution from another directory, which would cause relative-path
# file reads/writes to fail or target the wrong project.
if(!file.exists("Fertilizer_Dataset.csv")) {
  stop(
    paste0(
      "Fertilizer_Dataset.csv was not found in the current working directory.\n",
      "Set the RStudio working directory to the project root:\n",
      "Fertilizer_Consultancy_Project/"
    )
  )
}


# ------------------------------------------------------------------------------
# 1. PACKAGE SETUP
# ------------------------------------------------------------------------------

required_packages <- c("tidyverse")

missing_packages <- required_packages[
  !required_packages %in% rownames(installed.packages())
]

if(length(missing_packages) > 0) {
  stop(
    paste0(
      "The following required package(s) are not installed: ",
      paste(missing_packages, collapse = ", "),
      ".\nInstall them once using:\n",
      "install.packages(c(",
      paste0('"', missing_packages, '"', collapse = ", "),
      "))"
    )
  )
}

suppressPackageStartupMessages({
  library(tidyverse)
})


# ------------------------------------------------------------------------------
# 2. PROJECT PARAMETERS
# ------------------------------------------------------------------------------

project_name <- "Data-Driven Precision Fertilizer Advisory & Strategic Agriculture Framework"
module_code <- "IT3081 Statistical Modelling"
primary_technology <- "R"
dataset_file <- "Fertilizer_Dataset.csv"

# These parameters are derived from the supplied PRD. They provide a single
# documented source of project scope for subsequent scripts.
project_context <- paste(
  "Agriculture and fertilizer optimization in Sri Lanka."
)

business_problem <- paste(
  "Generalized fertilizer recommendations can contribute to soil degradation,",
  "unnecessary fertilizer expenditure, and volatile crop yields.",
  "The consultancy therefore evaluates soil parameters, geographical zones,",
  "and weather factors to support statistically informed fertilizer advisory",
  "recommendations."
)

consultancy_goal <- paste(
  "Operate as an independent Statistical Innovation Consultancy that provides",
  "rigorous statistical inference, predictive modelling, and strategic",
  "decision-support evidence for agricultural stakeholders."
)

# Primary continuous fertilizer recommendation outcomes identified in the PRD.
target_variables <- c(
  "Urea_kg_ha",
  "TSP_kg_ha",
  "MOP_kg_ha"
)

# Additional recommended nutrient-rate fields supplied in the dataset.
reference_recommendation_variables <- c(
  "Recommended_N_kg_ha",
  "Recommended_P2O5_kg_ha",
  "Recommended_K2O_kg_ha"
)

# Soil and environmental predictors explicitly represented in the dataset.
soil_predictors <- c(
  "Soil_pH",
  "Total_N_percent",
  "Available_P_mgkg",
  "Available_K_mgkg",
  "Organic_Carbon_percent"
)

weather_predictors <- c(
  "Rainfall_30d_mm",
  "Temperature_mean_C"
)

geographical_variables <- c(
  "District",
  "Agro_Zone"
)

seasonal_variables <- c(
  "Season"
)

identifier_variables <- c(
  "Sample_ID"
)


# ------------------------------------------------------------------------------
# 3. STAKEHOLDER DOCUMENTATION
# ------------------------------------------------------------------------------

# Stakeholders are described at the role level rather than assigning specific
# people. This keeps the project documentation reusable and aligned with the
# consultancy framing in the PRD.

stakeholders <- tibble(
  Stakeholder = c(
    "Senior agricultural decision-makers",
    "Agronomists / agricultural domain experts",
    "Farmers / growers",
    "Agricultural advisory and extension teams",
    "Statistical modelling / data science team",
    "Project / consultancy management"
  ),
  Information_Need = c(
    "Evidence for strategic fertilizer planning and resource allocation.",
    "Statistically defensible relationships between soil, weather, location and fertilizer requirements.",
    "Understandable fertilizer recommendations that account for field conditions.",
    "Decision-support evidence that can be translated into practical advisory guidance.",
    "Clean data, reproducible analysis, model diagnostics and measurable performance.",
    "Clear risks, assumptions, limitations, recommendations and implementation roadmap."
  ),
  Project_Value = c(
    "Supports evidence-based agricultural planning.",
    "Supports scientifically justified interpretation of statistical results.",
    "Supports more site-specific fertilizer decision-making.",
    "Provides a structured basis for advisory services.",
    "Provides reproducible statistical workflows.",
    "Creates a documented consultancy framework for later dashboard/decision-engine development."
  )
)

write.csv(
  stakeholders,
  "output/reports/01_stakeholder_map.csv",
  row.names = FALSE
)


# ------------------------------------------------------------------------------
# 4. CONSULTANCY OBJECTIVES
# ------------------------------------------------------------------------------

objectives <- tibble(
  Objective_ID = paste0("OBJ", sprintf("%02d", 1:6)),
  Objective = c(
    "Understand the structure and quality of the supplied fertilizer advisory dataset.",
    "Quantify relationships between soil properties, agro-ecological zones, seasons and weather factors.",
    "Test statistically meaningful differences across relevant agricultural groups.",
    "Develop and compare predictive statistical models for fertilizer recommendation outcomes.",
    "Evaluate experimental-design, dimensionality-reduction, Bayesian and time-dependent extensions.",
    "Translate statistical findings into an interpretable precision-agriculture decision-support framework."
  ),
  Success_Indicator = c(
    "Reproducible project setup, validated input file and documented data dictionary.",
    "Descriptive and inferential evidence identifies important agricultural patterns.",
    "Hypotheses are tested using appropriate statistical procedures and assumptions are assessed.",
    "Models are compared using out-of-sample performance and diagnostic evidence.",
    "Alternative methodologies are critically evaluated against project objectives.",
    "Findings can be communicated to agricultural and senior decision-making stakeholders."
  )
)

write.csv(
  objectives,
  "output/reports/01_consultancy_objectives.csv",
  row.names = FALSE
)


# ------------------------------------------------------------------------------
# 5. LOAD THE DATASET
# ------------------------------------------------------------------------------

# Required project-relative loading instruction:
# Do not replace this with a hard-coded path such as
# "/Users/<name>/Documents/Fertilizer_Consultancy_Project/Fertilizer_Dataset.csv".
fertilizer_data <- read.csv("Fertilizer_Dataset.csv")

# Preserve the original data frame name as a convenient project object.
df <- fertilizer_data


# ------------------------------------------------------------------------------
# 6. DATASET STRUCTURE VALIDATION
# ------------------------------------------------------------------------------

expected_columns <- c(
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

missing_expected_columns <- setdiff(expected_columns, names(df))
unexpected_columns <- setdiff(names(df), expected_columns)

if(length(missing_expected_columns) > 0) {
  stop(
    paste0(
      "Dataset validation failed. Missing expected column(s): ",
      paste(missing_expected_columns, collapse = ", ")
    )
  )
}

if(length(unexpected_columns) > 0) {
  warning(
    paste0(
      "The dataset contains additional column(s) not defined in the PRD-based ",
      "project schema: ",
      paste(unexpected_columns, collapse = ", "),
      ". These columns will be retained."
    )
  )
}


# ------------------------------------------------------------------------------
# 7. DATA DICTIONARY
# ------------------------------------------------------------------------------

# The descriptions below are based on the supplied PRD and the variable names
# in Fertilizer_Dataset.csv. Where the exact measurement protocol is not
# provided, the script explicitly avoids inventing one.

data_dictionary <- tibble(
  Variable = expected_columns,
  Role = case_when(
    Variable %in% identifier_variables ~ "Identifier",
    Variable %in% geographical_variables ~ "Categorical predictor",
    Variable %in% seasonal_variables ~ "Categorical predictor",
    Variable %in% soil_predictors ~ "Continuous predictor",
    Variable %in% weather_predictors ~ "Continuous predictor",
    Variable %in% reference_recommendation_variables ~ "Reference recommendation variable",
    Variable %in% target_variables ~ "Primary target variable",
    TRUE ~ "Dataset variable"
  ),
  Description = case_when(
    Variable == "Sample_ID" ~ "Unique sample identifier.",
    Variable == "District" ~ "Administrative/geographical district associated with the sample.",
    Variable == "Agro_Zone" ~ "Agro-ecological zone classification.",
    Variable == "Season" ~ "Cultivation season; the PRD specifically identifies Maha and Yala as relevant seasonal categories.",
    Variable == "Soil_pH" ~ "Soil pH measurement.",
    Variable == "Total_N_percent" ~ "Total soil nitrogen, expressed as a percentage.",
    Variable == "Available_P_mgkg" ~ "Available soil phosphorus, expressed in mg/kg.",
    Variable == "Available_K_mgkg" ~ "Available soil potassium, expressed in mg/kg.",
    Variable == "Organic_Carbon_percent" ~ "Soil organic carbon, expressed as a percentage.",
    Variable == "Rainfall_30d_mm" ~ "Rainfall accumulated over the preceding 30 days, expressed in mm.",
    Variable == "Temperature_mean_C" ~ "Mean temperature, expressed in degrees Celsius.",
    Variable == "Recommended_N_kg_ha" ~ "Reference recommended nitrogen rate, expressed in kg/ha.",
    Variable == "Recommended_P2O5_kg_ha" ~ "Reference recommended phosphorus rate as P2O5, expressed in kg/ha.",
    Variable == "Recommended_K2O_kg_ha" ~ "Reference recommended potassium rate as K2O, expressed in kg/ha.",
    Variable == "Urea_kg_ha" ~ "Urea fertilizer recommendation/rate, expressed in kg/ha.",
    Variable == "TSP_kg_ha" ~ "Triple superphosphate (TSP) fertilizer recommendation/rate, expressed in kg/ha.",
    Variable == "MOP_kg_ha" ~ "Muriate of potash (MOP) fertilizer recommendation/rate, expressed in kg/ha.",
    TRUE ~ "Not documented."
  ),
  Data_Type = vapply(df[expected_columns], function(x) class(x)[1], character(1)),
  Missing_N = vapply(df[expected_columns], function(x) sum(is.na(x)), integer(1)),
  Missing_Percent = round(
    vapply(df[expected_columns], function(x) mean(is.na(x)) * 100, numeric(1)),
    2
  ),
  Unique_Values = vapply(df[expected_columns], function(x) dplyr::n_distinct(x, na.rm = TRUE), integer(1))
)

write.csv(
  data_dictionary,
  "output/reports/01_data_dictionary.csv",
  row.names = FALSE
)


# ------------------------------------------------------------------------------
# 8. DATASET INVENTORY
# ------------------------------------------------------------------------------

# This is a structural inventory, not an EDA. It records dataset dimensions,
# variable classes, missingness and category counts so later scripts can start
# from a documented baseline.

categorical_inventory <- tibble(
  Variable = c("District", "Agro_Zone", "Season"),
  Unique_N = c(
    n_distinct(df$District, na.rm = TRUE),
    n_distinct(df$Agro_Zone, na.rm = TRUE),
    n_distinct(df$Season, na.rm = TRUE)
  )
)

dataset_inventory <- tibble(
  Metric = c(
    "Rows",
    "Columns",
    "Primary target variables",
    "Soil predictor variables",
    "Weather predictor variables",
    "Geographical variables",
    "Seasonal variables",
    "Total missing cells"
  ),
  Value = c(
    nrow(df),
    ncol(df),
    length(target_variables),
    length(soil_predictors),
    length(weather_predictors),
    length(geographical_variables),
    length(seasonal_variables),
    sum(is.na(df))
  )
)

write.csv(
  dataset_inventory,
  "output/reports/01_dataset_inventory.csv",
  row.names = FALSE
)

write.csv(
  categorical_inventory,
  "output/reports/01_categorical_inventory.csv",
  row.names = FALSE
)


# ------------------------------------------------------------------------------
# 9. TARGET AND PREDICTOR REGISTRY
# ------------------------------------------------------------------------------

variable_registry <- bind_rows(
  tibble(
    Variable_Group = "Primary Target",
    Variable = target_variables,
    Purpose = "Continuous fertilizer recommendation outcome for predictive modelling."
  ),
  tibble(
    Variable_Group = "Soil Predictor",
    Variable = soil_predictors,
    Purpose = "Soil condition / nutrient predictors identified for agricultural analysis."
  ),
  tibble(
    Variable_Group = "Weather Predictor",
    Variable = weather_predictors,
    Purpose = "Weather-related predictors available for modelling and interpretation."
  ),
  tibble(
    Variable_Group = "Geographical Predictor",
    Variable = geographical_variables,
    Purpose = "Location-related categorical variables for group comparisons and modelling."
  ),
  tibble(
    Variable_Group = "Seasonal Predictor",
    Variable = seasonal_variables,
    Purpose = "Cultivation-season variable for seasonal comparisons and modelling."
  ),
  tibble(
    Variable_Group = "Reference Recommendation",
    Variable = reference_recommendation_variables,
    Purpose = "Reference nutrient recommendation fields supplied with the dataset."
  )
)

write.csv(
  variable_registry,
  "output/reports/01_variable_registry.csv",
  row.names = FALSE
)


# ------------------------------------------------------------------------------
# 10. STATISTICAL ANALYSIS ROADMAP
# ------------------------------------------------------------------------------

analysis_roadmap <- tibble(
  Task = 1:12,
  Focus = c(
    "Problem Understanding & Setup",
    "Research Landscape & Literature Synthesis",
    "Data Quality, EDA & Initial Insights",
    "Statistical Inference Evidence",
    "Predictive Statistical Modelling & Diagnostics",
    "Experimental Design Evaluation: CRD vs RCBD",
    "Dimensionality Reduction via PCA",
    "Bayesian Statistical Methods Evaluation",
    "Time Series Extension Roadmap",
    "Industry Innovation Proposal",
    "Industry Expert Validation",
    "Board Consultancy Recommendations"
  ),
  Main_Methods_or_Output = c(
    "Project scope, stakeholders, objectives, data dictionary and reproducibility setup.",
    "Synthesis of 16 selected references; identify methods, limitations and methodological justification.",
    "Missingness, IQR/boxplots, distributions, correlations and categorical breakdowns.",
    "One-way ANOVA, Welch two-sample t-tests and Levene's test; report F/t statistics, p-values and confidence intervals.",
    "MLR, Ridge, LASSO and GLM; diagnostics and comparison using R2, adjusted R2, RMSE and MAE.",
    "Critical evaluation of CRD and RCBD, including blocking by agro-ecological zone or soil-moisture gradients.",
    "prcomp with scaling; scree plot, cumulative variance and biplot; interpret predictive efficiency versus stakeholder interpretability.",
    "Naive Bayes and Bayesian linear regression compared conceptually with frequentist approaches.",
    "ARIMA / seasonal decomposition roadmap for seasonal and rainfall-related temporal patterns.",
    "Precision agriculture advisory dashboard and decision-engine framework.",
    "Structured expert feedback collection and documented revisions.",
    "Strategic, operational, risk/ethical and future-research recommendations."
  )
)

write.csv(
  analysis_roadmap,
  "output/reports/01_analysis_roadmap.csv",
  row.names = FALSE
)


# ------------------------------------------------------------------------------
# 11. PROJECT SETUP REPORT
# ------------------------------------------------------------------------------

# The report combines the PRD-defined scope with dataset facts calculated
# directly from the supplied CSV. It is intended to be readable by both the
# technical team and non-technical project stakeholders.

report_file <- "output/reports/01_project_setup_summary.txt"

missing_total <- sum(is.na(df))
missing_pct_total <- round(100 * missing_total / (nrow(df) * ncol(df)), 2)

report_lines <- c(
  "==========================================================================",
  "IT3081 STATISTICAL MODELLING",
  "DATA-DRIVEN PRECISION FERTILIZER ADVISORY & STRATEGIC AGRICULTURE FRAMEWORK",
  "TASK 1: PROBLEM UNDERSTANDING & PROJECT SETUP",
  "==========================================================================",
  "",
  paste("Project:", project_name),
  paste("Module:", module_code),
  paste("Primary technology:", primary_technology),
  paste("Input dataset:", dataset_file),
  "",
  "1. INDUSTRY CONTEXT",
  project_context,
  "",
  "2. BUSINESS PROBLEM",
  business_problem,
  "",
  "3. CONSULTANCY GOAL",
  consultancy_goal,
  "",
  "4. PRIMARY TARGET OUTCOMES",
  paste(target_variables, collapse = ", "),
  "",
  "5. CORE SOIL PREDICTORS",
  paste(soil_predictors, collapse = ", "),
  "",
  "6. WEATHER PREDICTORS",
  paste(weather_predictors, collapse = ", "),
  "",
  "7. GEOGRAPHICAL / SEASONAL DIMENSIONS",
  paste(c(geographical_variables, seasonal_variables), collapse = ", "),
  "",
  "8. DATASET STRUCTURE",
  paste("Rows:", nrow(df)),
  paste("Columns:", ncol(df)),
  paste("Total missing cells:", missing_total),
  paste("Overall missing-cell percentage:", missing_pct_total, "%"),
  "",
  "9. IMPORTANT DATA INTERPRETATION NOTE",
  paste(
    "The dataset contains both reference recommended nutrient-rate variables",
    "and primary fertilizer recommendation variables (Urea, TSP and MOP).",
    "Their exact causal/derivation relationship is not established by Task 1",
    "alone and should be investigated in later EDA and modelling tasks rather",
    "than assumed."
  ),
  "",
  "10. STATISTICAL WORKFLOW",
  paste(
    "The project follows the PRD roadmap from data quality and EDA through",
    "statistical inference, predictive modelling, experimental-design evaluation,",
    "PCA, Bayesian methods, time-series extensions and strategic decision support."
  ),
  "",
  "11. REPRODUCIBILITY",
  "All project scripts should use project-relative paths.",
  "Input data should remain in the project root unless the project structure is explicitly changed.",
  "Generated figures belong in output/figures/ and reports/tables belong in output/reports/.",
  "",
  "==========================================================================",
  "END OF TASK 1 SETUP REPORT",
  "=========================================================================="
)

writeLines(report_lines, report_file)


# ------------------------------------------------------------------------------
# 12. SESSION / REPRODUCIBILITY INFORMATION
# ------------------------------------------------------------------------------

# Recording the R session helps the team reproduce results later if package or
# R-version differences cause changes in statistical output.

capture.output(
  sessionInfo(),
  file = "output/reports/01_session_info.txt"
)


# ------------------------------------------------------------------------------
# 13. CONSOLE SUMMARY
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("TASK 1 - PROJECT SETUP COMPLETED\n")
cat("============================================================\n")
cat("Dataset:", dataset_file, "\n")
cat("Rows:", nrow(df), "\n")
cat("Columns:", ncol(df), "\n")
cat("Missing cells:", sum(is.na(df)), "\n")
cat("Primary targets:", paste(target_variables, collapse = ", "), "\n")
cat("\nGenerated reports:\n")
cat("  - output/reports/01_project_setup_summary.txt\n")
cat("  - output/reports/01_data_dictionary.csv\n")
cat("  - output/reports/01_dataset_inventory.csv\n")
cat("  - output/reports/01_categorical_inventory.csv\n")
cat("  - output/reports/01_variable_registry.csv\n")
cat("  - output/reports/01_consultancy_objectives.csv\n")
cat("  - output/reports/01_stakeholder_map.csv\n")
cat("  - output/reports/01_analysis_roadmap.csv\n")
cat("  - output/reports/01_session_info.txt\n")
cat("============================================================\n")
cat("Task 1 is complete. Proceed to Task 2 / Task 3 only after\n")
cat("reviewing the generated setup and data dictionary outputs.\n")
cat("============================================================\n\n")
