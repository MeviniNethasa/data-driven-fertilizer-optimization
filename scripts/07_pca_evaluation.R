# scripts/07_pca_evaluation.R

# ============================================================
# IT3081 Statistical Modelling - Consultancy Project
# Task 7: Principal Component Analysis (PCA) Evaluation
# Data-Driven Precision Fertilizer Advisory & Strategic Agriculture Framework
# ============================================================

# ------------------------------------------------------------
# 0. DIRECTORY SAFETY
# ------------------------------------------------------------

if(!dir.exists("output/figures")) dir.create("output/figures", recursive = TRUE)
if(!dir.exists("output/reports")) dir.create("output/reports", recursive = TRUE)

# ------------------------------------------------------------
# 1. PACKAGE CHECK
# ------------------------------------------------------------

required_packages <- c(
  "tidyverse",
  "factoextra",
  "ggplot2",
  "gridExtra"
)

missing_packages <- required_packages[
  !required_packages %in% rownames(installed.packages())
]

if(length(missing_packages) > 0){
  stop(
    paste0(
      "Missing required packages: ",
      paste(missing_packages, collapse = ", "),
      "\nInstall them with:\n\n",
      "install.packages(c(",
      paste0('"', missing_packages, '"', collapse = ", "),
      "))"
    )
  )
}

suppressPackageStartupMessages({
  library(tidyverse)
  library(factoextra)
  library(ggplot2)
  library(gridExtra)
})

# ------------------------------------------------------------
# 2. PROJECT PATHS AND SETTINGS
# ------------------------------------------------------------

set.seed(123)

data_file <- "Fertilizer_Dataset.csv"
figure_dir <- "output/figures"
report_dir <- "output/reports"

# ------------------------------------------------------------
# 3. LOAD DATA
# ------------------------------------------------------------

if(!file.exists(data_file)){
  stop(
    paste0(
      "Dataset not found: ", data_file,
      "\nCurrent working directory: ", getwd(),
      "\nRun this script from the project root."
    )
  )
}

fertilizer_data <- read.csv(
  data_file,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

fertilizer_data <- as.data.frame(fertilizer_data)

# ------------------------------------------------------------
# 4. IDENTIFY REQUIRED PCA VARIABLES
# ------------------------------------------------------------

# The task specification uses these names:
#
#   30day_cumulative_rainfall_mm
#   mean_temperature_C
#
# The project dataset may use:
#
#   Rainfall_30d_mm
#   Temperature_mean_C
#
# Both naming conventions are supported.

rainfall_candidates <- c(
  "30day_cumulative_rainfall_mm",
  "Rainfall_30d_mm"
)

temperature_candidates <- c(
  "mean_temperature_C",
  "Temperature_mean_C"
)

rainfall_variable <- intersect(
  rainfall_candidates,
  names(fertilizer_data)
)[1]

temperature_variable <- intersect(
  temperature_candidates,
  names(fertilizer_data)
)[1]

if(is.na(rainfall_variable)){
  stop(
    paste0(
      "Rainfall variable not found.\nExpected one of: ",
      paste(rainfall_candidates, collapse = ", ")
    )
  )
}

if(is.na(temperature_variable)){
  stop(
    paste0(
      "Temperature variable not found.\nExpected one of: ",
      paste(temperature_candidates, collapse = ", ")
    )
  )
}

# ------------------------------------------------------------
# 5. REQUIRED PCA VARIABLES
# ------------------------------------------------------------

pca_source_variables <- c(
  "Soil_pH",
  "Total_N_percent",
  "Available_P_mgkg",
  "Available_K_mgkg",
  "Organic_Carbon_percent",
  rainfall_variable,
  temperature_variable
)

missing_variables <- setdiff(
  pca_source_variables,
  names(fertilizer_data)
)

if(length(missing_variables) > 0){
  stop(
    paste0(
      "The following required PCA variables are missing:\n",
      paste(missing_variables, collapse = ", ")
    )
  )
}

# ------------------------------------------------------------
# 6. CREATE PCA DATASET
# ------------------------------------------------------------

pca_data <- fertilizer_data[
  ,
  pca_source_variables,
  drop = FALSE
]

# Rename the two weather variables to the terminology required
# by the Task 7 specification.
names(pca_data) <- c(
  "Soil_pH",
  "Total_N_percent",
  "Available_P_mgkg",
  "Available_K_mgkg",
  "Organic_Carbon_percent",
  "30day_cumulative_rainfall_mm",
  "mean_temperature_C"
)

# Explicit numeric conversion.
for(variable in names(pca_data)){
  pca_data[[variable]] <- as.numeric(
    as.character(pca_data[[variable]])
  )
}

# ------------------------------------------------------------
# 7. MISSING VALUE CHECK
# ------------------------------------------------------------

missing_summary <- data.frame(
  Variable = names(pca_data),
  Missing_Count = sapply(
    pca_data,
    function(x) sum(is.na(x))
  ),
  Missing_Percentage = sapply(
    pca_data,
    function(x) mean(is.na(x)) * 100
  ),
  stringsAsFactors = FALSE
)

readr::write_csv(
  as.data.frame(missing_summary),
  file.path(
    report_dir,
    "07_pca_missingness_summary.csv"
  )
)

# ------------------------------------------------------------
# 8. COMPLETE-CASE CLEANING
# ------------------------------------------------------------

complete_case_index <- complete.cases(pca_data)

pca_data_complete <- pca_data[
  complete_case_index,
  ,
  drop = FALSE
]

pca_data_complete <- as.data.frame(
  pca_data_complete
)

if(nrow(pca_data_complete) < 3){
  stop(
    "Fewer than three complete observations are available. PCA cannot be performed reliably."
  )
}

# ------------------------------------------------------------
# 9. CHECK ZERO-VARIANCE VARIABLES
# ------------------------------------------------------------

variable_sd <- sapply(
  pca_data_complete,
  sd,
  na.rm = TRUE
)

zero_variance_variables <- names(
  variable_sd[
    !is.finite(variable_sd) |
      variable_sd == 0
  ]
)

if(length(zero_variance_variables) > 0){
  stop(
    paste0(
      "The following PCA variables have zero or non-finite variance: ",
      paste(
        zero_variance_variables,
        collapse = ", "
      )
    )
  )
}

# ------------------------------------------------------------
# 10. SAMPLE SUMMARY
# ------------------------------------------------------------

sample_summary <- data.frame(
  Metric = c(
    "Original observations",
    "Complete observations",
    "Removed observations",
    "Complete-case percentage",
    "Number of PCA predictors"
  ),
  Value = c(
    nrow(pca_data),
    nrow(pca_data_complete),
    nrow(pca_data) - nrow(pca_data_complete),
    round(
      100 * nrow(pca_data_complete) / nrow(pca_data),
      2
    ),
    ncol(pca_data_complete)
  ),
  stringsAsFactors = FALSE
)

readr::write_csv(
  as.data.frame(sample_summary),
  file.path(
    report_dir,
    "07_pca_sample_summary.csv"
  )
)

# ------------------------------------------------------------
# 11. DESCRIPTIVE STATISTICS
# ------------------------------------------------------------

descriptive_statistics <- data.frame(
  Variable = names(pca_data_complete),
  Mean = sapply(
    pca_data_complete,
    mean,
    na.rm = TRUE
  ),
  Standard_Deviation = sapply(
    pca_data_complete,
    sd,
    na.rm = TRUE
  ),
  Minimum = sapply(
    pca_data_complete,
    min,
    na.rm = TRUE
  ),
  Median = sapply(
    pca_data_complete,
    median,
    na.rm = TRUE
  ),
  Maximum = sapply(
    pca_data_complete,
    max,
    na.rm = TRUE
  ),
  stringsAsFactors = FALSE
)

readr::write_csv(
  as.data.frame(descriptive_statistics),
  file.path(
    report_dir,
    "07_pca_input_descriptive_statistics.csv"
  )
)

# ------------------------------------------------------------
# 12. CORRELATION MATRIX
# ------------------------------------------------------------

correlation_matrix <- cor(
  pca_data_complete,
  method = "pearson",
  use = "complete.obs"
)

correlation_table <- as.data.frame(
  correlation_matrix
)

correlation_table <- tibble::rownames_to_column(
  correlation_table,
  var = "Variable"
)

readr::write_csv(
  as.data.frame(correlation_table),
  file.path(
    report_dir,
    "07_pca_input_correlation_matrix.csv"
  )
)

# ------------------------------------------------------------
# 13. PRINCIPAL COMPONENT ANALYSIS
# ------------------------------------------------------------

# scale. = TRUE standardizes every variable before PCA.
#
# This is important because the predictors have different units:
#
# Soil pH                  -> pH units
# Total N                  -> percentage
# Available P/K            -> mg/kg
# Organic Carbon           -> percentage
# Rainfall                 -> mm
# Temperature              -> degrees Celsius
#
# Without standardization, variables with larger numerical scales
# could dominate the principal components.

pca_model <- prcomp(
  pca_data_complete,
  center = TRUE,
  scale. = TRUE
)

# ------------------------------------------------------------
# 14. EIGENVALUES AND VARIANCE EXPLAINED
# ------------------------------------------------------------

eigenvalues <- pca_model$sdev^2

variance_proportion <- eigenvalues /
  sum(eigenvalues)

variance_percentage <- variance_proportion * 100

cumulative_proportion <- cumsum(
  variance_proportion
)

cumulative_percentage <- cumulative_proportion * 100

pca_variance_table <- data.frame(
  Principal_Component = paste0(
    "PC",
    seq_along(eigenvalues)
  ),
  Eigenvalue = eigenvalues,
  Variance_Proportion = variance_proportion,
  Variance_Percentage = variance_percentage,
  Cumulative_Proportion = cumulative_proportion,
  Cumulative_Percentage = cumulative_percentage,
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------
# 15. PCA LOADINGS
# ------------------------------------------------------------

loadings_matrix <- pca_model$rotation

loadings_table <- as.data.frame(
  loadings_matrix
)

loadings_table <- tibble::rownames_to_column(
  loadings_table,
  var = "Variable"
)

# ------------------------------------------------------------
# 16. LONG-FORM LOADING TABLE
# ------------------------------------------------------------

loading_long <- loadings_table %>%
  pivot_longer(
    cols = -Variable,
    names_to = "Principal_Component",
    values_to = "Loading"
  ) %>%
  mutate(
    Absolute_Loading = abs(Loading),
    Squared_Loading = Loading^2
  )

# ------------------------------------------------------------
# 17. DOMINANT LOADINGS FOR PC1, PC2 AND PC3
# ------------------------------------------------------------

dominant_loadings <- loading_long %>%
  filter(
    Principal_Component %in% c(
      "PC1",
      "PC2",
      "PC3"
    )
  ) %>%
  arrange(
    Principal_Component,
    desc(Absolute_Loading)
  ) %>%
  group_by(
    Principal_Component
  ) %>%
  mutate(
    Loading_Rank = row_number()
  ) %>%
  ungroup()

readr::write_csv(
  as.data.frame(dominant_loadings),
  file.path(
    report_dir,
    "07_pca_loading_details.csv"
  )
)

# ------------------------------------------------------------
# 18. COMPLETE PCA SUMMARY
# ------------------------------------------------------------

# The requested summary combines:
#
#   - Eigenvalues
#   - Variance explained
#   - Cumulative variance
#   - Component loadings
#
# The resulting file is in long format so that every
# variable-component loading is explicitly represented.

pca_summary_metrics <- loading_long %>%
  left_join(
    pca_variance_table,
    by = "Principal_Component"
  ) %>%
  select(
    Principal_Component,
    Eigenvalue,
    Variance_Proportion,
    Variance_Percentage,
    Cumulative_Proportion,
    Cumulative_Percentage,
    Variable,
    Loading,
    Absolute_Loading
  )

readr::write_csv(
  as.data.frame(pca_summary_metrics),
  file.path(
    report_dir,
    "07_pca_summary_metrics.csv"
  )
)

# Also save the complete loading matrix.
readr::write_csv(
  as.data.frame(loadings_table),
  file.path(
    report_dir,
    "07_pca_loadings_matrix.csv"
  )
)

# ------------------------------------------------------------
# 19. IDENTIFY DIMENSIONALITY REDUCTION THRESHOLDS
# ------------------------------------------------------------

number_original_predictors <- ncol(
  pca_data_complete
)

number_eigenvalue_gt_one <- sum(
  eigenvalues > 1
)

components_80 <- which(
  cumulative_percentage >= 80
)[1]

components_90 <- which(
  cumulative_percentage >= 90
)[1]

components_95 <- which(
  cumulative_percentage >= 95
)[1]

dimensionality_assessment <- data.frame(
  Metric = c(
    "Original numerical predictors",
    "Total principal components",
    "Components with eigenvalue > 1",
    "Components explaining >= 80% variance",
    "Components explaining >= 90% variance",
    "Components explaining >= 95% variance"
  ),
  Value = c(
    number_original_predictors,
    length(eigenvalues),
    number_eigenvalue_gt_one,
    components_80,
    components_90,
    components_95
  ),
  stringsAsFactors = FALSE
)

readr::write_csv(
  as.data.frame(dimensionality_assessment),
  file.path(
    report_dir,
    "07_pca_dimensionality_assessment.csv"
  )
)

# ------------------------------------------------------------
# 20. SCREE PLOT USING FACTOEXTRA
# ------------------------------------------------------------

scree_plot <- factoextra::fviz_eig(
  pca_model,
  addlabels = TRUE,
  ylim = c(
    0,
    max(variance_percentage) * 1.15
  )
) +
  labs(
    title = "PCA Scree Plot",
    subtitle = "Percentage of standardized variance explained",
    x = "Principal Component",
    y = "Variance Explained (%)"
  ) +
  theme_minimal(
    base_size = 12
  ) +
  theme(
    plot.title = element_text(
      face = "bold"
    )
  )

ggsave(
  filename = file.path(
    figure_dir,
    "07_pca_scree_plot.png"
  ),
  plot = scree_plot,
  width = 9,
  height = 6,
  dpi = 300
)

# ------------------------------------------------------------
# 21. PCA BIPLOT
# ------------------------------------------------------------

biplot <- factoextra::fviz_pca_biplot(
  pca_model,
  geom.ind = "point",
  geom.var = c(
    "arrow",
    "text"
  ),
  alpha.ind = 0.20,
  col.ind = "grey40",
  col.var = "black",
  repel = TRUE,
  label = "var",
  pointsize = 1.2,
  arrowsize = 0.7
) +
  labs(
    title = "PCA Biplot: Soil Fertility and Climate Predictors",
    subtitle = paste0(
      "Complete observations used: ",
      format(
        nrow(pca_data_complete),
        big.mark = ","
      )
    ),
    x = paste0(
      "PC1 (",
      round(
        variance_percentage[1],
        1
      ),
      "%)"
    ),
    y = paste0(
      "PC2 (",
      round(
        variance_percentage[2],
        1
      ),
      "%)"
    )
  ) +
  theme_minimal(
    base_size = 12
  ) +
  theme(
    plot.title = element_text(
      face = "bold"
    )
  )

ggsave(
  filename = file.path(
    figure_dir,
    "07_pca_biplot.png"
  ),
  plot = biplot,
  width = 10,
  height = 8,
  dpi = 300
)

# ------------------------------------------------------------
# 22. VARIABLE LOADING PLOT USING FACTOEXTRA
# ------------------------------------------------------------

variable_loading_plot <- factoextra::fviz_pca_var(
  pca_model,
  axes = c(
    1,
    2
  ),
  col.var = "contrib",
  gradient.cols = c(
    "grey20",
    "grey60",
    "grey90"
  ),
  repel = TRUE
) +
  labs(
    title = "PCA Variable Loadings",
    subtitle = "Contribution of original variables to PC1 and PC2",
    x = paste0(
      "PC1 (",
      round(
        variance_percentage[1],
        1
      ),
      "%)"
    ),
    y = paste0(
      "PC2 (",
      round(
        variance_percentage[2],
        1
      ),
      "%)"
    )
  ) +
  theme_minimal(
    base_size = 12
  ) +
  theme(
    plot.title = element_text(
      face = "bold"
    )
  )

ggsave(
  filename = file.path(
    figure_dir,
    "07_pca_variable_loadings.png"
  ),
  plot = variable_loading_plot,
  width = 9,
  height = 7,
  dpi = 300
)

# ------------------------------------------------------------
# 23. ADDITIONAL PC1-PC3 LOADING HEATMAP
# ------------------------------------------------------------

loading_heatmap_data <- loading_long %>%
  filter(
    Principal_Component %in% c(
      "PC1",
      "PC2",
      "PC3"
    )
  ) %>%
  mutate(
    Principal_Component = factor(
      Principal_Component,
      levels = c(
        "PC1",
        "PC2",
        "PC3"
      )
    )
  )

loading_heatmap <- ggplot(
  loading_heatmap_data,
  aes(
    x = Principal_Component,
    y = Variable,
    fill = Loading
  )
) +
  geom_tile(
    colour = "white",
    linewidth = 0.6
  ) +
  geom_text(
    aes(
      label = round(
        Loading,
        2
      )
    ),
    size = 3.2
  ) +
  scale_fill_gradient2(
    low = "grey20",
    mid = "white",
    high = "grey80",
    midpoint = 0
  ) +
  labs(
    title = "PCA Variable Loading Heatmap",
    subtitle = "PC1, PC2 and PC3",
    x = "Principal Component",
    y = "Original Variable",
    fill = "Loading"
  ) +
  theme_minimal(
    base_size = 11
  ) +
  theme(
    plot.title = element_text(
      face = "bold"
    )
  )

ggsave(
  filename = file.path(
    figure_dir,
    "07_pca_loading_heatmap_pc1_pc3.png"
  ),
  plot = loading_heatmap,
  width = 9,
  height = 7,
  dpi = 300
)

# ------------------------------------------------------------
# 24. CUMULATIVE VARIANCE PLOT
# ------------------------------------------------------------

cumulative_variance_plot <- ggplot(
  pca_variance_table,
  aes(
    x = Principal_Component,
    y = Cumulative_Percentage,
    group = 1
  )
) +
  geom_line(
    linewidth = 1
  ) +
  geom_point(
    size = 3
  ) +
  geom_hline(
    yintercept = 80,
    linetype = "dashed"
  ) +
  geom_hline(
    yintercept = 90,
    linetype = "dashed"
  ) +
  scale_y_continuous(
    limits = c(
      0,
      100
    ),
    breaks = seq(
      0,
      100,
      10
    )
  ) +
  labs(
    title = "Cumulative PCA Variance Explained",
    subtitle = "Assessment of potential dimensionality reduction",
    x = "Principal Component",
    y = "Cumulative Variance Explained (%)"
  ) +
  theme_minimal(
    base_size = 12
  ) +
  theme(
    plot.title = element_text(
      face = "bold"
    )
  )

ggsave(
  filename = file.path(
    figure_dir,
    "07_pca_cumulative_variance.png"
  ),
  plot = cumulative_variance_plot,
  width = 9,
  height = 6,
  dpi = 300
)

# ------------------------------------------------------------
# 25. PCA SCORE OUTPUT
# ------------------------------------------------------------

pca_scores <- as.data.frame(
  pca_model$x
)

pca_scores$Observation <- seq_len(
  nrow(pca_scores)
)

pca_scores <- pca_scores %>%
  select(
    Observation,
    everything()
  )

readr::write_csv(
  as.data.frame(pca_scores),
  file.path(
    report_dir,
    "07_pca_scores.csv"
  )
)

# ------------------------------------------------------------
# 26. HELPER FUNCTION FOR DOMINANT VARIABLES
# ------------------------------------------------------------

dominant_variable_text <- function(
    component_name,
    n = 3
){

  component_data <- loading_long %>%
    filter(
      Principal_Component == component_name
    ) %>%
    arrange(
      desc(Absolute_Loading)
    ) %>%
    slice_head(
      n = n
    )

  if(nrow(component_data) == 0){
    return("No loading information available.")
  }

  paste(
    paste0(
      component_data$Variable,
      " (loading = ",
      round(
        component_data$Loading,
        3
      ),
      ")"
    ),
    collapse = ", "
  )
}

pc1_dominant <- dominant_variable_text(
  "PC1"
)

pc2_dominant <- dominant_variable_text(
  "PC2"
)

pc3_dominant <- dominant_variable_text(
  "PC3"
)

# ------------------------------------------------------------
# 27. PCA REPORT CONTENT
# ------------------------------------------------------------

pc1_variance <- variance_percentage[1]
pc2_variance <- variance_percentage[2]
pc3_variance <- variance_percentage[3]

pc3_index <- min(
  3,
  length(cumulative_percentage)
)

pc1_pc3_cumulative <- cumulative_percentage[
  pc3_index
]

if(number_original_predictors <= 7){

  dimensionality_evaluation <- paste(
    "The PCA contains only",
    number_original_predictors,
    "numerical predictors. This is a relatively small feature space,",
    "so dimensionality reduction is not required purely for computational",
    "efficiency."
  )

} else {

  dimensionality_evaluation <- paste(
    "The PCA contains",
    number_original_predictors,
    "numerical predictors. Dimensionality reduction may therefore provide",
    "some computational and modelling benefits."
  )
}

if(
  !is.na(components_90) &&
    components_90 < number_original_predictors
){

  efficiency_evaluation <- paste(
    components_90,
    "principal components are sufficient to explain at least 90% of",
    "the standardized predictor variance. This demonstrates potential",
    "feature compression."
  )

} else {

  efficiency_evaluation <- paste(
    "At the 90% variance threshold, PCA does not reduce the number",
    "of predictors relative to the original feature count."
  )
}

# ------------------------------------------------------------
# 28. GENERATE FULL MARKDOWN REPORT
# ------------------------------------------------------------

report_lines <- c(
  "# Task 7: Principal Component Analysis (PCA) Evaluation",
  "",
  "## IT3081 Statistical Modelling Consultancy Project",
  "",
  "**Data-Driven Precision Fertilizer Advisory & Strategic Agriculture Framework**",
  "",
  "**Script:** `scripts/07_pca_evaluation.R`  ",
  "**Dataset:** `Fertilizer_Dataset.csv`",
  "",
  "---",
  "",
  "## 1. Objective",
  "",
  "This analysis evaluates whether Principal Component Analysis (PCA) can provide a useful representation of the project's numerical soil fertility and climatic predictors.",
  "",
  "The analysis focuses on dimensionality, variance structure, principal component loadings and the practical trade-off between predictive modelling efficiency and agricultural interpretability.",
  "",
  "Categorical identifiers, fertilizer targets and recommendation variables are excluded from PCA because PCA is being used to analyse the structure of the numerical predictor variables rather than the fertilizer outcomes.",
  "",
  "---",
  "",
  "## 2. PCA Input Variables",
  "",
  "| Variable | Agricultural interpretation |",
  "|---|---|",
  "| `Soil_pH` | Soil acidity/alkalinity indicator |",
  "| `Total_N_percent` | Total soil nitrogen |",
  "| `Available_P_mgkg` | Plant-available phosphorus |",
  "| `Available_K_mgkg` | Plant-available potassium |",
  "| `Organic_Carbon_percent` | Soil organic carbon |",
  "| `30day_cumulative_rainfall_mm` | Recent cumulative rainfall |",
  "| `mean_temperature_C` | Mean temperature |",
  "",
  "The script supports both the Task 7 weather-variable names and the corresponding names used in the project dataset.",
  "",
  "---",
  "",
  "## 3. Data Preparation",
  "",
  paste0(
    "The original dataset contained **",
    format(
      nrow(pca_data),
      big.mark = ","
    ),
    "** observations."
  ),
  "",
  paste0(
    "After complete-case filtering, **",
    format(
      nrow(pca_data_complete),
      big.mark = ","
    ),
    "** observations were used for PCA."
  ),
  "",
  paste0(
    "**Complete-case percentage:** ",
    round(
      100 * nrow(pca_data_complete) / nrow(pca_data),
      2
    ),
    "%."
  ),
  "",
  "All seven PCA variables were standardized before PCA using `scale. = TRUE`. Standardization is necessary because the variables have different units and numerical scales.",
  "",
  "---",
  "",
  "## 4. PCA Method",
  "",
  "PCA was performed using R's `prcomp()` function with centering and standardization:",
  "",
  "```r",
  "prcomp(pca_data_complete, center = TRUE, scale. = TRUE)",
  "```",
  "",
  "The eigenvalue of a principal component represents the amount of standardized variance captured by that component.",
  "",
  "The variance proportion is the eigenvalue divided by the sum of all eigenvalues, while cumulative variance is the running total of the variance proportions.",
  "",
  "---",
  "",
  "## 5. Eigenvalues and Variance Explained",
  "",
  "| Component | Eigenvalue | Variance (%) | Cumulative (%) |",
  "|---|---:|---:|---:|"
)

for(i in seq_len(nrow(pca_variance_table))){

  report_lines <- c(
    report_lines,
    paste0(
      "| ",
      pca_variance_table$Principal_Component[i],
      " | ",
      round(
        pca_variance_table$Eigenvalue[i],
        4
      ),
      " | ",
      round(
        pca_variance_table$Variance_Percentage[i],
        2
      ),
      " | ",
      round(
        pca_variance_table$Cumulative_Percentage[i],
        2
      ),
      " |"
    )
  )
}

report_lines <- c(
  report_lines,
  "",
  paste0(
    "PC1 explains **",
    round(
      pc1_variance,
      2
    ),
    "%** of the standardized variance."
  ),
  "",
  paste0(
    "PC2 explains **",
    round(
      pc2_variance,
      2
    ),
    "%** of the standardized variance."
  ),
  "",
  paste0(
    "PC3 explains **",
    round(
      pc3_variance,
      2
    ),
    "%** of the standardized variance."
  ),
  "",
  paste0(
    "Together, PC1-PC3 explain **",
    round(
      pc1_pc3_cumulative,
      2
    ),
    "%** of the standardized variance."
  ),
  "",
  "The complete eigenvalue, loading and cumulative-variance results are available in `output/reports/07_pca_summary_metrics.csv`.",
  "",
  "---",
  "",
  "## 6. Principal Component Loadings",
  "",
  "Loadings indicate how strongly each original standardized variable contributes to a principal component.",
  "",
  "Large absolute loadings identify variables that contribute strongly to a component.",
  "",
  "### PC1",
  "",
  paste0(
    "The three largest absolute PC1 loadings are: **",
    pc1_dominant,
    "**."
  ),
  "",
  "### PC2",
  "",
  paste0(
    "The three largest absolute PC2 loadings are: **",
    pc2_dominant,
    "**."
  ),
  "",
  "### PC3",
  "",
  paste0(
    "The three largest absolute PC3 loadings are: **",
    pc3_dominant,
    "**."
  ),
  "",
  "The sign of an individual PCA loading should be interpreted relative to the other variables. PCA component signs are arbitrary: multiplying an entire component by -1 gives an equivalent solution. Consequently, loading magnitude is particularly important when identifying dominant variables.",
  "",
  "---",
  "",
  "## 7. Scree Plot",
  "",
  "The scree plot is saved as `output/figures/07_pca_scree_plot.png`.",
  "",
  "The plot shows the percentage of standardized variance explained by each principal component. A sharp reduction in additional explained variance can indicate that later components provide progressively less information.",
  "",
  paste0(
    "Using the eigenvalue-greater-than-one rule, **",
    number_eigenvalue_gt_one,
    "** component(s) have eigenvalues greater than 1."
  ),
  "",
  "The eigenvalue-greater-than-one rule is treated as a descriptive diagnostic rather than as the sole criterion for selecting the final number of components.",
  "",
  "---",
  "",
  "## 8. PCA Biplot",
  "",
  "The PCA biplot is saved as `output/figures/07_pca_biplot.png`.",
  "",
  "The biplot displays individual observations and variable vectors in the PC1-PC2 space.",
  "",
  "Variables pointing in similar directions indicate similar relationships within the displayed PCA space, while opposing directions indicate contrasting relationships. The biplot is exploratory and does not establish causal agricultural relationships.",
  "",
  "---",
  "",
  "## 9. Variable Loading Visualization",
  "",
  "The `factoextra::fviz_pca_var()` output is saved as `output/figures/07_pca_variable_loadings.png`.",
  "",
  "An additional PC1-PC3 loading heatmap is saved as `output/figures/07_pca_loading_heatmap_pc1_pc3.png`.",
  "",
  "These visualizations help identify which soil and climatic variables contribute most strongly to the major principal components.",
  "",
  "---",
  "",
  "## 10. Dimensionality Reduction Assessment",
  "",
  dimensionality_evaluation,
  "",
  efficiency_evaluation,
  "",
  paste0(
    "The 80% cumulative variance threshold is reached using **",
    components_80,
    "** component(s)."
  ),
  "",
  paste0(
    "The 90% cumulative variance threshold is reached using **",
    components_90,
    "** component(s)."
  ),
  "",
  paste0(
    "The 95% cumulative variance threshold is reached using **",
    components_95,
    "** component(s)."
  ),
  "",
  "With only seven numerical predictors, computational efficiency alone is unlikely to justify replacing the original variables with principal components.",
  "",
  "---",
  "",
  "## 11. Predictive Modelling Efficiency",
  "",
  "PCA can be useful for predictive modelling when the original predictors contain substantial correlation or when a downstream model benefits from a smaller set of orthogonal predictors.",
  "",
  "Principal components are mutually orthogonal in the PCA representation. Therefore, a PCA-based modelling approach can reduce the direct multicollinearity among the transformed predictors.",
  "",
  "However, PCA is an unsupervised method. It maximizes variance in the predictor space rather than maximizing prediction accuracy for fertilizer requirements.",
  "",
  "Consequently, a component that explains a large proportion of soil and climate variance is not automatically the component that is most useful for predicting `Urea_kg_ha`, `TSP_kg_ha` or `MOP_kg_ha`.",
  "",
  "---",
  "",
  "## 12. Agricultural Interpretability",
  "",
  "The principal limitation of PCA for a stakeholder-facing fertilizer advisory system is interpretability.",
  "",
  "A farmer, agronomist or agricultural manager can directly interpret:",
  "",
  "- Soil pH",
  "- Total nitrogen",
  "- Available phosphorus",
  "- Available potassium",
  "- Organic carbon",
  "- Recent rainfall",
  "- Mean temperature",
  "",
  "A statement such as 'PC1 is high' is less directly actionable. PC1 is a weighted combination of several standardized agricultural measurements, and its interpretation requires examining the component loadings.",
  "",
  "For example, explaining a recommendation using actual soil nitrogen or soil pH is generally more transparent than explaining it through an abstract principal component.",
  "",
  "---",
  "",
  "## 13. Consultancy Trade-off",
  "",
  "| Criterion | Original Predictors | PCA Components |",
  "|---|---|---|",
  "| Computational dimensionality | Seven predictors | Potentially fewer components |",
  "| Multicollinearity | May remain | Reduced among components |",
  "| Direct agronomic interpretation | High | Lower |",
  "| Communication to managers | Direct | Requires loading explanation |",
  "| Exploratory multivariate analysis | Moderate | Strong |",
  "| Variable-specific recommendations | Direct | Indirect |",
  "| Model transparency | High | Lower |",
  "",
  "For this project, PCA is most useful as a complementary analytical technique and modelling benchmark rather than as an automatic replacement for the original agricultural predictors.",
  "",
  "---",
  "",
  "## 14. Recommended Consultancy Workflow",
  "",
  "1. Build the primary predictive models using the original soil and climatic variables.",
  "",
  "2. Evaluate correlations and multicollinearity among the original predictors.",
  "",
  "3. Construct PCA components as an alternative representation of the numerical predictor space.",
  "",
  "4. If PCA-based predictive models are tested, evaluate them on the same held-out test data and using the same metrics as the original models.",
  "",
  "5. Compare predictive performance, dimensionality and interpretability.",
  "",
  "6. Preserve the original agricultural variables for stakeholder-facing explanations even if PCA is used internally for exploratory or modelling purposes.",
  "",
  "---",
  "",
  "## 15. Limitations",
  "",
  "- PCA is unsupervised and does not use fertilizer recommendation targets when constructing components.",
  "- PCA maximizes predictor variance, not fertilizer prediction accuracy.",
  "- Complete-case analysis can reduce the available sample when missing values exist.",
  "- PCA results depend on the variables included and the preprocessing method.",
  "- Loading signs are arbitrary and should not be interpreted as causal directions.",
  "- PCA components can be harder for non-technical stakeholders to interpret.",
  "- The current PCA is restricted to the seven numerical soil and climatic predictors specified for Task 7.",
  "",
  "---",
  "",
  "## 16. Generated Outputs",
  "",
  "- `output/reports/07_pca_summary_metrics.csv`",
  "- `output/reports/07_pca_evaluation_report.md`",
  "- `output/reports/07_pca_loading_details.csv`",
  "- `output/reports/07_pca_loadings_matrix.csv`",
  "- `output/reports/07_pca_dimensionality_assessment.csv`",
  "- `output/reports/07_pca_sample_summary.csv`",
  "- `output/reports/07_pca_input_descriptive_statistics.csv`",
  "- `output/reports/07_pca_input_correlation_matrix.csv`",
  "- `output/figures/07_pca_scree_plot.png`",
  "- `output/figures/07_pca_biplot.png`",
  "- `output/figures/07_pca_variable_loadings.png`",
  "- `output/figures/07_pca_loading_heatmap_pc1_pc3.png`",
  "- `output/figures/07_pca_cumulative_variance.png`",
  "",
  "---",
  "",
  "## 17. Final Consultancy Evaluation",
  "",
  "PCA provides a useful summary of the multivariate structure among the project's soil fertility and climatic predictors.",
  "",
  paste0(
    "Because the analysis contains only ",
    number_original_predictors,
    " numerical predictors, dimensionality reduction is not required solely for computational reasons."
  ),
  "",
  "Its main statistical value is the ability to represent correlated predictor information using orthogonal components and to provide an exploratory view of the dominant environmental gradients.",
  "",
  "However, the original agricultural measurements have substantially greater direct interpretability. Soil pH, nitrogen, phosphorus, potassium, organic carbon, rainfall and temperature can be communicated directly to farmers, agronomists and managers, whereas principal components require interpretation through their loadings.",
  "",
  "Therefore, PCA should be treated as a complementary analytical method and potential modelling benchmark. Whether PCA improves the final predictive model should be determined using out-of-sample predictive performance alongside the interpretability requirements of the fertilizer advisory system.",
  "",
  "---",
  "",
  "## 18. Reproducibility",
  "",
  "A random seed of 123 is used for reproducibility. The PCA computation itself is deterministic given the dataset and preprocessing steps.",
  ""
)

# ------------------------------------------------------------
# 29. WRITE MARKDOWN REPORT
# ------------------------------------------------------------

markdown_report <- paste(
  report_lines,
  collapse = "\n"
)

markdown_report_file <- file.path(
  report_dir,
  "07_pca_evaluation_report.md"
)

writeLines(
  markdown_report,
  con = markdown_report_file,
  useBytes = TRUE
)

# ------------------------------------------------------------
# 30. SAVE PCA MODEL SUMMARY
# ------------------------------------------------------------

capture.output(
  summary(pca_model),
  file = file.path(
    report_dir,
    "07_pca_model_summary.txt"
  )
)

# ------------------------------------------------------------
# 31. SAVE SESSION INFORMATION
# ------------------------------------------------------------

capture.output(
  sessionInfo(),
  file = file.path(
    report_dir,
    "07_session_info.txt"
  )
)

# ------------------------------------------------------------
# 32. OUTPUT VALIDATION
# ------------------------------------------------------------

required_outputs <- c(
  file.path(
    report_dir,
    "07_pca_summary_metrics.csv"
  ),
  file.path(
    report_dir,
    "07_pca_evaluation_report.md"
  ),
  file.path(
    figure_dir,
    "07_pca_scree_plot.png"
  ),
  file.path(
    figure_dir,
    "07_pca_biplot.png"
  ),
  file.path(
    figure_dir,
    "07_pca_variable_loadings.png"
  )
)

output_validation <- data.frame(
  Output_File = required_outputs,
  Exists = file.exists(
    required_outputs
  ),
  Size_KB = ifelse(
    file.exists(required_outputs),
    round(
      file.info(required_outputs)$size / 1024,
      2
    ),
    NA_real_
  ),
  stringsAsFactors = FALSE
)

readr::write_csv(
  as.data.frame(output_validation),
  file.path(
    report_dir,
    "07_pca_output_validation.csv"
  )
)

if(!all(output_validation$Exists)){

  missing_outputs <- output_validation$Output_File[
    !output_validation$Exists
  ]

  stop(
    paste0(
      "Task 7 completed with missing required outputs:\n",
      paste(
        missing_outputs,
        collapse = "\n"
      )
    )
  )
}

# ------------------------------------------------------------
# 33. FINAL CONSOLE SUMMARY
# ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("TASK 7 - PRINCIPAL COMPONENT ANALYSIS COMPLETED\n")
cat("============================================================\n\n")

cat(
  "Original observations: ",
  format(
    nrow(pca_data),
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat(
  "Complete observations used: ",
  format(
    nrow(pca_data_complete),
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat(
  "Numerical PCA predictors: ",
  number_original_predictors,
  "\n\n",
  sep = ""
)

cat(
  "PC1 variance explained: ",
  round(
    pc1_variance,
    2
  ),
  "%\n",
  sep = ""
)

cat(
  "PC2 variance explained: ",
  round(
    pc2_variance,
    2
  ),
  "%\n",
  sep = ""
)

cat(
  "PC3 variance explained: ",
  round(
    pc3_variance,
    2
  ),
  "%\n",
  sep = ""
)

cat(
  "PC1-PC3 cumulative variance: ",
  round(
    pc1_pc3_cumulative,
    2
  ),
  "%\n\n",
  sep = ""
)

cat(
  "Components with eigenvalue > 1: ",
  number_eigenvalue_gt_one,
  "\n",
  sep = ""
)

cat(
  "Components for >=80% variance: ",
  components_80,
  "\n",
  sep = ""
)

cat(
  "Components for >=90% variance: ",
  components_90,
  "\n",
  sep = ""
)

cat(
  "Components for >=95% variance: ",
  components_95,
  "\n\n",
  sep = ""
)

cat(
  "PC1 dominant variables: ",
  pc1_dominant,
  "\n\n",
  sep = ""
)

cat(
  "PC2 dominant variables: ",
  pc2_dominant,
  "\n\n",
  sep = ""
)

cat(
  "PC3 dominant variables: ",
  pc3_dominant,
  "\n\n",
  sep = ""
)

cat(
  "Required report:\n",
  markdown_report_file,
  "\n\n",
  sep = ""
)

cat("Required figures:\n")

cat(
  file.path(
    figure_dir,
    "07_pca_scree_plot.png"
  ),
  "\n"
)

cat(
  file.path(
    figure_dir,
    "07_pca_biplot.png"
  ),
  "\n"
)

cat(
  file.path(
    figure_dir,
    "07_pca_variable_loadings.png"
  ),
  "\n\n"
)

cat(
  "All required Task 7 outputs were generated successfully.\n"
)

cat("============================================================\n")
