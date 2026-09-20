# ==============================================================================
# IT3081 Statistical Modelling
# Task 5: Predictive Statistical Modelling & Diagnostics
# ==============================================================================
#
# Save as:
# scripts/05_predictive_modeling.R
#
# Working Directory:
# ~/Documents/Fertilizer_Consultancy_Project/
#
# Purpose:
#   Build, evaluate and diagnose predictive statistical models for:
#     1. Urea_kg_ha
#     2. TSP_kg_ha
#     3. MOP_kg_ha
#
# Candidate Models:
#   - Multiple Linear Regression (MLR)
#   - Ridge Regression (alpha = 0)
#   - LASSO Regression (alpha = 1)
#   - Gaussian Generalized Linear Model (GLM)
#
# Evaluation Metrics:
#   - R-squared
#   - Adjusted R-squared
#   - RMSE
#   - MAE
#
# MLR Diagnostics:
#   - Variance Inflation Factor (VIF)
#   - Breusch-Pagan heteroscedasticity test
#   - Shapiro-Wilk residual normality test
#   - Residuals vs Fitted
#   - Normal Q-Q
#   - Scale-Location
#   - VIF bar chart
#
# ==============================================================================


# ------------------------------------------------------------------------------
# 1. DIRECTORY SAFETY
# ------------------------------------------------------------------------------

if(!dir.exists("output/figures")) dir.create("output/figures", recursive = TRUE)
if(!dir.exists("output/reports")) dir.create("output/reports", recursive = TRUE)


# ------------------------------------------------------------------------------
# 2. REQUIRED PACKAGES
# ------------------------------------------------------------------------------

required_packages <- c(
  "tidyverse",
  "caret",
  "car",
  "lmtest",
  "glmnet",
  "patchwork"
)

missing_packages <- required_packages[
  !required_packages %in% rownames(installed.packages())
]

if(length(missing_packages) > 0){

  stop(
    paste0(
      "Missing required package(s): ",
      paste(missing_packages, collapse = ", "),
      "\n\nInstall them using:\n",
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
library(caret)
library(car)
library(lmtest)
library(glmnet)
library(patchwork)


# ------------------------------------------------------------------------------
# 3. PROJECT SETTINGS
# ------------------------------------------------------------------------------

set.seed(123)

figure_dir <- "output/figures"
report_dir <- "output/reports"

dataset_path <- "Fertilizer_Dataset.csv"

target_variables <- c(
  "Urea_kg_ha",
  "TSP_kg_ha",
  "MOP_kg_ha"
)

# Unique identifier and recommendation variables are deliberately excluded.
#
# The Recommended_* variables are excluded because they represent reference
# recommendation outputs that could contain information directly related to
# the fertilizer targets. Including them could cause target leakage and
# produce unrealistically optimistic predictive performance.

excluded_variables <- c(
  "Sample_ID",
  "Recommended_N_kg_ha",
  "Recommended_P2O5_kg_ha",
  "Recommended_K2O_kg_ha"
)


# ------------------------------------------------------------------------------
# 4. LOAD DATA
# ------------------------------------------------------------------------------

fertilizer_data <- read.csv(
  dataset_path,
  stringsAsFactors = FALSE
)

cat(
  "\n============================================================\n",
  "TASK 5: PREDICTIVE STATISTICAL MODELLING & DIAGNOSTICS\n",
  "============================================================\n\n",
  sep = ""
)

cat(
  "Dataset loaded successfully.\n",
  "Rows: ", nrow(fertilizer_data), "\n",
  "Columns: ", ncol(fertilizer_data), "\n\n",
  sep = ""
)


# ------------------------------------------------------------------------------
# 5. REQUIRED VARIABLE CHECK
# ------------------------------------------------------------------------------

required_variables <- c(
  target_variables,
  "District",
  "Agro_Zone",
  "Season",
  "Soil_pH",
  "Total_N_percent",
  "Available_P_mgkg",
  "Available_K_mgkg",
  "Organic_Carbon_percent",
  "Rainfall_30d_mm",
  "Temperature_mean_C"
)

missing_variables <- setdiff(
  required_variables,
  names(fertilizer_data)
)

if(length(missing_variables) > 0){

  stop(
    paste0(
      "The dataset is missing the following required variable(s): ",
      paste(missing_variables, collapse = ", ")
    )
  )

}


# ------------------------------------------------------------------------------
# 6. REMOVE DUPLICATES
# ------------------------------------------------------------------------------

original_n <- nrow(fertilizer_data)

duplicate_n <- sum(
  duplicated(fertilizer_data)
)

fertilizer_data <- fertilizer_data %>%
  distinct()

cleaned_n <- nrow(fertilizer_data)

cat(
  "Exact duplicate rows detected: ",
  duplicate_n,
  "\n",
  sep = ""
)

cat(
  "Rows removed: ",
  original_n - cleaned_n,
  "\n",
  sep = ""
)

cat(
  "Rows remaining: ",
  cleaned_n,
  "\n\n",
  sep = ""
)


# ------------------------------------------------------------------------------
# 7. REMOVE IDENTIFIERS AND POTENTIAL LEAKAGE VARIABLES
# ------------------------------------------------------------------------------

model_data <- fertilizer_data %>%
  select(
    -any_of(excluded_variables)
  )


# Convert categorical predictors to factors.

categorical_predictors <- c(
  "District",
  "Agro_Zone",
  "Season"
)

model_data <- model_data %>%
  mutate(
    across(
      any_of(categorical_predictors),
      as.factor
    )
  )


# ------------------------------------------------------------------------------
# 8. DEFINE PREDICTOR VARIABLES
# ------------------------------------------------------------------------------

predictor_variables <- setdiff(
  names(model_data),
  target_variables
)

cat(
  "Predictor variables used by the models:\n",
  paste(
    predictor_variables,
    collapse = "\n"
  ),
  "\n\n",
  sep = ""
)


# ------------------------------------------------------------------------------
# 9. MODELING FUNCTIONS
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# 9.1 RMSE
# ------------------------------------------------------------------------------

calculate_rmse <- function(actual, predicted){

  sqrt(
    mean(
      (actual - predicted)^2,
      na.rm = TRUE
    )
  )

}


# ------------------------------------------------------------------------------
# 9.2 MAE
# ------------------------------------------------------------------------------

calculate_mae <- function(actual, predicted){

  mean(
    abs(
      actual - predicted
    ),
    na.rm = TRUE
  )

}


# ------------------------------------------------------------------------------
# 9.3 R-squared
# ------------------------------------------------------------------------------

calculate_r_squared <- function(actual, predicted){

  residual_ss <- sum(
    (actual - predicted)^2,
    na.rm = TRUE
  )

  total_ss <- sum(
    (actual - mean(actual, na.rm = TRUE))^2,
    na.rm = TRUE
  )

  if(total_ss == 0){
    return(NA_real_)
  }

  1 - residual_ss / total_ss

}


# ------------------------------------------------------------------------------
# 9.4 Adjusted R-squared
# ------------------------------------------------------------------------------

calculate_adjusted_r_squared <- function(
  r_squared,
  n,
  p
){

  if(
    is.na(r_squared) ||
    n <= p + 1
  ){
    return(NA_real_)
  }

  1 -
    (
      (1 - r_squared) *
        (n - 1)
    ) /
    (
      n - p - 1
    )

}


# ------------------------------------------------------------------------------
# 9.5 General model metrics
# ------------------------------------------------------------------------------

calculate_metrics <- function(
  actual,
  predicted,
  n_predictors
){

  n <- length(actual)

  r_squared <- calculate_r_squared(
    actual,
    predicted
  )

  adjusted_r_squared <- calculate_adjusted_r_squared(
    r_squared = r_squared,
    n = n,
    p = n_predictors
  )

  tibble(
    n_test = n,
    r_squared = r_squared,
    adjusted_r_squared = adjusted_r_squared,
    RMSE = calculate_rmse(
      actual,
      predicted
    ),
    MAE = calculate_mae(
      actual,
      predicted
    )
  )

}


# ------------------------------------------------------------------------------
# 9.6 Safe Shapiro-Wilk test
# ------------------------------------------------------------------------------

safe_shapiro_test <- function(residuals){

  residuals <- residuals[
    is.finite(residuals)
  ]

  n <- length(residuals)

  if(n < 3){

    return(
      tibble(
        statistic = NA_real_,
        p_value = NA_real_,
        n_tested = n,
        note = "Insufficient observations for Shapiro-Wilk test."
      )
    )

  }

  # R's shapiro.test() supports at most 5000 observations.
  # For larger datasets, use a reproducible random sample of 5000 residuals.

  if(n > 5000){

    set.seed(123)

    residuals <- sample(
      residuals,
      size = 5000,
      replace = FALSE
    )

    test_note <- "Shapiro-Wilk test performed on a random sample of 5000 residuals."

  } else {

    test_note <- "Shapiro-Wilk test performed on all available residuals."

  }

  result <- shapiro.test(
    residuals
  )

  tibble(
    statistic = unname(result$statistic),
    p_value = result$p.value,
    n_tested = length(residuals),
    note = test_note
  )

}


# ------------------------------------------------------------------------------
# 9.7 Safe Breusch-Pagan test
# ------------------------------------------------------------------------------

safe_bp_test <- function(model){

  result <- tryCatch(
    lmtest::bptest(model),
    error = function(e) NULL
  )

  if(is.null(result)){

    return(
      tibble(
        statistic = NA_real_,
        df = NA_real_,
        p_value = NA_real_,
        note = "Breusch-Pagan test could not be computed."
      )
    )

  }

  tibble(
    statistic = unname(result$statistic),
    df = unname(result$parameter),
    p_value = result$p.value,
    note = "Breusch-Pagan test completed."
  )

}


# ------------------------------------------------------------------------------
# 9.8 Safe VIF extraction
# ------------------------------------------------------------------------------

safe_vif <- function(model){

  result <- tryCatch(
    car::vif(model),
    error = function(e) NULL
  )

  if(is.null(result)){

    return(
      tibble(
        variable = character(),
        VIF = numeric(),
        GVIF = numeric(),
        Df = numeric(),
        GVIF_adjusted = numeric()
      )
    )

  }

  # Numeric vector:
  # Usually occurs when all predictors have one degree of freedom.

  if(is.vector(result) && is.numeric(result)){

    return(
      tibble(
        variable = names(result),
        VIF = as.numeric(result),
        GVIF = NA_real_,
        Df = 1,
        GVIF_adjusted = as.numeric(result)
      )
    )

  }

  # Matrix:
  # Occurs when categorical predictors generate multi-degree-of-freedom
  # terms. car::vif() then returns GVIF and adjusted GVIF.

  result_df <- as.data.frame(
    result,
    stringsAsFactors = FALSE
  )

  result_df$variable <- rownames(
    result_df
  )

  rownames(
    result_df
  ) <- NULL

  if(
    all(
      c(
        "GVIF",
        "Df",
        "GVIF^(1/(2*Df))"
      ) %in% names(result_df)
    )
  ){

    return(
      result_df %>%
        transmute(
          variable = as.character(variable),
          VIF = NA_real_,
          GVIF = as.numeric(GVIF),
          Df = as.numeric(Df),
          GVIF_adjusted = as.numeric(
            `GVIF^(1/(2*Df))`
          )
        )
    )

  }

  tibble(
    variable = as.character(
      result_df$variable
    ),
    VIF = NA_real_,
    GVIF = NA_real_,
    Df = NA_real_,
    GVIF_adjusted = NA_real_
  )

}


# ==============================================================================
# 10. STORAGE OBJECTS
# ==============================================================================

performance_results <- list()

diagnostic_results <- list()

vif_results <- list()

prediction_results <- list()

model_objects <- list()


# ==============================================================================
# 11. LOOP THROUGH TARGET VARIABLES
# ==============================================================================

for(target in target_variables){

  cat(
    "\n\n============================================================\n",
    "TARGET: ",
    target,
    "\n",
    "============================================================\n",
    sep = ""
  )


  # --------------------------------------------------------------------------
  # 11.1 Prepare target-specific dataset
  # --------------------------------------------------------------------------

  target_model_data <- model_data %>%
    select(
      all_of(
        c(
          target,
          predictor_variables
        )
      )
    ) %>%
    drop_na()

  target_model_data <- droplevels(
    target_model_data
  )

  cat(
    "Complete observations for ",
    target,
    ": ",
    nrow(target_model_data),
    "\n",
    sep = ""
  )


  if(nrow(target_model_data) < 20){

    warning(
      paste(
        "Skipping target because fewer than 20 complete observations exist:",
        target
      )
    )

    next

  }


  # --------------------------------------------------------------------------
  # 11.2 80/20 Train/Test Split
  # --------------------------------------------------------------------------

  set.seed(123)

  train_index <- caret::createDataPartition(
    y = target_model_data[[target]],
    p = 0.80,
    list = FALSE
  )

  train_data <- target_model_data[
    train_index,
    ,
    drop = FALSE
  ]

  test_data <- target_model_data[
    -train_index,
    ,
    drop = FALSE
  ]

  cat(
    "Training observations: ",
    nrow(train_data),
    "\n",
    sep = ""
  )

  cat(
    "Testing observations: ",
    nrow(test_data),
    "\n",
    sep = ""
  )


  # --------------------------------------------------------------------------
  # 11.3 Dynamic Model Formula
  # --------------------------------------------------------------------------

  model_formula <- reformulate(
    termlabels = predictor_variables,
    response = target
  )


  # ==========================================================================
  # MODEL 1: MULTIPLE LINEAR REGRESSION
  # ==========================================================================

  cat(
    "\nFitting Multiple Linear Regression...\n"
  )

  mlr_model <- lm(
    formula = model_formula,
    data = train_data
  )

  mlr_predictions <- predict(
    mlr_model,
    newdata = test_data
  )

  mlr_n_predictors <- length(
    coef(mlr_model)
  ) - 1

  mlr_metrics <- calculate_metrics(
    actual = test_data[[target]],
    predicted = mlr_predictions,
    n_predictors = mlr_n_predictors
  ) %>%
    mutate(
      target = target,
      model = "MLR",
      alpha = NA_real_,
      lambda = NA_real_,
      .before = 1
    )


  # ==========================================================================
  # MODEL 2: RIDGE REGRESSION
  # ==========================================================================

  cat(
    "Fitting Ridge Regression...\n"
  )

  # model.matrix() converts categorical predictors into dummy variables.
  # The same training-derived column structure is applied to the test data.

  ridge_x_train <- model.matrix(
    model_formula,
    data = train_data
  )

  ridge_x_test <- model.matrix(
    model_formula,
    data = test_data
  )

  ridge_x_train <- ridge_x_train[, colnames(ridge_x_train) != "(Intercept)", drop = FALSE]

  ridge_x_test <- ridge_x_test[, colnames(ridge_x_test) != "(Intercept)", drop = FALSE]

  ridge_cv <- glmnet::cv.glmnet(
    x = ridge_x_train,
    y = train_data[[target]],
    alpha = 0,
    family = "gaussian",
    nfolds = 10,
    standardize = TRUE,
    type.measure = "mse"
  )

  ridge_predictions <- as.numeric(
    predict(
      ridge_cv,
      newx = ridge_x_test,
      s = "lambda.min"
    )
  )

  ridge_n_predictors <- ncol(
    ridge_x_test
  )

  ridge_metrics <- calculate_metrics(
    actual = test_data[[target]],
    predicted = ridge_predictions,
    n_predictors = ridge_n_predictors
  ) %>%
    mutate(
      target = target,
      model = "Ridge",
      alpha = 0,
      lambda = as.numeric(ridge_cv$lambda.min),
      .before = 1
    )


  # ==========================================================================
  # MODEL 3: LASSO REGRESSION
  # ==========================================================================

  cat(
    "Fitting LASSO Regression...\n"
  )

  lasso_x_train <- model.matrix(
    model_formula,
    data = train_data
  )

  lasso_x_test <- model.matrix(
    model_formula,
    data = test_data
  )

  lasso_x_train <- lasso_x_train[
    ,
    colnames(lasso_x_train) != "(Intercept)",
    drop = FALSE
  ]

  lasso_x_test <- lasso_x_test[
    ,
    colnames(lasso_x_test) != "(Intercept)",
    drop = FALSE
  ]

  lasso_cv <- glmnet::cv.glmnet(
    x = lasso_x_train,
    y = train_data[[target]],
    alpha = 1,
    family = "gaussian",
    nfolds = 10,
    standardize = TRUE,
    type.measure = "mse"
  )

  lasso_predictions <- as.numeric(
    predict(
      lasso_cv,
      newx = lasso_x_test,
      s = "lambda.min"
    )
  )

  lasso_n_predictors <- ncol(
    lasso_x_test
  )

  lasso_metrics <- calculate_metrics(
    actual = test_data[[target]],
    predicted = lasso_predictions,
    n_predictors = lasso_n_predictors
  ) %>%
    mutate(
      target = target,
      model = "LASSO",
      alpha = 1,
      lambda = as.numeric(lasso_cv$lambda.min),
      .before = 1
    )


  # ==========================================================================
  # MODEL 4: GAUSSIAN GENERALIZED LINEAR MODEL
  # ==========================================================================

  cat(
    "Fitting Gaussian Generalized Linear Model...\n"
  )

  glm_model <- glm(
    formula = model_formula,
    data = train_data,
    family = gaussian()
  )

  glm_predictions <- predict(
    glm_model,
    newdata = test_data,
    type = "response"
  )

  glm_n_predictors <- length(
    coef(glm_model)
  ) - 1

  glm_metrics <- calculate_metrics(
    actual = test_data[[target]],
    predicted = glm_predictions,
    n_predictors = glm_n_predictors
  ) %>%
    mutate(
      target = target,
      model = "GLM_Gaussian",
      alpha = NA_real_,
      lambda = NA_real_,
      .before = 1
    )


  # --------------------------------------------------------------------------
  # 11.4 Combine Model Performance
  # --------------------------------------------------------------------------

  target_performance <- bind_rows(
    mlr_metrics,
    ridge_metrics,
    lasso_metrics,
    glm_metrics
  )

  performance_results[[target]] <- target_performance


  # --------------------------------------------------------------------------
  # 11.5 Store Predictions
  # --------------------------------------------------------------------------

  prediction_results[[target]] <- bind_rows(

    tibble(
      target = target,
      model = "MLR",
      actual = test_data[[target]],
      predicted = as.numeric(mlr_predictions)
    ),

    tibble(
      target = target,
      model = "Ridge",
      actual = test_data[[target]],
      predicted = as.numeric(ridge_predictions)
    ),

    tibble(
      target = target,
      model = "LASSO",
      actual = test_data[[target]],
      predicted = as.numeric(lasso_predictions)
    ),

    tibble(
      target = target,
      model = "GLM_Gaussian",
      actual = test_data[[target]],
      predicted = as.numeric(glm_predictions)
    )

  )


  # --------------------------------------------------------------------------
  # 11.6 Store Model Objects
  # --------------------------------------------------------------------------

  model_objects[[target]] <- list(
    mlr = mlr_model,
    ridge = ridge_cv,
    lasso = lasso_cv,
    glm = glm_model
  )


  # ==========================================================================
  # 12. MLR DIAGNOSTICS
  # ==========================================================================

  cat(
    "\nRunning MLR diagnostics...\n"
  )


  # --------------------------------------------------------------------------
  # 12.1 VIF
  # --------------------------------------------------------------------------

  target_vif <- safe_vif(
    mlr_model
  ) %>%
    mutate(
      target = target,
      .before = 1
    )

  vif_results[[target]] <- target_vif


  # --------------------------------------------------------------------------
  # 12.2 Breusch-Pagan Test
  # --------------------------------------------------------------------------

  bp_result <- safe_bp_test(
    mlr_model
  ) %>%
    mutate(
      target = target,
      test = "Breusch-Pagan",
      .before = 1
    )


  # --------------------------------------------------------------------------
  # 12.3 Shapiro-Wilk Test
  # --------------------------------------------------------------------------

  mlr_residuals <- residuals(
    mlr_model
  )

  shapiro_result <- safe_shapiro_test(
    mlr_residuals
  ) %>%
    mutate(
      target = target,
      test = "Shapiro-Wilk",
      .before = 1
    )


  # --------------------------------------------------------------------------
  # 12.4 Diagnostic summary
  # --------------------------------------------------------------------------

  diagnostic_results[[target]] <- list(
    bp = bp_result,
    shapiro = shapiro_result
  )


  # ==========================================================================
  # 13. MLR DIAGNOSTIC PLOTS
  # ==========================================================================

  fitted_values <- fitted(
    mlr_model
  )

  standardized_residuals <- rstandard(
    mlr_model
  )

  sqrt_abs_standardized_residuals <- sqrt(
    abs(
      standardized_residuals
    )
  )


  # --------------------------------------------------------------------------
  # 13.1 Residuals vs Fitted
  # --------------------------------------------------------------------------

  residual_plot_data <- tibble(
    fitted = fitted_values,
    residuals = mlr_residuals
  )

  p_residuals <- ggplot(
    residual_plot_data,
    aes(
      x = fitted,
      y = residuals
    )
  ) +
    geom_point(
      alpha = 0.35
    ) +
    geom_hline(
      yintercept = 0,
      linetype = "dashed"
    ) +
    geom_smooth(
      method = "loess",
      se = FALSE
    ) +
    labs(
      title = "Residuals vs Fitted",
      x = "Fitted Values",
      y = "Residuals"
    ) +
    theme_minimal(
      base_size = 11
    )


  # --------------------------------------------------------------------------
  # 13.2 Normal Q-Q Plot
  # --------------------------------------------------------------------------

  qq_data <- tibble(
    theoretical = qqnorm(
      mlr_residuals,
      plot.it = FALSE
    )$x,
    sample = qqnorm(
      mlr_residuals,
      plot.it = FALSE
    )$y
  )

  qq_line_model <- lm(
    sample ~ theoretical,
    data = qq_data
  )

  p_qq <- ggplot(
    qq_data,
    aes(
      x = theoretical,
      y = sample
    )
  ) +
    geom_point(
      alpha = 0.35
    ) +
    geom_abline(
      intercept = coef(qq_line_model)[1],
      slope = coef(qq_line_model)[2],
      linetype = "dashed"
    ) +
    labs(
      title = "Normal Q-Q Plot",
      x = "Theoretical Quantiles",
      y = "Standardized Quantiles"
    ) +
    theme_minimal(
      base_size = 11
    )


  # --------------------------------------------------------------------------
  # 13.3 Scale-Location Plot
  # --------------------------------------------------------------------------

  scale_location_data <- tibble(
    fitted = fitted_values,
    sqrt_abs_standardized_residuals =
      sqrt_abs_standardized_residuals
  )

  p_scale <- ggplot(
    scale_location_data,
    aes(
      x = fitted,
      y = sqrt_abs_standardized_residuals
    )
  ) +
    geom_point(
      alpha = 0.35
    ) +
    geom_smooth(
      method = "loess",
      se = FALSE
    ) +
    labs(
      title = "Scale-Location",
      x = "Fitted Values",
      y = "Square Root of |Standardized Residuals|"
    ) +
    theme_minimal(
      base_size = 11
    )


  # --------------------------------------------------------------------------
  # 13.4 VIF Bar Chart
  # --------------------------------------------------------------------------

  if(
    nrow(target_vif) > 0 &&
    any(!is.na(target_vif$VIF))
  ){

    vif_plot_data <- target_vif %>%
      filter(
        !is.na(VIF)
      ) %>%
      mutate(
        variable = fct_reorder(
          variable,
          VIF
        )
      )

    p_vif <- ggplot(
      vif_plot_data,
      aes(
        x = variable,
        y = VIF
      )
    ) +
      geom_col() +
      geom_hline(
        yintercept = 5,
        linetype = "dashed"
      ) +
      coord_flip() +
      labs(
        title = "Variance Inflation Factor",
        subtitle = "Reference line at VIF = 5",
        x = "Predictor",
        y = "VIF"
      ) +
      theme_minimal(
        base_size = 11
      )

  } else {

    vif_plot_data <- target_vif %>%
      filter(
        !is.na(GVIF_adjusted)
      ) %>%
      mutate(
        variable = fct_reorder(
          variable,
          GVIF_adjusted
        )
      )

    p_vif <- ggplot(
      vif_plot_data,
      aes(
        x = variable,
        y = GVIF_adjusted
      )
    ) +
      geom_col() +
      geom_hline(
        yintercept = 1.5,
        linetype = "dashed"
      ) +
      coord_flip() +
      labs(
        title = "Adjusted Generalized VIF",
        subtitle = "For multi-degree-of-freedom predictors",
        x = "Predictor",
        y = "GVIF^(1/(2*Df))"
      ) +
      theme_minimal(
        base_size = 11
      )

  }


  # --------------------------------------------------------------------------
  # 13.5 Combine Diagnostic Plots
  # --------------------------------------------------------------------------

  diagnostic_grid <- (
    p_residuals +
      p_qq
  ) /
    (
      p_scale +
        p_vif
    ) +
    plot_annotation(
      title = paste(
        "MLR Diagnostic Panel -",
        target
      )
    )

  diagnostic_filename <- switch(
    target,
    "Urea_kg_ha" =
      "05_mlr_diagnostics_urea.png",
    "TSP_kg_ha" =
      "05_mlr_diagnostics_tsp.png",
    "MOP_kg_ha" =
      "05_mlr_diagnostics_mop.png"
  )

  ggsave(
    filename = file.path(
      figure_dir,
      diagnostic_filename
    ),
    plot = diagnostic_grid,
    width = 14,
    height = 11,
    dpi = 300
  )


  # --------------------------------------------------------------------------
  # 13.6 Print Diagnostic Results
  # --------------------------------------------------------------------------

  cat(
    "\nBreusch-Pagan test:\n"
  )

  print(
    bp_result
  )

  cat(
    "\nShapiro-Wilk test:\n"
  )

  print(
    shapiro_result
  )

  cat(
    "\nVIF results:\n"
  )

  print(
    target_vif
  )


  # --------------------------------------------------------------------------
  # 13.7 Print Model Comparison
  # --------------------------------------------------------------------------

  cat(
    "\nModel performance for ",
    target,
    ":\n",
    sep = ""
  )

  print(
    target_performance %>%
      arrange(
        RMSE
      )
  )

}


# ==============================================================================
# 14. COMBINE MODEL PERFORMANCE RESULTS
# ==============================================================================

performance_comparison <- bind_rows(
  performance_results
)

# Explicitly convert to a standard data.frame before export.
#
# This prevents possible tibble/pillar formatting issues when writing
# results generated from model objects.

performance_comparison <- as.data.frame(
  performance_comparison,
  stringsAsFactors = FALSE
)


# ------------------------------------------------------------------------------
# 14.1 Export performance comparison
# ------------------------------------------------------------------------------

readr::write_csv(
  performance_comparison,
  file.path(
    report_dir,
    "05_model_performance_comparison.csv"
  )
)


# ==============================================================================
# 15. EXPORT VIF RESULTS
# ==============================================================================

all_vif_results <- bind_rows(
  vif_results
)

all_vif_results <- as.data.frame(
  all_vif_results,
  stringsAsFactors = FALSE
)

readr::write_csv(
  all_vif_results,
  file.path(
    report_dir,
    "05_mlr_vif_results.csv"
  )
)


# ==============================================================================
# 16. EXPORT HETEROSCEDASTICITY RESULTS
# ==============================================================================

all_bp_results <- bind_rows(
  lapply(
    diagnostic_results,
    function(x) x$bp
  )
)

all_bp_results <- as.data.frame(
  all_bp_results,
  stringsAsFactors = FALSE
)

readr::write_csv(
  all_bp_results,
  file.path(
    report_dir,
    "05_breusch_pagan_results.csv"
  )
)


# ==============================================================================
# 17. EXPORT RESIDUAL NORMALITY RESULTS
# ==============================================================================

all_shapiro_results <- bind_rows(
  lapply(
    diagnostic_results,
    function(x) x$shapiro
  )
)

all_shapiro_results <- as.data.frame(
  all_shapiro_results,
  stringsAsFactors = FALSE
)

readr::write_csv(
  all_shapiro_results,
  file.path(
    report_dir,
    "05_shapiro_wilk_results.csv"
  )
)


# ==============================================================================
# 18. DETERMINE BEST MODEL FOR EACH TARGET
# ==============================================================================

# The primary selection criterion is lowest test-set RMSE.
#
# RMSE penalizes larger prediction errors more strongly than MAE and is
# therefore useful when large fertilizer recommendation errors are important.
#
# R-squared, adjusted R-squared and MAE remain available for comparison.
#
# This does not imply that the model is universally "best"; it identifies
# the model with the lowest observed test-set RMSE within this experiment.

best_models <- performance_comparison %>%
  group_by(
    target
  ) %>%
  slice_min(
    order_by = RMSE,
    n = 1,
    with_ties = FALSE
  ) %>%
  ungroup()

best_models <- as.data.frame(
  best_models,
  stringsAsFactors = FALSE
)

readr::write_csv(
  best_models,
  file.path(
    report_dir,
    "05_best_model_by_target.csv"
  )
)


# ==============================================================================
# 19. PREDICTED VS ACTUAL PLOTS FOR BEST MODEL
# ==============================================================================

best_prediction_data <- map_dfr(
  target_variables,
  function(target){

    if(!target %in% names(prediction_results)){
      return(tibble())
    }

    best_model_name <- best_models %>%
      filter(
        target == !!target
      ) %>%
      pull(
        model
      )

    prediction_results[[target]] %>%
      filter(
        model == best_model_name
      ) %>%
      mutate(
        target = target,
        best_model = best_model_name
      )

  }
)


# Calculate plotting ranges independently for each target.

prediction_plot <- ggplot(
  best_prediction_data,
  aes(
    x = actual,
    y = predicted
  )
) +
  geom_point(
    alpha = 0.45
  ) +
  geom_abline(
    intercept = 0,
    slope = 1,
    linetype = "dashed"
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE
  ) +
  facet_wrap(
    ~ target,
    scales = "free"
  ) +
  labs(
    title = "Predicted vs Actual Fertilizer Application Rates",
    subtitle = "Best model for each target selected using lowest test-set RMSE",
    x = "Actual Fertilizer Rate (kg/ha)",
    y = "Predicted Fertilizer Rate (kg/ha)"
  ) +
  theme_minimal(
    base_size = 12
  )

ggsave(
  filename = file.path(
    figure_dir,
    "05_predicted_vs_actual.png"
  ),
  plot = prediction_plot,
  width = 14,
  height = 8,
  dpi = 300
)


# ==============================================================================
# 20. SAVE ALL TEST-SET PREDICTIONS
# ==============================================================================

all_predictions <- bind_rows(
  prediction_results
)

all_predictions <- as.data.frame(
  all_predictions,
  stringsAsFactors = FALSE
)

readr::write_csv(
  all_predictions,
  file.path(
    report_dir,
    "05_all_model_predictions.csv"
  )
)


# ==============================================================================
# 21. MODEL COEFFICIENT REPORTS
# ==============================================================================

# ------------------------------------------------------------------------------
# 21.1 MLR coefficients
# ------------------------------------------------------------------------------

mlr_coefficients <- map_dfr(
  names(model_objects),
  function(target){

    model <- model_objects[[target]]$mlr

    coefficients <- summary(
      model
    )$coefficients

    coefficients <- as.data.frame(
      coefficients,
      stringsAsFactors = FALSE
    )

    coefficients$term <- rownames(
      coefficients
    )

    rownames(
      coefficients
    ) <- NULL

    coefficients %>%
      transmute(
        target = target,
        term = as.character(term),
        estimate = as.numeric(Estimate),
        std_error = as.numeric(`Std. Error`),
        t_value = as.numeric(`t value`),
        p_value = as.numeric(`Pr(>|t|)`)
      )

  }
)

mlr_coefficients <- as.data.frame(
  mlr_coefficients,
  stringsAsFactors = FALSE
)

readr::write_csv(
  mlr_coefficients,
  file.path(
    report_dir,
    "05_mlr_coefficients.csv"
  )
)


# ==============================================================================
# 22. RIDGE AND LASSO SELECTED COEFFICIENTS
# ==============================================================================

regularized_coefficients <- map_dfr(
  names(model_objects),
  function(target){

    ridge_model <- model_objects[[target]]$ridge
    lasso_model <- model_objects[[target]]$lasso


    # Ridge coefficients

    ridge_coef <- as.matrix(
      coef(
        ridge_model,
        s = "lambda.min"
      )
    )

    ridge_df <- tibble(
      target = target,
      model = "Ridge",
      term = rownames(
        ridge_coef
      ),
      coefficient = as.numeric(
        ridge_coef[, 1]
      )
    )


    # LASSO coefficients

    lasso_coef <- as.matrix(
      coef(
        lasso_model,
        s = "lambda.min"
      )
    )

    lasso_df <- tibble(
      target = target,
      model = "LASSO",
      term = rownames(
        lasso_coef
      ),
      coefficient = as.numeric(
        lasso_coef[, 1]
      )
    )


    bind_rows(
      ridge_df,
      lasso_df
    )

  }
)

regularized_coefficients <- as.data.frame(
  regularized_coefficients,
  stringsAsFactors = FALSE
)

readr::write_csv(
  regularized_coefficients,
  file.path(
    report_dir,
    "05_ridge_lasso_coefficients.csv"
  )
)


# ==============================================================================
# 23. MODEL PERFORMANCE RANKING
# ==============================================================================

performance_ranking <- performance_comparison %>%
  group_by(
    target
  ) %>%
  arrange(
    RMSE,
    .by_group = TRUE
  ) %>%
  mutate(
    RMSE_rank = row_number(),
    MAE_rank = min_rank(MAE),
    R2_rank = min_rank(
      desc(r_squared)
    )
  ) %>%
  ungroup()

performance_ranking <- as.data.frame(
  performance_ranking,
  stringsAsFactors = FALSE
)

readr::write_csv(
  performance_ranking,
  file.path(
    report_dir,
    "05_model_performance_ranking.csv"
  )
)


# ==============================================================================
# 24. FULL TEXT CONSULTANCY REPORT
# ==============================================================================

report_file <- file.path(
  report_dir,
  "05_predictive_modeling_report.txt"
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
  "TASK 5: PREDICTIVE STATISTICAL MODELLING & DIAGNOSTICS\n",
  "============================================================\n\n",
  sep = "",
  file = report_connection
)

cat(
  "Dataset: Fertilizer_Dataset.csv\n",
  "Train/Test Split: 80% / 20%\n",
  "Random Seed: 123\n",
  "Evaluation criterion for best model: Lowest test-set RMSE\n\n",
  sep = "",
  file = report_connection
)


# ------------------------------------------------------------------------------
# Data Preparation
# ------------------------------------------------------------------------------

cat(
  "------------------------------------------------------------\n",
  "1. DATA PREPARATION\n",
  "------------------------------------------------------------\n\n",
  sep = "",
  file = report_connection
)

cat(
  "Original rows: ",
  original_n,
  "\n",
  "Duplicate rows removed: ",
  duplicate_n,
  "\n",
  "Rows after duplicate removal: ",
  cleaned_n,
  "\n\n",
  sep = "",
  file = report_connection
)

cat(
  "Excluded identifier: Sample_ID\n",
  "Excluded potential leakage variables:\n",
  "  - Recommended_N_kg_ha\n",
  "  - Recommended_P2O5_kg_ha\n",
  "  - Recommended_K2O_kg_ha\n\n",
  sep = "",
  file = report_connection
)


# ------------------------------------------------------------------------------
# Model Comparison
# ------------------------------------------------------------------------------

cat(
  "------------------------------------------------------------\n",
  "2. TEST-SET MODEL PERFORMANCE\n",
  "------------------------------------------------------------\n\n",
  sep = "",
  file = report_connection
)

capture.output(
  print(
    performance_comparison %>%
      arrange(
        target,
        RMSE
      )
  ),
  file = report_connection,
  append = TRUE
)


# ------------------------------------------------------------------------------
# Best Models
# ------------------------------------------------------------------------------

cat(
  "\n------------------------------------------------------------\n",
  "3. BEST MODEL BY TARGET USING TEST-SET RMSE\n",
  "------------------------------------------------------------\n\n",
  sep = "",
  file = report_connection
)

capture.output(
  print(
    best_models
  ),
  file = report_connection,
  append = TRUE
)


# ------------------------------------------------------------------------------
# Diagnostics
# ------------------------------------------------------------------------------

cat(
  "\n------------------------------------------------------------\n",
  "4. MLR DIAGNOSTICS\n",
  "------------------------------------------------------------\n\n",
  sep = "",
  file = report_connection
)


for(target in names(diagnostic_results)){

  cat(
    "\nTarget: ",
    target,
    "\n\n",
    sep = "",
    file = report_connection
  )

  cat(
    "Breusch-Pagan Test:\n",
    sep = "",
    file = report_connection
  )

  capture.output(
    print(
      diagnostic_results[[target]]$bp
    ),
    file = report_connection,
    append = TRUE
  )

  cat(
    "\nShapiro-Wilk Test:\n",
    sep = "",
    file = report_connection
  )

  capture.output(
    print(
      diagnostic_results[[target]]$shapiro
    ),
    file = report_connection,
    append = TRUE
  )

  cat(
    "\nVIF Results:\n",
    sep = "",
    file = report_connection
  )

  capture.output(
    print(
      vif_results[[target]]
    ),
    file = report_connection,
    append = TRUE
  )

}


# ------------------------------------------------------------------------------
# Interpretation Guide
# ------------------------------------------------------------------------------

cat(
  "\n------------------------------------------------------------\n",
  "5. DIAGNOSTIC INTERPRETATION GUIDE\n",
  "------------------------------------------------------------\n\n",
  sep = "",
  file = report_connection
)

cat(
  "R-squared:\n",
  "Measures the proportion of variation in the target variable explained ",
  "by the predictors on the test set.\n\n",
  sep = "",
  file = report_connection
)

cat(
  "Adjusted R-squared:\n",
  "Adjusts R-squared for the number of predictors relative to the number ",
  "of test observations. It is useful when comparing models with different ",
  "numbers of effective predictors.\n\n",
  sep = "",
  file = report_connection
)

cat(
  "RMSE:\n",
  "Measures the square root of mean squared prediction error. Lower values ",
  "indicate smaller prediction errors in the target's original units.\n\n",
  sep = "",
  file = report_connection
)

cat(
  "MAE:\n",
  "Measures mean absolute prediction error. Lower values indicate smaller ",
  "average absolute prediction errors.\n\n",
  sep = "",
  file = report_connection
)

cat(
  "VIF:\n",
  "Large VIF values indicate stronger linear dependence among predictors. ",
  "For categorical predictors, car::vif() may report generalized VIF (GVIF). ",
  "The adjusted GVIF is reported as GVIF^(1/(2*Df)).\n\n",
  sep = "",
  file = report_connection
)

cat(
  "Breusch-Pagan test:\n",
  "Tests the null hypothesis that the regression errors have constant ",
  "variance. A small p-value provides evidence against homoscedasticity.\n\n",
  sep = "",
  file = report_connection
)

cat(
  "Shapiro-Wilk test:\n",
  "Tests the null hypothesis that the tested residual sample is normally ",
  "distributed. A small p-value provides evidence against normality. ",
  "Because large datasets can make normality tests extremely sensitive, ",
  "the Q-Q plot should also be considered when interpreting residual normality.\n\n",
  sep = "",
  file = report_connection
)


# ------------------------------------------------------------------------------
# Consultancy Interpretation
# ------------------------------------------------------------------------------

cat(
  "------------------------------------------------------------\n",
  "6. CONSULTANCY INTERPRETATION\n",
  "------------------------------------------------------------\n\n",
  sep = "",
  file = report_connection
)

cat(
  "The predictive modelling stage evaluates whether soil, weather and ",
  "categorical agricultural context variables provide useful statistical ",
  "information for predicting fertilizer application rates.\n\n",
  sep = "",
  file = report_connection
)

cat(
  "The three fertilizer targets are modelled independently because Urea, ",
  "TSP and MOP represent different fertilizer application quantities.\n\n",
  sep = "",
  file = report_connection
)

cat(
  "The Recommendation variables were deliberately excluded because they ",
  "are reference recommendation features and may contain information that ",
  "would not be legitimately available when making an independent prediction. ",
  "Their exclusion reduces the risk of target leakage.\n\n",
  sep = "",
  file = report_connection
)

cat(
  "The final model comparison should be interpreted using the held-out ",
  "20% test data rather than training-set fit alone. The model selected ",
  "using the lowest test-set RMSE is the best-performing model under this ",
  "specific evaluation criterion and dataset split.\n\n",
  sep = "",
  file = report_connection
)

cat(
  "Model performance should also be considered together with diagnostic ",
  "evidence. High predictive accuracy does not automatically imply that ",
  "the underlying statistical assumptions are satisfied.\n\n",
  sep = "",
  file = report_connection
)


# ------------------------------------------------------------------------------
# Generated Files
# ------------------------------------------------------------------------------

cat(
  "------------------------------------------------------------\n",
  "7. GENERATED FILES\n",
  "------------------------------------------------------------\n\n",
  sep = "",
  file = report_connection
)

cat(
  "Figures:\n",
  "  output/figures/05_mlr_diagnostics_urea.png\n",
  "  output/figures/05_mlr_diagnostics_tsp.png\n",
  "  output/figures/05_mlr_diagnostics_mop.png\n",
  "  output/figures/05_predicted_vs_actual.png\n\n",
  sep = "",
  file = report_connection
)

cat(
  "Reports:\n",
  "  output/reports/05_model_performance_comparison.csv\n",
  "  output/reports/05_best_model_by_target.csv\n",
  "  output/reports/05_model_performance_ranking.csv\n",
  "  output/reports/05_all_model_predictions.csv\n",
  "  output/reports/05_mlr_vif_results.csv\n",
  "  output/reports/05_breusch_pagan_results.csv\n",
  "  output/reports/05_shapiro_wilk_results.csv\n",
  "  output/reports/05_mlr_coefficients.csv\n",
  "  output/reports/05_ridge_lasso_coefficients.csv\n",
  "  output/reports/05_predictive_modeling_report.txt\n\n",
  sep = "",
  file = report_connection
)


# ------------------------------------------------------------------------------
# Session Information
# ------------------------------------------------------------------------------

cat(
  "------------------------------------------------------------\n",
  "8. SESSION INFORMATION\n",
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
# 25. FINAL CONSOLE SUMMARY
# ==============================================================================

cat(
  "\n\n============================================================\n",
  "TASK 5 COMPLETED SUCCESSFULLY\n",
  "============================================================\n\n",
  sep = ""
)

cat(
  "Model performance comparison saved to:\n",
  "output/reports/05_model_performance_comparison.csv\n\n",
  sep = ""
)

cat(
  "Best model by target:\n"
)

print(
  best_models %>%
    select(
      target,
      model,
      r_squared,
      adjusted_r_squared,
      RMSE,
      MAE
    )
)


cat(
  "\nDiagnostic figures saved to:\n",
  "output/figures/05_mlr_diagnostics_urea.png\n",
  "output/figures/05_mlr_diagnostics_tsp.png\n",
  "output/figures/05_mlr_diagnostics_mop.png\n",
  "output/figures/05_predicted_vs_actual.png\n\n",
  sep = ""
)

cat(
  "Full statistical modelling report saved to:\n",
  "output/reports/05_predictive_modeling_report.txt\n",
  "\n============================================================\n",
  sep = ""
)
