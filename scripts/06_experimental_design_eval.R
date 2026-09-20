# scripts/06_experimental_design_eval.R

# ============================================================
# IT3081 Statistical Modelling - Consultancy Project
# Task 6: Critical Evaluation of Experimental Design
# CRD vs RCBD for Sri Lankan Paddy Fertilizer Trials
# ============================================================

# ------------------------------------------------------------
# 0. DIRECTORY SAFETY
# ------------------------------------------------------------

if(!dir.exists("output/figures")) dir.create("output/figures", recursive = TRUE)
if(!dir.exists("output/reports")) dir.create("output/reports", recursive = TRUE)

# ------------------------------------------------------------
# 1. PACKAGE MANAGEMENT
# ------------------------------------------------------------

required_packages <- c(
  "tidyverse",
  "ggplot2",
  "gridExtra"
)

missing_packages <- required_packages[
  !required_packages %in% rownames(installed.packages())
]

if(length(missing_packages) > 0){
  stop(
    paste0(
      "The following required packages are missing: ",
      paste(missing_packages, collapse = ", "),
      "\nInstall them using:\ninstall.packages(c(",
      paste0('"', missing_packages, '"', collapse = ", "),
      "))"
    )
  )
}

suppressPackageStartupMessages({
  library(tidyverse)
  library(ggplot2)
  library(gridExtra)
})

# ------------------------------------------------------------
# 2. GLOBAL SETTINGS
# ------------------------------------------------------------

set.seed(123)

figure_dir <- "output/figures"
report_dir <- "output/reports"

# ------------------------------------------------------------
# 3. LOAD PROJECT DATA
# ------------------------------------------------------------

data_file <- "Fertilizer_Dataset.csv"

if(!file.exists(data_file)){
  stop(
    paste0(
      "Dataset not found: ", data_file,
      "\nWorking directory: ", getwd()
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
# 4. DATASET CONTEXT CHECK
# ------------------------------------------------------------

required_context_variables <- c(
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

missing_context_variables <- setdiff(
  required_context_variables,
  names(fertilizer_data)
)

if(length(missing_context_variables) > 0){
  warning(
    paste(
      "The following contextual variables are not present in the dataset:",
      paste(missing_context_variables, collapse = ", ")
    )
  )
}

# Convert categorical variables to factors when available
categorical_variables <- intersect(
  c("District", "Agro_Zone", "Season"),
  names(fertilizer_data)
)

for(v in categorical_variables){
  fertilizer_data[[v]] <- as.factor(fertilizer_data[[v]])
}

# ------------------------------------------------------------
# 5. PROJECT SCOPE
# ------------------------------------------------------------

# This task evaluates experimental designs for future field
# trials rather than treating the observational dataset itself
# as a randomized experiment.
#
# The current dataset provides contextual variables that are
# useful for identifying plausible sources of field variability:
#
#   - Agro_Zone
#   - Soil_pH
#   - Soil nutrient status
#   - Rainfall
#   - Temperature
#   - District
#   - Season
#
# Slope and drainage are not present in the supplied dataset.
# They are therefore discussed as recommended field-level
# blocking variables rather than analysed empirically.

# ------------------------------------------------------------
# 6. CRD AND RCBD THEORETICAL FRAMEWORK
# ------------------------------------------------------------

crd_description <- paste(
  "A Completely Randomized Design (CRD) randomly assigns treatments",
  "to all available experimental units without explicitly modelling",
  "a blocking factor. It is statistically simple and can be useful",
  "when experimental units are sufficiently homogeneous."
)

rcbd_description <- paste(
  "A Randomized Complete Block Design (RCBD) divides experimental",
  "units into relatively homogeneous blocks and assigns every treatment",
  "once within each block. The block effect is then included in the",
  "statistical model, allowing systematic nuisance variability to be",
  "separated from residual experimental error."
)

nuisance_variability <- data.frame(
  source = c(
    "Soil fertility gradient",
    "Soil pH variation",
    "Moisture / drainage gradient",
    "Agro-ecological differences",
    "Topographic or slope gradient",
    "Field position / spatial variation",
    "Seasonal environmental variation"
  ),
  relevance_to_paddy_trials = c(
    "Can influence nutrient availability and crop response independently of fertilizer treatment.",
    "Can alter nutrient availability and fertilizer response, particularly phosphorus availability.",
    "Unequal water availability can change nutrient uptake and crop growth.",
    "Rainfall, temperature and soil conditions differ across agro-ecological environments.",
    "Elevation and slope can affect drainage and water retention.",
    "Nearby plots may be more similar than distant plots, creating spatial dependence.",
    "Maha and Yala seasons can expose treatments to different environmental conditions."
  ),
  possible_blocking_strategy = c(
    "Block plots along a measured fertility gradient.",
    "Create pH-based homogeneous blocks after baseline soil sampling.",
    "Block across drainage or moisture zones.",
    "Use separate trials or blocks within a clearly defined agro-ecological environment.",
    "Arrange blocks perpendicular to the dominant slope gradient.",
    "Use spatially coherent blocks and randomized treatment order within each block.",
    "Treat season as a design factor or conduct repeated trials across seasons."
  ),
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------
# 7. BLOCKING FACTORS RELEVANT TO THE DATASET
# ------------------------------------------------------------

# Agro-Ecological Zone
#
# Agro_Zone is directly represented in the dataset and is a
# defensible contextual variable for stratification or for
# defining separate experimental environments.
#
# However, Agro_Zone should not automatically be used as the
# within-field RCBD block if the trial is conducted inside only
# one relatively homogeneous location. A block should represent
# local nuisance variability among plots.

# Soil pH category
#
# Soil pH is measured continuously in the supplied dataset.
# For field implementation, baseline soil sampling could be used
# to classify a field into meaningful pH strata before randomization.
#
# Categorization should be based on agronomic thresholds and
# field-specific distributions rather than arbitrary quantiles.

if("Soil_pH" %in% names(fertilizer_data)){

  pH_values <- fertilizer_data$Soil_pH[
    is.finite(fertilizer_data$Soil_pH)
  ]

  if(length(pH_values) > 0){

    # Quantile categories are used only for exploratory illustration.
    # They are NOT presented as official Sri Lankan agronomic
    # classification thresholds.
    pH_breaks <- quantile(
      pH_values,
      probs = c(0, 1/3, 2/3, 1),
      na.rm = TRUE,
      names = FALSE
    )

    pH_breaks <- unique(pH_breaks)

    if(length(pH_breaks) >= 3){

      fertilizer_data$Soil_pH_category <- cut(
        fertilizer_data$Soil_pH,
        breaks = pH_breaks,
        include.lowest = TRUE,
        labels = c(
          "Lower_pH",
          "Middle_pH",
          "Higher_pH"
        )[seq_len(length(pH_breaks) - 1)]
      )

    } else {

      fertilizer_data$Soil_pH_category <- factor(
        "Single_pH_range"
      )

    }
  }
}

# ------------------------------------------------------------
# 8. EXPERIMENTAL DESIGN COMPARISON TABLE
# ------------------------------------------------------------

design_comparison <- data.frame(
  Criterion = c(
    "Randomization",
    "Blocking",
    "Primary assumption",
    "Handling nuisance spatial variability",
    "Residual variance",
    "Statistical efficiency",
    "Analysis complexity",
    "Missing plots",
    "Suitability for heterogeneous field",
    "Suitability for homogeneous field",
    "Field implementation"
  ),
  CRD = c(
    "All experimental units randomized across treatments.",
    "No explicit blocking factor.",
    "Experimental units are reasonably homogeneous.",
    "Poorer control when systematic gradients exist.",
    "May contain both treatment error and systematic field heterogeneity.",
    "Can be efficient when the experimental area is homogeneous.",
    "Simple.",
    "Generally comparatively straightforward.",
    "Less suitable.",
    "Suitable.",
    "Simple randomization and management."
  ),
  RCBD = c(
    "Treatments randomized independently within each block.",
    "Each block contains the complete treatment set.",
    "Units within a block are more homogeneous than units across blocks.",
    "Explicitly accounts for block-level nuisance variation.",
    "Can be lower when the blocking factor explains meaningful variability.",
    "Often beneficial when field heterogeneity follows a recognizable gradient.",
    "More complex than CRD.",
    "Requires care because incomplete blocks can complicate analysis.",
    "More suitable when heterogeneity can be represented by blocks.",
    "May be unnecessary if blocks provide little variance reduction.",
    "Requires field mapping, block construction and restricted randomization."
  ),
  stringsAsFactors = FALSE
)

readr::write_csv(
  as.data.frame(design_comparison),
  file.path(report_dir, "06_crd_vs_rcbd_design_comparison.csv")
)

readr::write_csv(
  as.data.frame(nuisance_variability),
  file.path(report_dir, "06_nuisance_variability_and_blocking.csv")
)

# ------------------------------------------------------------
# 9. SIMULATION PARAMETERS
# ------------------------------------------------------------

# A hypothetical fertilizer experiment is simulated.
#
# Four fertilizer treatments:
#   T1 = Control
#   T2 = Low fertilizer
#   T3 = Medium fertilizer
#   T4 = High fertilizer
#
# Six blocks are used.
#
# The treatment effect is deliberately moderate while the block
# effect is substantial. This creates a realistic demonstration
# of why blocking can reduce unexplained residual variation when
# a field contains systematic heterogeneity.

n_treatments <- 4
n_blocks <- 6

treatment_names <- c(
  "Control",
  "Low",
  "Medium",
  "High"
)

treatment_effects <- c(
  0,
  4,
  8,
  12
)

block_effects <- c(
  -15,
  -8,
  -3,
  4,
  9,
  13
)

grand_mean <- 100
residual_sd <- 8

# ------------------------------------------------------------
# 10. GENERATE RCBD FIELD LAYOUT
# ------------------------------------------------------------

rcbd_layout <- expand.grid(
  Block = factor(
    paste0("B", seq_len(n_blocks)),
    levels = paste0("B", seq_len(n_blocks))
  ),
  Treatment = factor(
    treatment_names,
    levels = treatment_names
  ),
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = TRUE
)

# Randomize treatment order independently within every block.
rcbd_layout <- rcbd_layout %>%
  group_by(Block) %>%
  mutate(
    Random_Order = sample(seq_len(n()))
  ) %>%
  arrange(Block, Random_Order) %>%
  ungroup()

# Assign spatial positions for plotting.
rcbd_layout <- rcbd_layout %>%
  group_by(Block) %>%
  mutate(
    Plot = row_number()
  ) %>%
  ungroup()

# Generate response.
rcbd_data <- rcbd_layout %>%
  mutate(
    Treatment_Effect = treatment_effects[
      match(as.character(Treatment), treatment_names)
    ],
    Block_Effect = block_effects[
      as.numeric(Block)
    ],
    Error = rnorm(
      n(),
      mean = 0,
      sd = residual_sd
    ),
    Yield = grand_mean +
      Treatment_Effect +
      Block_Effect +
      Error
  )

# ------------------------------------------------------------
# 11. CRD LAYOUT FOR COMPARISON
# ------------------------------------------------------------

# A CRD does not explicitly organize plots into homogeneous
# blocks. For visual comparison, the same field observations are
# shown as a randomized collection of plots.
#
# This visualization is conceptual: the CRD analysis ignores
# the Block variable even though the simulated field contains
# spatial heterogeneity.

crd_layout <- rcbd_data %>%
  mutate(
    CRD_Position = sample(
      seq_len(n()),
      size = n(),
      replace = FALSE
    )
  ) %>%
  arrange(CRD_Position) %>%
  mutate(
    CRD_Row = ceiling(row_number() / n_treatments),
    CRD_Column = ((row_number() - 1) %% n_treatments) + 1
  )

# ------------------------------------------------------------
# 12. FIT CRD AND RCBD ANOVA MODELS
# ------------------------------------------------------------

crd_model <- aov(
  Yield ~ Treatment,
  data = rcbd_data
)

rcbd_model <- aov(
  Yield ~ Treatment + Block,
  data = rcbd_data
)

# ------------------------------------------------------------
# 13. EXTRACT ANOVA RESULTS
# ------------------------------------------------------------

crd_anova <- anova(crd_model)
rcbd_anova <- anova(rcbd_model)

crd_residual_mse <- summary(crd_model)[[1]][
  "Residuals",
  "Mean Sq"
]

rcbd_residual_mse <- summary(rcbd_model)[[1]][
  "Residuals",
  "Mean Sq"
]

crd_treatment_p <- crd_anova[
  "Treatment",
  "Pr(>F)"
]

rcbd_treatment_p <- rcbd_anova[
  "Treatment",
  "Pr(>F)"
]

crd_treatment_f <- crd_anova[
  "Treatment",
  "F value"
]

rcbd_treatment_f <- rcbd_anova[
  "Treatment",
  "F value"
]

crd_treatment_df <- crd_anova[
  "Treatment",
  "Df"
]

rcbd_treatment_df <- rcbd_anova[
  "Treatment",
  "Df"
]

# ------------------------------------------------------------
# 14. VARIANCE COMPONENTS
# ------------------------------------------------------------

total_ss <- sum(
  (rcbd_data$Yield - mean(rcbd_data$Yield))^2
)

crd_treatment_ss <- crd_anova[
  "Treatment",
  "Sum Sq"
]

crd_error_ss <- crd_anova[
  "Residuals",
  "Sum Sq"
]

rcbd_treatment_ss <- rcbd_anova[
  "Treatment",
  "Sum Sq"
]

rcbd_block_ss <- rcbd_anova[
  "Block",
  "Sum Sq"
]

rcbd_error_ss <- rcbd_anova[
  "Residuals",
  "Sum Sq"
]

variance_decomposition <- data.frame(
  Design = c(
    "CRD",
    "CRD",
    "RCBD",
    "RCBD",
    "RCBD"
  ),
  Component = c(
    "Treatment",
    "Residual/Error",
    "Treatment",
    "Block",
    "Residual/Error"
  ),
  Sum_Squares = c(
    crd_treatment_ss,
    crd_error_ss,
    rcbd_treatment_ss,
    rcbd_block_ss,
    rcbd_error_ss
  )
)

variance_decomposition <- variance_decomposition %>%
  mutate(
    Percent_Total_SS = 100 * Sum_Squares / total_ss
  )

# ------------------------------------------------------------
# 15. CRD VS RCBD SUMMARY
# ------------------------------------------------------------

simulation_summary <- data.frame(
  Design = c("CRD", "RCBD"),
  Treatment_DF = c(
    crd_treatment_df,
    rcbd_treatment_df
  ),
  Treatment_F = c(
    crd_treatment_f,
    rcbd_treatment_f
  ),
  Treatment_P_Value = c(
    crd_treatment_p,
    rcbd_treatment_p
  ),
  Residual_DF = c(
    crd_anova["Residuals", "Df"],
    rcbd_anova["Residuals", "Df"]
  ),
  Residual_MSE = c(
    crd_residual_mse,
    rcbd_residual_mse
  ),
  Treatment_SS = c(
    crd_treatment_ss,
    rcbd_treatment_ss
  ),
  Block_SS = c(
    0,
    rcbd_block_ss
  ),
  Residual_SS = c(
    crd_error_ss,
    rcbd_error_ss
  ),
  stringsAsFactors = FALSE
)

simulation_summary <- simulation_summary %>%
  mutate(
    Residual_SD = sqrt(Residual_MSE)
  )

# ------------------------------------------------------------
# 16. POWER SIMULATION FUNCTION
# ------------------------------------------------------------

# Power is estimated empirically.
#
# For each treatment-effect magnitude:
#   1. Generate many hypothetical experiments.
#   2. Fit the CRD model.
#   3. Fit the RCBD model.
#   4. Record whether the treatment p-value is below alpha.
#
# Power therefore represents the proportion of simulated trials
# that correctly reject the null hypothesis under the specified
# treatment effect.
#
# The simulation is conditional on the selected number of
# treatments, blocks and variance structure.

simulate_power <- function(
    treatment_effect_magnitude,
    n_sim = 1000,
    n_blocks = 6,
    residual_sd = 8,
    alpha = 0.05
){

  treatment_pattern <- c(
    0,
    treatment_effect_magnitude / 3,
    2 * treatment_effect_magnitude / 3,
    treatment_effect_magnitude
  )

  crd_rejections <- logical(n_sim)
  rcbd_rejections <- logical(n_sim)

  for(i in seq_len(n_sim)){

    block_effect_i <- block_effects[
      seq_len(n_blocks)
    ]

    simulation_data <- expand.grid(
      Block = factor(
        paste0("B", seq_len(n_blocks))
      ),
      Treatment = factor(
        treatment_names,
        levels = treatment_names
      ),
      KEEP.OUT.ATTRS = FALSE,
      stringsAsFactors = TRUE
    )

    simulation_data <- simulation_data %>%
      mutate(
        Treatment_Effect = treatment_pattern[
          match(
            as.character(Treatment),
            treatment_names
          )
        ],
        Block_Effect = block_effect_i[
          as.numeric(Block)
        ],
        Yield =
          grand_mean +
          Treatment_Effect +
          Block_Effect +
          rnorm(
            n(),
            mean = 0,
            sd = residual_sd
          )
      )

    crd_fit <- aov(
      Yield ~ Treatment,
      data = simulation_data
    )

    rcbd_fit <- aov(
      Yield ~ Treatment + Block,
      data = simulation_data
    )

    crd_p <- summary(crd_fit)[[1]][
      "Treatment",
      "Pr(>F)"
    ]

    rcbd_p <- summary(rcbd_fit)[[1]][
      "Treatment",
      "Pr(>F)"
    ]

    crd_rejections[i] <- is.finite(crd_p) &&
      crd_p < alpha

    rcbd_rejections[i] <- is.finite(rcbd_p) &&
      rcbd_p < alpha
  }

  data.frame(
    Treatment_Effect = treatment_effect_magnitude,
    Design = c("CRD", "RCBD"),
    Power = c(
      mean(crd_rejections),
      mean(rcbd_rejections)
    ),
    Simulations = n_sim,
    Alpha = alpha
  )
}

# ------------------------------------------------------------
# 17. POWER CURVE
# ------------------------------------------------------------

power_effects <- seq(
  from = 0,
  to = 20,
  by = 2
)

power_results_list <- lapply(
  power_effects,
  simulate_power,
  n_sim = 1000,
  n_blocks = n_blocks,
  residual_sd = residual_sd,
  alpha = 0.05
)

power_results <- bind_rows(
  power_results_list
)

# ------------------------------------------------------------
# 18. POWER CURVE PLOT
# ------------------------------------------------------------

power_plot <- ggplot(
  power_results,
  aes(
    x = Treatment_Effect,
    y = Power,
    linetype = Design,
    shape = Design
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.8) +
  geom_hline(
    yintercept = 0.80,
    linetype = "dashed"
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.1)
  ) +
  scale_x_continuous(
    breaks = power_effects
  ) +
  labs(
    title = "Empirical Statistical Power: CRD vs RCBD",
    subtitle = paste0(
      "Four treatments, ", n_blocks,
      " blocks, residual SD = ", residual_sd,
      ", 1,000 simulations per effect size"
    ),
    x = "Maximum treatment-effect difference",
    y = "Estimated power",
    linetype = "Design",
    shape = "Design",
    caption = "Power = proportion of simulations with treatment p-value < 0.05."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

ggsave(
  filename = file.path(
    figure_dir,
    "06_crd_vs_rcbd_power_comparison.png"
  ),
  plot = power_plot,
  width = 10,
  height = 6.5,
  dpi = 300
)

# ------------------------------------------------------------
# 19. FIELD LAYOUT PLOTS
# ------------------------------------------------------------

rcbd_plot <- ggplot(
  rcbd_data,
  aes(
    x = Plot,
    y = Block,
    fill = Treatment
  )
) +
  geom_tile(
    colour = "white",
    linewidth = 0.8
  ) +
  geom_text(
    aes(label = Treatment),
    size = 3.2
  ) +
  scale_x_continuous(
    breaks = seq_len(n_treatments)
  ) +
  labs(
    title = "Conceptual RCBD Field Layout",
    subtitle = "Every block contains all four treatments",
    x = "Plot position within block",
    y = "Block",
    fill = "Treatment"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

crd_plot <- ggplot(
  crd_layout,
  aes(
    x = CRD_Column,
    y = CRD_Row,
    fill = Treatment
  )
) +
  geom_tile(
    colour = "white",
    linewidth = 0.8
  ) +
  geom_text(
    aes(label = Treatment),
    size = 3.0
  ) +
  scale_y_reverse() +
  labs(
    title = "Conceptual CRD Field Layout",
    subtitle = "Treatments randomized without explicit blocks",
    x = "Field column",
    y = "Field row",
    fill = "Treatment"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

# ------------------------------------------------------------
# 20. VARIANCE DECOMPOSITION PLOT
# ------------------------------------------------------------

variance_plot_data <- variance_decomposition %>%
  mutate(
    Design = factor(
      Design,
      levels = c("CRD", "RCBD")
    ),
    Component = factor(
      Component,
      levels = c(
        "Treatment",
        "Block",
        "Residual/Error"
      )
    )
  )

variance_plot <- ggplot(
  variance_plot_data,
  aes(
    x = Design,
    y = Percent_Total_SS,
    fill = Component
  )
) +
  geom_col(
    position = "stack",
    width = 0.65
  ) +
  geom_text(
    aes(
      label = ifelse(
        Percent_Total_SS >= 5,
        paste0(
          round(Percent_Total_SS, 1),
          "%"
        ),
        ""
      )
    ),
    position = position_stack(vjust = 0.5),
    size = 3.5
  ) +
  labs(
    title = "Variance Decomposition: CRD vs RCBD",
    subtitle = "RCBD explicitly attributes systematic block variation to the block component",
    x = "Experimental design",
    y = "Percentage of total sum of squares",
    fill = "Variance component"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

# ------------------------------------------------------------
# 21. COMBINED EXPERIMENTAL DESIGN FIGURE
# ------------------------------------------------------------

combined_design_figure <- gridExtra::arrangeGrob(
  rcbd_plot,
  crd_plot,
  variance_plot,
  ncol = 2,
  heights = c(1, 1)
)

png(
  filename = file.path(
    figure_dir,
    "06_crd_vs_rcbd_variance_decomposition.png"
  ),
  width = 12,
  height = 10,
  units = "in",
  res = 300
)

grid::grid.newpage()
grid::grid.draw(combined_design_figure)

dev.off()

# ------------------------------------------------------------
# 22. EXPORT ANOVA COMPARISON TABLE
# ------------------------------------------------------------

anova_comparison <- simulation_summary %>%
  mutate(
    Residual_MSE_Reduction_vs_CRD = ifelse(
      Design == "CRD",
      0,
      100 * (
        crd_residual_mse - Residual_MSE
      ) / crd_residual_mse
    ),
    Residual_SS_Reduction_vs_CRD = ifelse(
      Design == "CRD",
      0,
      100 * (
        crd_error_ss - Residual_SS
      ) / crd_error_ss
    )
  )

readr::write_csv(
  as.data.frame(anova_comparison),
  file.path(
    report_dir,
    "06_crd_vs_rcbd_simulation_results.csv"
  )
)

# ------------------------------------------------------------
# 23. EXPORT VARIANCE DECOMPOSITION TABLE
# ------------------------------------------------------------

readr::write_csv(
  as.data.frame(variance_decomposition),
  file.path(
    report_dir,
    "06_variance_decomposition.csv"
  )
)

# ------------------------------------------------------------
# 24. EXPORT POWER SIMULATION RESULTS
# ------------------------------------------------------------

readr::write_csv(
  as.data.frame(power_results),
  file.path(
    report_dir,
    "06_crd_vs_rcbd_power_simulation.csv"
  )
)

# ------------------------------------------------------------
# 25. EXPORT SIMULATED FIELD DATA
# ------------------------------------------------------------

readr::write_csv(
  as.data.frame(rcbd_data),
  file.path(
    report_dir,
    "06_simulated_rcbd_field_trial.csv"
  )
)

# ------------------------------------------------------------
# 26. GENERATE STATISTICAL INTERPRETATION
# ------------------------------------------------------------

mse_reduction <- 100 * (
  crd_residual_mse - rcbd_residual_mse
) / crd_residual_mse

ss_reduction <- 100 * (
  crd_error_ss - rcbd_error_ss
) / crd_error_ss

treatment_p_interpretation <- if(
  rcbd_treatment_p < 0.05
){
  "The simulated RCBD provides evidence of a treatment effect at the 5% significance level."
} else {
  "The simulated RCBD does not provide evidence of a treatment effect at the 5% significance level."
}

if(
  rcbd_treatment_p < crd_treatment_p
){
  p_comparison_text <- paste(
    "In this simulated field, the RCBD produced a smaller treatment",
    "p-value than the CRD because the block effect captured systematic",
    "between-block variability that would otherwise contribute to the",
    "CRD residual term."
  )
} else {
  p_comparison_text <- paste(
    "In this particular simulation, the RCBD did not produce a smaller",
    "treatment p-value than the CRD. This demonstrates that blocking is",
    "not automatically beneficial; its value depends on whether the",
    "chosen block factor explains meaningful nuisance variability."
  )
}

# ------------------------------------------------------------
# 27. MARKDOWN REPORT CONTENT
# ------------------------------------------------------------

report_text <- paste0(
"# Task 6: Critical Evaluation of Experimental Design\n\n",

"## IT3081 Statistical Modelling Consultancy Project\n\n",

"### Data-Driven Precision Fertilizer Advisory & Strategic Agriculture Framework\n\n",

"**Analysis file:** `scripts/06_experimental_design_eval.R`  \n",
"**Dataset:** `Fertilizer_Dataset.csv`  \n",
"**Simulation seed:** 123  \n\n",

"---\n\n",

"## 1. Purpose of the Evaluation\n\n",

"This task evaluates Completely Randomized Design (CRD) and ",
"Randomized Complete Block Design (RCBD) for future fertilizer and ",
"agronomic field experiments in Sri Lankan paddy cultivation. ",
"The supplied dataset is used to establish the agricultural context ",
"and identify measurable sources of field heterogeneity. The ",
"experimental comparison itself is based on a controlled simulation ",
"because the supplied observational dataset is not a randomized ",
"field trial.\n\n",

"---\n\n",

"## 2. Agricultural Context\n\n",

"Rice is cultivated across a wide range of climatic, soil and ",
"hydrological conditions in Sri Lanka. Department of Agriculture ",
"material notes that rice production occurs across different ",
"agro-ecological environments and that crop performance can be ",
"affected by micro-environmental conditions including rainfall, ",
"temperature, soil characteristics and water regimes. ",
"These conditions make experimental control of local field ",
"heterogeneity important when evaluating fertilizer responses.\n\n",

"Source: Department of Agriculture, Sri Lanka, Rice Congress material: ",
"https://doa.gov.lk/wp-content/uploads/2020/05/Rice-Congress-2010.pdf\n\n",

"The Department of Agriculture also maintains resources covering ",
"paddy soil fertility, agro-ecological regions and soil pH mapping, ",
"demonstrating the practical relevance of spatial soil information ",
"to rice production and soil management.\n\n",

"Source: Department of Agriculture Map Gallery: ",
"https://doa.gov.lk/map-gallery/\n\n",

"---\n\n",

"## 3. Completely Randomized Design (CRD)\n\n",

crd_description,
"\n\n",

"### Advantages\n\n",

"- Simple randomization procedure.\n",
"- Straightforward ANOVA model.\n",
"- Easy to implement and explain.\n",
"- Flexible when experimental units are reasonably homogeneous.\n",
"- Less complicated if experimental units are lost or excluded.\n\n",

"### Limitations in heterogeneous paddy fields\n\n",

"The major limitation is that systematic field heterogeneity is ",
"absorbed into the residual error term. If plots differ because of ",
"soil fertility, drainage, moisture or another spatial gradient, ",
"the treatment comparison may become less precise.\n\n",

"The FAO description of field trial methods identifies this issue ",
"explicitly: CRD can be less precise because variation between ",
"experimental units is included in the experimental error term.\n\n",

"Source: FAO trial methods: ",
"https://www.fao.org/4/v5330e/V5330e0C.HTM\n\n",

"---\n\n",

"## 4. Randomized Complete Block Design (RCBD)\n\n",

rcbd_description,
"\n\n",

"### Advantages\n\n",

"- Controls a known or suspected source of nuisance variation.\n",
"- Provides treatment comparisons within relatively homogeneous blocks.\n",
"- Can reduce residual variance when the blocking factor explains meaningful field heterogeneity.\n",
"- Can improve precision and statistical power without necessarily increasing the number of treatments.\n",
"- Particularly useful for field experiments with identifiable spatial gradients.\n\n",

"### Limitations\n\n",

"- Blocks must be constructed using a meaningful source of heterogeneity.\n",
"- The number of treatments determines the size of a complete block.\n",
"- Very large blocks can themselves become heterogeneous.\n",
"- An inappropriate blocking variable can provide little benefit.\n",
"- More complicated field planning and randomization are required.\n",
"- If a treatment or plot is missing, the balanced structure can be affected.\n\n",

"The FAO field-trial guidance describes RCBD as appropriate when ",
"experimental units can be grouped meaningfully, including situations ",
"where plots differ along a slope or where soil depth changes across ",
"the experimental area.\n\n",

"Source: FAO trial methods: ",
"https://www.fao.org/4/v5330e/V5330e0C.HTM\n\n",

"---\n\n",

"## 5. Nuisance Variability in Sri Lankan Paddy Experiments\n\n",

"### 5.1 Soil fertility gradients\n\n",

"Total nitrogen, available phosphorus, available potassium and ",
"organic carbon can differ between plots. These differences may ",
"affect crop growth independently of the fertilizer treatment being ",
"tested. Blocking plots along a measured fertility gradient can ",
"therefore reduce unexplained variation.\n\n",

"The Department of Agriculture's Rice Research and Development ",
"Institute describes long-running fertilizer research directed at ",
"efficient use of nitrogen, phosphorus and potassium in paddy fields. ",
"This provides practical context for considering soil nutrient status ",
"when planning fertilizer experiments.\n\n",

"Source: Department of Agriculture, Soil Science Division: ",
"https://doa.gov.lk/rrdi_div_soilscience/\n\n",

"### 5.2 Soil pH\n\n",

"The supplied dataset contains `Soil_pH`. Soil pH is relevant to ",
"nutrient availability and fertilizer response. A field can therefore ",
"be sampled before treatment allocation and divided into relatively ",
"homogeneous pH strata if pH exhibits a meaningful spatial gradient.\n\n",

"The Department of Agriculture provides pH-based soil management ",
"information and paddy soil pH mapping resources, supporting the ",
"practical use of soil pH information in field management.\n\n",

"Source: Department of Agriculture soil mapping and management: ",
"https://doa.gov.lk/rrdi_technology_soilscience_MapBasedSoilReclamation/\n\n",

"### 5.3 Moisture and drainage gradients\n\n",

"Water availability is particularly important in paddy systems. ",
"Plots at different elevations or drainage positions may experience ",
"different water regimes. Blocking across a moisture or drainage ",
"gradient can reduce the influence of this nuisance source.\n\n",

"The Department of Agriculture reports that improved alternate ",
"wetting and drying practices depend on appropriate field and water ",
"management conditions and notes differences in suitability across ",
"irrigated areas and paddy environments.\n\n",

"Source: Department of Agriculture water management research: ",
"https://doa.gov.lk/rrdi_technology_soilscience_IAWD/\n\n",

"### 5.4 Agro-ecological variation\n\n",

"`Agro_Zone` is directly available in the project dataset. It captures ",
"broader environmental differences. For a single-location experiment, ",
"Agro-Zone would generally be better treated as a site-selection or ",
"stratification variable rather than automatically used as an ",
"individual within-field block. If trials span several agro-ecological ",
"environments, the zone can instead be incorporated into a multi-site ",
"experimental structure.\n\n",

"The Department of Agriculture identifies rice research stations and ",
"research activities associated with specific agro-ecological regions, ",
"illustrating the practical importance of environmental context in ",
"rice experimentation.\n\n",

"Source: Rice Research Station, Labuduwa: ",
"https://doa.gov.lk/rrdi_sub_labuduwa/\n\n",

"---\n\n",

"## 6. Recommended Blocking Variables\n\n",

"### Agro-Ecological Zone\n\n",

"Useful for stratifying multi-location trials or defining the ",
"environment in which a trial is conducted. It should not automatically ",
"be treated as the block factor inside one uniform field.\n\n",

"### Soil pH category\n\n",

"Potentially useful after baseline soil sampling identifies a spatial ",
"pH gradient. The categories should be based on agronomic interpretation ",
"rather than arbitrary statistical cut-points. The current script uses ",
"quantile categories only for exploratory illustration.\n\n",

"### Soil fertility index\n\n",

"A composite or carefully selected baseline measure based on nitrogen, ",
"phosphorus, potassium and organic carbon could be used when fertility ",
"is the dominant spatial gradient. This approach should be based on ",
"pre-treatment sampling and agronomic knowledge.\n\n",

"### Slope and drainage\n\n",

"Slope and drainage are not variables in the supplied dataset. ",
"Nevertheless, they are potentially important field-level blocking ",
"variables. Blocks should be oriented so that treatment comparisons ",
"occur among plots with similar elevation, slope position or drainage ",
"conditions.\n\n",

"---\n\n",

"## 7. Evidence from Sri Lankan Rice Research\n\n",

"RCBD is not merely a theoretical design for Sri Lankan agriculture. ",
"Published Sri Lankan rice research has used RCBD in field experiments. ",
"For example, a rice study examining responses in saline soil in the ",
"low-country wet zone used a factorial randomized complete block design ",
"with three replications.\n\n",

"Source: FAO AGRIS record, Rice yield responses in a saline soil in Sri Lanka: ",
"https://agris.fao.org/search/en/providers/122430/records/6471d3a52a40512c710e763b\n\n",

"Another Sri Lankan study investigating organic matter in lowland ",
"paddy soil under fertilizer and plant treatments used a randomized ",
"complete block design with four treatment combinations in a farmer ",
"field in Kurunegala District.\n\n",

"Source: FAO AGRIS record: ",
"https://agris.fao.org/search/en/providers/122436/records/67598f5ec7a957febdfc38b9\n\n",

"A further study of phosphorus sources for rice in acid sulphate soils ",
"in Matara District used six treatments in an RCBD with three replicates.\n\n",

"Source: FAO AGRIS record: ",
"https://agris.fao.org/search/en/providers/122436/records/67598fd9c7a957febdfc4c08\n\n",

"These examples demonstrate that RCBD is practically compatible with ",
"field-based rice research in Sri Lanka. They do not, however, establish ",
"that RCBD is optimal for every fertilizer experiment; the blocking ",
"factor must still correspond to meaningful field heterogeneity.\n\n",

"---\n\n",

"## 8. Experimental Simulation\n\n",

"The simulation represents a hypothetical fertilizer field trial with ",
"four treatments and six blocks. The treatment effects increase from ",
"the control to the high-fertilizer treatment. A systematic block effect ",
"is also introduced to represent a field gradient, while random error ",
"represents plot-to-plot variability not explained by treatment or block.\n\n",

"### Simulation parameters\n\n",

"| Parameter | Value |\n",
"|---|---:|\n",
"| Treatments | 4 |\n",
"| Blocks | 6 |\n",
"| Plots | 24 |\n",
"| Residual SD | ", residual_sd, " |\n",
"| Random seed | 123 |\n",
"| Alpha | 0.05 |\n\n",

"### Simulated treatment effects\n\n",

"| Treatment | Effect |\n",
"|---|---:|\n",
"| Control | 0 |\n",
"| Low | 4 |\n",
"| Medium | 8 |\n",
"| High | 12 |\n\n",

"---\n\n",

"## 9. CRD vs RCBD Simulation Results\n\n",

"```text\n",
"Design: CRD\n",
"Treatment F-statistic: ",
"    ", round(crd_treatment_f, 4), "\n",
"Treatment p-value: ",
"    ", format.pval(crd_treatment_p, digits = 5), "\n",
"Residual MSE: ",
"    ", round(crd_residual_mse, 4), "\n\n",

"Design: RCBD\n",
"Treatment F-statistic: ",
"    ", round(rcbd_treatment_f, 4), "\n",
"Treatment p-value: ",
"    ", format.pval(rcbd_treatment_p, digits = 5), "\n",
"Residual MSE: ",
"    ", round(rcbd_residual_mse, 4), "\n",
"Block F-statistic: ",
"    ", round(
  rcbd_anova["Block", "F value"],
  4
), "\n",
"Block p-value: ",
"    ", format.pval(
  rcbd_anova["Block", "Pr(>F)"],
  digits = 5
), "\n",
"```\n\n",

"### Residual MSE change\n\n",

"The simulated RCBD residual MSE is ",
round(rcbd_residual_mse, 3),
" compared with ",
round(crd_residual_mse, 3),
" for the CRD. The residual MSE reduction is approximately ",
round(mse_reduction, 2),
"%.\n\n",

"The corresponding reduction in residual sum of squares is approximately ",
round(ss_reduction, 2),
"%.\n\n",

"These quantities illustrate the central statistical benefit of an ",
"effective blocking factor: systematic variability that would otherwise ",
"be included in the residual term can be explicitly modelled as a block ",
"effect.\n\n",

p_comparison_text,
"\n\n",

treatment_p_interpretation,
"\n\n",

"---\n\n",

"## 10. Interpretation of Variance Decomposition\n\n",

"The variance decomposition separates the simulated total variation ",
"into treatment, block and residual components. Under CRD, block-related ",
"variation is not modelled and therefore contributes to the residual ",
"error. Under RCBD, the block component is explicitly included in the ",
"ANOVA model.\n\n",

"Consequently, the RCBD residual MSE can be substantially lower when ",
"the blocks capture an important spatial gradient. A lower residual MSE ",
"improves the precision of treatment comparisons because the treatment ",
"effect is evaluated relative to a smaller unexplained error variance.\n\n",

"The effect is conditional rather than guaranteed. If the proposed ",
"blocking variable is unrelated to the true nuisance variation, the ",
"RCBD may provide little or no efficiency gain while increasing design ",
"and analysis complexity.\n\n",

"---\n\n",

"## 11. Statistical Power Comparison\n\n",

"Empirical power was estimated through 1,000 simulated trials at each ",
"specified treatment-effect magnitude. Power is defined as the proportion ",
"of simulations in which the treatment p-value is below 0.05.\n\n",

"The resulting power curve is saved as:\n\n",

"`output/figures/06_crd_vs_rcbd_power_comparison.png`\n\n",

"The simulation should be interpreted as a controlled demonstration ",
"rather than a direct estimate of power for a future real-world trial. ",
"Actual power depends on the expected treatment effect, residual variance, ",
"number of treatments, number of blocks, number of seasons or locations, ",
"and the quality of the selected blocking factor.\n\n",

"---\n\n",

"## 12. Practical Feasibility for Sri Lankan Paddy Trials\n\n",

"An RCBD is operationally feasible when a field has a recognizable ",
"gradient and enough area to establish complete blocks. Before planting, ",
"researchers should map or sample relevant field characteristics such as ",
"soil nutrient status, pH, elevation, drainage and potentially electrical ",
"conductivity where salinity is relevant.\n\n",

"Treatment randomization should then occur independently within each ",
"block. Field maps should preserve the randomization record, and treatment ",
"application should be standardized to avoid introducing operational ",
"bias.\n\n",

"RCBD becomes less attractive when the number of treatments is very large, ",
"because each complete block must contain all treatments. Very large ",
"blocks may themselves contain substantial heterogeneity, weakening the ",
"assumption that plots within a block are sufficiently similar.\n\n",

"For multi-location or multi-agro-ecological research, a hierarchical ",
"or multi-environment design may be more appropriate than attempting to ",
"use Agro_Zone as a simple within-field block. The experimental structure ",
"should distinguish location-level environmental effects from local ",
"within-field block effects.\n\n",

"---\n\n",

"## 13. Recommended Experimental Workflow\n\n",

"1. Define the primary fertilizer treatment and response variable before field preparation.\n",
"2. Conduct baseline soil and field measurements before randomization.\n",
"3. Identify the dominant nuisance gradient using agronomic knowledge and spatial measurements.\n",
"4. Select blocks so that plots within a block are relatively homogeneous.\n",
"5. Include every treatment once within each complete block for an RCBD.\n",
"6. Randomize treatment positions independently within every block.\n",
"7. Maintain identical plot dimensions and management procedures where possible.\n",
"8. Record missing plots, irrigation interruptions, pest events and other deviations.\n",
"9. Analyse treatment effects using a model containing Treatment and Block.\n",
"10. Assess residual assumptions and inspect whether the blocking factor actually reduced residual variability.\n",
"11. Repeat experiments across relevant seasons or locations when the goal is broader agricultural generalization.\n\n",

"---\n\n",

"## 14. CRD vs RCBD Decision Framework\n\n",

"| Situation | Design consideration |\n",
"|---|---|\n",
"| Very homogeneous experimental area | CRD may be adequate. |\n",
"| Clear soil fertility gradient | RCBD is appropriate to consider. |\n",
"| Clear slope/elevation gradient | RCBD is appropriate to consider. |\n",
"| Clear drainage/moisture gradient | RCBD is appropriate to consider. |\n",
"| Many treatments causing very large blocks | Alternative designs may need consideration. |\n",
"| Multiple agro-ecological environments | Multi-location or stratified design should be considered. |\n",
"| Unknown field heterogeneity | Baseline mapping should precede design selection. |\n\n",

"The key design principle is not that RCBD is universally superior to CRD. ",
"Rather, blocking is valuable when a known nuisance source can be used to ",
"create genuinely more homogeneous experimental units. The simulation ",
"demonstrates the mechanism through which this can reduce residual variance ",
"and increase treatment-test precision.\n\n",

"---\n\n",

"## 15. Generated Outputs\n\n",

"- `output/reports/06_crd_vs_rcbd_simulation_results.csv`\n",
"- `output/reports/06_crd_vs_rcbd_design_comparison.csv`\n",
"- `output/reports/06_nuisance_variability_and_blocking.csv`\n",
"- `output/reports/06_variance_decomposition.csv`\n",
"- `output/reports/06_crd_vs_rcbd_power_simulation.csv`\n",
"- `output/reports/06_simulated_rcbd_field_trial.csv`\n",
"- `output/reports/06_crd_vs_rcbd_simulation_results.csv`\n",
"- `output/figures/06_crd_vs_rcbd_variance_decomposition.png`\n",
"- `output/figures/06_crd_vs_rcbd_power_comparison.png`\n\n",

"---\n\n",

"## 16. References and Practical Sources\n\n",

"1. Food and Agriculture Organization of the United Nations. Farming Systems Approach - Trial Methods. ",
"https://www.fao.org/4/v5330e/V5330e0C.HTM\n\n",

"2. Department of Agriculture, Sri Lanka. Rice Congress material on rice production environments and water management. ",
"https://doa.gov.lk/wp-content/uploads/2020/05/Rice-Congress-2010.pdf\n\n",

"3. Department of Agriculture, Sri Lanka. Rice Research and Development Institute, Soil Science Division. ",
"https://doa.gov.lk/rrdi_div_soilscience/\n\n",

"4. Department of Agriculture, Sri Lanka. Map-based soil reclamation and soil pH/organic matter management in rice cultivation. ",
"https://doa.gov.lk/rrdi_technology_soilscience_MapBasedSoilReclamation/\n\n",

"5. Department of Agriculture, Sri Lanka. Map Gallery including paddy soil fertility and agro-ecological resources. ",
"https://doa.gov.lk/map-gallery/\n\n",

"6. FAO AGRIS. Rice yield responses in a saline soil in Sri Lanka. ",
"https://agris.fao.org/search/en/providers/122430/records/6471d3a52a40512c710e763b\n\n",

"7. FAO AGRIS. Organic matter content of a lowland paddy soil as affected by plant growth and urea fertilization. ",
"https://agris.fao.org/search/en/providers/122436/records/67598f5ec7a957febdfc38b9\n\n",

"8. FAO AGRIS. Potential of Eppawala rock phosphate as a phosphorus fertilizer for rice cultivation in acid sulphate soils in Matara District of Sri Lanka. ",
"https://agris.fao.org/search/en/providers/122436/records/67598fd9c7a957febdfc4c08\n\n",

"---\n\n",

"## 17. Final Statistical Interpretation\n\n",

"The simulated experiment demonstrates the statistical rationale for ",
"blocking in a heterogeneous agricultural field. CRD places all ",
"between-plot variation not explained by treatment into the residual ",
"term. RCBD introduces a block term so that systematic between-block ",
"variation can be separated from within-block experimental error.\n\n",

"In this simulation, the block effect was intentionally substantial. ",
"Therefore, RCBD reduced residual variance and changed the precision ",
"of the treatment comparison. This is the expected outcome when the ",
"selected blocking variable captures a real field gradient.\n\n",

"For the fertilizer consultancy project, the practical implication is ",
"that future paddy fertilizer trials should collect baseline field ",
"information before treatment randomization. Soil fertility, pH, ",
"moisture/drainage, elevation and other spatial variables can then be ",
"used to determine whether blocking is justified. Agro-Ecological Zone ",
"should primarily guide environmental stratification or site selection ",
"when trials span broader geographic environments, rather than being ",
"automatically treated as a local block.\n\n",

"Therefore, experimental design should be driven by measured and ",
"agronomically meaningful heterogeneity rather than selecting RCBD ",
"simply because it is more complex. The central criterion is whether ",
"the proposed block structure produces genuinely more homogeneous ",
"comparisons among treatment plots.\n\n"
)

# ------------------------------------------------------------
# 28. WRITE MARKDOWN REPORT
# ------------------------------------------------------------

markdown_file <- file.path(
  report_dir,
  "06_experimental_design_evaluation.md"
)

writeLines(
  report_text,
  con = markdown_file,
  useBytes = TRUE
)

# ------------------------------------------------------------
# 29. WRITE HUMAN-READABLE SIMULATION SUMMARY
# ------------------------------------------------------------

simulation_summary_text <- paste0(
  "TASK 6 - CRD VS RCBD SIMULATION SUMMARY\n",
  "========================================\n\n",
  "Simulation seed: 123\n",
  "Treatments: ", n_treatments, "\n",
  "Blocks: ", n_blocks, "\n",
  "Total plots: ", nrow(rcbd_data), "\n",
  "Residual SD: ", residual_sd, "\n\n",

  "CRD\n",
  "---\n",
  "Treatment F-statistic: ",
  round(crd_treatment_f, 4), "\n",
  "Treatment p-value: ",
  format.pval(crd_treatment_p, digits = 6), "\n",
  "Residual MSE: ",
  round(crd_residual_mse, 4), "\n\n",

  "RCBD\n",
  "----\n",
  "Treatment F-statistic: ",
  round(rcbd_treatment_f, 4), "\n",
  "Treatment p-value: ",
  format.pval(rcbd_treatment_p, digits = 6), "\n",
  "Block F-statistic: ",
  round(
    rcbd_anova["Block", "F value"],
    4
  ), "\n",
  "Block p-value: ",
  format.pval(
    rcbd_anova["Block", "Pr(>F)"],
    digits = 6
  ), "\n",
  "Residual MSE: ",
  round(rcbd_residual_mse, 4), "\n\n",

  "Residual MSE reduction from CRD to RCBD: ",
  round(mse_reduction, 2), "%\n",
  "Residual SS reduction from CRD to RCBD: ",
  round(ss_reduction, 2), "%\n\n",

  "Interpretation:\n",
  "The simulation illustrates how a meaningful block effect can be\n",
  "separated from residual error under RCBD. The magnitude of the\n",
  "benefit depends on the strength of the nuisance gradient and the\n",
  "quality of the blocking strategy.\n"
)

writeLines(
  simulation_summary_text,
  con = file.path(
    report_dir,
    "06_crd_vs_rcbd_simulation_summary.txt"
  ),
  useBytes = TRUE
)

# ------------------------------------------------------------
# 30. SESSION INFORMATION
# ------------------------------------------------------------

session_info_file <- file.path(
  report_dir,
  "06_session_info.txt"
)

capture.output(
  sessionInfo(),
  file = session_info_file
)

# ------------------------------------------------------------
# 31. FINAL VALIDATION
# ------------------------------------------------------------

expected_files <- c(
  file.path(
    report_dir,
    "06_crd_vs_rcbd_simulation_results.csv"
  ),
  file.path(
    report_dir,
    "06_crd_vs_rcbd_design_comparison.csv"
  ),
  file.path(
    report_dir,
    "06_nuisance_variability_and_blocking.csv"
  ),
  file.path(
    report_dir,
    "06_variance_decomposition.csv"
  ),
  file.path(
    report_dir,
    "06_crd_vs_rcbd_power_simulation.csv"
  ),
  file.path(
    report_dir,
    "06_simulated_rcbd_field_trial.csv"
  ),
  file.path(
    report_dir,
    "06_experimental_design_evaluation.md"
  ),
  file.path(
    figure_dir,
    "06_crd_vs_rcbd_variance_decomposition.png"
  ),
  file.path(
    figure_dir,
    "06_crd_vs_rcbd_power_comparison.png"
  )
)

file_validation <- data.frame(
  File = expected_files,
  Exists = file.exists(expected_files),
  Size_KB = ifelse(
    file.exists(expected_files),
    round(
      file.info(expected_files)$size / 1024,
      2
    ),
    NA_real_
  )
)

readr::write_csv(
  as.data.frame(file_validation),
  file.path(
    report_dir,
    "06_output_validation.csv"
  )
)

# ------------------------------------------------------------
# 32. CONSOLE SUMMARY
# ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("TASK 6 - EXPERIMENTAL DESIGN EVALUATION COMPLETED\n")
cat("============================================================\n\n")

cat("CRD Treatment F-statistic: ",
    round(crd_treatment_f, 4), "\n", sep = "")

cat("CRD Treatment p-value: ",
    format.pval(crd_treatment_p, digits = 5), "\n", sep = "")

cat("CRD Residual MSE: ",
    round(crd_residual_mse, 4), "\n\n", sep = "")

cat("RCBD Treatment F-statistic: ",
    round(rcbd_treatment_f, 4), "\n", sep = "")

cat("RCBD Treatment p-value: ",
    format.pval(rcbd_treatment_p, digits = 5), "\n", sep = "")

cat("RCBD Block F-statistic: ",
    round(
      rcbd_anova["Block", "F value"],
      4
    ),
    "\n",
    sep = ""
)

cat("RCBD Block p-value: ",
    format.pval(
      rcbd_anova["Block", "Pr(>F)"],
      digits = 5
    ),
    "\n",
    sep = ""
)

cat("RCBD Residual MSE: ",
    round(rcbd_residual_mse, 4),
    "\n\n",
    sep = ""
)

cat("Residual MSE reduction: ",
    round(mse_reduction, 2),
    "%\n",
    sep = ""
)

cat("Residual SS reduction: ",
    round(ss_reduction, 2),
    "%\n\n",
    sep = ""
)

cat("Markdown report:\n")
cat(markdown_file, "\n\n")

cat("Figures:\n")
cat(
  file.path(
    figure_dir,
    "06_crd_vs_rcbd_variance_decomposition.png"
  ),
  "\n"
)

cat(
  file.path(
    figure_dir,
    "06_crd_vs_rcbd_power_comparison.png"
  ),
  "\n\n"
)

cat("Task 6 completed successfully.\n")
cat("============================================================\n")
