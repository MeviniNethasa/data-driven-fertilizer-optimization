# Task 7: Principal Component Analysis (PCA) Evaluation

## IT3081 Statistical Modelling Consultancy Project

**Data-Driven Precision Fertilizer Advisory & Strategic Agriculture Framework**

**Script:** `scripts/07_pca_evaluation.R`  
**Dataset:** `Fertilizer_Dataset.csv`

---

## 1. Objective

This analysis evaluates whether Principal Component Analysis (PCA) can provide a useful representation of the project's numerical soil fertility and climatic predictors.

The analysis focuses on dimensionality, variance structure, principal component loadings and the practical trade-off between predictive modelling efficiency and agricultural interpretability.

Categorical identifiers, fertilizer targets and recommendation variables are excluded from PCA because PCA is being used to analyse the structure of the numerical predictor variables rather than the fertilizer outcomes.

---

## 2. PCA Input Variables

| Variable | Agricultural interpretation |
|---|---|
| `Soil_pH` | Soil acidity/alkalinity indicator |
| `Total_N_percent` | Total soil nitrogen |
| `Available_P_mgkg` | Plant-available phosphorus |
| `Available_K_mgkg` | Plant-available potassium |
| `Organic_Carbon_percent` | Soil organic carbon |
| `30day_cumulative_rainfall_mm` | Recent cumulative rainfall |
| `mean_temperature_C` | Mean temperature |

The script supports both the Task 7 weather-variable names and the corresponding names used in the project dataset.

---

## 3. Data Preparation

The original dataset contained **10,000** observations.

After complete-case filtering, **7,004** observations were used for PCA.

**Complete-case percentage:** 70.04%.

All seven PCA variables were standardized before PCA using `scale. = TRUE`. Standardization is necessary because the variables have different units and numerical scales.

---

## 4. PCA Method

PCA was performed using R's `prcomp()` function with centering and standardization:

```r
prcomp(pca_data_complete, center = TRUE, scale. = TRUE)
```

The eigenvalue of a principal component represents the amount of standardized variance captured by that component.

The variance proportion is the eigenvalue divided by the sum of all eigenvalues, while cumulative variance is the running total of the variance proportions.

---

## 5. Eigenvalues and Variance Explained

| Component | Eigenvalue | Variance (%) | Cumulative (%) |
|---|---:|---:|---:|
| PC1 | 3.0086 | 42.98 | 42.98 |
| PC2 | 1.0008 | 14.3 | 57.28 |
| PC3 | 0.9461 | 13.52 | 70.79 |
| PC4 | 0.7888 | 11.27 | 82.06 |
| PC5 | 0.4898 | 7 | 89.06 |
| PC6 | 0.4399 | 6.28 | 95.34 |
| PC7 | 0.3261 | 4.66 | 100 |

PC1 explains **42.98%** of the standardized variance.

PC2 explains **14.3%** of the standardized variance.

PC3 explains **13.52%** of the standardized variance.

Together, PC1-PC3 explain **70.79%** of the standardized variance.

The complete eigenvalue, loading and cumulative-variance results are available in `output/reports/07_pca_summary_metrics.csv`.

---

## 6. Principal Component Loadings

Loadings indicate how strongly each original standardized variable contributes to a principal component.

Large absolute loadings identify variables that contribute strongly to a component.

### PC1

The three largest absolute PC1 loadings are: **Organic_Carbon_percent (loading = 0.496), mean_temperature_C (loading = -0.468), Soil_pH (loading = -0.458)**.

### PC2

The three largest absolute PC2 loadings are: **Total_N_percent (loading = -0.995), Available_P_mgkg (loading = 0.075), Available_K_mgkg (loading = -0.043)**.

### PC3

The three largest absolute PC3 loadings are: **Available_P_mgkg (loading = -0.98), Soil_pH (loading = 0.101), mean_temperature_C (loading = 0.097)**.

The sign of an individual PCA loading should be interpreted relative to the other variables. PCA component signs are arbitrary: multiplying an entire component by -1 gives an equivalent solution. Consequently, loading magnitude is particularly important when identifying dominant variables.

---

## 7. Scree Plot

The scree plot is saved as `output/figures/07_pca_scree_plot.png`.

The plot shows the percentage of standardized variance explained by each principal component. A sharp reduction in additional explained variance can indicate that later components provide progressively less information.

Using the eigenvalue-greater-than-one rule, **2** component(s) have eigenvalues greater than 1.

The eigenvalue-greater-than-one rule is treated as a descriptive diagnostic rather than as the sole criterion for selecting the final number of components.

---

## 8. PCA Biplot

The PCA biplot is saved as `output/figures/07_pca_biplot.png`.

The biplot displays individual observations and variable vectors in the PC1-PC2 space.

Variables pointing in similar directions indicate similar relationships within the displayed PCA space, while opposing directions indicate contrasting relationships. The biplot is exploratory and does not establish causal agricultural relationships.

---

## 9. Variable Loading Visualization

The `factoextra::fviz_pca_var()` output is saved as `output/figures/07_pca_variable_loadings.png`.

An additional PC1-PC3 loading heatmap is saved as `output/figures/07_pca_loading_heatmap_pc1_pc3.png`.

These visualizations help identify which soil and climatic variables contribute most strongly to the major principal components.

---

## 10. Dimensionality Reduction Assessment

The PCA contains only 7 numerical predictors. This is a relatively small feature space, so dimensionality reduction is not required purely for computational efficiency.

6 principal components are sufficient to explain at least 90% of the standardized predictor variance. This demonstrates potential feature compression.

The 80% cumulative variance threshold is reached using **4** component(s).

The 90% cumulative variance threshold is reached using **6** component(s).

The 95% cumulative variance threshold is reached using **6** component(s).

With only seven numerical predictors, computational efficiency alone is unlikely to justify replacing the original variables with principal components.

---

## 11. Predictive Modelling Efficiency

PCA can be useful for predictive modelling when the original predictors contain substantial correlation or when a downstream model benefits from a smaller set of orthogonal predictors.

Principal components are mutually orthogonal in the PCA representation. Therefore, a PCA-based modelling approach can reduce the direct multicollinearity among the transformed predictors.

However, PCA is an unsupervised method. It maximizes variance in the predictor space rather than maximizing prediction accuracy for fertilizer requirements.

Consequently, a component that explains a large proportion of soil and climate variance is not automatically the component that is most useful for predicting `Urea_kg_ha`, `TSP_kg_ha` or `MOP_kg_ha`.

---

## 12. Agricultural Interpretability

The principal limitation of PCA for a stakeholder-facing fertilizer advisory system is interpretability.

A farmer, agronomist or agricultural manager can directly interpret:

- Soil pH
- Total nitrogen
- Available phosphorus
- Available potassium
- Organic carbon
- Recent rainfall
- Mean temperature

A statement such as 'PC1 is high' is less directly actionable. PC1 is a weighted combination of several standardized agricultural measurements, and its interpretation requires examining the component loadings.

For example, explaining a recommendation using actual soil nitrogen or soil pH is generally more transparent than explaining it through an abstract principal component.

---

## 13. Consultancy Trade-off

| Criterion | Original Predictors | PCA Components |
|---|---|---|
| Computational dimensionality | Seven predictors | Potentially fewer components |
| Multicollinearity | May remain | Reduced among components |
| Direct agronomic interpretation | High | Lower |
| Communication to managers | Direct | Requires loading explanation |
| Exploratory multivariate analysis | Moderate | Strong |
| Variable-specific recommendations | Direct | Indirect |
| Model transparency | High | Lower |

For this project, PCA is most useful as a complementary analytical technique and modelling benchmark rather than as an automatic replacement for the original agricultural predictors.

---

## 14. Recommended Consultancy Workflow

1. Build the primary predictive models using the original soil and climatic variables.

2. Evaluate correlations and multicollinearity among the original predictors.

3. Construct PCA components as an alternative representation of the numerical predictor space.

4. If PCA-based predictive models are tested, evaluate them on the same held-out test data and using the same metrics as the original models.

5. Compare predictive performance, dimensionality and interpretability.

6. Preserve the original agricultural variables for stakeholder-facing explanations even if PCA is used internally for exploratory or modelling purposes.

---

## 15. Limitations

- PCA is unsupervised and does not use fertilizer recommendation targets when constructing components.
- PCA maximizes predictor variance, not fertilizer prediction accuracy.
- Complete-case analysis can reduce the available sample when missing values exist.
- PCA results depend on the variables included and the preprocessing method.
- Loading signs are arbitrary and should not be interpreted as causal directions.
- PCA components can be harder for non-technical stakeholders to interpret.
- The current PCA is restricted to the seven numerical soil and climatic predictors specified for Task 7.

---

## 16. Generated Outputs

- `output/reports/07_pca_summary_metrics.csv`
- `output/reports/07_pca_evaluation_report.md`
- `output/reports/07_pca_loading_details.csv`
- `output/reports/07_pca_loadings_matrix.csv`
- `output/reports/07_pca_dimensionality_assessment.csv`
- `output/reports/07_pca_sample_summary.csv`
- `output/reports/07_pca_input_descriptive_statistics.csv`
- `output/reports/07_pca_input_correlation_matrix.csv`
- `output/figures/07_pca_scree_plot.png`
- `output/figures/07_pca_biplot.png`
- `output/figures/07_pca_variable_loadings.png`
- `output/figures/07_pca_loading_heatmap_pc1_pc3.png`
- `output/figures/07_pca_cumulative_variance.png`

---

## 17. Final Consultancy Evaluation

PCA provides a useful summary of the multivariate structure among the project's soil fertility and climatic predictors.

Because the analysis contains only 7 numerical predictors, dimensionality reduction is not required solely for computational reasons.

Its main statistical value is the ability to represent correlated predictor information using orthogonal components and to provide an exploratory view of the dominant environmental gradients.

However, the original agricultural measurements have substantially greater direct interpretability. Soil pH, nitrogen, phosphorus, potassium, organic carbon, rainfall and temperature can be communicated directly to farmers, agronomists and managers, whereas principal components require interpretation through their loadings.

Therefore, PCA should be treated as a complementary analytical method and potential modelling benchmark. Whether PCA improves the final predictive model should be determined using out-of-sample predictive performance alongside the interpretability requirements of the fertilizer advisory system.

---

## 18. Reproducibility

A random seed of 123 is used for reproducibility. The PCA computation itself is deterministic given the dataset and preprocessing steps.

