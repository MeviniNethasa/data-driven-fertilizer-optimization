# Task 6: Critical Evaluation of Experimental Design

## IT3081 Statistical Modelling Consultancy Project

### Data-Driven Precision Fertilizer Advisory & Strategic Agriculture Framework

**Analysis file:** `scripts/06_experimental_design_eval.R`  
**Dataset:** `Fertilizer_Dataset.csv`  
**Simulation seed:** 123  

---

## 1. Purpose of the Evaluation

This task evaluates Completely Randomized Design (CRD) and Randomized Complete Block Design (RCBD) for future fertilizer and agronomic field experiments in Sri Lankan paddy cultivation. The supplied dataset is used to establish the agricultural context and identify measurable sources of field heterogeneity. The experimental comparison itself is based on a controlled simulation because the supplied observational dataset is not a randomized field trial.

---

## 2. Agricultural Context

Rice is cultivated across a wide range of climatic, soil and hydrological conditions in Sri Lanka. Department of Agriculture material notes that rice production occurs across different agro-ecological environments and that crop performance can be affected by micro-environmental conditions including rainfall, temperature, soil characteristics and water regimes. These conditions make experimental control of local field heterogeneity important when evaluating fertilizer responses.

Source: Department of Agriculture, Sri Lanka, Rice Congress material: https://doa.gov.lk/wp-content/uploads/2020/05/Rice-Congress-2010.pdf

The Department of Agriculture also maintains resources covering paddy soil fertility, agro-ecological regions and soil pH mapping, demonstrating the practical relevance of spatial soil information to rice production and soil management.

Source: Department of Agriculture Map Gallery: https://doa.gov.lk/map-gallery/

---

## 3. Completely Randomized Design (CRD)

A Completely Randomized Design (CRD) randomly assigns treatments to all available experimental units without explicitly modelling a blocking factor. It is statistically simple and can be useful when experimental units are sufficiently homogeneous.

### Advantages

- Simple randomization procedure.
- Straightforward ANOVA model.
- Easy to implement and explain.
- Flexible when experimental units are reasonably homogeneous.
- Less complicated if experimental units are lost or excluded.

### Limitations in heterogeneous paddy fields

The major limitation is that systematic field heterogeneity is absorbed into the residual error term. If plots differ because of soil fertility, drainage, moisture or another spatial gradient, the treatment comparison may become less precise.

The FAO description of field trial methods identifies this issue explicitly: CRD can be less precise because variation between experimental units is included in the experimental error term.

Source: FAO trial methods: https://www.fao.org/4/v5330e/V5330e0C.HTM

---

## 4. Randomized Complete Block Design (RCBD)

A Randomized Complete Block Design (RCBD) divides experimental units into relatively homogeneous blocks and assigns every treatment once within each block. The block effect is then included in the statistical model, allowing systematic nuisance variability to be separated from residual experimental error.

### Advantages

- Controls a known or suspected source of nuisance variation.
- Provides treatment comparisons within relatively homogeneous blocks.
- Can reduce residual variance when the blocking factor explains meaningful field heterogeneity.
- Can improve precision and statistical power without necessarily increasing the number of treatments.
- Particularly useful for field experiments with identifiable spatial gradients.

### Limitations

- Blocks must be constructed using a meaningful source of heterogeneity.
- The number of treatments determines the size of a complete block.
- Very large blocks can themselves become heterogeneous.
- An inappropriate blocking variable can provide little benefit.
- More complicated field planning and randomization are required.
- If a treatment or plot is missing, the balanced structure can be affected.

The FAO field-trial guidance describes RCBD as appropriate when experimental units can be grouped meaningfully, including situations where plots differ along a slope or where soil depth changes across the experimental area.

Source: FAO trial methods: https://www.fao.org/4/v5330e/V5330e0C.HTM

---

## 5. Nuisance Variability in Sri Lankan Paddy Experiments

### 5.1 Soil fertility gradients

Total nitrogen, available phosphorus, available potassium and organic carbon can differ between plots. These differences may affect crop growth independently of the fertilizer treatment being tested. Blocking plots along a measured fertility gradient can therefore reduce unexplained variation.

The Department of Agriculture's Rice Research and Development Institute describes long-running fertilizer research directed at efficient use of nitrogen, phosphorus and potassium in paddy fields. This provides practical context for considering soil nutrient status when planning fertilizer experiments.

Source: Department of Agriculture, Soil Science Division: https://doa.gov.lk/rrdi_div_soilscience/

### 5.2 Soil pH

The supplied dataset contains `Soil_pH`. Soil pH is relevant to nutrient availability and fertilizer response. A field can therefore be sampled before treatment allocation and divided into relatively homogeneous pH strata if pH exhibits a meaningful spatial gradient.

The Department of Agriculture provides pH-based soil management information and paddy soil pH mapping resources, supporting the practical use of soil pH information in field management.

Source: Department of Agriculture soil mapping and management: https://doa.gov.lk/rrdi_technology_soilscience_MapBasedSoilReclamation/

### 5.3 Moisture and drainage gradients

Water availability is particularly important in paddy systems. Plots at different elevations or drainage positions may experience different water regimes. Blocking across a moisture or drainage gradient can reduce the influence of this nuisance source.

The Department of Agriculture reports that improved alternate wetting and drying practices depend on appropriate field and water management conditions and notes differences in suitability across irrigated areas and paddy environments.

Source: Department of Agriculture water management research: https://doa.gov.lk/rrdi_technology_soilscience_IAWD/

### 5.4 Agro-ecological variation

`Agro_Zone` is directly available in the project dataset. It captures broader environmental differences. For a single-location experiment, Agro-Zone would generally be better treated as a site-selection or stratification variable rather than automatically used as an individual within-field block. If trials span several agro-ecological environments, the zone can instead be incorporated into a multi-site experimental structure.

The Department of Agriculture identifies rice research stations and research activities associated with specific agro-ecological regions, illustrating the practical importance of environmental context in rice experimentation.

Source: Rice Research Station, Labuduwa: https://doa.gov.lk/rrdi_sub_labuduwa/

---

## 6. Recommended Blocking Variables

### Agro-Ecological Zone

Useful for stratifying multi-location trials or defining the environment in which a trial is conducted. It should not automatically be treated as the block factor inside one uniform field.

### Soil pH category

Potentially useful after baseline soil sampling identifies a spatial pH gradient. The categories should be based on agronomic interpretation rather than arbitrary statistical cut-points. The current script uses quantile categories only for exploratory illustration.

### Soil fertility index

A composite or carefully selected baseline measure based on nitrogen, phosphorus, potassium and organic carbon could be used when fertility is the dominant spatial gradient. This approach should be based on pre-treatment sampling and agronomic knowledge.

### Slope and drainage

Slope and drainage are not variables in the supplied dataset. Nevertheless, they are potentially important field-level blocking variables. Blocks should be oriented so that treatment comparisons occur among plots with similar elevation, slope position or drainage conditions.

---

## 7. Evidence from Sri Lankan Rice Research

RCBD is not merely a theoretical design for Sri Lankan agriculture. Published Sri Lankan rice research has used RCBD in field experiments. For example, a rice study examining responses in saline soil in the low-country wet zone used a factorial randomized complete block design with three replications.

Source: FAO AGRIS record, Rice yield responses in a saline soil in Sri Lanka: https://agris.fao.org/search/en/providers/122430/records/6471d3a52a40512c710e763b

Another Sri Lankan study investigating organic matter in lowland paddy soil under fertilizer and plant treatments used a randomized complete block design with four treatment combinations in a farmer field in Kurunegala District.

Source: FAO AGRIS record: https://agris.fao.org/search/en/providers/122436/records/67598f5ec7a957febdfc38b9

A further study of phosphorus sources for rice in acid sulphate soils in Matara District used six treatments in an RCBD with three replicates.

Source: FAO AGRIS record: https://agris.fao.org/search/en/providers/122436/records/67598fd9c7a957febdfc4c08

These examples demonstrate that RCBD is practically compatible with field-based rice research in Sri Lanka. They do not, however, establish that RCBD is optimal for every fertilizer experiment; the blocking factor must still correspond to meaningful field heterogeneity.

---

## 8. Experimental Simulation

The simulation represents a hypothetical fertilizer field trial with four treatments and six blocks. The treatment effects increase from the control to the high-fertilizer treatment. A systematic block effect is also introduced to represent a field gradient, while random error represents plot-to-plot variability not explained by treatment or block.

### Simulation parameters

| Parameter | Value |
|---|---:|
| Treatments | 4 |
| Blocks | 6 |
| Plots | 24 |
| Residual SD | 8 |
| Random seed | 123 |
| Alpha | 0.05 |

### Simulated treatment effects

| Treatment | Effect |
|---|---:|
| Control | 0 |
| Low | 4 |
| Medium | 8 |
| High | 12 |

---

## 9. CRD vs RCBD Simulation Results

```text
Design: CRD
Treatment F-statistic:     2.5962
Treatment p-value:     0.08086
Residual MSE:     129.3611

Design: RCBD
Treatment F-statistic:     13.3048
Treatment p-value:     0.00016744
Residual MSE:     25.2425
Block F-statistic:     17.4989
Block p-value:     8.6963e-06
```

### Residual MSE change

The simulated RCBD residual MSE is 25.242 compared with 129.361 for the CRD. The residual MSE reduction is approximately 80.49%.

The corresponding reduction in residual sum of squares is approximately 85.37%.

These quantities illustrate the central statistical benefit of an effective blocking factor: systematic variability that would otherwise be included in the residual term can be explicitly modelled as a block effect.

In this simulated field, the RCBD produced a smaller treatment p-value than the CRD because the block effect captured systematic between-block variability that would otherwise contribute to the CRD residual term.

The simulated RCBD provides evidence of a treatment effect at the 5% significance level.

---

## 10. Interpretation of Variance Decomposition

The variance decomposition separates the simulated total variation into treatment, block and residual components. Under CRD, block-related variation is not modelled and therefore contributes to the residual error. Under RCBD, the block component is explicitly included in the ANOVA model.

Consequently, the RCBD residual MSE can be substantially lower when the blocks capture an important spatial gradient. A lower residual MSE improves the precision of treatment comparisons because the treatment effect is evaluated relative to a smaller unexplained error variance.

The effect is conditional rather than guaranteed. If the proposed blocking variable is unrelated to the true nuisance variation, the RCBD may provide little or no efficiency gain while increasing design and analysis complexity.

---

## 11. Statistical Power Comparison

Empirical power was estimated through 1,000 simulated trials at each specified treatment-effect magnitude. Power is defined as the proportion of simulations in which the treatment p-value is below 0.05.

The resulting power curve is saved as:

`output/figures/06_crd_vs_rcbd_power_comparison.png`

The simulation should be interpreted as a controlled demonstration rather than a direct estimate of power for a future real-world trial. Actual power depends on the expected treatment effect, residual variance, number of treatments, number of blocks, number of seasons or locations, and the quality of the selected blocking factor.

---

## 12. Practical Feasibility for Sri Lankan Paddy Trials

An RCBD is operationally feasible when a field has a recognizable gradient and enough area to establish complete blocks. Before planting, researchers should map or sample relevant field characteristics such as soil nutrient status, pH, elevation, drainage and potentially electrical conductivity where salinity is relevant.

Treatment randomization should then occur independently within each block. Field maps should preserve the randomization record, and treatment application should be standardized to avoid introducing operational bias.

RCBD becomes less attractive when the number of treatments is very large, because each complete block must contain all treatments. Very large blocks may themselves contain substantial heterogeneity, weakening the assumption that plots within a block are sufficiently similar.

For multi-location or multi-agro-ecological research, a hierarchical or multi-environment design may be more appropriate than attempting to use Agro_Zone as a simple within-field block. The experimental structure should distinguish location-level environmental effects from local within-field block effects.

---

## 13. Recommended Experimental Workflow

1. Define the primary fertilizer treatment and response variable before field preparation.
2. Conduct baseline soil and field measurements before randomization.
3. Identify the dominant nuisance gradient using agronomic knowledge and spatial measurements.
4. Select blocks so that plots within a block are relatively homogeneous.
5. Include every treatment once within each complete block for an RCBD.
6. Randomize treatment positions independently within every block.
7. Maintain identical plot dimensions and management procedures where possible.
8. Record missing plots, irrigation interruptions, pest events and other deviations.
9. Analyse treatment effects using a model containing Treatment and Block.
10. Assess residual assumptions and inspect whether the blocking factor actually reduced residual variability.
11. Repeat experiments across relevant seasons or locations when the goal is broader agricultural generalization.

---

## 14. CRD vs RCBD Decision Framework

| Situation | Design consideration |
|---|---|
| Very homogeneous experimental area | CRD may be adequate. |
| Clear soil fertility gradient | RCBD is appropriate to consider. |
| Clear slope/elevation gradient | RCBD is appropriate to consider. |
| Clear drainage/moisture gradient | RCBD is appropriate to consider. |
| Many treatments causing very large blocks | Alternative designs may need consideration. |
| Multiple agro-ecological environments | Multi-location or stratified design should be considered. |
| Unknown field heterogeneity | Baseline mapping should precede design selection. |

The key design principle is not that RCBD is universally superior to CRD. Rather, blocking is valuable when a known nuisance source can be used to create genuinely more homogeneous experimental units. The simulation demonstrates the mechanism through which this can reduce residual variance and increase treatment-test precision.

---

## 15. Generated Outputs

- `output/reports/06_crd_vs_rcbd_simulation_results.csv`
- `output/reports/06_crd_vs_rcbd_design_comparison.csv`
- `output/reports/06_nuisance_variability_and_blocking.csv`
- `output/reports/06_variance_decomposition.csv`
- `output/reports/06_crd_vs_rcbd_power_simulation.csv`
- `output/reports/06_simulated_rcbd_field_trial.csv`
- `output/reports/06_crd_vs_rcbd_simulation_results.csv`
- `output/figures/06_crd_vs_rcbd_variance_decomposition.png`
- `output/figures/06_crd_vs_rcbd_power_comparison.png`

---

## 16. References and Practical Sources

1. Food and Agriculture Organization of the United Nations. Farming Systems Approach - Trial Methods. https://www.fao.org/4/v5330e/V5330e0C.HTM

2. Department of Agriculture, Sri Lanka. Rice Congress material on rice production environments and water management. https://doa.gov.lk/wp-content/uploads/2020/05/Rice-Congress-2010.pdf

3. Department of Agriculture, Sri Lanka. Rice Research and Development Institute, Soil Science Division. https://doa.gov.lk/rrdi_div_soilscience/

4. Department of Agriculture, Sri Lanka. Map-based soil reclamation and soil pH/organic matter management in rice cultivation. https://doa.gov.lk/rrdi_technology_soilscience_MapBasedSoilReclamation/

5. Department of Agriculture, Sri Lanka. Map Gallery including paddy soil fertility and agro-ecological resources. https://doa.gov.lk/map-gallery/

6. FAO AGRIS. Rice yield responses in a saline soil in Sri Lanka. https://agris.fao.org/search/en/providers/122430/records/6471d3a52a40512c710e763b

7. FAO AGRIS. Organic matter content of a lowland paddy soil as affected by plant growth and urea fertilization. https://agris.fao.org/search/en/providers/122436/records/67598f5ec7a957febdfc38b9

8. FAO AGRIS. Potential of Eppawala rock phosphate as a phosphorus fertilizer for rice cultivation in acid sulphate soils in Matara District of Sri Lanka. https://agris.fao.org/search/en/providers/122436/records/67598fd9c7a957febdfc4c08

---

## 17. Final Statistical Interpretation

The simulated experiment demonstrates the statistical rationale for blocking in a heterogeneous agricultural field. CRD places all between-plot variation not explained by treatment into the residual term. RCBD introduces a block term so that systematic between-block variation can be separated from within-block experimental error.

In this simulation, the block effect was intentionally substantial. Therefore, RCBD reduced residual variance and changed the precision of the treatment comparison. This is the expected outcome when the selected blocking variable captures a real field gradient.

For the fertilizer consultancy project, the practical implication is that future paddy fertilizer trials should collect baseline field information before treatment randomization. Soil fertility, pH, moisture/drainage, elevation and other spatial variables can then be used to determine whether blocking is justified. Agro-Ecological Zone should primarily guide environmental stratification or site selection when trials span broader geographic environments, rather than being automatically treated as a local block.

Therefore, experimental design should be driven by measured and agronomically meaningful heterogeneity rather than selecting RCBD simply because it is more complex. The central criterion is whether the proposed block structure produces genuinely more homogeneous comparisons among treatment plots.


