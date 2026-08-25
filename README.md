# Standardized DAFOR metrics for *Tubastraea coccinea* monitoring

Repository containing the data-analysis workflow used in the study:

**"Standardized effort and extent DAFOR metrics support long-term *Tubastraea coccinea* invasion monitoring in a marine protected area at the southern Atlantic limit of occurrence"**

The repository contains the R scripts used to prepare the monitoring data, calculate effort- and extent-standardized metrics, evaluate temporal, spatial, and bathymetric patterns, perform sensitivity and permutation analyses, and generate the tables and figures associated with the manuscript.

---

## Analytical workflow

The complete analytical workflow can be reproduced by running:

```r
source("R/99_run_all.R")
```

The workflow clears the current R workspace and sequentially runs all scripts required to reproduce the final analyses, tables, and figures.

The active scripts are executed in the following order:

```text
R/
├── 00_setup.R
├── 01_prepare_monitoring_data.R
├── 02_prepare_site_year_metrics.R
├── 03_metric_relationship_and_effort.R
├── 04_raiw_sensitivity.R
├── 06_summary_table.R
├── 07_sampling_balance_and_cycles.R
├── 08_effort_standardised_analyses.R
├── 09_supplementary_tables.R
├── 10_main_figures.R
├── 11_supplementary_figures.R
└── 99_run_all.R
```

The script:

```text
R/05_figures_dpue_raiw.R
```

contains an earlier figure-generation workflow and is retained for reference, but it is no longer used by the final analytical pipeline.

---

## Script overview

### `00_setup.R`

Loads the packages and common functions used throughout the workflow and defines the DAFOR weighting scheme used to calculate RAI-W.

### `01_prepare_monitoring_data.R`

Imports and standardizes the monitoring dataset and locality information, including shoreline extent and regional classification.

### `02_prepare_site_year_metrics.R`

Calculates locality-year monitoring metrics, including sampling effort, positive records, DPUE, and RAI-W.

### `03_metric_relationship_and_effort.R`

Evaluates relationships between DPUE and RAI-W and examines associations between the standardized metrics and sampling effort.

### `04_raiw_sensitivity.R`

Evaluates the sensitivity of RAI-W to alternative DAFOR weighting schemes and compares locality rankings among weighting scenarios.

### `06_summary_table.R`

Generates summary information describing the monitoring dataset and the principal analytical metrics.

### `07_sampling_balance_and_cycles.R`

Prepares datasets used for effort-standardised analyses, including monitoring cycles, complete minute-level records, and bathymetric subsets.

### `08_effort_standardised_analyses.R`

Performs the effort-standardised temporal, spatial, and bathymetric analyses.

This includes:

- repeated random subsampling to standardize monitoring effort;
- temporal comparisons among monitoring cycles;
- spatial comparisons among regions;
- bathymetric comparisons among depth strata;
- locality-level exact permutation tests;
- locality-level occurrence analysis.

### `09_supplementary_tables.R`

Generates the supplementary tables associated with the final analytical workflow.

### `10_main_figures.R`

Generates the source data and R-based main manuscript figures.

Figures 2–5 are generated directly in R.

The script also exports the final locality-level DPUE and RAI-W values used to construct Figure 6 in QGIS.

### `11_supplementary_figures.R`

Generates Supplementary Figure S1 showing effort-standardised temporal patterns across the seven localities monitored during all three monitoring cycles.

### `99_run_all.R`

Runs the complete analytical workflow and performs final consistency checks on the principal datasets, analyses, tables, and figures.

---

## Monitoring metrics

Two effort- and extent-standardized metrics are used in the study.

### Detections per Unit Effort (DPUE)

DPUE represents the frequency of positive *Tubastraea coccinea* records standardized by both monitoring effort and monitored shoreline extent.

### Weighted Relative Abundance Index (RAI-W)

RAI-W incorporates the semiquantitative DAFOR abundance categories into an effort- and extent-standardized metric.

The final DAFOR weights are:

| DAFOR score | Category | Weight |
|---:|:---:|---:|
| 10 | D | 1.00 |
| 8 | A | 0.80 |
| 6 | F | 0.60 |
| 4 | O | 0.10 |
| 2 | R | 0.04 |
| 0 | Absent | 0.00 |

Alternative weighting schemes are evaluated as part of the sensitivity analysis.

---

## Effort-standardised analyses

Because sampling effort varied among localities and monitoring periods, additional analyses were performed using equal-effort subsampling.

### Temporal analysis

Temporal patterns are evaluated using seven localities represented in all three monitoring cycles:

```text
2022/2023
2023/2024
2024/2025
```

Sampling effort is standardized to:

```text
30 one-minute records per locality × monitoring cycle
```

using 1,000 random subsampling iterations.

### Spatial analysis

Spatial patterns are evaluated using the localities monitored in 2025.

Sampling effort is standardized to:

```text
34 one-minute records per locality
```

using 1,000 random subsampling iterations.

### Bathymetric analysis

Bathymetric patterns are evaluated using five localities represented in all three principal bathymetric strata:

```text
0–2 m
2.1–8 m
8.1–14 m
```

Sampling effort is standardized to:

```text
30 one-minute records per locality × bathymetric stratum
```

using 1,000 random subsampling iterations.

Observations deeper than 14 m are retained for descriptive purposes but are not included in the balanced bathymetric comparison.

---

## Figures

The analytical workflow generates the source data for the manuscript figures.

### Figure 2

Sampling effort across monitoring localities and bathymetric strata.

### Figure 3

Positive one-minute records and sampling effort across depth.

### Figure 4

Annual patterns of positive records by DAFOR category and weighted relative abundance.

### Figure 5

Pooled locality-level DPUE and RAI-W partitioned by bathymetric stratum.

### Figure 6

Figure 6 is assembled in **QGIS**.

The R workflow exports the final locality-level source table:

```text
outputs/figure_6_qgis_locality_metrics.csv
```

This table contains the audited pooled DPUE and RAI-W values used in Figure 5, ensuring consistency between the two figures.

### Supplementary Figure S1

Effort-standardised temporal patterns in DPUE and RAI-W for the seven localities monitored in all three monitoring cycles.

Locality-specific trajectories represent median estimates obtained from 1,000 equal-effort subsampling iterations.

Black points and lines represent the median cycle-level mean across the subsampling iterations, and error bars represent the corresponding 2.5–97.5% subsampling intervals.

---

## Outputs

Analytical results and source tables are generated automatically in:

```text
outputs/
```

Figures generated in R are saved in:

```text
figs/
```

Important output files include:

```text
outputs/figure_2_sampling_effort.csv
outputs/figure_3_detections_by_depth.csv
outputs/figure_3_sampling_by_depth.csv
outputs/figure_4_annual_annotations.csv
outputs/figure_4_annual_positive_dafor.csv
outputs/figure_4_annual_raiw_depth.csv
outputs/figure_5_locality_metrics.csv
outputs/figure_5_metrics_by_depth.csv
outputs/figure_5_stack_consistency_check.csv
outputs/figure_6_qgis_locality_metrics.csv
outputs/Figure_S1_temporal_locality_trajectories.csv
outputs/Figure_S1_temporal_summary.csv
```

Main R-generated figures include:

```text
figs/fig_2_sampling_effort.png
figs/fig_3_detections_depth_sampling.png
figs/fig_4_annual_positive_RAIW.png
figs/fig_5_dpue_raiw.png
figs/Figure_S1_temporal_effort_standardised.png
```

---

## Final consistency checks

At the end of the workflow, `99_run_all.R` performs consistency checks to confirm the principal characteristics of the final analytical dataset and outputs.

The workflow verifies, among other quantities:

```text
43 monitoring localities
8,415 one-minute monitoring records
173 positive records
68 locality-year observations
15 localities with positive records
7 final inferential tests
7 localities included in the balanced temporal analysis
```

The workflow also checks that the expected figure files and the QGIS source table were successfully generated.

A successful complete run ends with:

```text
============================================================
COMPLETE WORKFLOW FINISHED SUCCESSFULLY
============================================================
```

---

## Reproducibility

For a complete reproducibility check, start a fresh R session from the repository root and run:

```r
source("R/99_run_all.R")
```

The workflow is designed so that the final analyses and figures are regenerated from the prepared monitoring inputs without relying on objects previously stored in the interactive R workspace.