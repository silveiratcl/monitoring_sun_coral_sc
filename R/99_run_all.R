################################################################################
# 99_run_all.R
#
# Run the complete analytical workflow from raw monitoring data
# to summary metrics, sensitivity analyses, supplementary tables,
# main figures, and supplementary figures.
################################################################################

rm(list = ls())
gc()


cat(
  "\n============================================================\n",
  "RUNNING COMPLETE ANALYTICAL WORKFLOW\n",
  "============================================================\n"
)


################################################################################
# 1. Analytical workflow
################################################################################

scripts <- c(
  "R/00_setup.R",
  "R/01_prepare_monitoring_data.R",
  "R/02_prepare_site_year_metrics.R",
  "R/03_metric_relationship_and_effort.R",
  "R/04_raiw_sensitivity.R",
  "R/06_summary_table.R",
  "R/07_sampling_balance_and_cycles.R",
  "R/08_effort_standardised_analyses.R",
  "R/09_supplementary_tables.R",
  "R/10_main_figures.R",
  "R/11_supplementary_figures.R"
)


for (script in scripts) {
  
  cat(
    "\n------------------------------------------------------------\n",
    "Running: ", script, "\n",
    "------------------------------------------------------------\n",
    sep = ""
  )
  
  source(script)
}


################################################################################
# 2. FINAL CONSISTENCY CHECKS
################################################################################


################################################################################
# 2.1. Core monitoring dataset
################################################################################

stopifnot(
  nrow(site_year_metrics) == 68,
  
  dplyr::n_distinct(
    site_year_metrics$localidade
  ) == 43,
  
  sum(
    site_year_metrics$effort_minutes
  ) == 8415,
  
  sum(
    site_year_metrics$n_positive
  ) == 173
)


################################################################################
# 2.2. Final inferential analyses
################################################################################

stopifnot(
  nrow(
    effort_standardised_permutation_tests
  ) == 7
)


################################################################################
# 2.3. Supplementary tables
################################################################################

stopifnot(
  nrow(
    table_s1_localities
  ) == 43,
  
  sum(
    table_s2_locality_metrics$
      total_effort_minutes
  ) == 8415,
  
  sum(
    table_s2_locality_metrics$
      positive_one_minute_records
  ) == 173
)


################################################################################
# 2.4. Figure 5 pooled locality metrics
################################################################################

stopifnot(
  nrow(
    figure_5_locality_totals
  ) == 43,
  
  sum(
    figure_5_locality_totals$
      total_effort_minutes
  ) == 8415,
  
  sum(
    figure_5_locality_totals$
      total_positive_records
  ) == 173,
  
  sum(
    figure_5_locality_totals$
      total_positive_records > 0
  ) == 15
)


################################################################################
# 2.5. Figure 6 QGIS source table
################################################################################

stopifnot(
  nrow(
    figure_6_qgis
  ) == 43,
  
  sum(
    figure_6_qgis$
      positive_records
  ) == 173,
  
  sum(
    figure_6_qgis$
      occurrence == "PRESENT"
  ) == 15,
  
  sum(
    figure_6_qgis$
      occurrence == "ABSENT"
  ) == 28
)


################################################################################
# 2.6. Figure S1 temporal design
################################################################################

stopifnot(
  nrow(
    temporal_locality
  ) == 21,
  
  dplyr::n_distinct(
    temporal_locality$localidade
  ) == 7,
  
  dplyr::n_distinct(
    temporal_locality$monitoring_cycle
  ) == 3,
  
  nrow(
    temporal_summary
  ) == 3
)


################################################################################
# 2.7. Check final figure and source files
################################################################################

expected_files <- c(
  "figs/fig_2_sampling_effort.png",
  "figs/fig_3_detections_depth_sampling.png",
  "figs/fig_4_annual_positive_RAIW.png",
  "figs/fig_5_dpue_raiw.png",
  "figs/Figure_S1_temporal_effort_standardised.png",
  "outputs/figure_6_qgis_locality_metrics.csv"
)


stopifnot(
  all(
    file.exists(
      expected_files
    )
  )
)

################################################################################
# 2.8. Regional classification consistency
################################################################################

# Internal analytical coding
stopifnot(
  setequal(
    unique(site_year_metrics$region),
    c(
      "REBIO",
      "ADJACENT_REBIO",
      "SURROUNDINGS"
    )
  )
)

# Expected number of monitored localities in each region
region_locality_check <- site_year_metrics |>
  distinct(
    localidade,
    region
  ) |>
  count(
    region,
    name = "n_localities"
  )

stopifnot(
  region_locality_check$n_localities[
    region_locality_check$region == "REBIO"
  ] == 15,
  
  region_locality_check$n_localities[
    region_locality_check$region == "ADJACENT_REBIO"
  ] == 6,
  
  region_locality_check$n_localities[
    region_locality_check$region == "SURROUNDINGS"
  ] == 22
)

# Publication-facing supplementary tables
stopifnot(
  setequal(
    unique(table_s1_localities$region),
    c(
      "REBIO",
      "ADJACENT TO REBIO",
      "SURROUNDINGS"
    )
  ),
  
  setequal(
    unique(table_s2_locality_metrics$region),
    c(
      "REBIO",
      "ADJACENT TO REBIO",
      "SURROUNDINGS"
    )
  )
)



################################################################################
# 3. FINAL SUMMARY
################################################################################

cat(
  "\n============================================================\n",
  "COMPLETE WORKFLOW FINISHED SUCCESSFULLY\n",
  "============================================================\n",
  "Localities: 43\n",
  "Monitoring records: 8415\n",
  "Positive records: 173\n",
  "Locality-year observations: 68\n",
  "Final inferential tests: 7\n",
  "Positive localities: 15\n",
  "Temporal localities in Figure S1: 7\n",
  "Main Figures 2-5 generated successfully\n",
  "Figure 6 QGIS source table generated successfully\n",
  "Figure S1 generated successfully\n",
  "============================================================\n"
)