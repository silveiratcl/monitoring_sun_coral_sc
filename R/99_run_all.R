################################################################################
# 99_run_all.R
#
# Run the complete analytical workflow from raw monitoring data
# to figures, summary metrics, sensitivity analyses, and
# supplementary tables.
################################################################################

rm(list = ls())
gc()

cat(
  "\n============================================================\n",
  "RUNNING COMPLETE ANALYTICAL WORKFLOW\n",
  "============================================================\n"
)

scripts <- c(
  "R/00_setup.R",
  "R/01_prepare_monitoring_data.R",
  "R/02_prepare_site_year_metrics.R",
  "R/03_metric_relationship_and_effort.R",
  "R/04_raiw_sensitivity.R",
  "R/05_figures_dpue_raiw.R",
  "R/06_summary_table.R",
  "R/07_sampling_balance_and_cycles.R",
  "R/08_effort_standardised_analyses.R",
  "R/09_supplementary_tables.R"
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
# FINAL CONSISTENCY CHECKS
################################################################################

stopifnot(
  nrow(site_year_metrics) == 68,
  dplyr::n_distinct(site_year_metrics$localidade) == 43,
  sum(site_year_metrics$effort_minutes) == 8415,
  sum(site_year_metrics$n_positive) == 173,
  
  nrow(effort_standardised_permutation_tests) == 7,
  
  nrow(table_s1_localities) == 43,
  sum(table_s2_locality_metrics$total_effort_minutes) == 8415,
  sum(table_s2_locality_metrics$positive_one_minute_records) == 173
)

cat(
  "\n============================================================\n",
  "COMPLETE WORKFLOW FINISHED SUCCESSFULLY\n",
  "============================================================\n",
  "Localities: 43\n",
  "Monitoring records: 8415\n",
  "Positive records: 173\n",
  "Locality-year observations: 68\n",
  "Final inferential tests: 7\n",
  "============================================================\n"
)