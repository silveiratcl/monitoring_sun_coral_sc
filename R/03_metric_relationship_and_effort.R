################################################################################
# 03_metric_relationship_and_effort.R
#
# Evaluate:
# - relationship between DPUE and RAI-W
# - association between monitoring effort and invasion metrics
# - descriptive annual summaries
#
# Temporal inference is performed separately in
# 08_effort_standardised_analyses.R.
################################################################################

source("R/00_setup.R")

site_year_metrics <- readRDS(
  "outputs/site_year_metrics.rds"
)

################################################################################
# 1. CHECK REQUIRED VARIABLES
################################################################################

required_columns <- c(
  "localidade",
  "year",
  "effort_minutes",
  "n_positive",
  "dpue",
  "rai_w"
)

missing_columns <- setdiff(
  required_columns,
  names(site_year_metrics)
)

if (length(missing_columns) > 0) {
  stop(
    paste(
      "Missing columns in site_year_metrics:",
      paste(missing_columns, collapse = ", ")
    )
  )
}

################################################################################
# 2. DPUE × RAI-W RELATIONSHIP
################################################################################

dpue_raiw_cor <- cor.test(
  site_year_metrics$dpue,
  site_year_metrics$rai_w,
  method = "spearman",
  exact = FALSE
)

dpue_raiw_summary <- tibble(
  analysis = "DPUE_vs_RAIW",
  n_locality_years = sum(
    complete.cases(
      site_year_metrics$dpue,
      site_year_metrics$rai_w
    )
  ),
  rho = unname(dpue_raiw_cor$estimate),
  p_value = dpue_raiw_cor$p.value
)

################################################################################
# 3. MONITORING EFFORT EVALUATION
#
# These correlations are diagnostic analyses evaluating whether locality-year
# metric values remain associated with the amount of monitoring effort.
################################################################################

effort_dpue <- cor.test(
  site_year_metrics$effort_minutes,
  site_year_metrics$dpue,
  method = "spearman",
  exact = FALSE
)

effort_raiw <- cor.test(
  site_year_metrics$effort_minutes,
  site_year_metrics$rai_w,
  method = "spearman",
  exact = FALSE
)

effort_positive <- cor.test(
  site_year_metrics$effort_minutes,
  site_year_metrics$n_positive,
  method = "spearman",
  exact = FALSE
)

effort_correlations <- tibble(
  response = c(
    "DPUE",
    "RAI-W",
    "Positive detections"
  ),
  n_locality_years = c(
    sum(
      complete.cases(
        site_year_metrics$effort_minutes,
        site_year_metrics$dpue
      )
    ),
    sum(
      complete.cases(
        site_year_metrics$effort_minutes,
        site_year_metrics$rai_w
      )
    ),
    sum(
      complete.cases(
        site_year_metrics$effort_minutes,
        site_year_metrics$n_positive
      )
    )
  ),
  rho = c(
    unname(effort_dpue$estimate),
    unname(effort_raiw$estimate),
    unname(effort_positive$estimate)
  ),
  p_value = c(
    effort_dpue$p.value,
    effort_raiw$p.value,
    effort_positive$p.value
  )
)

################################################################################
# 4. DESCRIPTIVE ANNUAL SUMMARIES
#
# Civil-year summaries are descriptive only because the set of monitored
# localities varied among years.
################################################################################

annual_summary <- site_year_metrics |>
  group_by(year) |>
  summarise(
    n_localities = n(),
    effort_minutes = sum(effort_minutes, na.rm = TRUE),
    effort_hours = effort_minutes / 60,
    positive_detections = sum(n_positive, na.rm = TRUE),
    positive_per_hour =
      positive_detections / effort_hours,
    mean_dpue = mean(dpue, na.rm = TRUE),
    median_dpue = median(dpue, na.rm = TRUE),
    mean_raiw = mean(rai_w, na.rm = TRUE),
    median_raiw = median(rai_w, na.rm = TRUE),
    .groups = "drop"
  )

################################################################################
# 5. PRINT RESULTS
################################################################################

cat("\nDPUE × RAI-W relationship\n")
print(dpue_raiw_summary)

cat("\nMonitoring effort correlations\n")
print(effort_correlations)

cat("\nDescriptive annual summaries\n")
print(annual_summary, n = Inf)

################################################################################
# 6. EXPORT OUTPUTS
################################################################################

write_csv(
  dpue_raiw_summary,
  "outputs/dpue_raiw_relationship.csv"
)

write_csv(
  effort_correlations,
  "outputs/effort_correlations.csv"
)

write_csv(
  annual_summary,
  "outputs/annual_summary.csv"
)

saveRDS(
  dpue_raiw_summary,
  "outputs/dpue_raiw_relationship.rds"
)

saveRDS(
  effort_correlations,
  "outputs/effort_correlations.rds"
)

saveRDS(
  annual_summary,
  "outputs/annual_summary.rds"
)

################################################################################
# 7. REMOVE OBSOLETE TEMPORAL MODEL OUTPUTS
#
# Temporal linear models based on all locality-year combinations were
# superseded by the effort-standardised analyses implemented in script 08.
################################################################################

obsolete_files <- c(
  "outputs/temporal_models.csv",
  "outputs/temporal_models.rds"
)

file.remove(
  obsolete_files[file.exists(obsolete_files)]
)