################################################################################
# 07_supplementary_tables.R
################################################################################

source("R/00_setup.R")

df_localidade <- readRDS("outputs/df_localidade_clean.rds")
site_year_metrics <- readRDS("outputs/site_year_metrics.rds")

effort_correlations <- read_csv(
  "outputs/effort_correlations.csv",
  show_col_types = FALSE
)

temporal_models <- read_csv(
  "outputs/temporal_models.csv",
  show_col_types = FALSE
)

sensitivity_summary <- read_csv(
  "outputs/sensitivity_summary.csv",
  show_col_types = FALSE
)

table_s1_localities <- df_localidade |>
  transmute(
    locality = localidade,
    shoreline_extent_m = extent_m,
    shoreline_units_100m = Uni100m
  ) |>
  arrange(locality)

################################################################################
# Table S2
# Monitoring metrics by locality
#
# Complete table is exported as CSV.
# Formatted PNG tables are split into:
# - S2a: localities with detections
# - S2b: localities without detections
################################################################################

table_s2_locality_metrics <- site_year_metrics |>
  group_by(localidade, region) |>
  summarise(
    total_effort_minutes = sum(effort_minutes, na.rm = TRUE),
    total_effort_hours = sum(effort_hours, na.rm = TRUE),
    total_positive_detections = sum(n_positive, na.rm = TRUE),
    total_weighted_score = sum(sum_weight, na.rm = TRUE),
    shoreline_extent_m = first(shoreline_m),
    shoreline_units_100m = first(uni100m),
    total_dpue = sum(dpue, na.rm = TRUE),
    total_raiw = sum(rai_w, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(total_dpue))

table_s2a_positive_localities <- table_s2_locality_metrics |>
  filter(total_positive_detections > 0) |>
  arrange(desc(total_dpue))

table_s2b_negative_localities <- table_s2_locality_metrics |>
  filter(total_positive_detections == 0) |>
  arrange(region, localidade)

write_csv(
  table_s2_locality_metrics,
  "outputs/Table_S2_locality_metrics_complete.csv"
)

write_csv(
  table_s2a_positive_localities,
  "outputs/Table_S2a_positive_localities.csv"
)

write_csv(
  table_s2b_negative_localities,
  "outputs/Table_S2b_negative_localities.csv"
)

################################################################################
# GT Table S2a - Localities with detections
################################################################################

table_s2a_positive_localities |>
  gt() |>
  tab_header(
    title = "Table S2a. Monitoring metrics for localities with sun coral detections"
  ) |>
  cols_label(
    localidade = "Locality",
    region = "Region",
    total_effort_minutes = "Effort (min)",
    total_effort_hours = "Effort (h)",
    total_positive_detections = "Positive detections",
    total_weighted_score = "Weighted score",
    shoreline_extent_m = "Shoreline extent (m)",
    shoreline_units_100m = "100 m units",
    total_dpue = "DPUE",
    total_raiw = "RAI-W"
  ) |>
  fmt_number(
    columns = c(
      total_effort_hours,
      total_weighted_score,
      shoreline_extent_m,
      shoreline_units_100m,
      total_dpue,
      total_raiw
    ),
    decimals = 3
  ) |>
  fmt_number(
    columns = c(
      total_effort_minutes,
      total_positive_detections
    ),
    decimals = 0
  ) |>
  gtsave("outputs/gt_tables/Table_S2a_positive_localities.png")

################################################################################
# GT Table S2b - Localities without detections
################################################################################

table_s2b_negative_localities |>
  select(
    localidade,
    region,
    total_effort_minutes,
    total_effort_hours,
    shoreline_extent_m,
    shoreline_units_100m
  ) |>
  gt() |>
  tab_header(
    title = "Table S2b. Monitored localities without sun coral detections"
  ) |>
  cols_label(
    localidade = "Locality",
    region = "Region",
    total_effort_minutes = "Effort (min)",
    total_effort_hours = "Effort (h)",
    shoreline_extent_m = "Shoreline extent (m)",
    shoreline_units_100m = "100 m units"
  ) |>
  fmt_number(
    columns = c(
      total_effort_hours,
      shoreline_extent_m,
      shoreline_units_100m
    ),
    decimals = 3
  ) |>
  fmt_number(
    columns = total_effort_minutes,
    decimals = 0
  ) |>
  gtsave("outputs/gt_tables/Table_S2b_negative_localities.png")










table_s3_sensitivity <- sensitivity_summary |>
  mutate(
    spearman_rho = round(spearman_rho, 4)
  )

table_s4a_effort_correlations <- effort_correlations |>
  mutate(
    rho = round(rho, 4),
    p_value = signif(p_value, 3)
  )

table_s4b_temporal_models <- temporal_models |>
  filter(
    model %in% c("DPUE_year_effort", "RAIW_year_effort"),
    term %in% c("year", "effort_minutes")
  ) |>
  transmute(
    response_model = model,
    predictor = term,
    estimate = signif(estimate, 4),
    std_error = signif(std.error, 4),
    statistic = signif(statistic, 4),
    p_value = signif(p.value, 3),
    r_squared = signif(r_squared, 4),
    adj_r_squared = signif(adj_r_squared, 4)
  )

write_csv(table_s1_localities, "outputs/Table_S1_localities_extent.csv")
write_csv(table_s2_locality_metrics, "outputs/Table_S2_locality_metrics.csv")
write_csv(table_s3_sensitivity, "outputs/Table_S3_raiw_sensitivity.csv")
write_csv(table_s4a_effort_correlations, "outputs/Table_S4a_effort_correlations.csv")
write_csv(table_s4b_temporal_models, "outputs/Table_S4b_temporal_models.csv")

cat("\nSupplementary tables exported:\n")
print(list.files("outputs", pattern = "Table_S"))


################################################################################
# 8. Export formatted GT tables
################################################################################

library(gt)
library(webshot2)

dir.create("outputs/gt_tables", showWarnings = FALSE)

# Table S1
table_s1_localities |>
  gt() |>
  tab_header(
    title = "Table S1. Monitored localities and shoreline extent"
  ) |>
  fmt_number(
    columns = c(shoreline_extent_m, shoreline_units_100m),
    decimals = 2
  ) |>
  gtsave("outputs/gt_tables/Table_S1_localities_extent.png")

################################################################################
# Table S2
# Monitoring metrics by locality
#
# Complete table is exported as CSV.
# Formatted PNG tables are split into:
# - S2a: localities with detections
# - S2b: localities without detections
################################################################################

table_s2_locality_metrics <- site_year_metrics |>
  group_by(localidade, region) |>
  summarise(
    total_effort_minutes = sum(effort_minutes, na.rm = TRUE),
    total_effort_hours = sum(effort_hours, na.rm = TRUE),
    total_positive_detections = sum(n_positive, na.rm = TRUE),
    total_weighted_score = sum(sum_weight, na.rm = TRUE),
    shoreline_extent_m = first(shoreline_m),
    shoreline_units_100m = first(uni100m),
    total_dpue = sum(dpue, na.rm = TRUE),
    total_raiw = sum(rai_w, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(total_dpue))

table_s2a_positive_localities <- table_s2_locality_metrics |>
  filter(total_positive_detections > 0) |>
  arrange(desc(total_dpue))

table_s2b_negative_localities <- table_s2_locality_metrics |>
  filter(total_positive_detections == 0) |>
  arrange(region, localidade)

write_csv(
  table_s2_locality_metrics,
  "outputs/Table_S2_locality_metrics_complete.csv"
)

write_csv(
  table_s2a_positive_localities,
  "outputs/Table_S2a_positive_localities.csv"
)

write_csv(
  table_s2b_negative_localities,
  "outputs/Table_S2b_negative_localities.csv"
)











# Table S3
table_s3_sensitivity |>
  gt() |>
  tab_header(
    title = "Table S3. RAI-W sensitivity analysis"
  ) |>
  fmt_number(
    columns = spearman_rho,
    decimals = 4
  ) |>
  gtsave("outputs/gt_tables/Table_S3_raiw_sensitivity.png")

# Table S4a
table_s4a_effort_correlations |>
  gt() |>
  tab_header(
    title = "Table S4a. Relationship between monitoring effort and invasion metrics"
  ) |>
  fmt_number(
    columns = c(rho, p_value),
    decimals = 3
  ) |>
  gtsave("outputs/gt_tables/Table_S4a_effort_correlations.png")

# Table S4b
table_s4b_temporal_models |>
  gt() |>
  tab_header(
    title = "Table S4b. Temporal models accounting for monitoring effort"
  ) |>
  fmt_number(
    columns = c(
      estimate,
      std_error,
      statistic,
      p_value,
      r_squared,
      adj_r_squared
    ),
    decimals = 3
  ) |>
  gtsave("outputs/gt_tables/Table_S4b_temporal_models.png")

