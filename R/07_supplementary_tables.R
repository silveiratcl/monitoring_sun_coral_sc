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
