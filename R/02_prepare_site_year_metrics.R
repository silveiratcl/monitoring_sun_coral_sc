################################################################################
# 02_prepare_site_year_metrics.R
#
# Create the site-year analytical dataset used for:
# - DPUE × RAI-W relationship
# - effort evaluation
# - temporal models
# - RAI-W sensitivity analysis
################################################################################

source("R/00_setup.R")
source("R/01_prepare_monitoring_data.R")

################################################################################
# 1. Prepare locality extent
################################################################################

locality_extent <- df_localidade |>
  select(
    localidade,
    shoreline_m = extent_m,
    uni100m = Uni100m
  )

################################################################################
# 2. Build site-year metrics
#
# One row = locality × year
################################################################################

site_year_metrics <- df_monit |>
  mutate(
    year = lubridate::year(data),
    dafor_num = clean_num(dafor),
    weight = unname(manual_weights[as.character(dafor_num)]),
    weight = coalesce(weight, 0)
  ) |>
  filter(
    obs != "estimado dos dados do ICMBio",
    faixa_bat != "Na",
    !is.na(localidade),
    !is.na(year),
    !is.na(dafor_id)
  ) |>
  group_by(localidade, year, region) |>
  summarise(
    n_minutes = n(),
    n_transects = n_distinct(dafor_id),
    effort_minutes = n_minutes,
    effort_hours = effort_minutes / 60,
    n_positive = sum(dafor_num > 0, na.rm = TRUE),
    sum_weight = sum(weight, na.rm = TRUE),
    .groups = "drop"
  ) |>
  left_join(locality_extent, by = "localidade") |>
  mutate(
    denominator = effort_hours * uni100m,
    dpue = if_else(
      denominator > 0,
      n_positive / denominator,
      NA_real_
    ),
    rai_w = if_else(
      denominator > 0,
      sum_weight / denominator,
      NA_real_
    )
  ) |>
  arrange(localidade, year)

################################################################################
# 3. Quality checks
################################################################################

site_year_summary <- site_year_metrics |>
  summarise(
    n_rows = n(),
    n_sites = n_distinct(localidade),
    years = paste(sort(unique(year)), collapse = ", "),
    total_minutes = sum(effort_minutes, na.rm = TRUE),
    total_positive = sum(n_positive, na.rm = TRUE),
    total_weight = sum(sum_weight, na.rm = TRUE)
  )

missing_extent <- site_year_metrics |>
  filter(is.na(shoreline_m) | is.na(uni100m) | is.na(dpue))

unmatched_localities <- df_monit |>
  distinct(localidade) |>
  anti_join(
    df_localidade |> distinct(localidade),
    by = "localidade"
  )

region_summary <- site_year_metrics |>
  distinct(region, localidade) |>
  count(region, name = "n_localities")

top_dpue <- site_year_metrics |>
  arrange(desc(dpue)) |>
  select(
    localidade,
    region,
    year,
    effort_minutes,
    n_positive,
    shoreline_m,
    uni100m,
    denominator,
    dpue,
    rai_w
  )

################################################################################
# 4. Print checks
################################################################################

print(site_year_summary)
print(region_summary)

if (nrow(missing_extent) > 0) {
  warning("Some site-year rows have missing extent or metric values.")
  print(missing_extent, n = Inf)
}

if (nrow(unmatched_localities) > 0) {
  warning("Some monitored localities were not found in df_localidade.")
  print(unmatched_localities, n = Inf)
}

top_dpue |> print(n = 20)

################################################################################
# 5. Export outputs
################################################################################

write_csv(site_year_metrics, "outputs/site_year_metrics.csv")
saveRDS(site_year_metrics, "outputs/site_year_metrics.rds")

write_csv(site_year_summary, "outputs/site_year_summary.csv")
write_csv(region_summary, "outputs/region_summary.csv")
write_csv(top_dpue, "outputs/top_dpue_site_year.csv")

