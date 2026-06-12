################################################################################
# 03_metric_relationship_and_effort.R
#
# Evaluate:
# - DPUE × RAI-W relationship
# - influence of monitoring effort
# - temporal trends accounting for effort
################################################################################

source("R/00_setup.R")

site_year_metrics <- readRDS(
  "outputs/site_year_metrics.rds"
)

################################################################################
# 1. DPUE × RAI-W relationship
################################################################################

dpue_raiw_cor <- cor.test(
  site_year_metrics$dpue,
  site_year_metrics$rai_w,
  method = "spearman"
)

dpue_raiw_summary <- tibble(
  analysis = "DPUE_vs_RAIW",
  rho = unname(dpue_raiw_cor$estimate),
  p_value = dpue_raiw_cor$p.value
)

################################################################################
# 2. Effort evaluation
################################################################################

effort_dpue <- cor.test(
  site_year_metrics$effort_minutes,
  site_year_metrics$dpue,
  method = "spearman"
)

effort_raiw <- cor.test(
  site_year_metrics$effort_minutes,
  site_year_metrics$rai_w,
  method = "spearman"
)

effort_positive <- cor.test(
  site_year_metrics$effort_minutes,
  site_year_metrics$n_positive,
  method = "spearman"
)

effort_correlations <- tibble(
  response = c(
    "DPUE",
    "RAI_W",
    "Positive_detections"
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
# 3. Annual summaries
################################################################################

annual_summary <- site_year_metrics |>
  group_by(year) |>
  summarise(
    n_sites = n(),
    effort_minutes = sum(effort_minutes),
    positive_detections = sum(n_positive),
    positive_per_hour =
      sum(n_positive) /
      (sum(effort_minutes) / 60),
    mean_dpue = mean(dpue, na.rm = TRUE),
    mean_raiw = mean(rai_w, na.rm = TRUE),
    .groups = "drop"
  )

################################################################################
# 4. Temporal models
################################################################################

m1 <- lm(
  log1p(dpue) ~ year,
  data = site_year_metrics
)

m2 <- lm(
  log1p(dpue) ~ year + effort_minutes,
  data = site_year_metrics
)

m3 <- lm(
  log1p(rai_w) ~ year,
  data = site_year_metrics
)

m4 <- lm(
  log1p(rai_w) ~ year + effort_minutes,
  data = site_year_metrics
)

################################################################################
# 5. Extract model coefficients
################################################################################

extract_model <- function(model_object, model_name) {
  
  model_summary <- summary(model_object)
  
  broom::tidy(model_object) |>
    mutate(
      model = model_name,
      r_squared = model_summary$r.squared,
      adj_r_squared = model_summary$adj.r.squared
    ) |>
    select(
      model,
      term,
      estimate,
      std.error,
      statistic,
      p.value,
      r_squared,
      adj_r_squared
    )
}


temporal_models <- bind_rows(
  extract_model(m1, "DPUE_year"),
  extract_model(m2, "DPUE_year_effort"),
  extract_model(m3, "RAIW_year"),
  extract_model(m4, "RAIW_year_effort")
)

print(temporal_models)

write_csv(
  temporal_models,
  "outputs/temporal_models.csv"
)

saveRDS(
  temporal_models,
  "outputs/temporal_models.rds"
)

