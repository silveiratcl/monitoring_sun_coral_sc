################################################################################
# 04_raiw_sensitivity.R
#
# Evaluate robustness of RAI-W to alternative weighting schemes
################################################################################

source("R/00_setup.R")
source("R/01_prepare_monitoring_data.R")

################################################################################
# 1. Alternative weighting schemes
################################################################################

weights_manual <- c(
  `10` = 1.00,
  `8`  = 0.80,
  `6`  = 0.60,
  `4`  = 0.10,
  `2`  = 0.04,
  `0`  = 0.00
)

weights_moderate <- c(
  `10` = 1.00,
  `8`  = 0.80,
  `6`  = 0.60,
  `4`  = 0.25,
  `2`  = 0.10,
  `0`  = 0.00
)

weights_linear <- c(
  `10` = 1.00,
  `8`  = 0.80,
  `6`  = 0.60,
  `4`  = 0.40,
  `2`  = 0.20,
  `0`  = 0.00
)

################################################################################
# 2. Locality extent
################################################################################

locality_extent <- df_localidade |>
  select(
    localidade,
    extent_m,
    Uni100m
  )

################################################################################
# 3. Minute-level dataset
################################################################################

df_sensitivity <- df_monit |>
  mutate(
    year = lubridate::year(data),
    dafor_num = clean_num(dafor),
    
    weight_manual =
      coalesce(
        unname(weights_manual[as.character(dafor_num)]),
        0
      ),
    
    weight_moderate =
      coalesce(
        unname(weights_moderate[as.character(dafor_num)]),
        0
      ),
    
    weight_linear =
      coalesce(
        unname(weights_linear[as.character(dafor_num)]),
        0
      )
  ) |>
  filter(
    obs != "estimado dos dados do ICMBio",
    faixa_bat != "Na",
    !is.na(localidade),
    !is.na(year),
    !is.na(dafor_id)
  ) |>
  left_join(
    locality_extent,
    by = "localidade"
  )

################################################################################
# 4. Site-year RAI-W
################################################################################

site_year_sensitivity <- df_sensitivity |>
  group_by(localidade, year) |>
  summarise(
    effort_minutes = n(),
    effort_hours = effort_minutes / 60,
    
    sum_manual = sum(weight_manual, na.rm = TRUE),
    sum_moderate = sum(weight_moderate, na.rm = TRUE),
    sum_linear = sum(weight_linear, na.rm = TRUE),
    
    Uni100m = first(Uni100m),
    
    .groups = "drop"
  ) |>
  mutate(
    denominator = effort_hours * Uni100m,
    
    raiw_manual =
      sum_manual / denominator,
    
    raiw_moderate =
      sum_moderate / denominator,
    
    raiw_linear =
      sum_linear / denominator
  )

################################################################################
# 5. Site-year correlations
################################################################################

sensitivity_cor_site_year <- site_year_sensitivity |>
  select(
    raiw_manual,
    raiw_moderate,
    raiw_linear
  ) |>
  cor(
    method = "spearman",
    use = "complete.obs"
  )

################################################################################
# 6. Locality ranking stability
################################################################################

locality_sensitivity <- site_year_sensitivity |>
  group_by(localidade) |>
  summarise(
    raiw_manual = sum(raiw_manual),
    raiw_moderate = sum(raiw_moderate),
    raiw_linear = sum(raiw_linear),
    .groups = "drop"
  ) |>
  mutate(
    rank_manual =
      min_rank(desc(raiw_manual)),
    
    rank_moderate =
      min_rank(desc(raiw_moderate)),
    
    rank_linear =
      min_rank(desc(raiw_linear)),
    
    diff_moderate =
      rank_manual - rank_moderate,
    
    diff_linear =
      rank_manual - rank_linear
  )

################################################################################
# 7. Ranking correlations
################################################################################

ranking_cor <- locality_sensitivity |>
  select(
    rank_manual,
    rank_moderate,
    rank_linear
  ) |>
  cor(
    method = "spearman",
    use = "complete.obs"
  )

################################################################################
# 8. Largest rank changes
################################################################################

rank_changes <- locality_sensitivity |>
  mutate(
    max_abs_change =
      pmax(
        abs(diff_moderate),
        abs(diff_linear)
      )
  ) |>
  arrange(desc(max_abs_change))

################################################################################
# 9. Summary table for manuscript
################################################################################

sensitivity_summary <- tibble(
  comparison = c(
    "Manual_vs_Moderate",
    "Manual_vs_Linear",
    "Moderate_vs_Linear",
    "Rank_Manual_vs_Moderate",
    "Rank_Manual_vs_Linear",
    "Rank_Moderate_vs_Linear"
  ),
  spearman_rho = c(
    sensitivity_cor_site_year[1,2],
    sensitivity_cor_site_year[1,3],
    sensitivity_cor_site_year[2,3],
    ranking_cor[1,2],
    ranking_cor[1,3],
    ranking_cor[2,3]
  )
)

################################################################################
# 10. Print results
################################################################################

cat("\nSite-year correlations\n")
print(sensitivity_cor_site_year)

cat("\nRanking correlations\n")
print(ranking_cor)

cat("\nLargest rank changes\n")
print(rank_changes, n = 15)

################################################################################
# 11. Export outputs
################################################################################

write_csv(
  site_year_sensitivity,
  "outputs/site_year_raiw_sensitivity.csv"
)

write_csv(
  locality_sensitivity,
  "outputs/locality_raiw_sensitivity.csv"
)

write_csv(
  rank_changes,
  "outputs/locality_raiw_rank_changes.csv"
)

write_csv(
  sensitivity_summary,
  "outputs/sensitivity_summary.csv"
)

saveRDS(
  sensitivity_cor_site_year,
  "outputs/sensitivity_cor_site_year.rds"
)

saveRDS(
  ranking_cor,
  "outputs/ranking_cor.rds"
)

