################################################################################
# 07. RAI-W sensitivity analysis
################################################################################

library(tidyverse)
library(lubridate)
library(stringr)

################################################################################
# 1. Helper functions
################################################################################

clean_num <- function(x) {
  x %>%
    as.character() %>%
    str_trim() %>%
    na_if("") %>%
    na_if("Na") %>%
    str_replace_all(",", ".") %>%
    as.numeric()
}

################################################################################
# 2. Alternative weighting schemes
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
# 3. Prepare minute-level data
################################################################################

df_sens_minutes <- df_monit %>%
  mutate(
    localidade = str_to_upper(str_replace_all(localidade, "_", " ")),
    year = year(data),
    dafor_num = clean_num(dafor),
    
    weight_manual = unname(weights_manual[as.character(dafor_num)]),
    weight_moderate = unname(weights_moderate[as.character(dafor_num)]),
    weight_linear = unname(weights_linear[as.character(dafor_num)]),
    
    weight_manual = coalesce(weight_manual, 0),
    weight_moderate = coalesce(weight_moderate, 0),
    weight_linear = coalesce(weight_linear, 0)
  ) %>%
  filter(
    obs != "estimado dos dados do ICMBio",
    faixa_bat != "Na",
    !is.na(dafor_id),
    !is.na(localidade),
    !is.na(year)
  ) %>%
  left_join(
    df_localidade %>%
      select(localidade, extent_m, Uni100m),
    by = "localidade"
  )

################################################################################
# 4. Calculate site-year RAI-W under each scheme
################################################################################

site_year_sensitivity <- df_sens_minutes %>%
  group_by(localidade, year) %>%
  summarise(
    n_minutes = n(),
    n_positive = sum(dafor_num > 0, na.rm = TRUE),
    effort_minutes = n_minutes,
    effort_hours = effort_minutes / 60,
    
    sum_manual = sum(weight_manual, na.rm = TRUE),
    sum_moderate = sum(weight_moderate, na.rm = TRUE),
    sum_linear = sum(weight_linear, na.rm = TRUE),
    
    extent_m = first(extent_m),
    Uni100m = first(Uni100m),
    .groups = "drop"
  ) %>%
  mutate(
    denominator = effort_hours * Uni100m,
    
    raiw_manual = if_else(denominator > 0, sum_manual / denominator, NA_real_),
    raiw_moderate = if_else(denominator > 0, sum_moderate / denominator, NA_real_),
    raiw_linear = if_else(denominator > 0, sum_linear / denominator, NA_real_)
  )

################################################################################
# 5. Correlation among weighting schemes at site-year level
################################################################################

sensitivity_cor_site_year <- site_year_sensitivity %>%
  select(raiw_manual, raiw_moderate, raiw_linear) %>%
  cor(method = "spearman", use = "complete.obs")

sensitivity_cor_site_year

################################################################################
# 6. Locality-level ranking stability
################################################################################

locality_sensitivity <- site_year_sensitivity %>%
  group_by(localidade) %>%
  summarise(
    raiw_manual = sum(raiw_manual, na.rm = TRUE),
    raiw_moderate = sum(raiw_moderate, na.rm = TRUE),
    raiw_linear = sum(raiw_linear, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    rank_manual = min_rank(desc(raiw_manual)),
    rank_moderate = min_rank(desc(raiw_moderate)),
    rank_linear = min_rank(desc(raiw_linear)),
    
    diff_moderate = rank_manual - rank_moderate,
    diff_linear = rank_manual - rank_linear
  ) %>%
  arrange(rank_manual)

locality_sensitivity %>%
  print(n = Inf)

################################################################################
# 7. Correlation among locality rankings
################################################################################

ranking_cor <- locality_sensitivity %>%
  select(rank_manual, rank_moderate, rank_linear) %>%
  cor(method = "spearman", use = "complete.obs")

ranking_cor

################################################################################
# 8. Identify localities with largest rank changes
################################################################################

rank_changes <- locality_sensitivity %>%
  mutate(
    max_abs_change = pmax(abs(diff_moderate), abs(diff_linear))
  ) %>%
  arrange(desc(max_abs_change))

rank_changes %>%
  print(n = Inf)

################################################################################
# 9. Export outputs
################################################################################

write_csv(site_year_sensitivity, "outputs/site_year_raiw_sensitivity.csv")
write_csv(locality_sensitivity, "outputs/locality_raiw_sensitivity.csv")
write_csv(rank_changes, "outputs/locality_raiw_rank_changes.csv")


sensitivity_cor_site_year
ranking_cor
rank_changes |> print(n = 15)
