################################################################################
# 10_balanced_monitoring_cycle_analysis.R
#
# Complementary temporal robustness analysis
#
# This analysis:
# - retains only localities monitored in all three monitoring cycles;
# - recalculates DPUE and RAI-W by locality and monitoring cycle;
# - treats locality as a repeated sampling block;
# - evaluates whether temporal patterns persist under balanced coverage.
################################################################################

source("R/00_setup.R")

library(tidyverse)
library(broom)

dir.create(
  "outputs",
  showWarnings = FALSE,
  recursive = TRUE
)

################################################################################
# 1. LOAD DATA PREPARED BY SCRIPT 09
################################################################################

df_cycle_all <- readRDS(
  "outputs/monitoring_data_with_cycles_all.rds"
)

balanced_cycle_candidates <- read_csv(
  "outputs/balanced_cycle_candidates.csv",
  show_col_types = FALSE
)

expected_cycles <- c(
  "2022/2023",
  "2023/2024",
  "2024/2025"
)

################################################################################
# 2. CHECK THE BALANCED DESIGN
################################################################################

balanced_localities <- balanced_cycle_candidates |>
  distinct(
    localidade,
    region
  )

if (nrow(balanced_localities) != 7) {
  warning(
    paste0(
      "The balanced dataset contains ",
      nrow(balanced_localities),
      " localities rather than the expected 7."
    )
  )
}

################################################################################
# 3. PREPARE LOCALITY EXTENT
################################################################################

locality_extent <- df_localidade |>
  transmute(
    localidade = stringr::str_squish(
      stringr::str_to_upper(localidade)
    ),
    extent_m = as.numeric(extent_m),
    Uni100m = as.numeric(Uni100m)
  ) |>
  distinct(
    localidade,
    .keep_all = TRUE
  )

################################################################################
# 4. DEFINE THE RAI-W WEIGHTS
################################################################################

weights_manual <- c(
  `10` = 1.00,
  `8`  = 0.80,
  `6`  = 0.60,
  `4`  = 0.10,
  `2`  = 0.04,
  `0`  = 0.00
)

################################################################################
# 5. PREPARE THE BALANCED MINUTE-LEVEL DATASET
################################################################################

balanced_cycle_data <- df_cycle_all |>
  filter(
    localidade %in%
      balanced_localities$localidade
  ) |>
  mutate(
    dafor_num = as.numeric(dafor),
    
    raiw_weight = coalesce(
      unname(
        weights_manual[
          as.character(dafor_num)
        ]
      ),
      0
    ),
    
    cycle_factor = factor(
      monitoring_cycle,
      levels = expected_cycles,
      ordered = TRUE
    ),
    
    # Numeric variable used to test an ordered temporal trend
    cycle_index = match(
      monitoring_cycle,
      expected_cycles
    )
  ) |>
  left_join(
    locality_extent,
    by = "localidade"
  )

################################################################################
# 6. CHECK FOR MISSING LOCALITY EXTENT
################################################################################

missing_extent <- balanced_cycle_data |>
  filter(
    is.na(Uni100m) |
      Uni100m <= 0
  ) |>
  distinct(
    localidade,
    extent_m,
    Uni100m
  )

if (nrow(missing_extent) > 0) {
  print(missing_extent)
  
  stop(
    paste(
      "At least one balanced locality has",
      "missing or invalid shoreline extent."
    )
  )
}

################################################################################
# 7. CALCULATE DPUE AND RAI-W BY LOCALITY AND CYCLE
#
# Each row of balanced_cycle_data represents one monitoring minute.
################################################################################

balanced_cycle_metrics <- balanced_cycle_data |>
  group_by(
    localidade,
    region,
    monitoring_cycle,
    cycle_factor,
    cycle_index
  ) |>
  summarise(
    effort_minutes = n(),
    
    effort_hours =
      effort_minutes / 60,
    
    n_sampling_dates =
      n_distinct(data),
    
    n_positive = sum(
      dafor_num > 0,
      na.rm = TRUE
    ),
    
    sum_raiw_weights = sum(
      raiw_weight,
      na.rm = TRUE
    ),
    
    extent_m = first(
      extent_m
    ),
    
    Uni100m = first(
      Uni100m
    ),
    
    n_records_without_depth = sum(
      is.na(faixa_bat)
    ),
    
    .groups = "drop"
  ) |>
  mutate(
    denominator =
      effort_hours * Uni100m,
    
    dpue =
      n_positive / denominator,
    
    rai_w =
      sum_raiw_weights / denominator,
    
    positive_percentage =
      100 * n_positive / effort_minutes
  ) |>
  arrange(
    localidade,
    cycle_index
  )

################################################################################
# 8. VERIFY THE COMPLETE REPEATED-MEASURES DESIGN
################################################################################

balanced_design_check <- balanced_cycle_metrics |>
  count(
    localidade,
    name = "n_cycles"
  )

if (
  any(
    balanced_design_check$n_cycles !=
    length(expected_cycles)
  )
) {
  print(balanced_design_check)
  
  stop(
    paste(
      "The balanced dataset is incomplete:",
      "at least one locality does not contain all three cycles."
    )
  )
}

if (
  nrow(balanced_cycle_metrics) !=
  nrow(balanced_localities) *
  length(expected_cycles)
) {
  stop(
    "Unexpected number of locality-cycle combinations."
  )
}

print(
  balanced_cycle_metrics,
  n = Inf
)

################################################################################
# 9. DESCRIPTIVE SUMMARY BY MONITORING CYCLE
#
# These values describe only the seven balanced localities.
################################################################################

balanced_cycle_summary <- balanced_cycle_metrics |>
  group_by(
    monitoring_cycle,
    cycle_factor,
    cycle_index
  ) |>
  summarise(
    n_localities = n(),
    
    effort_minutes = sum(
      effort_minutes
    ),
    
    positive_records = sum(
      n_positive
    ),
    
    mean_dpue = mean(
      dpue,
      na.rm = TRUE
    ),
    
    median_dpue = median(
      dpue,
      na.rm = TRUE
    ),
    
    sd_dpue = sd(
      dpue,
      na.rm = TRUE
    ),
    
    mean_raiw = mean(
      rai_w,
      na.rm = TRUE
    ),
    
    median_raiw = median(
      rai_w,
      na.rm = TRUE
    ),
    
    sd_raiw = sd(
      rai_w,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  ) |>
  arrange(
    cycle_index
  )

print(balanced_cycle_summary)

################################################################################
# 10. FRIEDMAN TESTS
#
# Non-parametric repeated-measures tests:
# - monitoring cycle = treatment/time factor;
# - locality = repeated-measures block.
#
# These tests evaluate whether metric values differ among the three cycles,
# but do not specifically test a linear trend.
################################################################################

friedman_dpue <- friedman.test(
  dpue ~ cycle_factor | localidade,
  data = balanced_cycle_metrics
)

friedman_raiw <- friedman.test(
  rai_w ~ cycle_factor | localidade,
  data = balanced_cycle_metrics
)

friedman_results <- tibble(
  response = c(
    "DPUE",
    "RAI-W"
  ),
  
  statistic = c(
    unname(friedman_dpue$statistic),
    unname(friedman_raiw$statistic)
  ),
  
  degrees_freedom = c(
    unname(friedman_dpue$parameter),
    unname(friedman_raiw$parameter)
  ),
  
  p_value = c(
    friedman_dpue$p.value,
    friedman_raiw$p.value
  )
)

print(friedman_results)

################################################################################
# 11. ORDERED TEMPORAL-TREND MODELS
#
# Locality is included as a fixed blocking factor.
#
# The cycle coefficient represents the average change between consecutive
# monitoring cycles after controlling for persistent differences among
# localities.
################################################################################

model_dpue_cycle <- lm(
  log1p(dpue) ~
    cycle_index +
    factor(localidade),
  data = balanced_cycle_metrics
)

model_raiw_cycle <- lm(
  log1p(rai_w) ~
    cycle_index +
    factor(localidade),
  data = balanced_cycle_metrics
)

################################################################################
# 12. EFFORT-ADJUSTED SENSITIVITY MODELS
#
# DPUE and RAI-W are already effort-standardized. Therefore, effort is not
# included in the primary models above.
#
# These secondary models evaluate whether a residual relationship with effort
# remains after standardization.
################################################################################

model_dpue_cycle_effort <- lm(
  log1p(dpue) ~
    cycle_index +
    log1p(effort_minutes) +
    factor(localidade),
  data = balanced_cycle_metrics
)

model_raiw_cycle_effort <- lm(
  log1p(rai_w) ~
    cycle_index +
    log1p(effort_minutes) +
    factor(localidade),
  data = balanced_cycle_metrics
)

################################################################################
# 13. EXTRACT MODEL RESULTS
################################################################################

extract_cycle_model <- function(
    model_object,
    model_name
) {
  
  model_summary <- summary(
    model_object
  )
  
  broom::tidy(
    model_object,
    conf.int = TRUE
  ) |>
    mutate(
      model = model_name,
      n_observations = nobs(
        model_object
      ),
      r_squared =
        model_summary$r.squared,
      adjusted_r_squared =
        model_summary$adj.r.squared
    ) |>
    select(
      model,
      term,
      estimate,
      std.error,
      statistic,
      p.value,
      conf.low,
      conf.high,
      n_observations,
      r_squared,
      adjusted_r_squared
    )
}

balanced_cycle_models <- bind_rows(
  extract_cycle_model(
    model_dpue_cycle,
    "DPUE_cycle_locality"
  ),
  
  extract_cycle_model(
    model_raiw_cycle,
    "RAIW_cycle_locality"
  ),
  
  extract_cycle_model(
    model_dpue_cycle_effort,
    "DPUE_cycle_effort_locality"
  ),
  
  extract_cycle_model(
    model_raiw_cycle_effort,
    "RAIW_cycle_effort_locality"
  )
)

print(
  balanced_cycle_models |>
    filter(
      term %in% c(
        "cycle_index",
        "log1p(effort_minutes)"
      )
    ),
  n = Inf
)

################################################################################
# 14. EXTRACT ONLY THE TEMPORAL EFFECTS
################################################################################

balanced_cycle_temporal_effects <-
  balanced_cycle_models |>
  filter(
    term == "cycle_index"
  ) |>
  mutate(
    direction = case_when(
      estimate > 0 ~ "Positive",
      estimate < 0 ~ "Negative",
      TRUE ~ "No change"
    )
  )

print(
  balanced_cycle_temporal_effects,
  n = Inf
)

################################################################################
# 15. CHECK RESIDUAL RELATIONSHIPS WITH EFFORT
################################################################################

effort_dpue_balanced <- cor.test(
  balanced_cycle_metrics$effort_minutes,
  balanced_cycle_metrics$dpue,
  method = "spearman",
  exact = FALSE
)

effort_raiw_balanced <- cor.test(
  balanced_cycle_metrics$effort_minutes,
  balanced_cycle_metrics$rai_w,
  method = "spearman",
  exact = FALSE
)

balanced_effort_correlations <- tibble(
  response = c(
    "DPUE",
    "RAI-W"
  ),
  
  rho = c(
    unname(
      effort_dpue_balanced$estimate
    ),
    
    unname(
      effort_raiw_balanced$estimate
    )
  ),
  
  p_value = c(
    effort_dpue_balanced$p.value,
    effort_raiw_balanced$p.value
  )
)

print(balanced_effort_correlations)

################################################################################
# 16. SAVE OUTPUTS
################################################################################

write_csv(
  balanced_cycle_metrics,
  "outputs/balanced_cycle_metrics.csv"
)

write_csv(
  balanced_cycle_summary,
  "outputs/balanced_cycle_summary.csv"
)

write_csv(
  friedman_results,
  "outputs/balanced_cycle_friedman_tests.csv"
)

write_csv(
  balanced_cycle_models,
  "outputs/balanced_cycle_models.csv"
)

write_csv(
  balanced_cycle_temporal_effects,
  "outputs/balanced_cycle_temporal_effects.csv"
)

write_csv(
  balanced_effort_correlations,
  "outputs/balanced_cycle_effort_correlations.csv"
)

saveRDS(
  balanced_cycle_metrics,
  "outputs/balanced_cycle_metrics.rds"
)

saveRDS(
  model_dpue_cycle,
  "outputs/model_dpue_balanced_cycle.rds"
)

saveRDS(
  model_raiw_cycle,
  "outputs/model_raiw_balanced_cycle.rds"
)

saveRDS(
  model_dpue_cycle_effort,
  "outputs/model_dpue_balanced_cycle_effort.rds"
)

saveRDS(
  model_raiw_cycle_effort,
  "outputs/model_raiw_balanced_cycle_effort.rds"
)

################################################################################
# 17. FINAL SUMMARY
################################################################################

cat(
  "\n============================================================\n",
  "BALANCED MONITORING-CYCLE ANALYSIS\n",
  "============================================================\n",
  
  "Balanced localities: ",
  n_distinct(
    balanced_cycle_metrics$localidade
  ),
  "\n",
  
  "Monitoring cycles: ",
  n_distinct(
    balanced_cycle_metrics$monitoring_cycle
  ),
  "\n",
  
  "Locality-cycle observations: ",
  nrow(
    balanced_cycle_metrics
  ),
  "\n",
  
  "Minute-level records: ",
  sum(
    balanced_cycle_metrics$effort_minutes
  ),
  "\n",
  
  "DPUE Friedman p-value: ",
  signif(
    friedman_dpue$p.value,
    4
  ),
  "\n",
  
  "RAI-W Friedman p-value: ",
  signif(
    friedman_raiw$p.value,
    4
  ),
  "\n",
  
  "============================================================\n",
  
  sep = ""
)

print(
  balanced_cycle_summary,
  n = Inf
)

print(
  balanced_cycle_temporal_effects |>
    select(
      model,
      estimate,
      std.error,
      statistic,
      p.value,
      conf.low,
      conf.high,
      direction
    ),
  n = Inf
)

print(
  balanced_cycle_metrics |>
    select(
      localidade,
      monitoring_cycle,
      effort_minutes,
      n_positive,
      dpue,
      rai_w
    ),
  n = Inf
)



