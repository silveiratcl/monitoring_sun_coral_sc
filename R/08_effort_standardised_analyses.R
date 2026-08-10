################################################################################
# 08_effort_standardised_analyses.R
#
# Effort-standardised temporal, spatial, and bathymetric analyses
#
# This script:
# 1. Evaluates temporal patterns using comparable monitoring cycles
# 2. Evaluates spatial patterns using 2025 data
# 3. Evaluates bathymetric patterns using balanced locality-depth subsets
# 4. Uses repeated random subsampling to assess sensitivity to unequal effort
# 5. Performs locality-level exact permutation tests
# 6. Performs complementary locality-level occurrence analysis
################################################################################

source("R/00_setup.R")

library(tidyverse)

dir.create(
  "outputs",
  showWarnings = FALSE,
  recursive = TRUE
)

################################################################################
# 1. SETTINGS
################################################################################

set.seed(1234)

n_iterations <- 1000

expected_cycles <- c(
  "2022/2023",
  "2023/2024",
  "2024/2025"
)

################################################################################
# 2. LOAD DATA PREPARED IN SCRIPT 07
################################################################################

balanced_cycle_minutes <- readRDS(
  "outputs/balanced_cycle_minutes.rds"
)


# Additional datasets required by the spatial and bathymetric analyses
df_localidade <- readRDS(
  "outputs/df_localidade_clean.rds"
)

df_monitoring_all <- readRDS(
  "outputs/monitoring_data_all.rds"
)

df_monitoring_depth <- readRDS(
  "outputs/monitoring_data_depth_complete.rds"
)


################################################################################
# 3. STANDARDISE LOCALITY NAMES IN THE EXTENT TABLE
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
# 4. DEFINE RAI-W WEIGHTS
################################################################################

weights_manual <- manual_weights


# Validate DAFOR values before assigning weights
dafor_values <- clean_num(df_monitoring_all$dafor)

invalid_dafor <- sort(
  unique(
    dafor_values[
      !is.na(dafor_values) &
        !dafor_values %in% as.numeric(names(weights_manual))
    ]
  )
)

stopifnot(
  length(invalid_dafor) == 0,
  sum(is.na(dafor_values)) == 0
)

################################################################################
# 5. PREPARE MINUTE-LEVEL DATA
################################################################################

df_temporal <- balanced_cycle_minutes |>
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
    
    monitoring_cycle = factor(
      monitoring_cycle,
      levels = expected_cycles,
      ordered = TRUE
    )
  ) |>
  left_join(
    locality_extent,
    by = "localidade"
  )

################################################################################
# 6. CHECK THE TEMPORAL DESIGN
################################################################################

sampling_effort <- df_temporal |>
  count(
    localidade,
    region,
    monitoring_cycle,
    name = "effort_minutes"
  ) |>
  arrange(
    localidade,
    monitoring_cycle
  )

print(
  sampling_effort,
  n = Inf
)

################################################################################
# 7. VERIFY COMPLETE THREE-CYCLE COVERAGE
################################################################################

design_check <- sampling_effort |>
  count(
    localidade,
    name = "n_cycles"
  )

stopifnot(
  all(
    design_check$n_cycles ==
      length(expected_cycles)
  )
)

################################################################################
# 8. DEFINE THE RAREFACTION LEVEL
#
# Equal sampling effort is set to the smallest locality-cycle sample size.
################################################################################

rarefied_n <- min(
  sampling_effort$effort_minutes
)

cat(
  "\nMinimum sampling effort among locality-cycle combinations:",
  rarefied_n,
  "minutes\n"
)

stopifnot(
  rarefied_n == 30
)

################################################################################
# 9. CHECK LOCALITY EXTENT
################################################################################

missing_extent <- df_temporal |>
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
    "Missing or invalid shoreline extent for at least one locality."
  )
}

################################################################################
# 10. FUNCTION FOR ONE RAREFACTION ITERATION
################################################################################

run_rarefaction <- function(iteration_number) {
  
  rarefied_sample <- df_temporal |>
    group_by(
      localidade,
      region,
      monitoring_cycle
    ) |>
    slice_sample(
      n = rarefied_n,
      replace = FALSE
    ) |>
    ungroup()
  
  rarefied_metrics <- rarefied_sample |>
    group_by(
      localidade,
      region,
      monitoring_cycle
    ) |>
    summarise(
      effort_minutes = n(),
      
      effort_hours =
        effort_minutes / 60,
      
      n_positive = sum(
        dafor_num > 0,
        na.rm = TRUE
      ),
      
      detection_frequency =
        n_positive /
        effort_minutes,
      
      sum_raiw_weights = sum(
        raiw_weight,
        na.rm = TRUE
      ),
      
      mean_raiw_weight =
        mean(
          raiw_weight,
          na.rm = TRUE
        ),
      
      extent_m =
        first(extent_m),
      
      Uni100m =
        first(Uni100m),
      
      .groups = "drop"
    ) |>
    mutate(
      dpue =
        n_positive /
        (
          effort_hours *
            Uni100m
        ),
      
      rai_w =
        sum_raiw_weights /
        (
          effort_hours *
            Uni100m
        ),
      
      iteration =
        iteration_number
    )
  
  rarefied_metrics
}

################################################################################
# 11. RUN 1,000 RAREFACTION ITERATIONS
################################################################################

cat(
  "\nRunning",
  n_iterations,
  "rarefaction iterations...\n"
)

rarefaction_results <- map_dfr(
  seq_len(n_iterations),
  run_rarefaction
)

################################################################################
# 12. CHECK RAREFACTION OUTPUT
################################################################################

rarefaction_check <- rarefaction_results |>
  count(
    iteration,
    name = "n_locality_cycle_rows"
  )

stopifnot(
  all(
    rarefaction_check$n_locality_cycle_rows ==
      7 * 3
  )
)

cat(
  "\nTotal rarefied locality-cycle estimates:",
  nrow(rarefaction_results),
  "\n"
)

################################################################################
# 13. ESTIMATE STANDARDISED METRICS FOR EACH LOCALITY × CYCLE
#
# The median across the 1,000 rarefactions is used as the standardised
# estimate. Quantiles describe variation caused by random sub-sampling.
################################################################################

standardised_temporal_metrics <- rarefaction_results |>
  group_by(
    localidade,
    region,
    monitoring_cycle
  ) |>
  summarise(
    effort_minutes = rarefied_n,
    
    dpue_median =
      median(
        dpue,
        na.rm = TRUE
      ),
    
    dpue_lower =
      quantile(
        dpue,
        probs = 0.025,
        na.rm = TRUE,
        names = FALSE
      ),
    
    dpue_upper =
      quantile(
        dpue,
        probs = 0.975,
        na.rm = TRUE,
        names = FALSE
      ),
    
    rai_w_median =
      median(
        rai_w,
        na.rm = TRUE
      ),
    
    raiw_lower =
      quantile(
        rai_w,
        probs = 0.025,
        na.rm = TRUE,
        names = FALSE
      ),
    
    raiw_upper =
      quantile(
        rai_w,
        probs = 0.975,
        na.rm = TRUE,
        names = FALSE
      ),
    
    detection_frequency =
      median(
        detection_frequency,
        na.rm = TRUE
      ),
    
    mean_raiw_weight =
      median(
        mean_raiw_weight,
        na.rm = TRUE
      ),
    
    .groups = "drop"
  ) |>
  rename(
    dpue = dpue_median,
    rai_w = rai_w_median
  ) |>
  arrange(
    localidade,
    monitoring_cycle
  )

print(
  standardised_temporal_metrics,
  n = Inf
)

################################################################################
# 14. SUMMARY BY MONITORING CYCLE
#
# These summaries are based on the seven repeatedly monitored localities.
################################################################################

standardised_cycle_summary <- standardised_temporal_metrics |>
  group_by(
    monitoring_cycle
  ) |>
  summarise(
    n_localities = n(),
    
    mean_dpue =
      mean(
        dpue,
        na.rm = TRUE
      ),
    
    median_dpue =
      median(
        dpue,
        na.rm = TRUE
      ),
    
    mean_raiw =
      mean(
        rai_w,
        na.rm = TRUE
      ),
    
    median_raiw =
      median(
        rai_w,
        na.rm = TRUE
      ),
    
    mean_detection_frequency =
      mean(
        detection_frequency,
        na.rm = TRUE
      ),
    
    mean_raiw_weight =
      mean(
        mean_raiw_weight,
        na.rm = TRUE
      ),
    
    .groups = "drop"
  )

print(
  standardised_cycle_summary,
  n = Inf
)

################################################################################
# 15. CYCLE-LEVEL DISTRIBUTION ACROSS ALL RAREFACTION ITERATIONS
#
# For every iteration, calculate the average among the seven localities.
################################################################################

iteration_cycle_summary <- rarefaction_results |>
  group_by(
    iteration,
    monitoring_cycle
  ) |>
  summarise(
    mean_dpue =
      mean(
        dpue,
        na.rm = TRUE
      ),
    
    mean_raiw =
      mean(
        rai_w,
        na.rm = TRUE
      ),
    
    mean_detection_frequency =
      mean(
        detection_frequency,
        na.rm = TRUE
      ),
    
    .groups = "drop"
  )

################################################################################
# 16. RAREFACTION UNCERTAINTY BY CYCLE
################################################################################

rarefaction_cycle_distribution <- iteration_cycle_summary |>
  group_by(
    monitoring_cycle
  ) |>
  summarise(
    dpue_median =
      median(
        mean_dpue
      ),
    
    dpue_lower =
      quantile(
        mean_dpue,
        0.025
      ),
    
    dpue_upper =
      quantile(
        mean_dpue,
        0.975
      ),
    
    raiw_median =
      median(
        mean_raiw
      ),
    
    raiw_lower =
      quantile(
        mean_raiw,
        0.025
      ),
    
    raiw_upper =
      quantile(
        mean_raiw,
        0.975
      ),
    
    detection_frequency_median =
      median(
        mean_detection_frequency
      ),
    
    detection_frequency_lower =
      quantile(
        mean_detection_frequency,
        0.025
      ),
    
    detection_frequency_upper =
      quantile(
        mean_detection_frequency,
        0.975
      ),
    
    .groups = "drop"
  )

print(
  rarefaction_cycle_distribution,
  n = Inf
)


################################################################################
# 17. SAVE TEMPORAL OUTPUTS
################################################################################

write_csv(
  sampling_effort,
  "outputs/temporal_sampling_effort.csv"
)

write_csv(
  standardised_temporal_metrics,
  "outputs/temporal_rarefied_metrics.csv"
)

write_csv(
  standardised_cycle_summary,
  "outputs/temporal_cycle_summary.csv"
)

write_csv(
  rarefaction_cycle_distribution,
  "outputs/temporal_rarefaction_distribution.csv"
)

saveRDS(
  rarefaction_results,
  "outputs/temporal_rarefaction_iterations.rds"
)

saveRDS(
  standardised_temporal_metrics,
  "outputs/temporal_rarefied_metrics.rds"
)

################################################################################
# 18. TEMPORAL EFFORT-STANDARDISATION SUMMARY
################################################################################

cat(
  "\n============================================================\n",
  "TEMPORAL EFFORT-STANDARDISATION SUMMARY\n",
  "============================================================\n",
  "Localities retained: ", n_distinct(df_temporal$localidade), "\n",
  "Monitoring cycles: ", n_distinct(df_temporal$monitoring_cycle), "\n",
  "Original minute-level records: ", nrow(df_temporal), "\n",
  "Rarefied effort per locality-cycle: ", rarefied_n, " minutes\n",
  "Minutes per rarefaction iteration: ",
  rarefied_n * n_distinct(df_temporal$localidade) * length(expected_cycles),
  "\n",
  "Number of rarefaction iterations: ", n_iterations, "\n",
  "============================================================\n",
  sep = ""
)


################################################################################
# 19. 2025 SPATIAL SAMPLING DIAGNOSTIC
#
# Purpose:
# Inspect spatial sampling effort in 2025 before defining the rarefaction
# procedure used to evaluate whether spatial patterns persist after
# standardizing effort among localities.
################################################################################

df_2025 <- df_monitoring_all |>
  filter(year == 2025)

spatial_2025_sampling <- df_2025 |>
  group_by(
    localidade,
    region
  ) |>
  summarise(
    effort_minutes = n(),
    
    effort_hours =
      effort_minutes / 60,
    
    n_positive = sum(
      dafor > 0,
      na.rm = TRUE
    ),
    
    positive_percentage =
      100 * n_positive / effort_minutes,
    
    n_sampling_dates =
      n_distinct(data),
    
    .groups = "drop"
  ) |>
  arrange(
    region,
    desc(n_positive),
    localidade
  )

region_2025_sampling <- spatial_2025_sampling |>
  group_by(region) |>
  summarise(
    n_localities = n(),
    
    effort_minutes =
      sum(effort_minutes),
    
    effort_hours =
      effort_minutes / 60,
    
    n_positive_localities =
      sum(n_positive > 0),
    
    total_positive =
      sum(n_positive),
    
    .groups = "drop"
  ) |>
  arrange(region)

print(
  region_2025_sampling,
  n = Inf
)

print(
  region_2025_sampling,
  n = Inf
)

cat(
  "\n============================================================\n",
  "2025 SPATIAL SAMPLING DIAGNOSTIC\n",
  "============================================================\n",
  
  "2025 records: ",
  nrow(df_2025),
  "\n",
  
  "2025 localities: ",
  n_distinct(df_2025$localidade),
  "\n",
  
  "Minimum effort per locality: ",
  min(
    spatial_2025_sampling$effort_minutes
  ),
  " minutes\n",
  
  "Maximum effort per locality: ",
  max(
    spatial_2025_sampling$effort_minutes
  ),
  " minutes\n",
  
  "Positive records: ",
  sum(
    df_2025$dafor > 0,
    na.rm = TRUE
  ),
  "\n",
  
  "Positive localities: ",
  sum(
    spatial_2025_sampling$n_positive > 0
  ),
  "\n",
  
  "============================================================\n",
  
  sep = ""
)

################################################################################
# 20. SAVE 2025 SPATIAL DIAGNOSTIC
################################################################################

write_csv(
  spatial_2025_sampling,
  "outputs/spatial_2025_sampling.csv"
)

write_csv(
  region_2025_sampling,
  "outputs/region_2025_sampling.csv"
)


print(spatial_2025_sampling, n = Inf)
print(region_2025_sampling, n = Inf)

################################################################################
# 21. 2025 SPATIAL RAREFACTION
#
# Purpose:
# Evaluate whether the spatial concentration of T. coccinea detections
# persists when sampling effort is standardized among localities.
#
# All localities sampled in 2025 are rarefied to the minimum locality-level
# sampling effort (34 one-minute records).
#
# Rarefaction metrics:
# - detection frequency = proportion of positive one-minute records
# - mean RAI-W weight = mean relative-abundance weight per minute
#
# Locality is retained as the spatial analytical unit.
################################################################################

set.seed(1234)

n_spatial_iterations <- 1000

################################################################################
# 22. PREPARE 2025 DATA FOR RAREFACTION
################################################################################

df_2025_rarefaction <- df_2025 |>
  mutate(
    dafor_num = as.numeric(dafor),
    
    raiw_weight = coalesce(
      unname(
        weights_manual[
          as.character(dafor_num)
        ]
      ),
      0
    )
  )

################################################################################
# 23. DEFINE COMMON SAMPLING EFFORT
################################################################################

spatial_rarefied_n <- df_2025_rarefaction |>
  count(
    localidade,
    name = "effort_minutes"
  ) |>
  summarise(
    minimum_effort =
      min(effort_minutes)
  ) |>
  pull(
    minimum_effort
  )

cat(
  "\nSpatial rarefaction level:",
  spatial_rarefied_n,
  "minutes per locality\n"
)

stopifnot(
  spatial_rarefied_n == 34
)

################################################################################
# 24. FUNCTION FOR ONE SPATIAL RAREFACTION
################################################################################

run_spatial_rarefaction <- function(iteration_number) {
  
  df_2025_rarefaction |>
    group_by(
      localidade,
      region
    ) |>
    slice_sample(
      n = spatial_rarefied_n,
      replace = FALSE
    ) |>
    summarise(
      effort_minutes = n(),
      
      n_positive = sum(
        dafor_num > 0,
        na.rm = TRUE
      ),
      
      detection_frequency =
        n_positive /
        effort_minutes,
      
      mean_raiw_weight =
        mean(
          raiw_weight,
          na.rm = TRUE
        ),
      
      positive_locality =
        as.integer(
          n_positive > 0
        ),
      
      .groups = "drop"
    ) |>
    mutate(
      iteration =
        iteration_number
    )
}

################################################################################
# 25. RUN SPATIAL RAREFACTION
################################################################################

cat(
  "\nRunning",
  n_spatial_iterations,
  "spatial rarefaction iterations...\n"
)

spatial_rarefaction_results <- map_dfr(
  seq_len(
    n_spatial_iterations
  ),
  run_spatial_rarefaction
)

################################################################################
# 26. CHECK SPATIAL RAREFACTION OUTPUT
################################################################################

spatial_rarefaction_check <-
  spatial_rarefaction_results |>
  count(
    iteration,
    name = "n_localities"
  )

stopifnot(
  all(
    spatial_rarefaction_check$n_localities ==
      n_distinct(df_2025_rarefaction$localidade)
  )
)

################################################################################
# 27. STANDARDISED ESTIMATE FOR EACH LOCALITY
#
# Median across the 1,000 rarefaction iterations.
################################################################################

spatial_rarefied_localities <-
  spatial_rarefaction_results |>
  group_by(
    localidade,
    region
  ) |>
  summarise(
    detection_frequency_median =
      median(
        detection_frequency,
        na.rm = TRUE
      ),
    
    detection_frequency_lower =
      quantile(
        detection_frequency,
        probs = 0.025,
        na.rm = TRUE,
        names = FALSE
      ),
    
    detection_frequency_upper =
      quantile(
        detection_frequency,
        probs = 0.975,
        na.rm = TRUE,
        names = FALSE
      ),
    
    mean_raiw_weight_median =
      median(
        mean_raiw_weight,
        na.rm = TRUE
      ),
    
    raiw_weight_lower =
      quantile(
        mean_raiw_weight,
        probs = 0.025,
        na.rm = TRUE,
        names = FALSE
      ),
    
    raiw_weight_upper =
      quantile(
        mean_raiw_weight,
        probs = 0.975,
        na.rm = TRUE,
        names = FALSE
      ),
    
    detection_probability =
      mean(
        positive_locality
      ),
    
    .groups = "drop"
  ) |>
  rename(
    detection_frequency =
      detection_frequency_median,
    
    mean_raiw_weight =
      mean_raiw_weight_median
  ) |>
  arrange(
    region,
    desc(detection_frequency)
  )

print(
  spatial_rarefied_localities,
  n = Inf
)

################################################################################
# 28. STANDARDISED REGION SUMMARY
#
# Each locality contributes equally to its region.
################################################################################

spatial_rarefied_region_summary <-
  spatial_rarefaction_results |>
  group_by(
    iteration,
    region
  ) |>
  summarise(
    mean_detection_frequency =
      mean(
        detection_frequency,
        na.rm = TRUE
      ),
    
    mean_raiw_weight =
      mean(
        mean_raiw_weight,
        na.rm = TRUE
      ),
    
    proportion_positive_localities =
      mean(
        positive_locality
      ),
    
    .groups = "drop"
  ) |>
  group_by(region) |>
  summarise(
    detection_frequency_median =
      median(
        mean_detection_frequency
      ),
    
    detection_frequency_lower =
      quantile(
        mean_detection_frequency,
        0.025
      ),
    
    detection_frequency_upper =
      quantile(
        mean_detection_frequency,
        0.975
      ),
    
    raiw_weight_median =
      median(
        mean_raiw_weight
      ),
    
    raiw_weight_lower =
      quantile(
        mean_raiw_weight,
        0.025
      ),
    
    raiw_weight_upper =
      quantile(
        mean_raiw_weight,
        0.975
      ),
    
    positive_localities_median =
      median(
        proportion_positive_localities
      ),
    
    positive_localities_lower =
      quantile(
        proportion_positive_localities,
        0.025
      ),
    
    positive_localities_upper =
      quantile(
        proportion_positive_localities,
        0.975
      ),
    
    .groups = "drop"
  )

print(
  spatial_rarefied_region_summary,
  n = Inf
)


################################################################################
# 29. PRESENCE/ABSENCE TEST USING 2025 LOCALITIES
#
# This analysis uses locality, rather than one-minute observations,
# as the replicate.
################################################################################

spatial_presence_table <- spatial_2025_sampling |>
  mutate(
    presence =
      if_else(
        n_positive > 0,
        "Present",
        "Absent"
      )
  ) |>
  count(
    region,
    presence
  ) |>
  pivot_wider(
    names_from = presence,
    values_from = n,
    values_fill = 0
  )

print(
  spatial_presence_table
)

spatial_presence_matrix <-
  spatial_presence_table |>
  column_to_rownames(
    "region"
  ) |>
  as.matrix()

fisher_spatial_presence <-
  fisher.test(
    spatial_presence_matrix
  )

spatial_fisher_result <- tibble(
  analysis =
    "Locality presence by region - 2025",
  
  p_value =
    fisher_spatial_presence$p.value
)

print(
  spatial_fisher_result
)

################################################################################
# 30. SAVE SPATIAL RESULTS
################################################################################

write_csv(
  spatial_rarefied_localities,
  "outputs/spatial_2025_rarefied_localities.csv"
)

write_csv(
  spatial_rarefied_region_summary,
  "outputs/spatial_2025_rarefied_regions.csv"
)

write_csv(
  spatial_presence_table,
  "outputs/spatial_2025_presence_table.csv"
)

write_csv(
  spatial_fisher_result,
  "outputs/spatial_2025_fisher.csv"
)

saveRDS(
  spatial_rarefaction_results,
  "outputs/spatial_2025_rarefaction_iterations.rds"
)

################################################################################
# 31. SPATIAL EFFORT-STANDARDISATION SUMMARY
################################################################################

cat(
  "\n============================================================\n",
  "2025 SPATIAL EFFORT-STANDARDISATION SUMMARY\n",
  "============================================================\n",
  "2025 localities: ", n_distinct(df_2025_rarefaction$localidade), "\n",
  "2025 monitoring minutes: ", nrow(df_2025_rarefaction), "\n",
  "2025 positive records: ",
  sum(df_2025_rarefaction$dafor_num > 0, na.rm = TRUE), "\n",
  "Rarefied effort per locality: ", spatial_rarefied_n, " minutes\n",
  "Rarefaction iterations: ", n_spatial_iterations, "\n",
  "Fisher locality-presence p-value: ",
  signif(fisher_spatial_presence$p.value, 4), "\n",
  "============================================================\n",
  sep = ""
)

print(
  spatial_rarefied_region_summary,
  n = Inf
)

print(
  spatial_fisher_result
)
################################################################################
# 32. 2025 BATHYMETRIC SAMPLING DIAGNOSTIC
#
# Purpose:
# Reassess the bathymetric pattern using 2025 data only, following exactly
# the same depth classification used in Figure 4 of the manuscript.
#
# Depth classes are derived from maximum survey depth (prof_max):
# - 0-2 m
# - 2.1-8 m
# - 8.1-14 m
# - 14.1 m+
################################################################################

depth_levels <- c(
  "0-2m",
  "2.1-8m",
  "8.1-14m",
  "14.1m+"
)

################################################################################
# 33. PREPARE 2025 DEPTH DATA
#
# Use the depth-complete analytical dataset and reproduce the classification
# used in Figure 4.
################################################################################

df_2025_depth <- df_monitoring_depth |>
  filter(
    year == 2025
  ) |>
  mutate(
    dafor_num = clean_num(dafor),
    
    prof_max_num = clean_num(prof_max),
    
    faixa_bat_depth = case_when(
      !is.na(prof_max_num) &
        prof_max_num <= 2 ~
        "0-2m",
      
      !is.na(prof_max_num) &
        prof_max_num > 2 &
        prof_max_num <= 8 ~
        "2.1-8m",
      
      !is.na(prof_max_num) &
        prof_max_num > 8 &
        prof_max_num <= 14 ~
        "8.1-14m",
      
      !is.na(prof_max_num) &
        prof_max_num > 14 ~
        "14.1m+",
      
      TRUE ~
        NA_character_
    ),
    
    faixa_bat_depth = factor(
      faixa_bat_depth,
      levels = depth_levels
    )
  ) |>
  filter(
    !is.na(faixa_bat_depth)
  )

################################################################################
# 34. CHECK DEPTH CLASSIFICATION
################################################################################

depth_classification_check <- df_2025_depth |>
  group_by(
    faixa_bat_depth
  ) |>
  summarise(
    minimum_prof_max =
      min(
        prof_max_num,
        na.rm = TRUE
      ),
    
    maximum_prof_max =
      max(
        prof_max_num,
        na.rm = TRUE
      ),
    
    n_records = n(),
    
    .groups = "drop"
  )

print(
  depth_classification_check,
  n = Inf
)

################################################################################
# 35. SAMPLING EFFORT AND DETECTIONS BY DEPTH CLASS
################################################################################

depth_2025_sampling <- df_2025_depth |>
  group_by(
    faixa_bat_depth
  ) |>
  summarise(
    effort_minutes = n(),
    
    effort_hours =
      effort_minutes / 60,
    
    n_positive = sum(
      dafor_num > 0,
      na.rm = TRUE
    ),
    
    detection_frequency =
      n_positive /
      effort_minutes,
    
    positive_percentage =
      100 *
      detection_frequency,
    
    n_localities =
      n_distinct(
        localidade
      ),
    
    n_positive_localities =
      n_distinct(
        localidade[
          dafor_num > 0
        ]
      ),
    
    n_sampling_dates =
      n_distinct(
        data
      ),
    
    .groups = "drop"
  ) |>
  complete(
    faixa_bat_depth =
      factor(
        depth_levels,
        levels = depth_levels
      ),
    
    fill = list(
      effort_minutes = 0,
      effort_hours = 0,
      n_positive = 0,
      detection_frequency = 0,
      positive_percentage = 0,
      n_localities = 0,
      n_positive_localities = 0,
      n_sampling_dates = 0
    )
  ) |>
  arrange(
    faixa_bat_depth
  )

print(
  depth_2025_sampling,
  n = Inf
)

################################################################################
# 36. LOCALITY × DEPTH CLASS SUMMARY
#
# This table is important because it shows how many localities contribute
# to each bathymetric class and whether detections within a depth class
# are concentrated in particular localities.
################################################################################

locality_depth_2025 <- df_2025_depth |>
  group_by(
    localidade,
    region,
    faixa_bat_depth
  ) |>
  summarise(
    effort_minutes = n(),
    
    effort_hours =
      effort_minutes / 60,
    
    n_positive = sum(
      dafor_num > 0,
      na.rm = TRUE
    ),
    
    detection_frequency =
      n_positive /
      effort_minutes,
    
    positive =
      n_positive > 0,
    
    .groups = "drop"
  ) |>
  arrange(
    faixa_bat_depth,
    desc(n_positive),
    localidade
  )

print(
  locality_depth_2025,
  n = Inf
)

################################################################################
# 37. DISTRIBUTION OF POSITIVE RECORDS AMONG DEPTH CLASSES
################################################################################

depth_positive_distribution <- depth_2025_sampling |>
  mutate(
    proportion_sampling_effort =
      effort_minutes /
      sum(effort_minutes),
    
    percentage_sampling_effort =
      100 *
      proportion_sampling_effort,
    
    proportion_of_all_detections =
      n_positive /
      sum(n_positive),
    
    percentage_of_all_detections =
      100 *
      proportion_of_all_detections
  )

print(
  depth_positive_distribution,
  n = Inf
)

################################################################################
# 38. CHECK AGAINST FIGURE 4 LOGIC
#
# The number of positive records in 2025 should correspond to the same
# bathymetric classification used to generate Figure 4.
################################################################################

depth_2025_check <- depth_positive_distribution |>
  summarise(
    total_minutes =
      sum(effort_minutes),
    
    total_positive =
      sum(n_positive)
  )

print(
  depth_2025_check
)

################################################################################
# 39. BATHYMETRIC SAMPLING SUMMARY
################################################################################

cat(
  "\n============================================================\n",
  "2025 BATHYMETRIC SAMPLING DIAGNOSTIC\n",
  "============================================================\n",
  
  "2025 depth-classified records: ",
  nrow(df_2025_depth),
  "\n",
  
  "Bathymetric classes: ",
  n_distinct(
    df_2025_depth$faixa_bat_depth
  ),
  "\n",
  
  "Positive records: ",
  sum(
    df_2025_depth$dafor_num > 0,
    na.rm = TRUE
  ),
  "\n",
  
  "Minimum effort among depth classes: ",
  min(
    depth_2025_sampling$effort_minutes[
      depth_2025_sampling$effort_minutes > 0
    ]
  ),
  " minutes\n",
  
  "Maximum effort among depth classes: ",
  max(
    depth_2025_sampling$effort_minutes
  ),
  " minutes\n",
  
  "============================================================\n",
  
  sep = ""
)

################################################################################
# 40. SAVE BATHYMETRIC DIAGNOSTIC
################################################################################

write_csv(
  depth_classification_check,
  "outputs/depth_2025_classification_check.csv"
)

write_csv(
  depth_2025_sampling,
  "outputs/depth_2025_sampling.csv"
)

write_csv(
  locality_depth_2025,
  "outputs/locality_depth_2025.csv"
)

write_csv(
  depth_positive_distribution,
  "outputs/depth_2025_positive_distribution.csv"
)


print(
  depth_classification_check,
  n = Inf
)

print(
  depth_2025_sampling,
  n = Inf
)

print(
  depth_positive_distribution,
  n = Inf
)

print(
  locality_depth_2025,
  n = Inf
)

################################################################################
# 41. BALANCED 2025 BATHYMETRIC ANALYSIS
#
# Purpose:
# Evaluate whether the bathymetric pattern observed in 2025 persists after
# controlling for both locality identity and sampling effort.
#
# The analysis is restricted to localities sampled in all three main depth
# strata:
# - 0-2 m
# - 2.1-8 m
# - 8.1-14 m
#
# The >14 m stratum is retained descriptively but excluded from the balanced
# inferential analysis because only one locality was sampled in this depth
# class during 2025.
################################################################################

set.seed(1234)

n_depth_iterations <- 1000

main_depth_levels <- c(
  "0-2m",
  "2.1-8m",
  "8.1-14m"
)

################################################################################
# 42. IDENTIFY LOCALITIES REPRESENTED IN ALL THREE MAIN DEPTH STRATA
################################################################################

depth_locality_coverage <- df_2025_depth |>
  filter(
    faixa_bat_depth %in%
      main_depth_levels
  ) |>
  distinct(
    localidade,
    region,
    faixa_bat_depth
  ) |>
  group_by(
    localidade,
    region
  ) |>
  summarise(
    n_depth_strata =
      n_distinct(
        faixa_bat_depth
      ),
    
    .groups = "drop"
  )

balanced_depth_localities <- depth_locality_coverage |>
  filter(
    n_depth_strata ==
      length(main_depth_levels)
  )

print(
  balanced_depth_localities,
  n = Inf
)

################################################################################
# 43. PREPARE BALANCED DEPTH DATASET
################################################################################

balanced_depth_names <-
  balanced_depth_localities |>
  pull(localidade)

df_depth_balanced <- df_2025_depth |>
  filter(
    localidade %in%
      balanced_depth_names,
    faixa_bat_depth %in%
      main_depth_levels
  ) |>
  mutate(
    faixa_bat_depth = factor(
      faixa_bat_depth,
      levels = main_depth_levels,
      ordered = TRUE
    ),
    
    raiw_weight = coalesce(
      unname(
        weights_manual[
          as.character(dafor_num)
        ]
      ),
      0
    )
  )

################################################################################
# 44. CHECK EFFORT BY LOCALITY × DEPTH
################################################################################

balanced_depth_effort <- df_depth_balanced |>
  count(
    localidade,
    region,
    faixa_bat_depth,
    name = "effort_minutes"
  ) |>
  arrange(
    localidade,
    faixa_bat_depth
  )

print(
  balanced_depth_effort,
  n = Inf
)

################################################################################
# 45. DEFINE COMMON RAREFACTION LEVEL
################################################################################

depth_rarefied_n <- min(
  balanced_depth_effort$effort_minutes
)

cat(
  "\nBalanced depth rarefaction level:",
  depth_rarefied_n,
  "minutes per locality-depth combination\n"
)

stopifnot(
  depth_rarefied_n == 30
)

################################################################################
# 46. VERIFY COMPLETE BALANCED DESIGN
################################################################################

balanced_depth_check <- balanced_depth_effort |>
  count(
    localidade,
    name = "n_depth_strata"
  )

stopifnot(
  all(
    balanced_depth_check$n_depth_strata ==
      length(main_depth_levels)
  )
)

################################################################################
# 47. FUNCTION FOR ONE DEPTH RAREFACTION
################################################################################

run_depth_rarefaction <- function(iteration_number) {
  
  df_depth_balanced |>
    group_by(
      localidade,
      region,
      faixa_bat_depth
    ) |>
    slice_sample(
      n = depth_rarefied_n,
      replace = FALSE
    ) |>
    summarise(
      effort_minutes = n(),
      
      n_positive = sum(
        dafor_num > 0,
        na.rm = TRUE
      ),
      
      detection_frequency =
        n_positive /
        effort_minutes,
      
      mean_raiw_weight =
        mean(
          raiw_weight,
          na.rm = TRUE
        ),
      
      .groups = "drop"
    ) |>
    mutate(
      iteration =
        iteration_number
    )
}

################################################################################
# 48. RUN 1,000 DEPTH RAREFACTIONS
################################################################################

cat(
  "\nRunning",
  n_depth_iterations,
  "depth rarefaction iterations...\n"
)

depth_rarefaction_results <- map_dfr(
  seq_len(
    n_depth_iterations
  ),
  run_depth_rarefaction
)

################################################################################
# 49. CHECK DEPTH RAREFACTION OUTPUT
################################################################################

depth_rarefaction_check <- depth_rarefaction_results |>
  count(
    iteration,
    name = "n_locality_depth_rows"
  )

expected_depth_rows <-
  n_distinct(
    df_depth_balanced$localidade
  ) *
  length(main_depth_levels)

stopifnot(
  all(
    depth_rarefaction_check$
      n_locality_depth_rows ==
      expected_depth_rows
  )
)

################################################################################
# 50. STANDARDISED LOCALITY × DEPTH ESTIMATES
#
# Median values across the 1,000 rarefaction iterations are used as
# descriptive effort-standardised estimates.
################################################################################

standardised_depth_metrics <- depth_rarefaction_results |>
  group_by(
    localidade,
    region,
    faixa_bat_depth
  ) |>
  summarise(
    effort_minutes =
      depth_rarefied_n,
    
    detection_frequency_median =
      median(
        detection_frequency,
        na.rm = TRUE
      ),
    
    detection_frequency_lower =
      quantile(
        detection_frequency,
        probs = 0.025,
        na.rm = TRUE,
        names = FALSE
      ),
    
    detection_frequency_upper =
      quantile(
        detection_frequency,
        probs = 0.975,
        na.rm = TRUE,
        names = FALSE
      ),
    
    mean_raiw_weight_median =
      median(
        mean_raiw_weight,
        na.rm = TRUE
      ),
    
    raiw_weight_lower =
      quantile(
        mean_raiw_weight,
        probs = 0.025,
        na.rm = TRUE,
        names = FALSE
      ),
    
    raiw_weight_upper =
      quantile(
        mean_raiw_weight,
        probs = 0.975,
        na.rm = TRUE,
        names = FALSE
      ),
    
    .groups = "drop"
  ) |>
  rename(
    detection_frequency =
      detection_frequency_median,
    
    mean_raiw_weight =
      mean_raiw_weight_median
  ) |>
  arrange(
    localidade,
    faixa_bat_depth
  )


print(
  standardised_depth_metrics,
  n = Inf
)

################################################################################
# 51. STANDARDISED SUMMARY BY DEPTH
################################################################################

standardised_depth_summary <- standardised_depth_metrics |>
  group_by(
    faixa_bat_depth
  ) |>
  summarise(
    n_localities = n(),
    
    mean_detection_frequency =
      mean(
        detection_frequency,
        na.rm = TRUE
      ),
    
    median_detection_frequency =
      median(
        detection_frequency,
        na.rm = TRUE
      ),
    
    mean_raiw_weight =
      mean(
        mean_raiw_weight,
        na.rm = TRUE
      ),
    
    median_raiw_weight =
      median(
        mean_raiw_weight,
        na.rm = TRUE
      ),
    
    .groups = "drop"
  )

print(
  standardised_depth_summary,
  n = Inf
)


################################################################################
# 52. DEPTH DISTRIBUTION ACROSS RAREFACTION ITERATIONS
#
# For each iteration, average the standardized response across the five
# repeatedly sampled localities.
################################################################################

iteration_depth_summary <- depth_rarefaction_results |>
  group_by(
    iteration,
    faixa_bat_depth
  ) |>
  summarise(
    mean_detection_frequency =
      mean(
        detection_frequency,
        na.rm = TRUE
      ),
    
    mean_raiw_weight =
      mean(
        mean_raiw_weight,
        na.rm = TRUE
      ),
    
    .groups = "drop"
  )

depth_rarefaction_distribution <- iteration_depth_summary |>
  group_by(
    faixa_bat_depth
  ) |>
  summarise(
    detection_frequency_median =
      median(
        mean_detection_frequency
      ),
    
    detection_frequency_lower =
      quantile(
        mean_detection_frequency,
        0.025
      ),
    
    detection_frequency_upper =
      quantile(
        mean_detection_frequency,
        0.975
      ),
    
    raiw_weight_median =
      median(
        mean_raiw_weight
      ),
    
    raiw_weight_lower =
      quantile(
        mean_raiw_weight,
        0.025
      ),
    
    raiw_weight_upper =
      quantile(
        mean_raiw_weight,
        0.975
      ),
    
    .groups = "drop"
  )

print(
  depth_rarefaction_distribution,
  n = Inf
)

################################################################################
# 53. DESCRIPTIVE SUMMARY OF THE >14 M STRATUM
#
# The deepest stratum is not included in the inferential balanced analysis
# because only one locality contributed observations in 2025.
################################################################################

deepest_stratum_2025 <- depth_2025_sampling |>
  filter(
    faixa_bat_depth == "14.1m+"
  )

print(
  deepest_stratum_2025,
  n = Inf
)

################################################################################
# 54. SAVE BATHYMETRIC RESULTS
################################################################################

write_csv(
  balanced_depth_localities,
  "outputs/depth_2025_balanced_localities.csv"
)

write_csv(
  balanced_depth_effort,
  "outputs/depth_2025_balanced_effort.csv"
)

write_csv(
  standardised_depth_metrics,
  "outputs/depth_2025_rarefied_metrics.csv"
)

write_csv(
  standardised_depth_summary,
  "outputs/depth_2025_standardised_summary.csv"
)

write_csv(
  depth_rarefaction_distribution,
  "outputs/depth_2025_rarefaction_distribution.csv"
)

saveRDS(
  depth_rarefaction_results,
  "outputs/depth_2025_rarefaction_iterations.rds"
)

################################################################################
# 55. BATHYMETRIC EFFORT-STANDARDISATION SUMMARY
################################################################################

cat(
  "\n============================================================\n",
  "2025 BATHYMETRIC EFFORT-STANDARDISATION SUMMARY\n",
  "============================================================\n",
  "2025 total depth-classified records: ", nrow(df_2025_depth), "\n",
  "Localities represented in all three main depth strata: ",
  n_distinct(df_depth_balanced$localidade), "\n",
  "Depth strata in balanced analysis: ",
  paste(main_depth_levels, collapse = ", "), "\n",
  "Rarefied effort per locality-depth combination: ",
  depth_rarefied_n, " minutes\n",
  "Number of rarefaction iterations: ", n_depth_iterations, "\n",
  ">14 m observations retained descriptively only: ",
  deepest_stratum_2025$effort_minutes, " minutes / ",
  deepest_stratum_2025$n_positive, " positive record(s)\n",
  "============================================================\n",
  sep = ""
)

print(
  balanced_depth_localities,
  n = Inf
)

print(
  standardised_depth_summary,
  n = Inf
)

print(
  depth_rarefaction_distribution,
  n = Inf
)

################################################################################
# 56. TEMPORAL DESCRIPTIVE RESULTS
################################################################################

temporal_descriptive_sensitivity <- rarefaction_cycle_distribution |>
  transmute(
    analysis = "Temporal",
    category = as.character(monitoring_cycle),
    
    detection_metric =
      dpue_median,
    
    detection_lower =
      dpue_lower,
    
    detection_upper =
      dpue_upper,
    
    abundance_metric =
      raiw_median,
    
    abundance_lower =
      raiw_lower,
    
    abundance_upper =
      raiw_upper
  )

print(
  temporal_descriptive_sensitivity,
  n = Inf
)

################################################################################
# 57. SPATIAL DESCRIPTIVE RESULTS
################################################################################

spatial_descriptive_sensitivity <- spatial_rarefied_region_summary |>
  transmute(
    analysis = "Spatial",
    category = region,
    
    detection_metric =
      detection_frequency_median,
    
    detection_lower =
      detection_frequency_lower,
    
    detection_upper =
      detection_frequency_upper,
    
    abundance_metric =
      raiw_weight_median,
    
    abundance_lower =
      raiw_weight_lower,
    
    abundance_upper =
      raiw_weight_upper
  )

print(
  spatial_descriptive_sensitivity,
  n = Inf
)

################################################################################
# 58. BATHYMETRIC DESCRIPTIVE RESULTS
################################################################################

depth_descriptive_sensitivity <- depth_rarefaction_distribution |>
  transmute(
    analysis = "Bathymetric",
    category =
      as.character(faixa_bat_depth),
    
    detection_metric =
      detection_frequency_median,
    
    detection_lower =
      detection_frequency_lower,
    
    detection_upper =
      detection_frequency_upper,
    
    abundance_metric =
      raiw_weight_median,
    
    abundance_lower =
      raiw_weight_lower,
    
    abundance_upper =
      raiw_weight_upper
  )

print(
  depth_descriptive_sensitivity,
  n = Inf
)

################################################################################
# 59. COMBINE DESCRIPTIVE SENSITIVITY RESULTS
################################################################################

sensitivity_descriptive_summary <- bind_rows(
  temporal_descriptive_sensitivity,
  spatial_descriptive_sensitivity,
  depth_descriptive_sensitivity
)

print(
  sensitivity_descriptive_summary,
  n = Inf
)

################################################################################
# 60. ANALYTICAL DESIGN SUMMARY
################################################################################

sensitivity_design_summary <- tribble(
  
  ~analysis,
  ~dataset,
  ~sampling_units,
  ~standardised_effort,
  ~iterations,
  ~purpose,
  
  "Temporal",
  "Seven localities represented in all three monitoring cycles",
  "7 localities × 3 cycles",
  "30 min per locality × cycle",
  1000,
  "Evaluate temporal patterns while maintaining locality identity and equal sampling effort",
  
  "Spatial",
  "All 16 localities monitored in 2025",
  "16 localities",
  "34 min per locality",
  1000,
  "Evaluate whether the spatial distribution persists within a single year after equalising sampling effort",
  
  "Bathymetric",
  "Five 2025 localities represented in all three main depth strata",
  "5 localities × 3 depth strata",
  "30 min per locality × depth stratum",
  1000,
  "Evaluate bathymetric patterns while maintaining locality identity and equal sampling effort"
)

print(
  sensitivity_design_summary,
  n = Inf
)

################################################################################
# 61. SAVE CONSOLIDATED OUTPUTS
################################################################################

write_csv(
  sensitivity_descriptive_summary,
  "outputs/sensitivity_descriptive_summary.csv"
)

write_csv(
  sensitivity_design_summary,
  "outputs/sensitivity_design_summary.csv"
)

################################################################################
# 62. FINAL EXACT PERMUTATION TESTS
#
# Rarefaction is retained as the sensitivity analysis for unequal sampling
# effort. Inferential tests below use locality-level rates and exact
# permutation distributions, avoiding dependence on rarefaction seed and
# small-sample asymptotic approximations.
################################################################################


################################################################################
# A. EXACT BLOCKED RANK PERMUTATION
#
# Used for temporal and bathymetric repeated-measures designs.
# Condition labels are permuted independently within each locality.
################################################################################

exact_blocked_rank_test <- function(
    data,
    response,
    condition,
    block,
    condition_levels
) {
  
  response_name <- rlang::as_name(
    rlang::ensym(response)
  )
  
  condition_name <- rlang::as_name(
    rlang::ensym(condition)
  )
  
  block_name <- rlang::as_name(
    rlang::ensym(block)
  )
  
  dat <- data |>
    transmute(
      block =
        .data[[block_name]],
      
      condition =
        as.character(
          .data[[condition_name]]
        ),
      
      response =
        as.numeric(
          .data[[response_name]]
        )
    ) |>
    filter(
      condition %in% condition_levels,
      !is.na(response)
    )
  
  design_check <- dat |>
    count(
      block,
      condition
    )
  
  stopifnot(
    all(design_check$n == 1),
    n_distinct(dat$block) *
      length(condition_levels) ==
      nrow(dat)
  )
  
  wide <- dat |>
    mutate(
      condition = factor(
        condition,
        levels = condition_levels
      )
    ) |>
    tidyr::pivot_wider(
      names_from = condition,
      values_from = response
    ) |>
    arrange(block)
  
  x <- as.matrix(
    wide[, condition_levels]
  )
  
  n_blocks <- nrow(x)
  n_conditions <- ncol(x)
  
  stopifnot(
    n_conditions == 3,
    all(complete.cases(x))
  )
  
  # Rank responses within each locality
  rank_matrix <- t(
    apply(
      x,
      1,
      rank,
      ties.method = "average"
    )
  )
  
  # Observed rank-dispersion statistic.
  # Tie correction is constant across permutations.
  rank_sums_observed <-
    colSums(rank_matrix)
  
  expected_rank_sum <-
    n_blocks *
    (n_conditions + 1) / 2
  
  observed_score <-
    sum(
      (
        rank_sums_observed -
          expected_rank_sum
      )^2
    )
  
  # All 3! = 6 permutations
  permutations <- rbind(
    c(1, 2, 3),
    c(1, 3, 2),
    c(2, 1, 3),
    c(2, 3, 1),
    c(3, 1, 2),
    c(3, 2, 1)
  )
  
  # All possible combinations of permutations among blocks
  permutation_grid <- expand.grid(
    rep(
      list(
        seq_len(
          nrow(permutations)
        )
      ),
      n_blocks
    ),
    KEEP.OUT.ATTRS = FALSE
  )
  
  rank_sums_perm <- matrix(
    0,
    nrow = nrow(permutation_grid),
    ncol = n_conditions
  )
  
  for (i in seq_len(n_blocks)) {
    
    block_contribution <- t(
      vapply(
        seq_len(
          nrow(permutations)
        ),
        function(p) {
          
          rank_matrix[
            i,
            permutations[p, ]
          ]
          
        },
        numeric(n_conditions)
      )
    )
    
    rank_sums_perm <-
      rank_sums_perm +
      block_contribution[
        permutation_grid[[i]],
        ,
        drop = FALSE
      ]
  }
  
  permutation_scores <-
    rowSums(
      (
        rank_sums_perm -
          expected_rank_sum
      )^2
    )
  
  exact_p <-
    mean(
      permutation_scores >=
        observed_score -
        sqrt(.Machine$double.eps)
    )
  
  # Friedman statistic retained for familiar reporting
  observed_test <-
    friedman.test(x)
  
  tibble(
    statistic =
      unname(
        observed_test$statistic
      ),
    
    degrees_freedom =
      unname(
        observed_test$parameter
      ),
    
    n_permutations =
      nrow(
        permutation_grid
      ),
    
    p_value =
      exact_p
  )
}


################################################################################
# B. EXACT THREE-GROUP RANK PERMUTATION
#
# Used for the 2025 spatial analysis.
# Region labels are permuted among localities while preserving group sizes.
################################################################################

exact_three_group_rank_test <- function(
    data,
    response,
    group
) {
  
  response_name <- rlang::as_name(
    rlang::ensym(response)
  )
  
  group_name <- rlang::as_name(
    rlang::ensym(group)
  )
  
  dat <- data |>
    transmute(
      response =
        as.numeric(
          .data[[response_name]]
        ),
      
      group =
        as.character(
          .data[[group_name]]
        )
    ) |>
    filter(
      !is.na(response),
      !is.na(group)
    )
  
  group_sizes <-
    sort(
      table(dat$group)
    )
  
  stopifnot(
    length(group_sizes) == 3
  )
  
  group_levels <-
    names(group_sizes)
  
  group_n <-
    as.integer(
      group_sizes
    )
  
  values <-
    dat$response
  
  groups <-
    dat$group
  
  ranks <-
    rank(
      values,
      ties.method = "average"
    )
  
  observed_rank_sums <-
    vapply(
      group_levels,
      function(g) {
        sum(
          ranks[
            groups == g
          ]
        )
      },
      numeric(1)
    )
  
  observed_score <-
    sum(
      observed_rank_sums^2 /
        group_n
    )
  
  all_indices <-
    seq_along(values)
  
  total_rank_sum <-
    sum(ranks)
  
  # The smallest group is assigned first for computational efficiency
  first_group_sets <-
    combn(
      all_indices,
      group_n[1],
      simplify = FALSE
    )
  
  n_permutations <-
    choose(
      length(values),
      group_n[1]
    ) *
    choose(
      length(values) -
        group_n[1],
      group_n[2]
    )
  
  n_extreme <- 0
  
  for (idx1 in first_group_sets) {
    
    remaining <-
      setdiff(
        all_indices,
        idx1
      )
    
    idx2_matrix <-
      combn(
        remaining,
        group_n[2]
      )
    
    rank_sum_1 <-
      sum(
        ranks[idx1]
      )
    
    rank_sum_2 <-
      colSums(
        matrix(
          ranks[idx2_matrix],
          nrow = group_n[2]
        )
      )
    
    rank_sum_3 <-
      total_rank_sum -
      rank_sum_1 -
      rank_sum_2
    
    permutation_score <-
      rank_sum_1^2 /
      group_n[1] +
      rank_sum_2^2 /
      group_n[2] +
      rank_sum_3^2 /
      group_n[3]
    
    n_extreme <-
      n_extreme +
      sum(
        permutation_score >=
          observed_score -
          sqrt(.Machine$double.eps)
      )
  }
  
  observed_test <-
    kruskal.test(
      response ~ group,
      data = dat
    )
  
  tibble(
    statistic =
      unname(
        observed_test$statistic
      ),
    
    degrees_freedom =
      unname(
        observed_test$parameter
      ),
    
    n_permutations =
      n_permutations,
    
    p_value =
      n_extreme /
      n_permutations
  )
}


################################################################################
# C. TEMPORAL TEST DATA
################################################################################

temporal_test_metrics <- df_temporal |>
  group_by(
    localidade,
    region,
    monitoring_cycle
  ) |>
  summarise(
    effort_minutes = n(),
    
    detection_frequency =
      mean(
        dafor_num > 0,
        na.rm = TRUE
      ),
    
    mean_dafor_weight =
      mean(
        raiw_weight,
        na.rm = TRUE
      ),
    
    Uni100m =
      first(Uni100m),
    
    .groups = "drop"
  ) |>
  mutate(
    dpue =
      detection_frequency *
      60 /
      Uni100m,
    
    rai_w =
      mean_dafor_weight *
      60 /
      Uni100m
  )


temporal_perm_dpue <-
  exact_blocked_rank_test(
    temporal_test_metrics,
    response = dpue,
    condition = monitoring_cycle,
    block = localidade,
    condition_levels = expected_cycles
  )


temporal_perm_raiw <-
  exact_blocked_rank_test(
    temporal_test_metrics,
    response = rai_w,
    condition = monitoring_cycle,
    block = localidade,
    condition_levels = expected_cycles
  )


temporal_permutation_results <- bind_rows(
  
  temporal_perm_dpue |>
    mutate(
      analysis = "Temporal",
      response = "DPUE",
      .before = 1
    ),
  
  temporal_perm_raiw |>
    mutate(
      analysis = "Temporal",
      response = "RAI-W",
      .before = 1
    )
)


################################################################################
# D. SPATIAL TEST DATA - 2025
################################################################################

spatial_test_metrics <- df_2025_rarefaction |>
  group_by(
    localidade,
    region
  ) |>
  summarise(
    effort_minutes = n(),
    
    detection_frequency =
      mean(
        dafor_num > 0,
        na.rm = TRUE
      ),
    
    mean_dafor_weight =
      mean(
        raiw_weight,
        na.rm = TRUE
      ),
    
    .groups = "drop"
  )


spatial_perm_detection <-
  exact_three_group_rank_test(
    spatial_test_metrics,
    response = detection_frequency,
    group = region
  )


spatial_perm_weight <-
  exact_three_group_rank_test(
    spatial_test_metrics,
    response = mean_dafor_weight,
    group = region
  )


spatial_permutation_results <- bind_rows(
  
  spatial_perm_detection |>
    mutate(
      analysis = "Spatial",
      response = "Detection frequency",
      .before = 1
    ),
  
  spatial_perm_weight |>
    mutate(
      analysis = "Spatial",
      response = "Mean DAFOR weight",
      .before = 1
    )
)


################################################################################
# E. BATHYMETRIC TEST DATA - 2025
################################################################################

depth_test_metrics <- df_depth_balanced |>
  group_by(
    localidade,
    region,
    faixa_bat_depth
  ) |>
  summarise(
    effort_minutes = n(),
    
    detection_frequency =
      mean(
        dafor_num > 0,
        na.rm = TRUE
      ),
    
    mean_dafor_weight =
      mean(
        raiw_weight,
        na.rm = TRUE
      ),
    
    .groups = "drop"
  )


depth_perm_detection <-
  exact_blocked_rank_test(
    depth_test_metrics,
    response = detection_frequency,
    condition = faixa_bat_depth,
    block = localidade,
    condition_levels = main_depth_levels
  )


depth_perm_weight <-
  exact_blocked_rank_test(
    depth_test_metrics,
    response = mean_dafor_weight,
    condition = faixa_bat_depth,
    block = localidade,
    condition_levels = main_depth_levels
  )


depth_permutation_results <- bind_rows(
  
  depth_perm_detection |>
    mutate(
      analysis = "Bathymetric",
      response = "Detection frequency",
      .before = 1
    ),
  
  depth_perm_weight |>
    mutate(
      analysis = "Bathymetric",
      response = "Mean DAFOR weight",
      .before = 1
    )
)


################################################################################
# F. COMPLEMENTARY LOCALITY-PRESENCE TEST
#
# Important: Fisher's test uses observed locality presence/absence and is NOT
# an effort-rarefied analysis.
################################################################################

fisher_final <- spatial_fisher_result |>
  transmute(
    analysis =
      "Spatial",
    
    response =
      "Observed locality occurrence",
    
    statistic =
      NA_real_,
    
    degrees_freedom =
      NA_real_,
    
    n_permutations =
      NA_real_,
    
    p_value =
      p_value
  )


################################################################################
# G. FINAL INFERENTIAL TABLE
################################################################################

effort_standardised_permutation_tests <- bind_rows(
  temporal_permutation_results,
  spatial_permutation_results,
  fisher_final,
  depth_permutation_results
) |>
  mutate(
    test = case_when(
      analysis == "Temporal" ~
        "Exact blocked permutation",
      
      analysis == "Bathymetric" ~
        "Exact blocked permutation",
      
      response ==
        "Observed locality occurrence" ~
        "Fisher exact",
      
      analysis == "Spatial" ~
        "Exact rank permutation"
    )
  ) |>
  select(
    analysis,
    response,
    test,
    statistic,
    degrees_freedom,
    n_permutations,
    p_value
  )


print(
  effort_standardised_permutation_tests,
  n = Inf
)


write_csv(
  effort_standardised_permutation_tests,
  "outputs/effort_standardised_permutation_tests.csv"
)



