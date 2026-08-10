################################################################################
# 07_sampling_balance_and_cycles.R
#
# Prepare sampling-balance diagnostics and comparable monitoring subsets
# for temporal, spatial, and bathymetric analyses.
#
# This script:
# 1. Defines the complete monitoring dataset (2022-2025)
# 2. Separates records with and without valid bathymetric information
# 3. Defines three annual monitoring cycles
# 4. Summarises sampling effort by locality and cycle
# 5. Identifies localities sampled in all three monitoring cycles
# 6. Quantifies sampling balance among cycles
# 7. Prepares datasets for effort-standardised analyses
################################################################################

source("R/00_setup.R")
source("R/01_prepare_monitoring_data.R")

library(tidyverse)
library(lubridate)

dir.create(
  "outputs",
  showWarnings = FALSE,
  recursive = TRUE
)

################################################################################
# 1. SETTINGS
################################################################################

study_years <- 2022:2025

cycle_start_month <- 6L

expected_cycles <- c(
  "2022/2023",
  "2023/2024",
  "2024/2025"
)

icmbio_estimated_label <- "estimado dos dados do ICMBio"

################################################################################
# 2. CHECK REQUIRED VARIABLES
################################################################################

required_columns <- c(
  "localidade",
  "data",
  "faixa_bat",
  "prof_max",
  "dafor",
  "dafor_id",
  "geo_id",
  "id_horus",
  "obs",
  "region",
  "tempo_censo"
  
)

missing_columns <- setdiff(
  required_columns,
  names(df_monit)
)

if (length(missing_columns) > 0) {
  stop(
    paste(
      "Missing columns in df_monit:",
      paste(missing_columns, collapse = ", ")
    )
  )
}

################################################################################
# 3. PREPARE RAW DATA
################################################################################

df_raw <- df_monit |>
  mutate(
    data = as.Date(data),
    
    year = year(data),
    month = month(data),
    prof_max_num = clean_num(prof_max),
   
     localidade = str_squish(
      str_to_upper(localidade)
    ),
    
    region = str_squish(
      str_to_upper(region)
    ),
    
    obs_clean = str_squish(
      str_to_lower(
        coalesce(as.character(obs), "")
      )
    ),
    
    faixa_bat_clean = str_squish(
      as.character(faixa_bat)
    ),
    
    positive_record =
      !is.na(dafor) &
      dafor > 0
  )

################################################################################
# 4. IDENTIFY ESTIMATED ICMBIO RECORDS
################################################################################

df_raw <- df_raw |>
  mutate(
    estimated_icmbio =
      obs_clean ==
      str_to_lower(icmbio_estimated_label)
  )

icmbio_summary <- df_raw |>
  count(
    estimated_icmbio,
    name = "n_records"
  )

print(icmbio_summary)

################################################################################
# 5. COMPLETE MONITORING DATASET
#
# General monitoring analyses:
# - only 2022-2025
# - exclude estimated ICMBio records
# - retain records without bathymetric information
################################################################################

df_monitoring_all <- df_raw |>
  filter(
    year %in% study_years,
    !estimated_icmbio,
    !is.na(localidade),
    localidade != "",
    !is.na(dafor_id)
  ) |>
  mutate(
    faixa_bat = case_when(
      is.na(faixa_bat_clean) ~ NA_character_,
      str_to_lower(faixa_bat_clean) %in%
        c("", "na", "n/a") ~ NA_character_,
      TRUE ~ faixa_bat_clean
    )
  )

################################################################################
# 6. DEPTH-COMPLETE DATASET
#
# Used only for analyses involving bathymetric strata.
################################################################################

df_monitoring_depth <- df_monitoring_all |>
  filter(
    !is.na(prof_max_num)
  )


################################################################################
# 7. RECORDS WITHOUT BATHYMETRIC INFORMATION
################################################################################

df_monitoring_no_depth <- df_monitoring_all |>
  filter(
    is.na(prof_max_num)
  )

no_depth_summary <- df_monitoring_no_depth |>
  group_by(
    localidade,
    region,
    year
  ) |>
  summarise(
    n_records = n(),
    
    n_positive = sum(
      positive_record,
      na.rm = TRUE
    ),
    
    n_sampling_dates =
      n_distinct(data),
    
    first_date = min(data),
    
    last_date = max(data),
    
    .groups = "drop"
  )

print(no_depth_summary, n = Inf)

################################################################################
# 8. DATASET SUMMARY
################################################################################

dataset_summary <- tibble(
  dataset = c(
    "Raw dataset",
    "Monitoring dataset 2022-2025",
    "Monitoring records with prof_max",
    "Monitoring records without prof_max"
  ),
  
  n_records = c(
    nrow(df_raw),
    nrow(df_monitoring_all),
    nrow(df_monitoring_depth),
    nrow(df_monitoring_no_depth)
  ),
  
  n_positive = c(
    sum(
      df_raw$positive_record,
      na.rm = TRUE
    ),
    
    sum(
      df_monitoring_all$positive_record,
      na.rm = TRUE
    ),
    
    sum(
      df_monitoring_depth$positive_record,
      na.rm = TRUE
    ),
    
    sum(
      df_monitoring_no_depth$positive_record,
      na.rm = TRUE
    )
  ),
  
  n_localities = c(
    n_distinct(
      df_raw$localidade,
      na.rm = TRUE
    ),
    
    n_distinct(
      df_monitoring_all$localidade,
      na.rm = TRUE
    ),
    
    n_distinct(
      df_monitoring_depth$localidade,
      na.rm = TRUE
    ),
    
    n_distinct(
      df_monitoring_no_depth$localidade,
      na.rm = TRUE
    )
  ),
  
  effort_hours = c(
    nrow(df_raw) / 60,
    nrow(df_monitoring_all) / 60,
    nrow(df_monitoring_depth) / 60,
    nrow(df_monitoring_no_depth) / 60
  )
)

print(dataset_summary)

################################################################################
# 9. VERIFY EXPECTED DATASET TOTALS
################################################################################

stopifnot(
  nrow(df_raw) == 8441,
  nrow(df_monitoring_all) == 8415,
  nrow(df_monitoring_depth) == 8185,
  nrow(df_monitoring_no_depth) == 230,
  
  sum(df_monitoring_all$positive_record, na.rm = TRUE) == 173,
  sum(df_monitoring_depth$positive_record, na.rm = TRUE) == 173,
  sum(df_monitoring_no_depth$positive_record, na.rm = TRUE) == 0
)
################################################################################
# 10. DEFINE MONITORING CYCLES
#
# 2022/2023 = June 2022 to May 2023
# 2023/2024 = June 2023 to May 2024
# 2024/2025 = June 2024 to May 2025
################################################################################

df_cycle <- df_monitoring_all |>
  mutate(
    cycle_start_year = if_else(
      month >= cycle_start_month,
      year,
      year - 1L
    ),
    
    monitoring_cycle = paste0(
      cycle_start_year,
      "/",
      cycle_start_year + 1L
    ),
    
    cycle_index = match(
      monitoring_cycle,
      expected_cycles
    )
  ) |>
  filter(
    monitoring_cycle %in%
      expected_cycles
  )

################################################################################
# 11. GENERAL SUMMARY BY MONITORING CYCLE
################################################################################

cycle_summary <- df_cycle |>
  group_by(
    monitoring_cycle,
    cycle_index
  ) |>
  summarise(
    first_date = min(data),
    
    last_date = max(data),
    
    n_sampling_dates =
      n_distinct(data),
    
    n_localities =
      n_distinct(localidade),
    
    effort_minutes = n(),
    
    effort_hours =
      effort_minutes / 60,
    
    n_positive = sum(
      positive_record,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  ) |>
  arrange(
    cycle_index
  )

print(cycle_summary)

################################################################################
# 12. SUMMARISE EACH LOCALITY WITHIN EACH MONITORING CYCLE
################################################################################

locality_cycle_effort <- df_cycle |>
  group_by(
    localidade,
    region,
    monitoring_cycle,
    cycle_index
  ) |>
  summarise(
    effort_minutes = n(),
    
    effort_hours =
      effort_minutes / 60,
    
    n_sampling_dates =
      n_distinct(data),
    
    n_positive = sum(
      positive_record,
      na.rm = TRUE
    ),
    
    positive_percentage =
      100 *
      n_positive /
      effort_minutes,
    
    n_records_without_depth =
      sum(
        is.na(faixa_bat)
      ),
    
    .groups = "drop"
  ) |>
  arrange(
    localidade,
    cycle_index
  )

print(
  locality_cycle_effort,
  n = Inf
)

################################################################################
# 13. TEMPORAL COVERAGE OF EACH LOCALITY
################################################################################

locality_cycle_balance <- locality_cycle_effort |>
  group_by(
    localidade,
    region
  ) |>
  summarise(
    n_cycles =
      n_distinct(
        monitoring_cycle
      ),
    
    sampled_cycles = paste(
      monitoring_cycle[
        order(cycle_index)
      ],
      collapse = ", "
    ),
    
    total_effort =
      sum(
        effort_minutes
      ),
    
    minimum_effort =
      min(
        effort_minutes
      ),
    
    maximum_effort =
      max(
        effort_minutes
      ),
    
    mean_effort =
      mean(
        effort_minutes
      ),
    
    sd_effort =
      sd(
        effort_minutes
      ),
    
    cv_effort = if_else(
      n_cycles > 1 &
        mean_effort > 0,
      
      sd_effort /
        mean_effort,
      
      NA_real_
    ),
    
    effort_ratio = if_else(
      minimum_effort > 0,
      
      maximum_effort /
        minimum_effort,
      
      NA_real_
    ),
    
    total_positive =
      sum(
        n_positive
      ),
    
    .groups = "drop"
  ) |>
  arrange(
    desc(n_cycles),
    effort_ratio,
    localidade
  )

print(
  locality_cycle_balance,
  n = Inf
)

################################################################################
# 14. IDENTIFY LOCALITIES PRESENT IN ALL THREE CYCLES
#
# At this stage we define consistency only as presence in all three cycles.
# Sampling evenness is evaluated separately using effort statistics.
################################################################################

balanced_cycle_candidates <- locality_cycle_balance |>
  filter(
    n_cycles ==
      length(expected_cycles)
  ) |>
  arrange(
    effort_ratio,
    localidade
  )

print(
  balanced_cycle_candidates,
  n = Inf
)

################################################################################
# 15. EFFORT MATRIX FOR THE THREE-CYCLE LOCALITIES
################################################################################

balanced_locality_names <-
  balanced_cycle_candidates |>
  pull(localidade)

balanced_effort_matrix <- locality_cycle_effort |>
  filter(
    localidade %in%
      balanced_locality_names
  ) |>
  select(
    localidade,
    region,
    monitoring_cycle,
    effort_minutes
  ) |>
  pivot_wider(
    names_from =
      monitoring_cycle,
    
    values_from =
      effort_minutes,
    
    names_prefix =
      "effort_"
  ) |>
  arrange(
    region,
    localidade
  )

print(
  balanced_effort_matrix,
  n = Inf
)

################################################################################
# 16. BALANCED MINUTE-LEVEL SUBSET
#
# Contains all records from localities represented in all three cycles.
# It will later be used for the temporal sensitivity analysis.
################################################################################

balanced_cycle_minutes <- df_cycle |>
  filter(
    localidade %in%
      balanced_locality_names
  ) |>
  arrange(
    localidade,
    cycle_index,
    data,
    tempo_censo
  )

################################################################################
# 17. CHECK THREE-CYCLE COMPLETENESS
################################################################################

balanced_cycle_check <- balanced_cycle_minutes |>
  distinct(
    localidade,
    region,
    monitoring_cycle
  ) |>
  count(
    localidade,
    region,
    name = "n_cycles"
  ) |>
  mutate(
    complete_three_cycles =
      n_cycles ==
      length(expected_cycles)
  )

print(
  balanced_cycle_check,
  n = Inf
)

stopifnot(
  all(
    balanced_cycle_check$
      complete_three_cycles
  )
)

################################################################################
# 18. COVERAGE SUMMARY
################################################################################

coverage_summary <- locality_cycle_balance |>
  count(
    n_cycles,
    name = "n_localities"
  ) |>
  arrange(
    n_cycles
  )

print(coverage_summary)

################################################################################
# 19. SAVE DATASETS
################################################################################

saveRDS(
  df_monitoring_all,
  "outputs/monitoring_data_all.rds"
)

saveRDS(
  df_monitoring_depth,
  "outputs/monitoring_data_depth_complete.rds"
)

saveRDS(
  df_cycle,
  "outputs/monitoring_data_with_cycles.rds"
)

saveRDS(
  balanced_cycle_minutes,
  "outputs/balanced_cycle_minutes.rds"
)

################################################################################
# 20. SAVE DIAGNOSTIC TABLES
################################################################################

write_csv(
  dataset_summary,
  "outputs/sampling_dataset_summary.csv"
)

write_csv(
  no_depth_summary,
  "outputs/records_without_depth.csv"
)

write_csv(
  cycle_summary,
  "outputs/monitoring_cycle_summary.csv"
)

write_csv(
  locality_cycle_effort,
  "outputs/locality_cycle_effort.csv"
)

write_csv(
  locality_cycle_balance,
  "outputs/locality_cycle_balance.csv"
)

write_csv(
  balanced_cycle_candidates,
  "outputs/balanced_cycle_candidates.csv"
)

write_csv(
  balanced_effort_matrix,
  "outputs/balanced_effort_matrix.csv"
)

write_csv(
  balanced_cycle_check,
  "outputs/balanced_cycle_check.csv"
)

write_csv(
  coverage_summary,
  "outputs/cycle_coverage_summary.csv"
)
################################################################################
# 21. FINAL SUMMARY
################################################################################

  cat(
    "\n============================================================\n",
    "SAMPLING BALANCE AND MONITORING CYCLE SUMMARY\n",
    "============================================================\n",
  
  "Raw records: ",
  nrow(df_raw),
  "\n",
  
  "Monitoring records (2022-2025): ",
  nrow(df_monitoring_all),
  "\n",
  
  "Records with prof_max: ",
  nrow(df_monitoring_depth),
  "\n",
  
  "Records without prof_max: ",
  nrow(df_monitoring_no_depth),
  "\n",
  
  "Positive monitoring records: ",
  sum(
    df_monitoring_all$positive_record,
    na.rm = TRUE
  ),
  "\n",
  
  "Monitoring cycles: ",
  paste(
    expected_cycles,
    collapse = ", "
  ),
  "\n",
  
  "Localities in complete monitoring dataset: ",
  n_distinct(
    df_monitoring_all$localidade
  ),
  "\n",
  
  "Localities present in all three cycles: ",
  nrow(
    balanced_cycle_candidates
  ),
  "\n",
  
  "Records in three-cycle locality subset: ",
  nrow(
    balanced_cycle_minutes
  ),
  "\n",
  
  "============================================================\n",
  
  sep = ""
)

#source("R/07_sampling_balance_and_cycles.R")
