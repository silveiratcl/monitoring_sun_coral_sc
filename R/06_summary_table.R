



################################################################################



### new version

################################################################################
# 06_summary_table.R
#
# Create manuscript summary table:
# - monitoring effort by region
# - locality prevalence by region
# - positive one-minute records by region
#
# IMPORTANT:
# Table 1 uses the GENERAL MONITORING DATASET (2022-2025), not the
# depth-complete dataset. Therefore, records without valid bathymetry are
# retained here, because this table summarises regional monitoring effort and
# occurrence, not bathymetric patterns.
################################################################################

source("R/00_setup.R")
source("R/01_prepare_monitoring_data.R")

################################################################################
# 1. Prepare GENERAL analytical monitoring dataset
################################################################################

df_summary <- df_monit |>
  mutate(
    year = lubridate::year(data),
    dafor_num = clean_num(dafor),
    positive = dafor_num > 0
  ) |>
  filter(
    year %in% 2022:2025,
    coalesce(obs, "") != "estimado dos dados do ICMBio",
    !is.na(localidade),
    !is.na(dafor_id),
    !is.na(region)
  )

################################################################################
# 1a. Check analytical dataset
################################################################################

stopifnot(
  nrow(df_summary) == 8415,
  n_distinct(df_summary$localidade) == 43,
  sum(df_summary$positive, na.rm = TRUE) == 173
)

################################################################################
# 2. Summary by region
################################################################################

regional_summary <- df_summary |>
  group_by(region) |>
  summarise(
    n_localities = n_distinct(localidade),
    positive_localities = n_distinct(localidade[positive]),
    prevalence_percent =
      100 * positive_localities / n_localities,
    effort_minutes = n(),
    effort_hours = effort_minutes / 60,
    positive_records = sum(positive, na.rm = TRUE),
    positive_records_percent =
      100 * positive_records / effort_minutes,
    .groups = "drop"
  ) |>
  arrange(
    factor(
      region,
      levels = c("REBIO", "ADJACENT_REBIO", "SURROUNDINGS")
    )
  )

################################################################################
# 3. Combined REBIO + ADJACENT_REBIO summary
################################################################################

rebio_adjacent_summary <- regional_summary |>
  filter(region %in% c("REBIO", "ADJACENT_REBIO")) |>
  summarise(
    region = "REBIO + ADJACENT_REBIO",
    n_localities = sum(n_localities),
    positive_localities = sum(positive_localities),
    prevalence_percent =
      100 * positive_localities / n_localities,
    effort_minutes = sum(effort_minutes),
    effort_hours = sum(effort_hours),
    positive_records = sum(positive_records),
    positive_records_percent =
      100 * positive_records / effort_minutes
  )

################################################################################
# 4. Total summary
################################################################################

total_summary <- regional_summary |>
  summarise(
    region = "TOTAL",
    n_localities = sum(n_localities),
    positive_localities = sum(positive_localities),
    prevalence_percent =
      100 * positive_localities / n_localities,
    effort_minutes = sum(effort_minutes),
    effort_hours = sum(effort_hours),
    positive_records = sum(positive_records),
    positive_records_percent =
      100 * positive_records / effort_minutes
  )

################################################################################
# 5. Final manuscript summary table
################################################################################

summary_table_region <- bind_rows(
  regional_summary,
  rebio_adjacent_summary,
  total_summary
) |>
  mutate(
    prevalence_percent = round(prevalence_percent, 1),
    effort_hours = round(effort_hours, 1),
    positive_records_percent = round(positive_records_percent, 2)
  )

################################################################################
# 5a. Check final totals and key regional values
################################################################################

total_check <- summary_table_region |>
  filter(region == "TOTAL")

rebio_near_check <- summary_table_region |>
  filter(region == "REBIO + ADJACENT_REBIO")

surroundings_check <- summary_table_region |>
  filter(region == "SURROUNDINGS")

stopifnot(
  total_check$n_localities == 43,
  total_check$positive_localities == 15,
  total_check$effort_minutes == 8415,
  total_check$positive_records == 173,

  rebio_near_check$n_localities == 21,
  rebio_near_check$positive_localities == 15,
  rebio_near_check$effort_minutes == 4855,
  rebio_near_check$positive_records == 173,

  surroundings_check$n_localities == 22,
  surroundings_check$positive_localities == 0,
  surroundings_check$positive_records == 0
)

################################################################################
# 6. Print table
################################################################################

cat("\nRegional monitoring summary\n")
print(summary_table_region, n = Inf)

################################################################################
# 7. Export outputs
################################################################################

write_csv(
  summary_table_region,
  "outputs/Table_1_regional_monitoring_summary.csv"
)

saveRDS(
  summary_table_region,
  "outputs/Table_1_regional_monitoring_summary.rds"
)

################################################################################
# 8. Key manuscript numbers
################################################################################

key_summary_numbers <- summary_table_region |>
  filter(region == "TOTAL") |>
  transmute(
    total_localities = n_localities,
    total_positive_localities = positive_localities,
    total_prevalence_percent = prevalence_percent,
    total_effort_minutes = effort_minutes,
    total_effort_hours = effort_hours,
    total_positive_records = positive_records,
    total_positive_records_percent = positive_records_percent
  )

cat("\nKey manuscript numbers\n")
print(key_summary_numbers)

write_csv(
  key_summary_numbers,
  "outputs/key_summary_numbers.csv"
)

################################################################################
# 9. GT table for manuscript
################################################################################

library(gt)

table_1_gt <- summary_table_region |>
  mutate(
    region = case_when(
      region %in% c(
        "REBIO_ADJACENT",
        "ADJACENT_REBIO"
      ) ~ "ADJACENT TO REBIO",
      
      region %in% c(
        "REBIO + REBIO_ADJACENT",
        "REBIO + ADJACENT_REBIO"
      ) ~ "REBIO + ADJACENT TO REBIO",
      
      TRUE ~ region
    ),
    
    prevalence_percent =
      paste0(prevalence_percent, "%"),
    
    positive_records_percent =
      paste0(positive_records_percent, "%")
  ) |>
  gt() |>
  
  cols_label(
    region = "Region",
    n_localities = "Monitored\nlocalities",
    positive_localities = "Positive\nlocalities",
    prevalence_percent = "Prevalence",
    effort_minutes = "Effort\n(min.)",
    effort_hours = "Effort\n(h)",
    positive_records = "Positive\n1-min records",
    positive_records_percent = "Positive\nrecords (%)"
  ) |>
  
  fmt_number(
    columns = effort_hours,
    decimals = 1
  ) |>
  
  opt_table_font(
    font = list(
      gt::google_font("Arial"),
      default_fonts()
    )
  ) |>
  
  tab_options(
    table.font.size = px(14),
    heading.title.font.size = px(14),
    heading.subtitle.font.size = px(11),
    source_notes.font.size = px(10),
    data_row.padding = px(4)
  ) |>
  
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      rows = region == "REBIO + ADJACENT TO REBIO"
    )
  )

table_1_gt

################################################################################
# 10. Export GT table
################################################################################

dir.create("outputs/gt_tables", showWarnings = FALSE)

gtsave(
  table_1_gt,
  "outputs/gt_tables/Table_1_regional_monitoring_summary.png",
  expand = 10
)

gtsave(
  table_1_gt,
  "outputs/gt_tables/Table_1_regional_monitoring_summary.html"
)

# source("R/06_summary_table.R")



















