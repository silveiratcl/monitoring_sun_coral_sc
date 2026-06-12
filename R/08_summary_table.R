################################################################################
# 08_summary_table.R
#
# Create manuscript summary table:
# - monitoring effort by region
# - locality prevalence by region
# - positive one-minute records by region
#
# This table helps distinguish patterns within REBIO / NEAR_REBIO from the
# broader study area and addresses the reviewer request for total positive
# detections.
################################################################################

source("R/00_setup.R")
source("R/01_prepare_monitoring_data.R")

################################################################################
# 1. Prepare analytical monitoring dataset
################################################################################

df_summary <- df_monit |>
  mutate(
    dafor_num = clean_num(dafor),
    positive = dafor_num > 0
  ) |>
  filter(
    obs != "estimado dos dados do ICMBio",
    faixa_bat != "Na",
    !is.na(localidade),
    !is.na(dafor_id),
    !is.na(region)
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
      levels = c("REBIO", "NEAR_REBIO", "SURROUNDINGS")
    )
  )

################################################################################
# 3. Combined REBIO + NEAR_REBIO summary
################################################################################

rebio_near_summary <- regional_summary |>
  filter(region %in% c("REBIO", "NEAR_REBIO")) |>
  summarise(
    region = "REBIO + NEAR_REBIO",
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
  rebio_near_summary,
  total_summary
) |>
  mutate(
    prevalence_percent = round(prevalence_percent, 1),
    effort_hours = round(effort_hours, 1),
    positive_records_percent = round(positive_records_percent, 2)
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
    prevalence_percent =
      paste0(prevalence_percent, "%"),
    
    positive_records_percent =
      paste0(positive_records_percent, "%")
  ) |>
  gt() |>
  
  # tab_header(
  #  # title = md("**Table 1. Regional summary of monitoring effort and prevalence of *Tubastraea coccinea***"),
  #   subtitle = paste(
  #     "Summary of monitored localities, prevalence, effort,",
  #     "and positive one-minute records."
  #   )
  # ) |>
  
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
    columns = c(effort_hours),
    decimals = 1
  ) |>
  # 
  # tab_source_note(
  #   source_note = paste(
  #     "Positive records correspond to one-minute transects",
  #     "with the presence of T. coccinea."
  #   )
  # ) |>
  
  opt_table_font(
    font = list(
      gt::google_font("Arial"),
      default_fonts()
    )
  ) |>
  
  tab_options(
    table.font.size = px(12),
    heading.title.font.size = px(14),
    heading.subtitle.font.size = px(11),
    source_notes.font.size = px(10),
    data_row.padding = px(4)
  ) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      rows = region == "REBIO + NEAR_REBIO"
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



