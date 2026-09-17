################################################################################
# 09_supplementary_tables.R
#
# Generate supplementary tables from the final analytical outputs.
#
# Tables:
# S1  Monitored localities and shoreline extent
# S2  Pooled monitoring metrics by locality
# S3  RAI-W weighting sensitivity
# S4a Relationship between monitoring effort and invasion metrics
# S4b Sensitivity analyses addressing unequal sampling effort
################################################################################

source("R/00_setup.R")

library(tidyverse)

dir.create(
  "outputs/gt_tables",
  showWarnings = FALSE,
  recursive = TRUE
)

################################################################################
# 1. REQUIRED INPUTS
################################################################################

required_files <- c(
  "outputs/df_localidade_clean.rds",
  "outputs/site_year_metrics.rds",
  "outputs/effort_correlations.csv",
  "outputs/sensitivity_summary.csv",
  "outputs/locality_raiw_rank_changes.csv",
  "outputs/sensitivity_design_summary.csv",
  "outputs/effort_standardised_permutation_tests.csv"
)

missing_files <- required_files[
  !file.exists(required_files)
]

if (length(missing_files) > 0) {
  stop(
    paste0(
      "Missing required input file(s):\n",
      paste(
        missing_files,
        collapse = "\n"
      )
    )
  )
}

################################################################################
# 2. LOAD FINAL ANALYTICAL OUTPUTS
################################################################################

df_localidade <- readRDS(
  "outputs/df_localidade_clean.rds"
)

site_year_metrics <- readRDS(
  "outputs/site_year_metrics.rds"
)

effort_correlations <- read_csv(
  "outputs/effort_correlations.csv",
  show_col_types = FALSE
)

sensitivity_summary <- read_csv(
  "outputs/sensitivity_summary.csv",
  show_col_types = FALSE
)

raiw_rank_changes <- read_csv(
  "outputs/locality_raiw_rank_changes.csv",
  show_col_types = FALSE
)

sensitivity_design_summary <- read_csv(
  "outputs/sensitivity_design_summary.csv",
  show_col_types = FALSE
)

effort_standardised_tests <- read_csv(
  "outputs/effort_standardised_permutation_tests.csv",
  show_col_types = FALSE
)

################################################################################
# 3. REMOVE OBSOLETE SUPPLEMENTARY OUTPUTS
################################################################################

obsolete_outputs <- c(
  "outputs/Table_S1_localities_extent.csv",
  "outputs/gt_tables/Table_S1_localities_extent.png",
  "outputs/Table_S2_locality_metrics_complete.csv",
  "outputs/Table_S4b_temporal_models.csv",
  "outputs/gt_tables/Table_S4b_temporal_models.png"
)

unlink(
  obsolete_outputs[
    file.exists(obsolete_outputs)
  ],
  force = TRUE
)

################################################################################
# 4. TABLE S1 - MONITORED LOCALITIES AND SHORELINE EXTENT
#
# The shoreline reference file contains more localities than were sampled in
# 2022-2025. Therefore, Table S1 is restricted to the 43 localities represented
# in the final monitoring dataset.
#
# Locality abbreviations used in Figure 6 are included for the 21 localities
# represented in the detailed map panels.
################################################################################

monitored_localities <- site_year_metrics |>
  distinct(
    localidade,
    region
  )


table_s1_localities <- monitored_localities |>
  left_join(
    df_localidade |>
      select(
        localidade,
        shoreline_extent_m = extent_m,
        shoreline_units_100m = Uni100m
      ),
    by = "localidade"
  ) |>
  mutate(
    region = recode(
      region,
      "ADJACENT_REBIO" = "ADJACENT TO REBIO"
    ),
    
    locality_abbr = case_when(
      localidade == "BAIA DAS TARTARUGAS"  ~ "BT",
      localidade == "ENGENHO"              ~ "ENG",
      localidade == "FAROL"                ~ "FAR",
      localidade == "SACO DO BATISMO"      ~ "SB",
      localidade == "SACO DO CAPIM"        ~ "SC",
      localidade == "VIDAL"                ~ "VID",
      localidade == "COSTA DO ELEFANTE"    ~ "CE",
      localidade == "COSTAO DO SACO DAGUA" ~ "CSD",
      localidade == "DESERTA NORTE"        ~ "DN",
      localidade == "DESERTA SUL"          ~ "DS",
      localidade == "ENSEADA DO LILI"      ~ "EL",
      localidade == "LETREIRO"             ~ "LET",
      localidade == "NAUFRAGIO DO LILI"    ~ "NL",
      localidade == "PEDRA DO ELEFANTE"    ~ "PE",
      localidade == "PORTINHO NORTE"       ~ "PN",
      localidade == "PORTINHO SUL"         ~ "PS",
      localidade == "RANCHO NORTE"         ~ "RN",
      localidade == "SACO DA MULATA NORTE" ~ "SMN",
      localidade == "SACO DA MULATA SUL"   ~ "SMS",
      localidade == "SACO DAGUA"           ~ "SD",
      localidade == "SAQUINHO DAGUA"       ~ "SQD",
      TRUE                                 ~ NA_character_
    )
  ) |>
  arrange(
    region,
    localidade
  ) |>
  rename(
    locality = localidade
  ) |>
  select(
    locality,
    locality_abbr,
    region,
    shoreline_extent_m,
    shoreline_units_100m
  )


stopifnot(
  nrow(table_s1_localities) == 43,
  
  sum(
    is.na(
      table_s1_localities$shoreline_extent_m
    )
  ) == 0,
  
  sum(
    is.na(
      table_s1_localities$shoreline_units_100m
    )
  ) == 0,
  
  sum(
    !is.na(
      table_s1_localities$locality_abbr
    )
  ) == 21,
  
  n_distinct(
    table_s1_localities$locality_abbr,
    na.rm = TRUE
  ) == 21
)

################################################################################
# 5. TABLE S2 - POOLED MONITORING METRICS BY LOCALITY
#
# DPUE and RAI-W are pooled across the complete monitoring period by first
# aggregating their numerators and total sampling effort. We do NOT sum annual
# DPUE or annual RAI-W values because doing so would give greater weight to
# localities sampled in more civil years.
################################################################################

table_s2_locality_metrics <- site_year_metrics |>
  group_by(
    localidade,
    region
  ) |>
  summarise(
    n_years_sampled = n_distinct(year),

    total_effort_minutes =
      sum(
        effort_minutes,
        na.rm = TRUE
      ),

    total_effort_hours =
      sum(
        effort_hours,
        na.rm = TRUE
      ),

    positive_one_minute_records =
      sum(
        n_positive,
        na.rm = TRUE
      ),

    weighted_dafor_sum =
      sum(
        sum_weight,
        na.rm = TRUE
      ),

    shoreline_extent_m =
      first(shoreline_m),

    shoreline_units_100m =
      first(uni100m),

    .groups = "drop"
  ) |>
  mutate(
    region = recode(
      region,
      "ADJACENT_REBIO" = "ADJACENT TO REBIO"
    ),
    
    pooled_denominator =
      total_effort_hours *
      shoreline_units_100m,

    pooled_dpue =
      if_else(
        pooled_denominator > 0,
        positive_one_minute_records /
          pooled_denominator,
        NA_real_
      ),

    pooled_raiw =
      if_else(
        pooled_denominator > 0,
        weighted_dafor_sum /
          pooled_denominator,
        NA_real_
      )
  ) |>
  select(
    -pooled_denominator
  ) |>
  arrange(
    desc(pooled_dpue),
    localidade
  )

table_s2a_positive_localities <- table_s2_locality_metrics |>
  filter(
    positive_one_minute_records > 0
  ) |>
  arrange(
    desc(pooled_dpue),
    localidade
  )

table_s2b_negative_localities <- table_s2_locality_metrics |>
  filter(
    positive_one_minute_records == 0
  ) |>
  arrange(
    region,
    localidade
  )

stopifnot(
  nrow(table_s2_locality_metrics) == 43,
  sum(table_s2_locality_metrics$total_effort_minutes) == 8415,
  sum(table_s2_locality_metrics$positive_one_minute_records) == 173,
  nrow(table_s2a_positive_localities) == 15,
  nrow(table_s2b_negative_localities) == 28
)

################################################################################
# 6. TABLE S3 - RAI-W WEIGHTING SENSITIVITY
################################################################################

max_rank_change <- max(
  raiw_rank_changes$max_abs_change,
  na.rm = TRUE
)

table_s3_raiw_sensitivity <- sensitivity_summary |>
  mutate(
    measure = case_when(
      str_starts(
        comparison,
        "Rank_"
      ) ~
        "Locality ranking Spearman correlation",

      TRUE ~
        "Locality-year RAI-W Spearman correlation"
    ),

    comparison = comparison |>
      str_remove(
        "^Rank_"
      ) |>
      str_replace_all(
        "_vs_",
        " vs "
      ),

    value = round(
      spearman_rho,
      4
    ),

    units = "Spearman rho"
  ) |>
  select(
    measure,
    comparison,
    value,
    units
  ) |>
  bind_rows(
    tibble(
      measure =
        "Maximum absolute locality rank change",

      comparison =
        "Across manual, moderate, and linear weighting schemes",

      value =
        max_rank_change,

      units =
        "rank positions"
    )
  )

stopifnot(
  max_rank_change == 2
)

################################################################################
# 7. TABLE S4a - RELATIONSHIP BETWEEN EFFORT AND METRICS
################################################################################

table_s4a_effort_correlations <- effort_correlations |>
  mutate(
    response = recode(
      response,
      "RAI_W" = "RAI-W",
      "Positive_detections" = "Positive one-minute records",
      .default = response
    ),

    rho = round(
      rho,
      4
    ),

    p_value = signif(
      p_value,
      3
    )
  )

################################################################################
# 8. TABLE S4b - SENSITIVITY ANALYSES ADDRESSING UNEQUAL SAMPLING EFFORT
#
# Repeated random subsampling is used to evaluate robustness to unequal effort.
# Inferential p-values come from exact locality-level permutation procedures.
# Fisher's exact test uses observed locality-level occurrence and is not
# rarefied.
################################################################################

table_s4b_effort_standardised <- effort_standardised_tests |>
  left_join(
    sensitivity_design_summary |>
      select(
        analysis,
        dataset,
        sampling_units,
        standardised_effort,
        iterations
      ),
    by = "analysis"
  ) |>
  mutate(
    standardisation = case_when(
      response ==
        "Observed locality occurrence" ~
        "Observed locality-level presence/absence; not rarefied",

      TRUE ~
        paste0(
          "1,000 repeated random subsamples at ",
          standardised_effort,
          " for sensitivity; exact test uses locality-level rates"
        )
    ),

    rarefaction_iterations = case_when(
      response ==
        "Observed locality occurrence" ~
        NA_real_,

      TRUE ~
        as.numeric(iterations)
    ),

    interpretation = case_when(
      analysis == "Temporal" ~
        paste0(
          "Increasing descriptive pattern after effort standardisation, ",
          "but no statistical evidence of differences among monitoring cycles"
        ),

      analysis == "Spatial" &
        response %in% c(
          "Detection frequency",
          "Mean DAFOR weight"
        ) ~
        "Significant differences among regions after effort standardisation",

      response ==
        "Observed locality occurrence" ~
        paste0(
          "Observed locality occurrence differed significantly among regions; ",
          "Fisher's exact test was not rarefied"
        ),

      analysis == "Bathymetric" &
        response ==
          "Detection frequency" ~
        paste0(
          "No statistical evidence of differences among the three main depth ",
          "strata in the balanced locality subset"
        ),

      analysis == "Bathymetric" &
        response ==
          "Mean DAFOR weight" ~
        paste0(
          "Suggestive but non-significant difference among the three main depth ",
          "strata in the balanced locality subset"
        ),

      TRUE ~
        NA_character_
    )
  ) |>
  transmute(
    pattern = analysis,
    dataset,
    standardisation,
    analytical_units = sampling_units,
    rarefaction_iterations,
    metric = response,
    test,
    statistic,
    degrees_freedom,
    exact_permutations = n_permutations,
    p_value,
    interpretation
  )

stopifnot(
  nrow(table_s4b_effort_standardised) == 7,
  sum(table_s4b_effort_standardised$pattern == "Temporal") == 2,
  sum(table_s4b_effort_standardised$pattern == "Spatial") == 3,
  sum(table_s4b_effort_standardised$pattern == "Bathymetric") == 2
)

################################################################################
# 9. EXPORT CSV TABLES
################################################################################

write_csv(
  table_s1_localities,
  "outputs/Table_S1_monitored_localities_extent.csv"
)

write_csv(
  table_s2_locality_metrics,
  "outputs/Table_S2_locality_metrics.csv"
)

write_csv(
  table_s2a_positive_localities,
  "outputs/Table_S2a_positive_localities.csv"
)

write_csv(
  table_s2b_negative_localities,
  "outputs/Table_S2b_negative_localities.csv"
)

write_csv(
  table_s3_raiw_sensitivity,
  "outputs/Table_S3_raiw_sensitivity.csv"
)

write_csv(
  table_s4a_effort_correlations,
  "outputs/Table_S4a_effort_correlations.csv"
)

write_csv(
  table_s4b_effort_standardised,
  "outputs/Table_S4b_effort_standardised_sensitivity.csv"
)

################################################################################
# 10. OPTIONAL FORMATTED GT TABLES
#
# PNG export is skipped if gt/webshot2 are not available. CSV tables above are
# always produced and are the canonical supplementary outputs.
################################################################################

can_export_gt_png <-
  requireNamespace(
    "gt",
    quietly = TRUE
  ) &&
  requireNamespace(
    "webshot2",
    quietly = TRUE
  )

if (can_export_gt_png) {

  table_s1_localities |>
    gt::gt() |>
    gt::tab_header(
      title =
        "Table S1. Monitored localities and shoreline extent"
    ) |>
    gt::cols_label(
      locality = "Locality",
      locality_abbr = "Map code",
      region = "Region",
      shoreline_extent_m = "Shoreline extent (m)",
      shoreline_units_100m = "100 m units"
    ) |>
    gt::fmt_number(
      columns = c(
        shoreline_extent_m,
        shoreline_units_100m
      ),
      decimals = 2
    ) |>
    gt::gtsave(
      "outputs/gt_tables/Table_S1_monitored_localities_extent.png"
    )

  table_s2a_positive_localities |>
    gt::gt() |>
    gt::tab_header(
      title =
        "Table S2a. Monitoring metrics for localities with sun coral detections"
    ) |>
    gt::cols_label(
      localidade = "Locality",
      region = "Region",
      n_years_sampled = "Years sampled",
      total_effort_minutes = "Effort (min)",
      total_effort_hours = "Effort (h)",
      positive_one_minute_records = "Positive one-minute records",
      weighted_dafor_sum = "Weighted DAFOR sum",
      shoreline_extent_m = "Shoreline extent (m)",
      shoreline_units_100m = "100 m units",
      pooled_dpue = "Pooled DPUE",
      pooled_raiw = "Pooled RAI-W"
    ) |>
    gt::fmt_number(
      columns = c(
        total_effort_hours,
        weighted_dafor_sum,
        shoreline_extent_m,
        shoreline_units_100m,
        pooled_dpue,
        pooled_raiw
      ),
      decimals = 3
    ) |>
    gt::fmt_number(
      columns = c(
        n_years_sampled,
        total_effort_minutes,
        positive_one_minute_records
      ),
      decimals = 0
    ) |>
    gt::gtsave(
      "outputs/gt_tables/Table_S2a_positive_localities.png"
    )

  table_s2b_negative_localities |>
    select(
      localidade,
      region,
      n_years_sampled,
      total_effort_minutes,
      total_effort_hours,
      shoreline_extent_m,
      shoreline_units_100m
    ) |>
    gt::gt() |>
    gt::tab_header(
      title =
        "Table S2b. Monitored localities without sun coral detections"
    ) |>
    gt::cols_label(
      localidade = "Locality",
      region = "Region",
      n_years_sampled = "Years sampled",
      total_effort_minutes = "Effort (min)",
      total_effort_hours = "Effort (h)",
      shoreline_extent_m = "Shoreline extent (m)",
      shoreline_units_100m = "100 m units"
    ) |>
    gt::fmt_number(
      columns = c(
        total_effort_hours,
        shoreline_extent_m,
        shoreline_units_100m
      ),
      decimals = 3
    ) |>
    gt::fmt_number(
      columns = c(
        n_years_sampled,
        total_effort_minutes
      ),
      decimals = 0
    ) |>
    gt::gtsave(
      "outputs/gt_tables/Table_S2b_negative_localities.png"
    )

  table_s3_raiw_sensitivity |>
    gt::gt() |>
    gt::tab_header(
      title =
        "Table S3. RAI-W weighting sensitivity analysis"
    ) |>
    gt::cols_label(
      measure = "Measure",
      comparison = "Comparison",
      value = "Value",
      units = "Units"
    ) |>
    gt::fmt_number(
      columns = value,
      rows = units == "Spearman rho",
      decimals = 4
    ) |>
    gt::fmt_number(
      columns = value,
      rows = units == "rank positions",
      decimals = 0
    ) |>
    gt::gtsave(
      "outputs/gt_tables/Table_S3_raiw_sensitivity.png"
    )

  table_s4a_effort_correlations |>
    gt::gt() |>
    gt::tab_header(
      title =
        "Table S4a. Relationship between monitoring effort and invasion metrics"
    ) |>
    gt::fmt_number(
      columns = any_of(
        c(
          "rho",
          "p_value"
        )
      ),
      decimals = 3
    ) |>
    gt::gtsave(
      "outputs/gt_tables/Table_S4a_effort_correlations.png"
    )

  table_s4b_effort_standardised |>
    gt::gt() |>
    gt::tab_header(
      title =
        "Table S4b. Sensitivity analyses addressing unequal sampling effort"
    ) |>
    gt::cols_label(
      pattern = "Pattern",
      dataset = "Dataset",
      standardisation = "Standardisation",
      analytical_units = "Analytical units",
      rarefaction_iterations = "Rarefaction iterations",
      metric = "Metric",
      test = "Inferential test",
      statistic = "Statistic",
      degrees_freedom = "df",
      exact_permutations = "Exact permutations",
      p_value = "P-value",
      interpretation = "Interpretation"
    ) |>
    gt::fmt_number(
      columns = statistic,
      decimals = 3
    ) |>
    gt::fmt_number(
      columns = p_value,
      decimals = 4
    ) |>
    gt::fmt_number(
      columns = c(
        rarefaction_iterations,
        degrees_freedom,
        exact_permutations
      ),
      decimals = 0,
      use_seps = TRUE
    ) |>
    gt::tab_source_note(
      source_note = gt::md(
        paste0(
          "Repeated random subsampling (1,000 iterations) was used as a ",
          "sensitivity analysis to evaluate patterns after equalising sampling ",
          "effort. Inferential tests were conducted at the locality level using ",
          "exact permutation procedures and therefore did not treat individual ",
          "one-minute records or rarefaction iterations as independent ",
          "replicates. Temporal comparisons used annual monitoring cycles ",
          "(June-May). Bathymetric inference was restricted to the 0-2 m, ",
          "2.1-8 m, and 8.1-14 m strata because only one locality contributed ",
          "observations deeper than 14 m in 2025. Fisher's exact test was based ",
          "on observed locality-level presence/absence and was not rarefied."
        )
      )
    ) |>
    gt::gtsave(
      "outputs/gt_tables/Table_S4b_effort_standardised_sensitivity.png"
    )

} else {

  message(
    paste0(
      "gt/webshot2 not available: PNG supplementary tables were not exported. ",
      "CSV tables were generated successfully."
    )
  )
}

################################################################################
# 11. FINAL CHECKS AND SUMMARY
################################################################################

cat(
  "\n============================================================\n",
  "SUPPLEMENTARY TABLE SUMMARY\n",
  "============================================================\n",
  "Table S1 monitored localities: ",
  nrow(table_s1_localities),
  "\n",
  "Table S2 monitoring minutes: ",
  sum(table_s2_locality_metrics$total_effort_minutes),
  "\n",
  "Table S2 positive one-minute records: ",
  sum(table_s2_locality_metrics$positive_one_minute_records),
  "\n",
  "Table S2 positive localities: ",
  nrow(table_s2a_positive_localities),
  "\n",
  "Table S3 maximum absolute rank change: ",
  max_rank_change,
  "\n",
  "Table S4b inferential rows: ",
  nrow(table_s4b_effort_standardised),
  "\n",
  "============================================================\n",
  sep = ""
)

cat(
  "\nSupplementary CSV files exported:\n"
)

print(
  list.files(
    "outputs",
    pattern = "^Table_S.*\\.csv$"
  )
)

cat(
  "\nFinal effort-standardised sensitivity table:\n"
)

print(
  table_s4b_effort_standardised,
  n = Inf
)

