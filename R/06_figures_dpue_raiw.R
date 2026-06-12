################################################################################
# 06_figures_dpue_raiw.R
#
# Figure 4
#
# Panel A:
# Annual distribution of positive one-minute records by DAFOR category.
#
# Panel B:
# Annual RAI-W partitioned by bathymetric stratum.
################################################################################

source("R/00_setup.R")
source("R/01_prepare_monitoring_data.R")
source("R/02_prepare_site_year_metrics.R")

################################################################################
# 0. Remove obsolete Figure 4 outputs
################################################################################

old_figure_4_files <- c(
  "outputs/figure_4_annual_dpue_dafor.csv",
  "outputs/figure_4_annual_dpue_totals.csv",
  "outputs/figure_4_annual_locality_denominator.csv",
  "outputs/figure_4_annual_raiw_totals.csv",
  "outputs/figure_4_consistency_check.csv",
  "figs/fig_4_annual_DPUE_RAIW.png"
)

file.remove(old_figure_4_files[file.exists(old_figure_4_files)])

################################################################################
# 1. Parameters
################################################################################

dafor_levels <- c("D", "A", "F", "O", "R")

dafor_colors <- c(
  "D" = "#FDC827",
  "A" = "#F28349",
  "F" = "#C73E73",
  "O" = "#9011A3",
  "R" = "#450DA3"
)

depth_levels <- c(
  "0-2m",
  "2.1-8m",
  "8.1-14m",
  "14.1m+"
)

depth_colors <- c(
  "0-2m"    = "#db6d10",
  "2.1-8m"  = "#aaee4b",
  "8.1-14m" = "#416f02",
  "14.1m+"  = "#536e99"
)

################################################################################
# 2. Prepare minute-level dataset
################################################################################

df_annual_minutes <- df_monit |>
  mutate(
    year = lubridate::year(data),
    dafor_num = clean_num(dafor),
    
    dafor_cat = case_when(
      dafor_num == 10 ~ "D",
      dafor_num == 8  ~ "A",
      dafor_num == 6  ~ "F",
      dafor_num == 4  ~ "O",
      dafor_num == 2  ~ "R",
      TRUE ~ NA_character_
    ),
    
    prof_max_num = clean_num(prof_max),
    
    faixa_bat_depth = case_when(
      !is.na(prof_max_num) & prof_max_num <= 2 ~ "0-2m",
      !is.na(prof_max_num) & prof_max_num > 2  & prof_max_num <= 8  ~ "2.1-8m",
      !is.na(prof_max_num) & prof_max_num > 8  & prof_max_num <= 14 ~ "8.1-14m",
      !is.na(prof_max_num) & prof_max_num > 14 ~ "14.1m+",
      TRUE ~ NA_character_
    ),
    
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
  left_join(
    df_localidade |>
      select(
        localidade,
        shoreline_m = extent_m,
        uni100m = Uni100m
      ),
    by = "localidade"
  ) |>
  mutate(
    dafor_cat = factor(dafor_cat, levels = dafor_levels),
    faixa_bat_depth = factor(faixa_bat_depth, levels = depth_levels)
  )

################################################################################
# 3. Locality-year denominator for RAI-W
################################################################################

annual_locality_denominator <- df_annual_minutes |>
  group_by(localidade, year) |>
  summarise(
    effort_minutes = n(),
    effort_hours = effort_minutes / 60,
    uni100m = first(uni100m),
    denominator = effort_hours * uni100m,
    .groups = "drop"
  )

################################################################################
# 4. Annual annotations
#
# Total = total monitored one-minute records
# Abs. = one-minute records classified as Absent
################################################################################

annual_annotations <- df_annual_minutes |>
  group_by(year) |>
  summarise(
    total_minutes = n(),
    absent_minutes = sum(dafor_num == 0, na.rm = TRUE),
    positive_minutes = sum(dafor_num > 0, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    label = paste0(
      "Abs. ",
      absent_minutes,
      " | Tot. ",
      total_minutes
    )
  )

print(annual_annotations, n = Inf)

################################################################################
# 5. Panel A
# Annual distribution of positive one-minute records by DAFOR category
################################################################################

annual_positive_dafor <- df_annual_minutes |>
  filter(!is.na(dafor_cat)) |>
  group_by(year, dafor_cat) |>
  summarise(
    positive_records = n(),
    .groups = "drop"
  ) |>
  complete(
    year = sort(unique(df_annual_minutes$year)),
    dafor_cat = factor(dafor_levels, levels = dafor_levels),
    fill = list(positive_records = 0)
  ) |>
  mutate(
    year = factor(
      year,
      levels = sort(unique(df_annual_minutes$year))
    ),
    dafor_cat = factor(
      dafor_cat,
      levels = dafor_levels
    )
  )

positive_bar_tops <- annual_positive_dafor |>
  group_by(year) |>
  summarise(
    bar_top = sum(positive_records, na.rm = TRUE),
    .groups = "drop"
  ) |>
  left_join(
    annual_annotations |>
      mutate(
        year = factor(
          year,
          levels = levels(annual_positive_dafor$year)
        )
      ),
    by = "year"
  )

plot_positive_dafor_by_year <- ggplot(
  annual_positive_dafor,
  aes(
    x = year,
    y = positive_records,
    fill = dafor_cat
  )
) +
  geom_col(position = "stack") +
  geom_text(
    data = positive_bar_tops,
    aes(
      x = year,
      y = bar_top,
      label = label
    ),
    vjust = -0.45,
    size = 2.5,
    inherit.aes = FALSE
  ) +
  scale_fill_manual(
    values = dafor_colors,
    drop = FALSE,
    name = NULL
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.10))
  ) +
  labs(
    x = NULL,
    y = "Positive one-minute records"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(),
    panel.border = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.key.size = unit(0.8, "cm"),
    legend.position = "right"
  )

################################################################################
# 6. Panel B
# Annual RAI-W contribution by bathymetric stratum
################################################################################

annual_raiw_depth <- df_annual_minutes |>
  filter(!is.na(faixa_bat_depth)) |>
  group_by(localidade, year, faixa_bat_depth) |>
  summarise(
    sum_weight_depth = sum(weight, na.rm = TRUE),
    .groups = "drop"
  ) |>
  left_join(
    annual_locality_denominator,
    by = c("localidade", "year")
  ) |>
  mutate(
    raiw_depth = if_else(
      denominator > 0,
      sum_weight_depth / denominator,
      NA_real_
    )
  ) |>
  filter(
    sum_weight_depth > 0,
    is.finite(raiw_depth)
  ) |>
  group_by(year, faixa_bat_depth) |>
  summarise(
    total_raiw = sum(raiw_depth, na.rm = TRUE),
    .groups = "drop"
  ) |>
  complete(
    year = sort(unique(df_annual_minutes$year)),
    faixa_bat_depth = factor(depth_levels, levels = depth_levels),
    fill = list(total_raiw = 0)
  ) |>
  mutate(
    year = factor(
      year,
      levels = sort(unique(df_annual_minutes$year))
    ),
    faixa_bat_depth = factor(
      faixa_bat_depth,
      levels = depth_levels
    )
  )

print(annual_raiw_depth, n = Inf)

plot_raiw_by_year <- ggplot(
  annual_raiw_depth,
  aes(
    x = year,
    y = total_raiw,
    fill = faixa_bat_depth
  )
) +
  geom_col(position = "stack") +
  scale_fill_manual(
    values = depth_colors,
    drop = FALSE,
    name = "Depth (m)"
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    x = NULL,
    y = "RAI-W"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(),
    panel.border = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.key.size = unit(0.8, "cm"),
    legend.position = "right"
  )

################################################################################
# 7. Combine panels
################################################################################

figure_4 <- (
  plot_positive_dafor_by_year +
    guides(fill = guide_legend(ncol = 1))
) /
  (
    plot_raiw_by_year +
      guides(fill = guide_legend(ncol = 1))
  ) +
  plot_layout(
    heights = c(1, 1),
    guides = "keep"
  ) +
  plot_annotation(
    tag_levels = "A"
  ) &
  theme(
    plot.tag = element_text(face = "bold", size = 16),
    plot.tag.position = c(0, 1)
  )

figure_4

# ggsave(
#   filename = "figs/fig_4_annual_positive_RAIW.png",
#   plot = figure_4,
#   width = 18,
#   height = 15,
#   dpi = 300
# )

ggsave(
  filename = "figs/fig_4_annual_positive_RAIW.png",
  plot = figure_4,
  width = 8,
  height = 7,
  units = "in",
  dpi = 300
)



################################################################################
# 8. Export figure source values
################################################################################

write_csv(
  annual_annotations,
  "outputs/figure_4_annual_annotations.csv"
)

write_csv(
  annual_positive_dafor,
  "outputs/figure_4_annual_positive_dafor.csv"
)

write_csv(
  annual_raiw_depth,
  "outputs/figure_4_annual_raiw_depth.csv"
)

################################################################################
# 9. Checks
################################################################################

figure_4_positive_check <- annual_positive_dafor |>
  group_by(year) |>
  summarise(
    positive_records = sum(positive_records, na.rm = TRUE),
    .groups = "drop"
  ) |>
  left_join(
    annual_annotations |>
      mutate(
        year = factor(
          year,
          levels = levels(annual_positive_dafor$year)
        )
      ) |>
      select(year, positive_minutes),
    by = "year"
  ) |>
  mutate(
    difference = positive_records - positive_minutes
  )

cat("\nFigure 4 positive-record check\n")
print(figure_4_positive_check, n = Inf)

write_csv(
  figure_4_positive_check,
  "outputs/figure_4_positive_record_check.csv"
)

if (any(figure_4_positive_check$difference != 0, na.rm = TRUE)) {
  warning("Figure 4 positive-record totals do not match annual annotations.")
}

#source("R/06_figures_dpue_raiw.R")

