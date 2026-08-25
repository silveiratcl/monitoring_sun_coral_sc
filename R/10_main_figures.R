################################################################################
# 10_main_figures.R
#
# Main manuscript figures and figure source data
#
# Figures 2-5 are generated in R.
# Figure 6 is assembled in QGIS from the source table exported below.
################################################################################


source("R/00_setup.R")

df_localidade <- readRDS(
  "outputs/df_localidade_clean.rds"
)


df_monitoring_all <- readRDS(
  "outputs/monitoring_data_all.rds"
)


################################################################################
# FIGURE 2
################################################################################

# Use the complete final monitoring dataset prepared in script 07.
# This dataset includes all valid monitoring records from 2022-2025,
# including records without bathymetric information.

################################################################################
# 2.1. Parameters
################################################################################

depth_levels_fig2 <- c(
  "0-2m",
  "2.1-8m",
  "8.1-14m",
  "14.1m+",
  "NA"
)

depth_colors_fig2 <- c(
  "0-2m"    = "#db6d10",
  "2.1-8m"  = "#aaee4b",
  "8.1-14m" = "#416f02",
  "14.1m+"  = "#536e99",
  "NA"      = "#BDBDBD"
)

region_levels_fig2 <- c(
  "REBIO",
  "NEAR_REBIO",
  "SURROUNDINGS"
)

################################################################################
# 2.2. Prepare Figure 2 data
#
# One row in df_monitoring_all = one minute of monitoring effort.
#
# Bathymetric strata are derived from prof_max, following the same
# classification used in the final analytical pipeline.
#
# Records without prof_max are retained as "NA".
################################################################################

figure_2_data <- df_monitoring_all |>
  mutate(
    depth_stratum = case_when(
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
      
      TRUE ~ "NA"
    ),
    
    depth_stratum = factor(
      depth_stratum,
      levels = depth_levels_fig2
    ),
    
    region = factor(
      region,
      levels = region_levels_fig2
    )
  ) |>
  count(
    region,
    localidade,
    depth_stratum,
    name = "effort_minutes"
  )

################################################################################
# 2.3. Checks
################################################################################

figure_2_check <- figure_2_data |>
  summarise(
    total_minutes = sum(effort_minutes),
    n_localities = n_distinct(localidade)
  )

print(figure_2_check)

stopifnot(
  figure_2_check$total_minutes == 8415,
  figure_2_check$n_localities == 43
)

figure_2_depth_check <- figure_2_data |>
  group_by(depth_stratum) |>
  summarise(
    effort_minutes = sum(effort_minutes),
    .groups = "drop"
  )

print(figure_2_depth_check, n = Inf)

################################################################################
# 2.4. Figure 2
################################################################################

figure_2 <- figure_2_data |>
  ggplot(
    aes(
      x = effort_minutes,
      y = reorder(localidade, effort_minutes, FUN = sum),
      fill = depth_stratum
    )
  ) +
  geom_col(
    position = "stack",
    width = 0.8
  ) +
  facet_grid(
    rows = vars(region),
    scales = "free_y",
    space = "free_y",
    switch = "both",
    labeller = labeller(
      region = c(
        "REBIO" = "REBIO",
        "NEAR_REBIO" = "NEAR REBIO",
        "SURROUNDINGS" = "SURROUNDINGS"
      )
    )
  ) +
  scale_fill_manual(
    values = depth_colors_fig2,
    drop = FALSE,
    name = "Depth"
  ) +
  scale_x_continuous(
    position = "top",
    n.breaks = 10,
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    x = "Sampling effort (min)",
    y = NULL
  ) +
  theme(
    panel.background = element_blank(),
    panel.grid = element_blank(),
    
    axis.ticks.length.x = unit(0.2, "cm"),
    
    axis.ticks.x = element_line(
      colour = "grey",
      linewidth = 0.8
    ),
    
    axis.line.x = element_line(
      colour = "grey",
      linewidth = 0.8
    ),
    
    axis.ticks.y = element_blank(),
    
    axis.title.x = element_text(
      size = 14
    ),
    
    axis.title.y = element_blank(),
    
    axis.text.x = element_text(
      size = 12
    ),
    
    axis.text.y = element_text(
      size = 12
    ),
    
    legend.title = element_text(
      size = 12
    ),
    
    legend.text = element_text(
      size = 12
    ),
    
    legend.key.size = unit(
      0.8,
      "cm"
    ),
    
    panel.spacing = unit(
      1,
      "lines"
    ),
    
    strip.text.y = element_text(
      size = 10
    )
  )

figure_2

################################################################################
# 2.5. Export
################################################################################

ggsave(
  filename = "figs/fig_2_sampling_effort.png",
  plot = figure_2,
  width = 10,
  height = 15,
  units = "in",
  dpi = 300
)

write_csv(
  figure_2_data,
  "outputs/figure_2_sampling_effort.csv"
)

################################################################################
# end Figure 2
################################################################################


################################################################################
# FIGURE 3
# Positive one-minute records and sampling effort across depth
################################################################################

df_monitoring_depth <- readRDS(
  "outputs/monitoring_data_depth_complete.rds"
)

################################################################################
# 3.1. Parameters
################################################################################

depth_levels_fig3 <- c(
  "0-2 m",
  "2.1-8 m",
  "8.1-14 m",
  "14.1-20 m"
)

################################################################################
# 3.2. Prepare independent monitored transects
#
# dafor_id identifies a monitored transect.
# Minute-level records belonging to the same transect are reduced to one row
# before reconstructing the minimum-to-maximum depth interval surveyed.
################################################################################

transects_fig3 <- df_monitoring_depth |>
  mutate(
    prof_min_num = clean_num(prof_min),
    prof_max_num = clean_num(prof_max)
  ) |>
  filter(
    !is.na(dafor_id),
    !is.na(prof_min_num),
    !is.na(prof_max_num)
  ) |>
  group_by(
    localidade,
    data,
    dafor_id
  ) |>
  summarise(
    prof_min_num = min(prof_min_num, na.rm = TRUE),
    prof_max_num = max(prof_max_num, na.rm = TRUE),
    n_detection = max_or_na(n_trans_pres),
    .groups = "drop"
  ) |>
  mutate(
    # Figure 3 retains the original plotted depth domain of 0-20 m
    prof_min_num = pmax(
      0,
      pmin(20, prof_min_num)
    ),
    prof_max_num = pmax(
      0,
      pmin(20, prof_max_num)
    )
  ) |>
  filter(
    prof_max_num >= prof_min_num
  )

################################################################################
# 3.3. Panel A
# Number of positive one-minute records by bathymetric stratum
################################################################################

figure_3_detections <- df_monitoring_depth |>
  mutate(
    dafor_num = clean_num(dafor),
    
    depth_stratum = case_when(
      prof_max_num <= 2 ~
        "0-2 m",
      
      prof_max_num > 2 &
        prof_max_num <= 8 ~
        "2.1-8 m",
      
      prof_max_num > 8 &
        prof_max_num <= 14 ~
        "8.1-14 m",
      
      prof_max_num > 14 &
        prof_max_num <= 20 ~
        "14.1-20 m",
      
      TRUE ~ NA_character_
    ),
    
    depth_stratum = factor(
      depth_stratum,
      levels = depth_levels_fig3
    )
  ) |>
  filter(
    !is.na(depth_stratum),
    dafor_num > 0
  ) |>
  count(
    depth_stratum,
    name = "n_detection"
  ) |>
  complete(
    depth_stratum = factor(
      depth_levels_fig3,
      levels = depth_levels_fig3
    ),
    fill = list(
      n_detection = 0
    )
  )

print(
  figure_3_detections,
  n = Inf
)

################################################################################
# 3.4. Panel B
# Reconstruct cumulative sampling effort in 0.5-m depth intervals
################################################################################

depth_intervals_05 <- tibble(
  z_min = seq(
    0,
    19.5,
    by = 0.5
  ),
  z_max = seq(
    0.5,
    20,
    by = 0.5
  )
) |>
  mutate(
    z_mid = (
      z_min + z_max
    ) / 2
  )

figure_3_depth_effort <- transects_fig3 |>
  crossing(
    depth_intervals_05
  ) |>
  mutate(
    overlap_m = pmax(
      0,
      pmin(
        prof_max_num,
        z_max
      ) -
        pmax(
          prof_min_num,
          z_min
        )
    ),
    
    # Proportional contribution when a transect overlaps
    # only part of a 0.5-m depth interval
    transect_contribution =
      overlap_m / 0.5
  ) |>
  group_by(
    z_mid
  ) |>
  summarise(
    transect_effort =
      sum(
        transect_contribution,
        na.rm = TRUE
      ),
    .groups = "drop"
  )

print(
  figure_3_depth_effort,
  n = Inf
)

################################################################################
# 3.5. Checks
################################################################################

figure_3_check <- tibble(
  depth_complete_minutes =
    nrow(df_monitoring_depth),
  
  positive_depth_complete_minutes =
    sum(
      clean_num(df_monitoring_depth$dafor) > 0,
      na.rm = TRUE
    ),
  
  monitored_transects =
    nrow(transects_fig3),
  
  detections_panel_a =
    sum(
      figure_3_detections$n_detection,
      na.rm = TRUE
    )
)

print(
  figure_3_check
)

stopifnot(
  figure_3_check$depth_complete_minutes == 8185,
  figure_3_check$positive_depth_complete_minutes == 173,
  figure_3_check$detections_panel_a == 173
)

################################################################################
# 3.6. Panel A
################################################################################

plot_figure_3a <- ggplot(
  figure_3_detections,
  aes(
    x = depth_stratum,
    y = n_detection
  )
) +
  geom_col(
    fill = "#db6c10",
    linewidth = 0.3,
    alpha = 0.85
  ) +
  labs(
    x = "Bathymetric stratum",
    y = "Positive one-minute records"
  ) +
  scale_y_continuous(
    limits = c(
      0,
      ceiling(
        max(
          figure_3_detections$n_detection,
          na.rm = TRUE
        ) / 10
      ) * 10
    ),
    breaks = seq(
      0,
      ceiling(
        max(
          figure_3_detections$n_detection,
          na.rm = TRUE
        ) / 10
      ) * 10,
      by = 20
    ),
    expand = expansion(
      mult = c(0, 0.02)
    )
  ) +
  theme(
    panel.background = element_blank(),
    panel.grid = element_blank(),
    
    axis.ticks.length.x =
      unit(0.2, "cm"),
    
    axis.ticks.x =
      element_line(
        colour = "grey",
        linewidth = 0.8
      ),
    
    axis.line.x =
      element_line(
        colour = "grey",
        linewidth = 0.8
      ),
    
    axis.line.y =
      element_line(
        colour = "grey",
        linewidth = 0.8
      ),
    
    axis.title =
      element_text(
        size = 14
      ),
    
    axis.text =
      element_text(
        size = 12
      )
  )

################################################################################
# 3.7. Panel B
################################################################################


plot_figure_3b <- ggplot(
  figure_3_depth_effort,
  aes(
    x = z_mid,
    y = transect_effort
  )
) +
  geom_col(
    width = 0.5,
    fill = "royalblue"
  ) +
  labs(
    x = "Depth (m)",
    y = "Sampling effort"
  ) +
  scale_y_continuous(
    limits = c(
      0,
      ceiling(
        max(
          figure_3_depth_effort$transect_effort,
          na.rm = TRUE
        ) / 10
      ) * 10
    ),
    breaks = seq(
      0,
      ceiling(
        max(
          figure_3_depth_effort$transect_effort,
          na.rm = TRUE
        ) / 10
      ) * 10,
      by = 10
    ),
    expand = expansion(
      mult = c(0, 0.02)
    )
  ) +
  
  scale_x_continuous(
    breaks = seq(
      0,
      20,
      by = 2
    ),
    limits = c(
      0,
      20
    )
  ) +
  theme(
    panel.background = element_blank(),
    panel.grid = element_blank(),
    
    axis.ticks.length.x =
      unit(0.2, "cm"),
    
    axis.ticks.x =
      element_line(
        colour = "grey",
        linewidth = 0.8
      ),
    
    axis.line.x =
      element_line(
        colour = "grey",
        linewidth = 0.8
      ),
    
    axis.line.y =
      element_line(
        colour = "grey",
        linewidth = 0.8
      ),
    
    axis.title =
      element_text(
        size = 14
      ),
    
    axis.text =
      element_text(
        size = 12
      )
  )

################################################################################
# 3.8. Combine Figure 3
################################################################################

figure_3 <- (
  plot_figure_3a /
    plot_figure_3b
) +
  plot_layout(
    heights = c(1, 1)
  ) +
  plot_annotation(
    tag_levels = "A"
  ) &
  theme(
    plot.tag =
      element_text(
        face = "bold",
        size = 16
      )
  )

figure_3

################################################################################
# 3.9. Export
################################################################################

ggsave(
  filename = "figs/fig_3_detections_depth_sampling.png",
  plot = figure_3,
  width = 8,
  height = 7,
  units = "in",
  dpi = 300
)

write_csv(
  figure_3_detections,
  "outputs/figure_3_detections_by_depth.csv"
)

write_csv(
  figure_3_depth_effort,
  "outputs/figure_3_sampling_by_depth.csv"
)

################################################################################
# end Figure 3
################################################################################

################################################################################
# FIGURE 4
# Annual distribution of positive records and RAI-W
#
# Panel A:
# Positive one-minute records by DAFOR category
#
# Panel B:
# Annual RAI-W contribution by bathymetric stratum
################################################################################


################################################################################
# 4.1. Parameters
################################################################################

dafor_levels_fig4 <- c(
  "D",
  "A",
  "F",
  "O",
  "R"
)

dafor_colors_fig4 <- c(
  "D" = "#FDC827",
  "A" = "#F28349",
  "F" = "#C73E73",
  "O" = "#9011A3",
  "R" = "#450DA3"
)

depth_levels_fig4 <- c(
  "0-2m",
  "2.1-8m",
  "8.1-14m",
  "14.1m+"
)

depth_colors_fig4 <- c(
  "0-2m"    = "#db6d10",
  "2.1-8m"  = "#aaee4b",
  "8.1-14m" = "#416f02",
  "14.1m+"  = "#536e99"
)


################################################################################
# 4.2. Prepare minute-level data
################################################################################

figure_4_minutes <- df_monitoring_all |>
  mutate(
    dafor_num = clean_num(dafor),
    
    dafor_cat = case_when(
      dafor_num == 10 ~ "D",
      dafor_num == 8  ~ "A",
      dafor_num == 6  ~ "F",
      dafor_num == 4  ~ "O",
      dafor_num == 2  ~ "R",
      TRUE            ~ NA_character_
    ),
    
    depth_stratum = case_when(
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
      
      TRUE ~ NA_character_
    ),
    
    weight = unname(
      manual_weights[
        as.character(dafor_num)
      ]
    ),
    
    weight = coalesce(
      weight,
      0
    ),
    
    dafor_cat = factor(
      dafor_cat,
      levels = dafor_levels_fig4
    ),
    
    depth_stratum = factor(
      depth_stratum,
      levels = depth_levels_fig4
    )
  ) |>
  left_join(
    df_localidade |>
      select(
        localidade,
        shoreline_m = extent_m,
        uni100m = Uni100m
      ),
    by = "localidade"
  )


################################################################################
# 4.3. Dataset checks
################################################################################

stopifnot(
  nrow(figure_4_minutes) == 8415,
  
  sum(
    figure_4_minutes$dafor_num > 0,
    na.rm = TRUE
  ) == 173,
  
  n_distinct(
    figure_4_minutes$localidade
  ) == 43
)


################################################################################
# 4.4. Locality-year denominator for RAI-W
################################################################################

figure_4_denominator <- figure_4_minutes |>
  group_by(
    localidade,
    year
  ) |>
  summarise(
    effort_minutes = n(),
    
    effort_hours =
      effort_minutes / 60,
    
    uni100m =
      first(uni100m),
    
    denominator =
      effort_hours * uni100m,
    
    .groups = "drop"
  )


################################################################################
# 4.5. Annual annotations
#
# Abs. = one-minute records classified as Absent
# Tot. = total one-minute monitoring records
################################################################################

figure_4_annotations <- figure_4_minutes |>
  group_by(year) |>
  summarise(
    total_minutes = n(),
    
    absent_minutes =
      sum(
        dafor_num == 0,
        na.rm = TRUE
      ),
    
    positive_minutes =
      sum(
        dafor_num > 0,
        na.rm = TRUE
      ),
    
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

print(
  figure_4_annotations,
  n = Inf
)


################################################################################
# 4.6. Panel A data
# Positive one-minute records by year and DAFOR category
################################################################################

figure_4_positive_dafor <- figure_4_minutes |>
  filter(
    !is.na(dafor_cat)
  ) |>
  group_by(
    year,
    dafor_cat
  ) |>
  summarise(
    positive_records = n(),
    .groups = "drop"
  ) |>
  complete(
    year =
      sort(
        unique(
          figure_4_minutes$year
        )
      ),
    
    dafor_cat = factor(
      dafor_levels_fig4,
      levels = dafor_levels_fig4
    ),
    
    fill = list(
      positive_records = 0
    )
  ) |>
  mutate(
    year = factor(
      year,
      levels =
        sort(
          unique(
            figure_4_minutes$year
          )
        )
    ),
    
    dafor_cat = factor(
      dafor_cat,
      levels = dafor_levels_fig4
    )
  )


figure_4_positive_tops <- figure_4_positive_dafor |>
  group_by(year) |>
  summarise(
    bar_top =
      sum(
        positive_records,
        na.rm = TRUE
      ),
    
    .groups = "drop"
  ) |>
  left_join(
    figure_4_annotations |>
      mutate(
        year = factor(
          year,
          levels =
            levels(
              figure_4_positive_dafor$year
            )
        )
      ),
    
    by = "year"
  )


################################################################################
# 4.7. Check annual positive records
################################################################################

figure_4_annual_positive_check <-
  figure_4_positive_dafor |>
  group_by(year) |>
  summarise(
    positive_records =
      sum(
        positive_records,
        na.rm = TRUE
      ),
    
    .groups = "drop"
  )

print(
  figure_4_annual_positive_check,
  n = Inf
)

stopifnot(
  figure_4_annual_positive_check$
    positive_records ==
    c(
      7,
      30,
      44,
      92
    )
)


################################################################################
# 4.8. Panel A
################################################################################

plot_figure_4a <- ggplot(
  figure_4_positive_dafor,
  aes(
    x = year,
    y = positive_records,
    fill = dafor_cat
  )
) +
  geom_col(
    position = "stack"
  ) +
  
  geom_text(
    data = figure_4_positive_tops,
    
    aes(
      x = year,
      y = bar_top,
      label = label
    ),
    
    vjust = -0.45,
    size = 3,
    inherit.aes = FALSE
  ) +
  
  scale_fill_manual(
    values = dafor_colors_fig4,
    drop = FALSE,
    name = "DAFOR category"
  ) +
  
  scale_y_continuous(
    limits = c(
      0,
      100
    ),
    
    breaks = seq(
      0,
      100,
      by = 20
    ),
    
    expand = expansion(
      mult = c(
        0,
        0.04
      )
    )
  ) +
  
  labs(
    x = NULL,
    y = "Positive one-minute records"
  ) +
  
  theme_minimal(
    base_size = 16
  ) +
  
  theme(
    panel.grid =
      element_blank(),
    
    axis.line =
      element_line(),
    
    panel.border =
      element_blank(),
    
    axis.title.x =
      element_blank(),
    
    axis.text.x =
      element_blank(),
    
    axis.text.y =
      element_text(
        size = 12
      ),
    
    axis.title.y =
      element_text(
        size = 14
      ),
    
    legend.title =
      element_text(
        size = 12
      ),
    
    legend.text =
      element_text(
        size = 12
      ),
    
    legend.key.size =
      unit(
        0.8,
        "cm"
      ),
    
    legend.position =
      "right"
  )


################################################################################
# 4.9. Check bathymetric information
################################################################################

figure_4_depth_check <- figure_4_minutes |>
  summarise(
    records_with_depth =
      sum(
        !is.na(depth_stratum)
      ),
    
    records_without_depth =
      sum(
        is.na(depth_stratum)
      ),
    
    positive_with_depth =
      sum(
        dafor_num > 0 &
          !is.na(depth_stratum),
        na.rm = TRUE
      ),
    
    positive_without_depth =
      sum(
        dafor_num > 0 &
          is.na(depth_stratum),
        na.rm = TRUE
      )
  )

print(
  figure_4_depth_check
)

stopifnot(
  figure_4_depth_check$
    records_with_depth == 8185,
  
  figure_4_depth_check$
    records_without_depth == 230,
  
  figure_4_depth_check$
    positive_with_depth == 173,
  
  figure_4_depth_check$
    positive_without_depth == 0
)


################################################################################
# 4.10. Panel B data
# Annual RAI-W contribution by bathymetric stratum
################################################################################

figure_4_raiw_depth <- figure_4_minutes |>
  filter(
    !is.na(depth_stratum)
  ) |>
  group_by(
    localidade,
    year,
    depth_stratum
  ) |>
  summarise(
    sum_weight_depth =
      sum(
        weight,
        na.rm = TRUE
      ),
    
    .groups = "drop"
  ) |>
  left_join(
    figure_4_denominator,
    
    by = c(
      "localidade",
      "year"
    )
  ) |>
  mutate(
    raiw_depth = if_else(
      denominator > 0,
      sum_weight_depth /
        denominator,
      NA_real_
    )
  ) |>
  filter(
    sum_weight_depth > 0,
    is.finite(raiw_depth)
  ) |>
  group_by(
    year,
    depth_stratum
  ) |>
  summarise(
    total_raiw =
      sum(
        raiw_depth,
        na.rm = TRUE
      ),
    
    .groups = "drop"
  ) |>
  complete(
    year =
      sort(
        unique(
          figure_4_minutes$year
        )
      ),
    
    depth_stratum = factor(
      depth_levels_fig4,
      levels = depth_levels_fig4
    ),
    
    fill = list(
      total_raiw = 0
    )
  ) |>
  mutate(
    year = factor(
      year,
      
      levels =
        sort(
          unique(
            figure_4_minutes$year
          )
        )
    ),
    
    depth_stratum = factor(
      depth_stratum,
      levels = depth_levels_fig4
    )
  )

print(
  figure_4_raiw_depth,
  n = Inf
)


################################################################################
# 4.11. Calculate explicit upper limit for Panel B
################################################################################

figure_4_raiw_totals <- figure_4_raiw_depth |>
  group_by(year) |>
  summarise(
    annual_raiw =
      sum(
        total_raiw,
        na.rm = TRUE
      ),
    
    .groups = "drop"
  )

print(
  figure_4_raiw_totals,
  n = Inf
)


# Create a "nice" axis whose final tick is always visible

figure_4b_pretty_breaks <- pretty(
  c(
    0,
    max(
      figure_4_raiw_totals$annual_raiw,
      na.rm = TRUE
    )
  ),
  n = 5
)

figure_4b_ymax <- min(
  figure_4b_pretty_breaks[
    figure_4b_pretty_breaks >=
      max(
        figure_4_raiw_totals$annual_raiw,
        na.rm = TRUE
      )
  ]
)

figure_4b_breaks <-
  figure_4b_pretty_breaks[
    figure_4b_pretty_breaks >= 0 &
      figure_4b_pretty_breaks <=
      figure_4b_ymax
  ]


################################################################################
# 4.12. Panel B
################################################################################

plot_figure_4b <- ggplot(
  figure_4_raiw_depth,
  aes(
    x = year,
    y = total_raiw,
    fill = depth_stratum
  )
) +
  geom_col(
    position = "stack"
  ) +
  
  scale_fill_manual(
    values = depth_colors_fig4,
    drop = FALSE,
    name = "Bathymetric stratum"
  ) +
  
  scale_y_continuous(
    limits = c(
      0,
      figure_4b_ymax
    ),
    
    breaks =
      figure_4b_breaks,
    
    expand = expansion(
      mult = c(
        0,
        0.02
      )
    )
  ) +
  
  labs(
    x = "Year",
    y = "RAI-W"
  ) +
  
  theme_minimal(
    base_size = 16
  ) +
  
  theme(
    panel.grid =
      element_blank(),
    
    axis.line =
      element_line(),
    
    panel.border =
      element_blank(),
    
    axis.text.x =
      element_text(
        size = 12
      ),
    
    axis.text.y =
      element_text(
        size = 12
      ),
    
    axis.title.x =
      element_text(
        size = 14
      ),
    
    axis.title.y =
      element_text(
        size = 14
      ),
    
    legend.title =
      element_text(
        size = 12
      ),
    
    legend.text =
      element_text(
        size = 12
      ),
    
    legend.key.size =
      unit(
        0.8,
        "cm"
      ),
    
    legend.position =
      "right"
  )


################################################################################
# 4.13. Combine Figure 4
################################################################################

figure_4 <- (
  plot_figure_4a /
    plot_figure_4b
) +
  plot_layout(
    heights = c(
      1,
      1
    ),
    
    guides = "keep"
  ) +
  plot_annotation(
    tag_levels = "A"
  ) &
  
  theme(
    plot.tag =
      element_text(
        face = "bold",
        size = 16
      )
  )

figure_4


################################################################################
# 4.14. Export Figure 4
################################################################################

ggsave(
  filename =
    "figs/fig_4_annual_positive_RAIW.png",
  
  plot =
    figure_4,
  
  width = 8,
  height = 7,
  units = "in",
  dpi = 300
)


################################################################################
# 4.15. Export Figure 4 source values
################################################################################

write_csv(
  figure_4_annotations,
  "outputs/figure_4_annual_annotations.csv"
)

write_csv(
  figure_4_positive_dafor,
  "outputs/figure_4_annual_positive_dafor.csv"
)

write_csv(
  figure_4_raiw_depth,
  "outputs/figure_4_annual_raiw_depth.csv"
)


################################################################################
# 4.16. Final consistency check
################################################################################

figure_4_positive_check <-
  figure_4_positive_dafor |>
  group_by(year) |>
  summarise(
    positive_records =
      sum(
        positive_records,
        na.rm = TRUE
      ),
    
    .groups = "drop"
  ) |>
  left_join(
    figure_4_annotations |>
      mutate(
        year = factor(
          year,
          levels =
            levels(
              figure_4_positive_dafor$year
            )
        )
      ) |>
      select(
        year,
        positive_minutes
      ),
    
    by = "year"
  ) |>
  mutate(
    difference =
      positive_records -
      positive_minutes
  )

cat(
  "\nFigure 4 positive-record check\n"
)

print(
  figure_4_positive_check,
  n = Inf
)

stopifnot(
  all(
    figure_4_positive_check$
      difference == 0
  )
)

write_csv(
  figure_4_positive_check,
  "outputs/figure_4_positive_record_check.csv"
)


################################################################################
# end Figure 4
################################################################################

################################################################################
# FIGURE 5
# Pooled locality-level DPUE and RAI-W partitioned by bathymetric stratum
#
# Panel A:
# Detections per Unit Effort (DPUE)
#
# Panel B:
# weighted Relative Abundance Index (RAI-W)
################################################################################


################################################################################
# 5.1. Parameters
################################################################################

depth_levels_fig5 <- c(
  "0-2m",
  "2.1-8m",
  "8.1-14m",
  "14.1m+"
)

depth_colors_fig5 <- c(
  "0-2m"    = "#db6d10",
  "2.1-8m"  = "#aaee4b",
  "8.1-14m" = "#416f02",
  "14.1m+"  = "#536e99"
)


################################################################################
# 5.2. Prepare minute-level data
#
# One row in df_monitoring_all = one minute.
#
# The denominator is calculated across ALL valid monitoring minutes for each
# locality.
#
# Bathymetric strata partition only the DPUE and RAI-W numerators.
#
# Therefore:
#
# sum(depth-specific DPUE contributions) = pooled locality DPUE
# sum(depth-specific RAI-W contributions) = pooled locality RAI-W
################################################################################

figure_5_minutes <- df_monitoring_all |>
  mutate(
    dafor_num = clean_num(dafor),
    
    depth_stratum = case_when(
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
      
      TRUE ~ NA_character_
    ),
    
    depth_stratum = factor(
      depth_stratum,
      levels = depth_levels_fig5
    ),
    
    weight = unname(
      manual_weights[
        as.character(dafor_num)
      ]
    ),
    
    weight = coalesce(
      weight,
      0
    )
  ) |>
  left_join(
    df_localidade |>
      select(
        localidade,
        shoreline_m = extent_m,
        uni100m = Uni100m
      ),
    by = "localidade"
  )


################################################################################
# 5.3. Dataset checks
################################################################################

stopifnot(
  nrow(figure_5_minutes) == 8415,
  
  sum(
    figure_5_minutes$dafor_num > 0,
    na.rm = TRUE
  ) == 173,
  
  n_distinct(
    figure_5_minutes$localidade
  ) == 43,
  
  sum(
    is.na(
      figure_5_minutes$uni100m
    )
  ) == 0
)


################################################################################
# 5.4. Pooled locality metrics
#
# Pool numerators and total sampling effort before calculating the metrics.
# This is the same locality-level logic used in Supplementary Table S2.
################################################################################

figure_5_locality_totals <- figure_5_minutes |>
  group_by(
    localidade,
    region
  ) |>
  summarise(
    total_effort_minutes = n(),
    
    total_effort_hours =
      total_effort_minutes / 60,
    
    total_positive_records =
      sum(
        dafor_num > 0,
        na.rm = TRUE
      ),
    
    total_weighted_score =
      sum(
        weight,
        na.rm = TRUE
      ),
    
    shoreline_m =
      first(shoreline_m),
    
    uni100m =
      first(uni100m),
    
    .groups = "drop"
  ) |>
  mutate(
    pooled_denominator =
      total_effort_hours *
      uni100m,
    
    pooled_dpue = if_else(
      pooled_denominator > 0,
      
      total_positive_records /
        pooled_denominator,
      
      NA_real_
    ),
    
    pooled_raiw = if_else(
      pooled_denominator > 0,
      
      total_weighted_score /
        pooled_denominator,
      
      NA_real_
    )
  )


################################################################################
# 5.5. Check pooled locality totals
################################################################################

stopifnot(
  nrow(
    figure_5_locality_totals
  ) == 43,
  
  sum(
    figure_5_locality_totals$
      total_effort_minutes
  ) == 8415,
  
  sum(
    figure_5_locality_totals$
      total_positive_records
  ) == 173,
  
  sum(
    figure_5_locality_totals$
      total_positive_records > 0
  ) == 15
)


cat(
  "\nFigure 5 pooled locality metrics\n"
)

print(
  figure_5_locality_totals |>
    filter(
      total_positive_records > 0
    ) |>
    arrange(
      desc(pooled_dpue)
    ) |>
    select(
      localidade,
      region,
      total_effort_minutes,
      total_positive_records,
      pooled_dpue,
      pooled_raiw
    ),
  n = Inf
)


################################################################################
# 5.6. Numerators by bathymetric stratum
#
# All 173 positive records contain bathymetric information.
#
# Records without depth remain part of the locality-level sampling denominator
# but contribute zero to the depth-specific numerators.
################################################################################

figure_5_depth_numerators <- figure_5_minutes |>
  filter(
    !is.na(depth_stratum)
  ) |>
  group_by(
    localidade,
    depth_stratum
  ) |>
  summarise(
    positive_records_depth =
      sum(
        dafor_num > 0,
        na.rm = TRUE
      ),
    
    weighted_score_depth =
      sum(
        weight,
        na.rm = TRUE
      ),
    
    .groups = "drop"
  )


################################################################################
# 5.7. Restrict Figure 5 to positive localities
#
# Localities without detections remain reported in Supplementary Table S2b.
################################################################################

figure_5_positive_localities <-
  figure_5_locality_totals |>
  filter(
    total_positive_records > 0
  )


stopifnot(
  nrow(
    figure_5_positive_localities
  ) == 15
)


################################################################################
# 5.8. Complete locality x depth combinations
################################################################################

figure_5_plot_values <- expand_grid(
  localidade =
    figure_5_positive_localities$
    localidade,
  
  depth_stratum =
    depth_levels_fig5
) |>
  mutate(
    depth_stratum = factor(
      depth_stratum,
      levels = depth_levels_fig5
    )
  ) |>
  left_join(
    figure_5_depth_numerators,
    
    by = c(
      "localidade",
      "depth_stratum"
    )
  ) |>
  mutate(
    positive_records_depth =
      coalesce(
        positive_records_depth,
        0L
      ),
    
    weighted_score_depth =
      coalesce(
        weighted_score_depth,
        0
      )
  ) |>
  left_join(
    figure_5_positive_localities |>
      select(
        localidade,
        region,
        pooled_denominator,
        pooled_dpue,
        pooled_raiw
      ),
    
    by = "localidade"
  ) |>
  mutate(
    dpue_depth = if_else(
      pooled_denominator > 0,
      
      positive_records_depth /
        pooled_denominator,
      
      NA_real_
    ),
    
    raiw_depth = if_else(
      pooled_denominator > 0,
      
      weighted_score_depth /
        pooled_denominator,
      
      NA_real_
    )
  )


################################################################################
# 5.9. Check bathymetric numerators
################################################################################

stopifnot(
  sum(
    figure_5_plot_values$
      positive_records_depth,
    na.rm = TRUE
  ) == 173
)


################################################################################
# 5.10. Verify that stacked values reproduce pooled metrics
################################################################################

figure_5_stack_check <- figure_5_plot_values |>
  group_by(
    localidade
  ) |>
  summarise(
    stacked_dpue =
      sum(
        dpue_depth,
        na.rm = TRUE
      ),
    
    stacked_raiw =
      sum(
        raiw_depth,
        na.rm = TRUE
      ),
    
    pooled_dpue =
      first(
        pooled_dpue
      ),
    
    pooled_raiw =
      first(
        pooled_raiw
      ),
    
    dpue_difference =
      stacked_dpue -
      pooled_dpue,
    
    raiw_difference =
      stacked_raiw -
      pooled_raiw,
    
    .groups = "drop"
  )


cat(
  "\nFigure 5 stacked-metric consistency check\n"
)

print(
  figure_5_stack_check,
  n = Inf
)


stopifnot(
  max(
    abs(
      figure_5_stack_check$
        dpue_difference
    ),
    na.rm = TRUE
  ) < 1e-10,
  
  max(
    abs(
      figure_5_stack_check$
        raiw_difference
    ),
    na.rm = TRUE
  ) < 1e-10
)


################################################################################
# 5.11. Check expected relationship between DPUE and RAI-W
#
# All DAFOR weights are <= 1 and both metrics use the same denominator.
################################################################################

stopifnot(
  all(
    figure_5_locality_totals$
      pooled_raiw <=
      figure_5_locality_totals$
      pooled_dpue +
      1e-12,
    na.rm = TRUE
  )
)


################################################################################
# 5.12. Independent locality rankings
#
# IMPORTANT:
#
# Panel A is independently ranked by pooled DPUE.
# Panel B is independently ranked by pooled RAI-W.
#
# Factor levels are stored from LOWEST to HIGHEST because ggplot displays the
# final factor level at the top of a horizontal discrete axis.
################################################################################


# DPUE ranking: highest to lowest for inspection

figure_5_dpue_ranking <-
  figure_5_positive_localities |>
  arrange(
    desc(pooled_dpue)
  ) |>
  select(
    localidade,
    pooled_dpue
  )


# RAI-W ranking: highest to lowest for inspection

figure_5_raiw_ranking <-
  figure_5_positive_localities |>
  arrange(
    desc(pooled_raiw)
  ) |>
  select(
    localidade,
    pooled_raiw
  )


cat(
  "\nFigure 5 DPUE ranking\n"
)

print(
  figure_5_dpue_ranking,
  n = Inf
)


cat(
  "\nFigure 5 RAI-W ranking\n"
)

print(
  figure_5_raiw_ranking,
  n = Inf
)


################################################################################
# 5.13. Explicit factor orders for plotting
################################################################################


# Ascending factor order:
# lowest at bottom -> highest at top

figure_5_dpue_order <-
  figure_5_positive_localities |>
  arrange(
    pooled_dpue
  ) |>
  pull(
    localidade
  ) |>
  as.character()


figure_5_raiw_order <-
  figure_5_positive_localities |>
  arrange(
    pooled_raiw
  ) |>
  pull(
    localidade
  ) |>
  as.character()


# Separate plotting datasets are essential because each panel uses
# an independent locality order.

figure_5_plot_dpue <- figure_5_plot_values |>
  mutate(
    localidade = factor(
      as.character(localidade),
      levels = figure_5_dpue_order
    )
  )


figure_5_plot_raiw <- figure_5_plot_values |>
  mutate(
    localidade = factor(
      as.character(localidade),
      levels = figure_5_raiw_order
    )
  )


################################################################################
# 5.14. Verify plotted ordering
################################################################################

cat(
  "\nPanel A order, TOP to BOTTOM\n"
)

print(
  rev(
    levels(
      figure_5_plot_dpue$localidade
    )
  )
)


cat(
  "\nPanel B order, TOP to BOTTOM\n"
)

print(
  rev(
    levels(
      figure_5_plot_raiw$localidade
    )
  )
)


stopifnot(
  identical(
    rev(
      levels(
        figure_5_plot_dpue$
          localidade
      )
    ),
    
    as.character(
      figure_5_dpue_ranking$
        localidade
    )
  ),
  
  identical(
    rev(
      levels(
        figure_5_plot_raiw$
          localidade
      )
    ),
    
    as.character(
      figure_5_raiw_ranking$
        localidade
    )
  )
)


################################################################################
# 5.15. Explicit X-axis limits and breaks
################################################################################


# DPUE

figure_5_dpue_max <-
  max(
    figure_5_positive_localities$
      pooled_dpue,
    na.rm = TRUE
  )


figure_5_dpue_pretty <-
  pretty(
    c(
      0,
      figure_5_dpue_max
    ),
    n = 5
  )


figure_5_dpue_xmax <-
  min(
    figure_5_dpue_pretty[
      figure_5_dpue_pretty >=
        figure_5_dpue_max
    ]
  )


figure_5_dpue_breaks <-
  figure_5_dpue_pretty[
    figure_5_dpue_pretty >= 0 &
      figure_5_dpue_pretty <=
      figure_5_dpue_xmax
  ]


# RAI-W

figure_5_raiw_max <-
  max(
    figure_5_positive_localities$
      pooled_raiw,
    na.rm = TRUE
  )


figure_5_raiw_pretty <-
  pretty(
    c(
      0,
      figure_5_raiw_max
    ),
    n = 5
  )


figure_5_raiw_xmax <-
  min(
    figure_5_raiw_pretty[
      figure_5_raiw_pretty >=
        figure_5_raiw_max
    ]
  )


figure_5_raiw_breaks <-
  figure_5_raiw_pretty[
    figure_5_raiw_pretty >= 0 &
      figure_5_raiw_pretty <=
      figure_5_raiw_xmax
  ]


################################################################################
# 5.16. Common theme
################################################################################

theme_figure_5 <- theme(
  panel.background =
    element_blank(),
  
  panel.grid =
    element_blank(),
  
  axis.ticks.length.x =
    unit(
      0.2,
      "cm"
    ),
  
  axis.ticks.x =
    element_line(
      colour = "grey",
      linewidth = 0.8
    ),
  
  axis.line.x =
    element_line(
      colour = "grey",
      linewidth = 0.8
    ),
  
  axis.ticks.y =
    element_blank(),
  
  axis.title.x.top =
    element_text(
      size = 14
    ),
  
  axis.title.y =
    element_blank(),
  
  axis.text.x =
    element_text(
      size = 12
    ),
  
  axis.text.y =
    element_text(
      size = 12
    ),
  
  legend.title =
    element_text(
      size = 12
    ),
  
  legend.text =
    element_text(
      size = 12
    ),
  
  legend.key.size =
    unit(
      0.8,
      "cm"
    ),
  
  plot.margin =
    margin(
      8,
      8,
      8,
      8
    )
)


################################################################################
# 5.17. Panel A - DPUE
#
# Independently ordered by pooled DPUE.
################################################################################

plot_figure_5a <- ggplot(
  figure_5_plot_dpue,
  aes(
    x = dpue_depth,
    y = localidade,
    fill = depth_stratum
  )
) +
  geom_col(
    position = "stack",
    width = 0.8
  ) +
  
  scale_y_discrete(
    limits = figure_5_dpue_order
  ) +
  
  scale_fill_manual(
    values =
      depth_colors_fig5,
    
    drop = FALSE,
    
    name =
      "Bathymetric stratum"
  ) +
  
  scale_x_continuous(
    position = "top",
    
    limits = c(
      0,
      figure_5_dpue_xmax
    ),
    
    breaks =
      figure_5_dpue_breaks,
    
    expand = expansion(
      mult = c(
        0,
        0.02
      )
    )
  ) +
  
  labs(
    x = "DPUE",
    y = NULL
  ) +
  
  theme_figure_5


################################################################################
# 5.18. Panel B - RAI-W
#
# Independently ordered by pooled RAI-W.
################################################################################

plot_figure_5b <- ggplot(
  figure_5_plot_raiw,
  aes(
    x = raiw_depth,
    y = localidade,
    fill = depth_stratum
  )
) +
  geom_col(
    position = "stack",
    width = 0.8
  ) +
  
  scale_y_discrete(
    limits = figure_5_raiw_order
  ) +
  
  scale_fill_manual(
    values =
      depth_colors_fig5,
    
    drop = FALSE,
    
    name =
      "Bathymetric stratum"
  ) +
  
  scale_x_continuous(
    position = "top",
    
    limits = c(
      0,
      figure_5_raiw_xmax
    ),
    
    breaks =
      figure_5_raiw_breaks,
    
    expand = expansion(
      mult = c(
        0,
        0.02
      )
    )
  ) +
  
  labs(
    x = "RAI-W",
    y = NULL
  ) +
  
  theme_figure_5


################################################################################
# 5.19. Combine Figure 5
################################################################################

figure_5 <- (
  plot_figure_5a +
    plot_figure_5b
) +
  plot_layout(
    ncol = 2,
    guides = "collect"
  ) +
  plot_annotation(
    tag_levels = "A"
  ) &
  
  theme(
    legend.position =
      "bottom",
    
    plot.tag =
      element_text(
        face = "bold",
        size = 16
      ),
    
    plot.tag.position =
      c(
        0,
        1
      )
  )


figure_5


################################################################################
# 5.20. Export Figure 5
################################################################################

ggsave(
  filename =
    "figs/fig_5_dpue_raiw.png",
  
  plot =
    figure_5,
  
  width = 12,
  height = 5,
  units = "in",
  dpi = 300
)


################################################################################
# 5.21. Export Figure 5 source values
################################################################################

write_csv(
  figure_5_locality_totals,
  "outputs/figure_5_locality_metrics.csv"
)

write_csv(
  figure_5_plot_values,
  "outputs/figure_5_metrics_by_depth.csv"
)

write_csv(
  figure_5_stack_check,
  "outputs/figure_5_stack_consistency_check.csv"
)


################################################################################
# end Figure 5
################################################################################

################################################################################
# FIGURE 6
# Export locality-level data for map preparation in QGIS
#
# Figure 6 is assembled in QGIS.
# This section exports the final pooled DPUE and RAI-W values used for mapping.
################################################################################


################################################################################
# 6.1. Prepare QGIS table
#
# The values are taken directly from the audited pooled locality metrics used
# in Figure 5, ensuring that Figures 5 and 6 use exactly the same DPUE and RAI-W
# calculations.
#
# All 43 monitored localities are retained, including localities with no
# detections.
################################################################################

figure_6_qgis <- figure_5_locality_totals |>
  transmute(
    localidade,
    region,
    
    effort_minutes =
      total_effort_minutes,
    
    positive_records =
      total_positive_records,
    
    shoreline_m =
      shoreline_m,
    
    dpue =
      pooled_dpue,
    
    raiw =
      pooled_raiw,
    
    occurrence = if_else(
      total_positive_records > 0,
      "PRESENT",
      "ABSENT"
    )
  ) |>
  arrange(
    region,
    localidade
  )


################################################################################
# 6.2. Checks
################################################################################

stopifnot(
  nrow(
    figure_6_qgis
  ) == 43,
  
  sum(
    figure_6_qgis$
      effort_minutes
  ) == 8415,
  
  sum(
    figure_6_qgis$
      positive_records
  ) == 173,
  
  sum(
    figure_6_qgis$
      occurrence == "PRESENT"
  ) == 15,
  
  sum(
    figure_6_qgis$
      occurrence == "ABSENT"
  ) == 28,
  
  sum(
    is.na(
      figure_6_qgis$dpue
    )
  ) == 0,
  
  sum(
    is.na(
      figure_6_qgis$raiw
    )
  ) == 0
)


################################################################################
# 6.3. Check regional occurrence
################################################################################

figure_6_region_check <- figure_6_qgis |>
  group_by(
    region
  ) |>
  summarise(
    n_localities =
      n(),
    
    positive_localities =
      sum(
        occurrence == "PRESENT"
      ),
    
    positive_records =
      sum(
        positive_records
      ),
    
    effort_minutes =
      sum(
        effort_minutes
      ),
    
    .groups = "drop"
  )


cat(
  "\nFigure 6 regional check\n"
)

print(
  figure_6_region_check,
  n = Inf
)


################################################################################
# 6.4. Check mapped metric rankings
################################################################################

figure_6_dpue_check <- figure_6_qgis |>
  filter(
    occurrence == "PRESENT"
  ) |>
  arrange(
    desc(dpue)
  ) |>
  select(
    localidade,
    dpue
  )


figure_6_raiw_check <- figure_6_qgis |>
  filter(
    occurrence == "PRESENT"
  ) |>
  arrange(
    desc(raiw)
  ) |>
  select(
    localidade,
    raiw
  )


cat(
  "\nFigure 6 DPUE ranking\n"
)

print(
  figure_6_dpue_check,
  n = Inf
)


cat(
  "\nFigure 6 RAI-W ranking\n"
)

print(
  figure_6_raiw_check,
  n = Inf
)


################################################################################
# 6.5. Export QGIS source table
################################################################################

write_csv(
  figure_6_qgis,
  "outputs/figure_6_qgis_locality_metrics.csv"
)


################################################################################
# end Figure 6
################################################################################

