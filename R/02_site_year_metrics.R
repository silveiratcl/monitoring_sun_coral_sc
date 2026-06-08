library(tidyverse)
library(lubridate)
library(stringr)

# ------------------------------------------------------------
# Helper functions
# ------------------------------------------------------------

clean_num <- function(x) {
  x |>
    as.character() |>
    str_trim() |>
    na_if("") |>
    na_if("Na") |>
    str_replace_all(",", ".") |>
    as.numeric()
}

manual_weights <- c(
  `10` = 1.00,
  `8`  = 0.80,
  `6`  = 0.60,
  `4`  = 0.10,
  `2`  = 0.04,
  `0`  = 0.00
)

# ------------------------------------------------------------
# Prepare locality extent
# ------------------------------------------------------------

locality_extent <- df_localidade |>
  mutate(
    localidade = str_to_upper(str_replace_all(localidade, "_", " ")),
    shoreline_m = comp_m,
    uni100m = shoreline_m / 100
  ) |>
  select(localidade, shoreline_m, uni100m)
# ------------------------------------------------------------
# Site-year metrics
# One row = locality x year
# ------------------------------------------------------------

site_year_metrics <- df_monit |>
  mutate(
    localidade = str_to_upper(str_replace_all(localidade, "_", " ")),
    year = year(data),
    dafor_num = clean_num(dafor),
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
  group_by(localidade, year) |>
  summarise(
    n_minutes = n(),
    n_transects = n_distinct(dafor_id),
    effort_minutes = n_minutes,
    effort_hours = effort_minutes / 60,
    n_positive = sum(dafor_num > 0, na.rm = TRUE),
    sum_weight = sum(weight, na.rm = TRUE),
    .groups = "drop"
  ) |>
  left_join(locality_extent, by = "localidade") |>
  mutate(
    denominator = effort_hours * uni100m,
    dpue = if_else(denominator > 0, n_positive / denominator, NA_real_),
    rai_w = if_else(denominator > 0, sum_weight / denominator, NA_real_)
  ) |>
  arrange(localidade, year)

# ------------------------------------------------------------
# Inspect
# ------------------------------------------------------------

site_year_metrics |>
  print(n = Inf)

summary(site_year_metrics$dpue)
summary(site_year_metrics$rai_w)

site_year_metrics |>
  summarise(
    n_rows = n(),
    n_sites = n_distinct(localidade),
    years = paste(sort(unique(year)), collapse = ", "),
    total_minutes = sum(effort_minutes, na.rm = TRUE),
    total_positive = sum(n_positive, na.rm = TRUE)
  )


site_year_metrics |>
  count(localidade, sort = TRUE) |>
  print(n = Inf)

site_year_metrics |>
  filter(is.na(shoreline_m) | is.na(dpue)) |>
  print(n = Inf)

df_monit |>
  mutate(localidade = str_to_upper(str_replace_all(localidade, "_", " "))) |>
  distinct(localidade) |>
  anti_join(
    df_localidade |>
      mutate(localidade = str_to_upper(str_replace_all(localidade, "_", " "))) |>
      distinct(localidade),
    by = "localidade"
  ) |>
  print(n = Inf)

site_year_metrics <- site_year_metrics |>
  mutate(
    region = case_when(
      localidade %in% c(
        "RANCHO NORTE", "LETREIRO", "PEDRA DO ELEFANTE",
        "COSTA DO ELEFANTE", "DESERTA NORTE", "DESERTA SUL",
        "PORTINHO NORTE", "PORTINHO SUL", "ENSEADA DO LILI",
        "COSTAO DO SACO DAGUA", "SACO DAGUA",
        "SACO DA MULATA NORTE", "SACO DA MULATA SUL",
        "NAUFRAGIO DO LILI", "SAQUINHO DAGUA"
      ) ~ "REBIO",
      localidade %in% c(
        "BAIA DAS TARTARUGAS", "SACO DO BATISMO", "VIDAL",
        "FAROL", "ENGENHO", "SACO DO CAPIM"
      ) ~ "NEAR_REBIO",
      TRUE ~ "SURROUNDINGS"
    )
  )

site_year_metrics |>
  count(region, localidade) |>
  count(region)

site_year_metrics |>
  arrange(desc(dpue)) |>
  select(
    localidade,
    year,
    effort_minutes,
    n_positive,
    shoreline_m,
    uni100m,
    denominator,
    dpue,
    rai_w
  ) |>
  print(n = 20)

site_year_metrics |>
  filter(localidade == "NAUFRAGIO DO LILI") |>
  select(
    localidade,
    shoreline_m,
    uni100m,
    effort_hours,
    n_positive,
    dpue,
    rai_w
  )


df_localidade |>
  filter(localidade == "NAUFRAGIO DO LILI") |>
  select(localidade, comp_m)



summary(site_year_metrics$dpue)
summary(site_year_metrics$rai_w)

site_year_metrics |>
  arrange(desc(dpue)) |>
  select(localidade, year, effort_minutes, n_positive, shoreline_m, dpue, rai_w) |>
  print(n = 20)

site_year_metrics |>
  summarise(
    n_positive_total = sum(n_positive),
    weight_total = sum(sum_weight)
  )

cor.test(
  site_year_metrics$dpue,
  site_year_metrics$rai_w,
  method = "spearman"
)

cor.test(
  site_year_metrics$dpue,
  site_year_metrics$rai_w,
  method = "spearman"
)


ranking_compare <- site_year_metrics |>
  group_by(localidade) |>
  summarise(
    dpue = sum(dpue),
    rai_w = sum(rai_w),
    .groups = "drop"
  ) |>
  mutate(
    rank_dpue = min_rank(desc(dpue)),
    rank_raiw = min_rank(desc(rai_w))
  ) |>
  arrange(rank_dpue)

ranking_compare



ranking_compare |>
  mutate(
    rank_difference = rank_dpue - rank_raiw
  ) |>
  arrange(desc(abs(rank_difference)))





site_year_metrics |> glimpse()
site_year_metrics |>
       group_by(year) |>
       summarise(
             effort_minutes = sum(effort_minutes),
             n_positive = sum(n_positive),
             mean_dpue = mean(dpue, na.rm = TRUE),
             mean_raiw = mean(rai_w, na.rm = TRUE)
         )


cor.test(
  site_year_metrics$effort_minutes,
  site_year_metrics$dpue,
  method = "spearman"
)

cor.test(
  site_year_metrics$effort_minutes,
  site_year_metrics$n_positive,
  method = "spearman"
)

cor.test(
  site_year_metrics$effort_minutes,
  site_year_metrics$rai_w,
  method = "spearman"
)


site_year_metrics |>
  group_by(year) |>
  summarise(
    n_sites = n(),
    effort_minutes = sum(effort_minutes),
    positive = sum(n_positive),
    positive_per_hour =
      sum(n_positive) /
      (sum(effort_minutes) / 60)
  )

lm(log1p(dpue) ~ year, data = site_year_metrics)

lm(log1p(rai_w) ~ year, data = site_year_metrics)

lm(log1p(dpue) ~ year + effort_minutes,
   data = site_year_metrics)

lm(log1p(rai_w) ~ year + effort_minutes,
   data = site_year_metrics)



m1 <- lm(log1p(dpue) ~ year,
         data = site_year_metrics)

m2 <- lm(log1p(dpue) ~ year + effort_minutes,
         data = site_year_metrics)

m3 <- lm(log1p(rai_w) ~ year,
         data = site_year_metrics)

m4 <- lm(log1p(rai_w) ~ year + effort_minutes,
         data = site_year_metrics)

summary(m1)
summary(m2)
summary(m3)
summary(m4)



