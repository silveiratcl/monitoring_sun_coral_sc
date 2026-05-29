# Script RSMA

################################################################################
# 0. Packages
################################################################################
library(tidyverse)
library(readr)
library(tidyr)
library(stringr)
library(dplyr)
library(lubridate)
library(sf)
library(ggplot2)
library(patchwork)
library(ggforce)
library(ggplot2)
library(RColorBrewer)
library(hrbrthemes)



#### Data ####

# monitoring
df_monit = read_delim("data/dados_monitoramento_cs_2025-04-30.csv",
                      col_types = list(localidade = col_character(),
                                       data = col_date(format = "%d/%m/%Y"),
                                       visib_horiz = col_double(),
                                       faixa_bat = col_character(),
                                       prof_min = col_double(),
                                       prof_max = col_double(),
                                       metodo = col_character(),
                                       observer = col_character(),
                                       n_divers = col_double(),
                                       tempo_censo = col_double(),
                                       dafor = col_double(),
                                       iar_medio = col_double(),
                                       n_trans_vis = col_double(),
                                       n_trans_pres = col_double(),
                                       dafor_id = col_double(),
                                       geo_id = col_character(),
                                       obs = col_character(), 
                                       id_horus = col_double()
                      ))



spec(df_monit)
df_monit[2000,]


df_monit <- df_monit %>%
  mutate(localidade_rebio = if_else(localidade %in% c("rancho_norte",
                                                      "letreiro",
                                                      "pedra_do_elefante",
                                                      "costa_do_elefante",
                                                      "deserta_norte",  
                                                      "deserta_sul",
                                                      "portinho_norte",
                                                      "portinho_sul",
                                                      "enseada_do_lili",
                                                      "letreiro",
                                                      "costao_do_saco_dagua",
                                                      "saco_dagua",
                                                      "saco_da_mulata_norte",
                                                      "saco_da_mulata_sul",
                                                      "naufragio_do_lili",
                                                      "saquinho_dagua"
                                                      
  ), "rebio", 
  
  if_else(localidade %in% c( "baia_das_tartarugas", 
                             "saco_do_batismo",
                             "vidal",
                             "farol",
                             "engenho",
                             "saco_do_capim"), "entorno_imediato", "entorno")))


df_monit$localidade_rebio


# localities - from shape data to get the extent of each locality

df_localidade = read_delim("data/localidade_rebio2.csv", delim = ";", 
                           col_types = c("i","c","c","d"))
df_localidade
print(df_localidade, n = 48)

df_localidade_raw <- read_delim(
  "data/localidade_rebio2.csv",
  delim = ";",
  col_types = c("i", "c", "c", "d")
)

df_localidade_raw %>%
  select(localidade, comp_m) %>%
  arrange(desc(comp_m)) %>%
  print(n = 50)

summary(df_localidade_raw$comp_m)


df_localidade = df_localidade %>% 
  mutate(localidade = str_to_upper(str_replace_all(localidade, "_", " ")))

df_localidade

df_localidade$comp_m = df_localidade$comp_m/1000



### Data processing ### 

# Aggregate by locality 
# total time
# total detections 

# create faixa_bat based on prof_min e prof_max data

# Obtaining the detection and effort df
# detection is presence/absence by locality by each monitoring strata
# effort is the number of visual transects where cs were detected

table(df_monit$prof_min)
table(df_monit$prof_max)

df_monit_effort <- df_monit  %>% 
  group_by(localidade_rebio, localidade, data, faixa_bat, dafor_id) %>%
  filter(obs != "estimado dos dados do ICMBio", faixa_bat != "Na") %>% 
  mutate(localidade = str_to_upper(str_replace_all(localidade, "_", " ")),
         localidade_rebio = str_to_upper(str_replace_all(localidade_rebio, "_", " "))) %>%
  summarise(max_trsct_vis = sum(max(n_trans_vis)),
            n_detection = max(n_trans_pres),
            n_divers = max(n_divers),
            visib_m = max(visib_horiz)) %>%
  ungroup()
df_monit_effort
print(df_monit_effort, n=86)


df_monit_effort$localidade

df_monit_effort %>% 
  filter(localidade == "BAIA DAS TARTARUGAS")

######################################
######################################
df_monit_effort <- df_monit %>% 
  # Convert prof_min and prof_max to numeric
  mutate(prof_min_num = as.numeric(prof_min),
         prof_max_num = as.numeric(prof_max)) %>%
  # Create the new depth interval variable
  mutate(faixa_bat_depth = case_when(
    prof_max_num <= 2 ~ "0-2m",
    prof_max_num > 2.1 & prof_max_num <= 8 ~ "2.1-8m",
    prof_max_num > 8.1 & prof_max_num <= 14 ~ "8.1-14m",
    prof_max_num > 14.1 ~ "14.1m+",
    TRUE ~ NA_character_
  )) %>%
  # Now proceed with your original processing but using faixa_bat_depth
  group_by(localidade_rebio, localidade, data, faixa_bat_depth, dafor_id) %>%
  filter(obs != "estimado dos dados do ICMBio", faixa_bat != "Na") %>% 
  mutate(localidade = str_to_upper(str_replace_all(localidade, "_", " ")),
         localidade_rebio = str_to_upper(str_replace_all(localidade_rebio, "_", " "))) %>%
  summarise(max_trsct_vis = sum(max(n_trans_vis)),
            n_detection = max(n_trans_pres),
            n_divers = max(n_divers),
            visib_m = max(visib_horiz)) %>%
  ungroup()

df_monit_effort
print(df_monit_effort, n=140)


#####################################
#####################################


# left_join distance of localities

df_monit_effort <- df_monit_effort %>% 
  left_join(
    df_localidade %>% dplyr::select(localidade, comp_m), 
    by = "localidade"
  )


print(df_monit_effort, n= 140
)



############# Relative abundance index weighed by effort
######################

clean_num <- function(x) {
  x %>%
    as.character() %>%
    str_trim() %>%
    na_if("") %>%
    na_if("Na") %>%
    str_replace_all(",", ".") %>%  # decimal comma -> dot
    as.numeric()
}

df_monit_iarw <- df_monit %>% 
  # Clean & convert to numeric safely
  mutate(
    prof_min_num   = clean_num(prof_min),
    prof_max_num   = clean_num(prof_max),
    visib_horiz_num= clean_num(visib_horiz)
  ) %>%
  # Depth bins (contiguous)
  mutate(faixa_bat_depth = case_when(
    !is.na(prof_max_num) & prof_max_num <= 2                      ~ "0-2m",
    !is.na(prof_max_num) & prof_max_num > 2  & prof_max_num <= 8  ~ "2.1-8m",
    !is.na(prof_max_num) & prof_max_num > 8  & prof_max_num <= 14 ~ "8.1-14m",
    !is.na(prof_max_num) & prof_max_num > 14                      ~ "14.1m+",
    TRUE ~ NA_character_
  )) %>%
  group_by(localidade_rebio, localidade, data, faixa_bat_depth) %>%
  filter(obs != "estimado dos dados do ICMBio", faixa_bat != "Na") %>% 
  mutate(
    localidade       = str_to_upper(str_replace_all(localidade, "_", " ")),
    localidade_rebio = str_to_upper(str_replace_all(localidade_rebio, "_", " ")),
    weight           = coalesce(as.numeric(dafor) / 10, 0)  # robust if dafor has NA
  ) %>%
  summarise(
    n_minutes   = dplyr::n(),                # 1 row = 1 minute
    Nhours      = n_minutes / 60,
    sum_weight  = sum(weight, na.rm = TRUE),
    n_detection = max(n_trans_pres, na.rm = TRUE) %>% na_if(-Inf),
    n_divers    = max(n_divers,    na.rm = TRUE) %>% na_if(-Inf),
    visib_m     = max(visib_horiz_num, na.rm = TRUE) %>% na_if(-Inf),
    .groups = "drop"
  ) %>%
  # Join distance and compute Uni100m + index
  left_join(
    df_localidade %>%
      mutate(localidade = str_to_upper(str_replace_all(localidade, "_", " "))) %>%
      dplyr::select(localidade, comp_m),
    by = "localidade"
  ) %>%
  mutate(
    Uni100m = comp_m / 100,
    DAFOR_weighted_index = if_else(
      Nhours > 0 & Uni100m > 0,
      sum_weight / (Nhours * Uni100m),
      NA_real_
    )
  )

print(df_monit_iarw, n = 140)

#### Charting by bathimetry strata ############################################### 


library(ggplot2)
library(RColorBrewer)
library(hrbrthemes)

# Positive instead DAFOR
# Getting the numbers
# df_monit_dafor
# table(df_monit_dafor$dafor_DAFOR)
# length(df_monit_dafor$dafor_DAFOR)

df_monit
table(df_monit$dafor)
sum(table(df_monit$dafor))
length(df_monit$dafor)/60

table(df_monit_effort$localidade)
table(df_monit_effort$faixa_bat_depth)


# without icmbio data

df_table  = df_monit %>% 
  filter(obs != "estimado dos dados do ICMBio") 

table(df_table$dafor)
sum(table(df_table$dafor))

################################################################################
# Figure 2
# Sampling effort expressed as the number of minutes spent at each surveyed site


library(ggplot2)
library(dplyr)

plot_transec_strata_english <- df_monit_effort %>% 
  mutate(
    # keep the real data values, just set the order of the strips
    localidade_rebio = factor(localidade_rebio,
                              levels = c("REBIO", "ENTORNO IMEDIATO", "ENTORNO")),
    localidade = factor(localidade)
  ) %>%
  ggplot(aes(fill = factor(faixa_bat_depth, levels = c("0-2m", "2.1-8m", "8.1-14m", "14.1m+")), 
             y = reorder(localidade, max_trsct_vis, sum), 
             x = max_trsct_vis)) +
  scale_fill_manual(values=c('#db6d10', '#aaee4b','#416f02','#536e99')) +
  geom_bar(position="stack", stat="identity", width = 0.8) +
  facet_grid(
    rows = vars(localidade_rebio),
    scales = "free_y", space = "free_y", switch = "both",
    labeller = labeller(localidade_rebio = c(
      "REBIO"             = "REBIO ARVOREDO",
      "ENTORNO IMEDIATO"  = "NEAR REBIO",
      "ENTORNO"           = "SURROUNDINGS"
    ))
  ) +
  scale_x_continuous(position="top", n.breaks = 10, expand = c(0, 0)) +
  #ggtitle("Esforço - Total de Transectos (1 min.) por localidade (2022-2025)") +
  theme(
    panel.background = element_blank(),
    axis.ticks.length.x = unit(0.2, "cm"), 
    axis.ticks.x = element_line(colour = "grey", linewidth = 0.8, linetype = "solid"), 
    axis.line.x = element_line(colour = "grey", linewidth = 0.8, linetype = "solid"),
    axis.ticks.y= element_blank(),
    axis.title.x = element_blank(),
    #plot.title = element_text(hjust = 0.5, size = 18, color ="#284b80", margin = margin(t = 10, b = 20)),
    axis.title.y = element_blank(), 
    legend.text = element_text(size=15, color ="#284b80"),
    legend.title = element_blank(),
    legend.key.size = unit(.8, 'cm'),
    axis.text.y = element_text(size = 12),
    panel.spacing = unit(1, "lines"),
    strip.text.y = element_text(size = 10)
  )

x11()
plot_transec_strata_english
ggsave("figs/fig_2_transec_batimetria_english.png", width = 10, height = 15, dpi = 300)


################################################################################
# FIGURE 3
# Number of detections and cumulative sampled depths by bathymetric range
#
# Panel A: number of detections by bathymetric range
# Panel B: cumulative sampled depths reconstructed from the minimum and maximum
#          monitored depth of each transect, using 0.5 m intervals
################################################################################
################################################################################
# 3.1. Parameters
################################################################################

depth_levels_fig3 <- c(
  "0-2 m",
  "2.1-8 m",
  "8.1-14 m",
  "14.1-20 m"
)

depth_bins_fig3 <- tibble(
  faixa_bat_depth = factor(depth_levels_fig3, levels = depth_levels_fig3),
  dmin = c(0,   2.1,  8.1, 14.1),
  dmax = c(2,   8.0, 14.0, 20.0)
)

max_or_na <- function(x) {
  if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
}

################################################################################
# 3.2. Prepare independent monitored transects
#
# dafor_id identifies the monitored transect.
# The repeated minute rows are reduced to one row per transect.
################################################################################

transects_fig3 <- df_monit %>%
  mutate(
    prof_min_num = as.numeric(prof_min),
    prof_max_num = as.numeric(prof_max)
  ) %>%
  filter(
    obs != "estimado dos dados do ICMBio",
    faixa_bat != "Na",
    !is.na(dafor_id),
    !is.na(prof_min_num),
    !is.na(prof_max_num)
  ) %>%
  group_by(
    localidade,
    data,
    dafor_id
  ) %>%
  summarise(
    prof_min_num = min(prof_min_num, na.rm = TRUE),
    prof_max_num = max(prof_max_num, na.rm = TRUE),
    n_detection  = max_or_na(n_trans_pres),
    .groups = "drop"
  ) %>%
  mutate(
    # Restrict the plotted domain to 0-20 m, matching the original figure
    prof_min_num = pmax(0, pmin(20, prof_min_num)),
    prof_max_num = pmax(0, pmin(20, prof_max_num))
  ) %>%
  filter(prof_max_num >= prof_min_num)

################################################################################
# 3.3. Panel A - corrected number of detections by bathymetric range
#
# This follows the old figure logic of assigning each detection to a depth
# range according to the maximum depth monitored in its transect, but now
# keeps independent transects separated by dafor_id before summing.
################################################################################

df_depth_corrected <- transects_fig3 %>%
  mutate(
    faixa_bat_depth = case_when(
      prof_max_num <= 2                        ~ "0-2 m",
      prof_max_num > 2  & prof_max_num <= 8    ~ "2.1-8 m",
      prof_max_num > 8  & prof_max_num <= 14   ~ "8.1-14 m",
      prof_max_num > 14 & prof_max_num <= 20   ~ "14.1-20 m",
      TRUE ~ NA_character_
    ),
    faixa_bat_depth = factor(faixa_bat_depth, levels = depth_levels_fig3)
  ) %>%
  filter(
    !is.na(faixa_bat_depth),
    n_detection > 0
  ) %>%
  group_by(faixa_bat_depth) %>%
  summarise(
    n_detection = sum(n_detection, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  complete(
    faixa_bat_depth = factor(depth_levels_fig3, levels = depth_levels_fig3),
    fill = list(n_detection = 0)
  )

print(df_depth_corrected, n = Inf)

################################################################################
# 3.4. Diagnostic - compare corrected Panel A against the old code route
#
# The old code omitted dafor_id from its grouping. If this table shows the
# same values, Panel A from the paper is already safe. If values differ, the
# original panel collapsed some independent transects.
################################################################################

df_depth_old_route <- df_monit %>%
  mutate(
    prof_max_num = as.numeric(prof_max),
    faixa_bat_depth = case_when(
      prof_max_num <= 2                      ~ "0-2 m",
      prof_max_num > 2  & prof_max_num <= 8  ~ "2.1-8 m",
      prof_max_num > 8  & prof_max_num <= 14 ~ "8.1-14 m",
      prof_max_num > 14                      ~ "14.1-20 m",
      TRUE ~ NA_character_
    ),
    faixa_bat_depth = factor(faixa_bat_depth, levels = depth_levels_fig3)
  ) %>%
  filter(
    obs != "estimado dos dados do ICMBio",
    faixa_bat_depth != "Na",
    !is.na(faixa_bat_depth)
  ) %>%
  group_by(localidade, data, faixa_bat_depth) %>%
  summarise(
    n_detection = max_or_na(n_trans_pres),
    .groups = "drop"
  ) %>%
  filter(n_detection > 0) %>%
  group_by(faixa_bat_depth) %>%
  summarise(
    n_detection_old = sum(n_detection, na.rm = TRUE),
    .groups = "drop"
  )

panel_a_check <- df_depth_corrected %>%
  left_join(df_depth_old_route, by = "faixa_bat_depth") %>%
  mutate(
    n_detection_old = coalesce(n_detection_old, 0),
    difference = n_detection - n_detection_old
  )

print(panel_a_check, n = Inf)

################################################################################
# 3.5. Panel B - cumulative sampled depths in 0.5 m intervals
#
# This reconstructs the missing depth_density object:
# every transect contributes to each 0.5 m interval overlapped by its recorded
# minimum-to-maximum monitored depth range.
################################################################################

depth_intervals_05 <- tibble(
  z_min = seq(0, 19.5, by = 0.5),
  z_max = seq(0.5, 20, by = 0.5)
) %>%
  mutate(
    z_mid = (z_min + z_max) / 2
  )

depth_density <- transects_fig3 %>%
  crossing(depth_intervals_05) %>%
  mutate(
    overlap_m = pmax(
      0,
      pmin(prof_max_num, z_max) - pmax(prof_min_num, z_min)
    ),
    # A transect contributes proportionally when it overlaps only part of
    # a 0.5 m depth interval.
    transect_contribution = overlap_m / 0.5
  ) %>%
  group_by(z_mid) %>%
  summarise(
    effort_per_m = sum(transect_contribution, na.rm = TRUE),
    .groups = "drop"
  )

print(depth_density, n = Inf)

################################################################################
# 3.6. Create Panel A
################################################################################

p1 <- ggplot(
  df_depth_corrected,
  aes(x = faixa_bat_depth, y = n_detection)
) +
  geom_col(
    fill = "#db6c10",
    linewidth = 0.3,
    alpha = 0.85
  ) +
  labs(
    x = NULL,
    y = "Number of detections"
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    expand = expansion(mult = c(0, 0.02))
  ) +
  theme(
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.ticks.length.x = unit(0.2, "cm"),
    axis.ticks.x = element_line(colour = "grey", linewidth = 0.8),
    axis.line.x = element_line(colour = "grey", linewidth = 0.8),
    axis.line.y = element_line(colour = "grey", linewidth = 0.8),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    axis.text.x = element_text(size = 18)
  )

################################################################################
# 3.7. Create Panel B
################################################################################

max_effort <- ceiling(max(depth_density$effort_per_m, na.rm = TRUE))

p2 <- ggplot(
  depth_density,
  aes(x = z_mid, y = effort_per_m)
) +
  geom_col(
    width = 0.5,
    fill = "royalblue"
  ) +
  labs(
    x = "Depth (m)",
    y = "Number of transects"
  ) +
  scale_y_continuous(
    breaks = seq(0, max_effort, by = 10),
    expand = expansion(mult = c(0, 0.02))
  ) +
  scale_x_continuous(
    breaks = seq(0, 20, by = 2),
    limits = c(0, 20)
  ) +
  theme(
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.ticks.length.x = unit(0.2, "cm"),
    axis.ticks.x = element_line(colour = "grey", linewidth = 0.8),
    axis.line.x = element_line(colour = "grey", linewidth = 0.8),
    axis.line.y = element_line(colour = "grey", linewidth = 0.8),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    axis.text.x = element_text(size = 18)
  )

################################################################################
# 3.8. Combine Figure 3
################################################################################

figure_3 <- (p1 / p2) +
  plot_layout(heights = c(1, 1)) +
  plot_annotation(tag_levels = "A") &
  theme(
    plot.tag = element_text(face = "bold", size = 18),
    plot.tag.position = c(0.98, 0.98)
  )

figure_3

################################################################################
# 3.9. Export
################################################################################



x11()
figure_3
ggsave("figs/fig_3_detections_depth_sampling.png", width = 15, height = 13, dpi = 300)






################################################################################
# Figure 4
# ANNUAL FIGURE
# Panel A: Annual DPUE partitioned by DAFOR category
# Panel B: Annual RAI-W partitioned by bathymetric stratum
################################################################################
################################################################################
# 4.1. Parameters
################################################################################

dafor_levels <- c("D", "A", "F", "O", "R")

dafor_colors <- c(
  "D" = "#FDC827",
  "A" = "#F28349",
  "F" = "#C73E73",
  "O" = "#9011A3",
  "R" = "#450DA3"
)

depth_levels <- c("0-2m", "2.1-8m", "8.1-14m", "14.1m+")

depth_colors <- c(
  "0-2m"    = "#db6d10",
  "2.1-8m"  = "#aaee4b",
  "8.1-14m" = "#416f02",
  "14.1m+"  = "#536e99"
)

manual_weights <- c(
  `10` = 1.00,
  `8`  = 0.80,
  `6`  = 0.60,
  `4`  = 0.10,
  `2`  = 0.04,
  `0`  = 0.00
)

clean_num <- function(x) {
  x %>%
    as.character() %>%
    str_trim() %>%
    na_if("") %>%
    na_if("Na") %>%
    str_replace_all(",", ".") %>%
    as.numeric()
}

max_or_na <- function(x) {
  if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
}

################################################################################
# 4.2. Locality groups
################################################################################

rebio_localities <- c(
  "rancho_norte",
  "letreiro",
  "pedra_do_elefante",
  "costa_do_elefante",
  "deserta_norte",
  "deserta_sul",
  "portinho_norte",
  "portinho_sul",
  "enseada_do_lili",
  "costao_do_saco_dagua",
  "saco_dagua",
  "saco_da_mulata_norte",
  "saco_da_mulata_sul",
  "naufragio_do_lili",
  "saquinho_dagua"
)

near_rebio_localities <- c(
  "baia_das_tartarugas",
  "saco_do_batismo",
  "vidal",
  "farol",
  "engenho",
  "saco_do_capim"
)

################################################################################
# 4.3. Locality extent
################################################################################

df_localidade_annual <- read_delim(
  "data/localidade_rebio2.csv",
  delim = ";",
  col_types = cols(.default = col_guess()),
  show_col_types = FALSE
) %>%
  mutate(
    localidade = str_to_upper(str_replace_all(localidade, "_", " ")),
    extent_m   = comp_m / 1000,
    Uni100m    = extent_m / 100
  ) %>%
  select(localidade, extent_m, Uni100m)

################################################################################
# 4.4. Prepare minute-level records
#
# One row = one monitored minute
# dafor_id = one monitored transect
################################################################################

df_annual_minutes <- df_monit %>%
  mutate(
    localidade_original = localidade,
    
    localidade_rebio = case_when(
      localidade_original %in% rebio_localities      ~ "REBIO",
      localidade_original %in% near_rebio_localities ~ "ENTORNO IMEDIATO",
      TRUE                                           ~ "ENTORNO"
    ),
    
    localidade = str_to_upper(str_replace_all(localidade_original, "_", " ")),
    year       = year(data),
    dafor_num  = coalesce(clean_num(dafor), 0),
    
    dafor_cat = case_when(
      dafor_num == 10 ~ "D",
      dafor_num == 8  ~ "A",
      dafor_num == 6  ~ "F",
      dafor_num == 4  ~ "O",
      dafor_num == 2  ~ "R",
      TRUE            ~ NA_character_
    ),
    
    prof_max_num = clean_num(prof_max),
    faixa_bat_depth = case_when(
      !is.na(prof_max_num) & prof_max_num <= 2                      ~ "0-2m",
      !is.na(prof_max_num) & prof_max_num > 2  & prof_max_num <= 8  ~ "2.1-8m",
      !is.na(prof_max_num) & prof_max_num > 8  & prof_max_num <= 14 ~ "8.1-14m",
      !is.na(prof_max_num) & prof_max_num > 14                      ~ "14.1m+",
      TRUE ~ NA_character_
    ),
    
    weight = unname(manual_weights[as.character(dafor_num)]),
    weight = coalesce(weight, 0)
  ) %>%
  filter(
    localidade_rebio != "ENTORNO",
    obs != "estimado dos dados do ICMBio",
    !is.na(dafor_id)
  ) %>%
  left_join(df_localidade_annual, by = "localidade") %>%
  mutate(
    dafor_cat       = factor(dafor_cat, levels = dafor_levels),
    faixa_bat_depth = factor(faixa_bat_depth, levels = depth_levels)
  )

################################################################################
# 4.5. Annual annotations
#
# These annotations refer to total monitored one-minute records and absent
# one-minute records in each year.
################################################################################

annual_annotations <- df_annual_minutes %>%
  group_by(year) %>%
  summarise(
    Total   = n(),
    Ausente = sum(dafor_num == 0, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    label = paste0("Abs.", Ausente, "| Tot.", Total)
  )

print(annual_annotations, n = Inf)

################################################################################
# 4.6. PANEL A
# Annual DPUE partitioned by DAFOR category
#
# For each transect and DAFOR category:
#   DPUE_cat = N_cat / (Nhours * Uni100m)
#
# Then summed by year.
################################################################################

dpue_by_transect_category <- df_annual_minutes %>%
  filter(!is.na(dafor_cat)) %>%
  group_by(
    localidade,
    data,
    year,
    dafor_id,
    dafor_cat
  ) %>%
  summarise(
    category_minutes = n(),
    effort_minutes   = max_or_na(n_trans_vis),
    Uni100m          = first(Uni100m),
    .groups = "drop"
  ) %>%
  mutate(
    effort_hours = effort_minutes / 60,
    denominator  = effort_hours * Uni100m,
    dpue_standard = if_else(
      !is.na(denominator) & denominator > 0,
      category_minutes / denominator,
      NA_real_
    )
  )

annual_dpue <- dpue_by_transect_category %>%
  filter(is.finite(dpue_standard)) %>%
  group_by(year, dafor_cat) %>%
  summarise(
    total_dpue = sum(dpue_standard, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  complete(
    year = sort(unique(df_annual_minutes$year)),
    dafor_cat = factor(dafor_levels, levels = dafor_levels),
    fill = list(total_dpue = 0)
  ) %>%
  mutate(
    year      = factor(year, levels = sort(unique(df_annual_minutes$year))),
    dafor_cat = factor(dafor_cat, levels = dafor_levels)
  )

annual_dpue_totals <- annual_dpue %>%
  group_by(year) %>%
  summarise(
    annual_dpue = sum(total_dpue, na.rm = TRUE),
    .groups = "drop"
  )

print(annual_dpue_totals, n = Inf)

dpue_bar_tops <- annual_dpue %>%
  group_by(year) %>%
  summarise(
    bar_top = sum(total_dpue, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(
    annual_annotations %>%
      mutate(year = factor(year, levels = levels(annual_dpue$year))),
    by = "year"
  )

plot_dpue_by_year <- ggplot(
  annual_dpue,
  aes(x = year, y = total_dpue, fill = dafor_cat)
) +
  geom_col(position = "stack") +
  geom_text(
    data = dpue_bar_tops,
    aes(x = year, y = bar_top, label = label),
    vjust = -0.45,
    size = 3.5,
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
    y = "DPUE"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(),
    panel.border = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 16),
    legend.text = element_text(size = 12),
    legend.key.size = unit(0.8, "cm"),
    legend.position = "right"
  )

################################################################################
# 4.7. PANEL B
# Annual RAI-W partitioned by depth stratum
################################################################################

raiw_by_transect_depth <- df_annual_minutes %>%
  filter(!is.na(faixa_bat_depth)) %>%
  group_by(
    localidade,
    data,
    year,
    faixa_bat_depth,
    dafor_id
  ) %>%
  summarise(
    total_sum_weight = sum(weight, na.rm = TRUE),
    effort_minutes   = max_or_na(n_trans_vis),
    Uni100m          = first(Uni100m),
    .groups = "drop"
  ) %>%
  mutate(
    effort_hours = effort_minutes / 60,
    denominator  = effort_hours * Uni100m,
    raiw_standard = if_else(
      !is.na(denominator) & denominator > 0,
      total_sum_weight / denominator,
      NA_real_
    )
  )

annual_raiw <- raiw_by_transect_depth %>%
  filter(
    total_sum_weight > 0,
    is.finite(raiw_standard)
  ) %>%
  group_by(year, faixa_bat_depth) %>%
  summarise(
    total_raiw = sum(raiw_standard, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  complete(
    year = sort(unique(df_annual_minutes$year)),
    faixa_bat_depth = factor(depth_levels, levels = depth_levels),
    fill = list(total_raiw = 0)
  ) %>%
  mutate(
    year = factor(year, levels = sort(unique(df_annual_minutes$year))),
    faixa_bat_depth = factor(faixa_bat_depth, levels = depth_levels)
  )

annual_raiw_totals <- annual_raiw %>%
  group_by(year) %>%
  summarise(
    annual_raiw = sum(total_raiw, na.rm = TRUE),
    .groups = "drop"
  )

print(annual_raiw_totals, n = Inf)

plot_raiw_by_year <- ggplot(
  annual_raiw,
  aes(x = year, y = total_raiw, fill = faixa_bat_depth)
) +
  geom_col(position = "stack") +
  scale_fill_manual(
    values = depth_colors,
    drop = FALSE,
    name = "Depth(m)"
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    x = NULL,
    y = "RAI-W"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(),
    panel.border = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 16),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.key.size = unit(0.8, "cm"),
    legend.position = "right"
  )

################################################################################
# 4.8. Combine panels
################################################################################

annual_figure <- (
  plot_dpue_by_year +
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
    plot.tag = element_text(face = "bold", size = 14),
    plot.tag.position = c(0, 1)
  )



x11()
annual_figure
ggsave(
  filename = "figs/fig_4_anual_DPUE_RAIW.png",
  plot = annual_figure,
  width = 18,
  height = 15,
  dpi = 300
)



################################################################################
# FIGURE 5
# DPUE and RAI-W by locality and bathymetric stratum
#
# Final publication pipeline:
# - One row of df_monit = one one-minute visual observation
# - dafor_id = one monitored visual transect composed of minute observations
# - DPUE and RAI-W are calculated for each dafor_id
# - Transect-level standardized values are stacked by locality and depth
# - RAI-W uses manually established DAFOR weights


################################################################################
# 5.1. Data
################################################################################

head(df_monit) 

################################################################################
# 2. Import locality extent
#
# The raw values in comp_m are converted by /1000, following the conversion
# used in the original analysis and yielding plausible monitored extents in m.
#
# Example:
# naufragio_do_lili: 93052 / 1000 = 93.052 m
################################################################################

df_localidade_fig5 <- read_delim(
  "data/localidade_rebio2.csv",
  delim = ";",
  col_types = cols(
    .default = col_guess()
  ),
  show_col_types = FALSE
) %>%
  mutate(
    localidade = str_to_upper(str_replace_all(localidade, "_", " ")),
    extent_m   = comp_m / 1000,
    Uni100m    = extent_m / 100
  ) %>%
  select(localidade, extent_m, Uni100m)

# Inspect converted shoreline extents
df_localidade_fig5 %>%
  arrange(desc(extent_m)) %>%
  print(n = Inf)

################################################################################
# 5.3. Parameters used in Figure 5
################################################################################

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

# Manual DAFOR weights 
manual_weights <- c(
  `10` = 1.00,  # Dominant
  `8`  = 0.80,  # Abundant
  `6`  = 0.60,  # Frequent
  `4`  = 0.10,  # Occasional
  `2`  = 0.04,  # Rare
  `0`  = 0.00   # Absent
)

################################################################################
# 5.4. Helper functions
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

max_or_na <- function(x) {
  if (all(is.na(x))) {
    NA_real_
  } else {
    max(x, na.rm = TRUE)
  }
}

################################################################################
# 5.5. Prepare minute-level monitoring data
#
# Each row represents one monitored minute.
# Each transect is identified by dafor_id.
################################################################################

df_monit_fig5 <- df_monit %>%
  mutate(
    localidade   = str_to_upper(str_replace_all(localidade, "_", " ")),
    prof_min_num = clean_num(prof_min),
    prof_max_num = clean_num(prof_max),
    dafor_num    = coalesce(clean_num(dafor), 0),
    
    faixa_bat_depth = case_when(
      !is.na(prof_max_num) & prof_max_num <= 2                      ~ "0-2m",
      !is.na(prof_max_num) & prof_max_num > 2  & prof_max_num <= 8  ~ "2.1-8m",
      !is.na(prof_max_num) & prof_max_num > 8  & prof_max_num <= 14 ~ "8.1-14m",
      !is.na(prof_max_num) & prof_max_num > 14                      ~ "14.1m+",
      TRUE ~ NA_character_
    ),
    
    # Manual nonlinear contribution of each DAFOR record
    weight = unname(manual_weights[as.character(dafor_num)]),
    weight = coalesce(weight, 0)
  ) %>%
  filter(
    obs != "estimado dos dados do ICMBio",
    faixa_bat != "Na",
    !is.na(dafor_id),
    !is.na(faixa_bat_depth)
  ) %>%
  mutate(
    faixa_bat_depth = factor(faixa_bat_depth, levels = depth_levels)
  )

################################################################################
# 5.6. Data quality checks before metric calculation
################################################################################

# 5.6.1 Check DAFOR values represented in the analysis
df_monit_fig5 %>%
  count(dafor_num, weight) %>%
  arrange(desc(dafor_num)) %>%
  print(n = Inf)

# 6.2 Check whether each dafor_id occurs in only one locality/date/depth stratum
check_transect_identity <- df_monit_fig5 %>%
  group_by(dafor_id) %>%
  summarise(
    n_localidade = n_distinct(localidade),
    n_dates      = n_distinct(data),
    n_depth      = n_distinct(faixa_bat_depth),
    .groups = "drop"
  ) %>%
  filter(
    n_localidade > 1 |
      n_dates > 1 |
      n_depth > 1
  )

if (nrow(check_transect_identity) > 0) {
  message(
    "Attention: some dafor_id values occur in more than one locality, ",
    "date, or depth stratum. This is acceptable only if dafor_id is not ",
    "globally unique."
  )
  print(check_transect_identity, n = Inf)
}

# 5.6.2 Check number of minute rows against repeated n_trans_vis
#
# Expected in most cases:
# rows_minutes == n_trans_vis
#
# Values can differ if rows from a transect were removed by filtering.
check_minutes_by_transect <- df_monit_fig5 %>%
  group_by(
    localidade,
    data,
    faixa_bat_depth,
    dafor_id
  ) %>%
  summarise(
    rows_minutes = n(),
    n_trans_vis  = max_or_na(n_trans_vis),
    same_minutes = rows_minutes == n_trans_vis,
    .groups = "drop"
  )

table(check_minutes_by_transect$same_minutes, useNA = "ifany")

check_minutes_by_transect %>%
  filter(!same_minutes | is.na(same_minutes)) %>%
  print(n = 100)

# 6.4 Check localities without an extent value
check_missing_extent <- df_monit_fig5 %>%
  distinct(localidade) %>%
  anti_join(df_localidade_fig5, by = "localidade")

if (nrow(check_missing_extent) > 0) {
  warning("Some monitored localities do not have an extent value.")
  print(check_missing_extent, n = Inf)
}

################################################################################
# 5.7. DPUE
#
# DPUE = Ndetec / (Nhours * Uni100m)
#
# Calculated separately for each monitored transect identified by dafor_id.
# n_trans_pres and n_trans_vis are repeated transect-level values in the
# minute-level input table; therefore max() retrieves one value per transect.
################################################################################

df_monit_effort_dpue <- df_monit_fig5 %>%
  group_by(
    localidade,
    data,
    faixa_bat_depth,
    dafor_id
  ) %>%
  summarise(
    effort_minutes   = max_or_na(n_trans_vis),
    total_detections = max_or_na(n_trans_pres),
    .groups = "drop"
  ) %>%
  left_join(df_localidade_fig5, by = "localidade") %>%
  mutate(
    effort_hours  = effort_minutes / 60,
    denominator   = effort_hours * Uni100m,
    
    dpue_standard = if_else(
      !is.na(denominator) & denominator > 0,
      total_detections / denominator,
      NA_real_
    )
  )

################################################################################
# 5.8. RAI-W
#
# RAI-W = sum_i w(s_i) / (Nhours * Uni100m)
#
# Calculated at the same transect level as DPUE:
# - numerator: sum of manual DAFOR weights for the minute observations
# - denominator: monitoring hours and shoreline extent for that transect
################################################################################

df_monit_effort_raiw <- df_monit_fig5 %>%
  group_by(
    localidade,
    data,
    faixa_bat_depth,
    dafor_id
  ) %>%
  summarise(
    effort_minutes   = max_or_na(n_trans_vis),
    total_sum_weight = sum(weight, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(df_localidade_fig5, by = "localidade") %>%
  mutate(
    effort_hours  = effort_minutes / 60,
    denominator   = effort_hours * Uni100m,
    
    raiw_standard = if_else(
      !is.na(denominator) & denominator > 0,
      total_sum_weight / denominator,
      NA_real_
    )
  )

################################################################################
# 5.9. Inspect event-level metrics
################################################################################

print(df_monit_effort_dpue, n = 140)
print(df_monit_effort_raiw, n = 140)

################################################################################
# 5.10. Values represented by the final stacked bars
#
# geom_col(position = "stack") sums transect-level standardized values within
# locality and bathymetric stratum.
################################################################################

dpue_plot_values <- df_monit_effort_dpue %>%
  filter(
    total_detections > 0,
    is.finite(dpue_standard)
  ) %>%
  group_by(localidade, faixa_bat_depth) %>%
  summarise(
    plotted_dpue = sum(dpue_standard, na.rm = TRUE),
    .groups = "drop"
  )

raiw_plot_values <- df_monit_effort_raiw %>%
  filter(
    total_sum_weight > 0,
    is.finite(raiw_standard)
  ) %>%
  group_by(localidade, faixa_bat_depth) %>%
  summarise(
    plotted_raiw = sum(raiw_standard, na.rm = TRUE),
    .groups = "drop"
  )

################################################################################
# 5.11. Confirm ranking and range of the corrected figure
#
# Under the manual weights, ENGENHO is expected to be the highest RAI-W
# locality, replacing NAUFRAGIO DO LILI from the exploratory gamma-based chart.
################################################################################

dpue_ranking <- dpue_plot_values %>%
  group_by(localidade) %>%
  summarise(
    total_dpue = sum(plotted_dpue, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(total_dpue))

raiw_ranking <- raiw_plot_values %>%
  group_by(localidade) %>%
  summarise(
    total_raiw = sum(plotted_raiw, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(total_raiw))

dpue_ranking %>% print(n = Inf)
raiw_ranking %>% print(n = Inf)

dpue_ranking %>%
  summarise(
    highest_dpue_locality = first(localidade),
    max_dpue              = first(total_dpue)
  ) %>%
  print()

raiw_ranking %>%
  summarise(
    highest_raiw_locality = first(localidade),
    max_raiw              = first(total_raiw)
  ) %>%
  print()

################################################################################
# 5.12. Check expected relationship between DPUE and RAI-W
#
# Because all manual weights are between 0 and 1 and use the same denominator,
# RAI-W should not exceed DPUE for a comparable transect where presence was
# recorded consistently.
################################################################################

metric_check <- df_monit_effort_dpue %>%
  select(
    localidade,
    data,
    faixa_bat_depth,
    dafor_id,
    dpue_standard
  ) %>%
  full_join(
    df_monit_effort_raiw %>%
      select(
        localidade,
        data,
        faixa_bat_depth,
        dafor_id,
        raiw_standard
      ),
    by = c("localidade", "data", "faixa_bat_depth", "dafor_id")
  ) %>%
  mutate(
    raiw_greater_than_dpue = raiw_standard > dpue_standard
  )

metric_check %>%
  filter(raiw_greater_than_dpue %in% TRUE) %>%
  print(n = Inf)

################################################################################
# 5.13. Theme for Figure 5
################################################################################

theme_fig5 <- theme(
  panel.background = element_blank(),
  panel.grid = element_blank(),
  
  axis.ticks.length.x = unit(0.2, "cm"),
  
  axis.ticks.x = element_line(
    colour = "grey",
    linewidth = 0.8,
    linetype = "solid"
  ),
  
  axis.line.x = element_line(
    colour = "grey",
    linewidth = 0.8,
    linetype = "solid"
  ),
  
  axis.ticks.y = element_blank(),
  
  axis.title.x = element_blank(),
  axis.title.x.top = element_text(size = 14),
  axis.title.y = element_blank(),
  
  axis.text.x = element_text(size = 12),
  axis.text.y = element_text(size = 12),
  
  legend.title = element_blank(),
  legend.text = element_text(size = 12),
  legend.key.size = unit(0.8, "cm"),
  
  plot.margin = margin(8, 8, 8, 8)
)

################################################################################
# 5.14. Panel A - DPUE
#
# Values are already summarised for plotting by locality and depth stratum.
################################################################################

plot_dpue_strata <- dpue_plot_values %>%
  mutate(
    localidade = fct_reorder(localidade, plotted_dpue, .fun = sum),
    faixa_bat_depth = factor(faixa_bat_depth, levels = depth_levels)
  ) %>%
  ggplot(aes(
    x = plotted_dpue,
    y = localidade,
    fill = faixa_bat_depth
  )) +
  geom_col(position = "stack") +
  scale_fill_manual(
    values = depth_colors,
    drop = FALSE
  ) +
  scale_x_continuous(
    position = "top",
    n.breaks = 10,
    expand = c(0, 0)
  ) +
  labs(
    x = "DPUE",
    y = NULL
  ) +
  theme_fig5

################################################################################
# 5.15. Panel B - RAI-W
################################################################################

plot_raiw_strata <- raiw_plot_values %>%
  mutate(
    localidade = fct_reorder(localidade, plotted_raiw, .fun = sum),
    faixa_bat_depth = factor(faixa_bat_depth, levels = depth_levels)
  ) %>%
  ggplot(aes(
    x = plotted_raiw,
    y = localidade,
    fill = faixa_bat_depth
  )) +
  geom_col(position = "stack") +
  scale_fill_manual(
    values = depth_colors,
    drop = FALSE
  ) +
  scale_x_continuous(
    position = "top",
    n.breaks = 10,
    expand = c(0, 0)
  ) +
  labs(
    x = "RAI-W",
    y = NULL
  ) +
  theme_fig5

################################################################################
# 5.16. Combined Figure 5
################################################################################

figure_5 <- (plot_dpue_strata + plot_raiw_strata) +
  plot_layout(
    ncol = 2,
    guides = "collect"
  ) +
  plot_annotation(
    tag_levels = "A"
  ) &
  theme(
    legend.position = "bottom",
    
    plot.tag = element_text(
      face = "bold",
      size = 14
    ),
    
    plot.tag.position = c(1, 1)
  )

figure_5

################################################################################
# 5.17. Export Figure 5
################################################################################
x11()
ggsave(
  filename = "figs/fig_5_dpue_raiw_manual_weights.png",
  plot = figure_5,
  width = 12,
  height = 5,
  dpi = 300
)

################################################################################
# end figure 5



