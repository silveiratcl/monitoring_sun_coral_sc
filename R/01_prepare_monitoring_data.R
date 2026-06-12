################################################################################
# 01_prepare_monitoring_data.R
#
# Import and standardize monitoring and locality datasets
################################################################################

################################################################################
# Monitoring data
################################################################################

df_monit <- read_delim(
  "data/dados_monitoramento_cs_2025-04-30.csv",
  col_types = list(
    localidade = col_character(),
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
  )
)

################################################################################
# Locality extent
################################################################################

df_localidade <- read_delim(
  "data/localidade_rebio2.csv",
  delim = ";",
  col_types = c("i","c","c","d")
) |>
  mutate(
    localidade = str_to_upper(
      str_replace_all(localidade, "_", " ")
    ),
    
    extent_m = comp_m / 1000,
    
    Uni100m = extent_m / 100
  )

################################################################################
# Standard locality names
################################################################################

df_monit <- df_monit |>
  mutate(
    localidade = str_to_upper(
      str_replace_all(localidade, "_", " ")
    )
  )

################################################################################
# Region classification
################################################################################

df_monit <- df_monit |>
  mutate(
    region = case_when(
      
      localidade %in% c(
        "RANCHO NORTE",
        "LETREIRO",
        "PEDRA DO ELEFANTE",
        "COSTA DO ELEFANTE",
        "DESERTA NORTE",
        "DESERTA SUL",
        "PORTINHO NORTE",
        "PORTINHO SUL",
        "ENSEADA DO LILI",
        "COSTAO DO SACO DAGUA",
        "SACO DAGUA",
        "SACO DA MULATA NORTE",
        "SACO DA MULATA SUL",
        "NAUFRAGIO DO LILI",
        "SAQUINHO DAGUA"
      ) ~ "REBIO",
      
      localidade %in% c(
        "BAIA DAS TARTARUGAS",
        "SACO DO BATISMO",
        "VIDAL",
        "FAROL",
        "ENGENHO",
        "SACO DO CAPIM"
      ) ~ "NEAR_REBIO",
      
      TRUE ~ "SURROUNDINGS"
    )
  )

################################################################################
# Save cleaned datasets
################################################################################

saveRDS(df_monit, "outputs/df_monit_clean.rds")
saveRDS(df_localidade, "outputs/df_localidade_clean.rds")

