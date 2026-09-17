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
  delim = ";",
  
  # Values used in the original database to represent missing data
  na = c("", "NA", "Na", "na"),
  
  # Avoid the empty 19th column created by the final semicolon in each row
  col_select = 1:18,
  
  col_types = cols(
    localidade = col_character(),
    data = col_date(format = "%d/%m/%Y"),
    
    # Mixed decimal separators in the source file
    visib_horiz = col_character(),
    
    faixa_bat = col_character(),
    prof_min = col_double(),
    prof_max = col_double(),
    metodo = col_character(),
    observer = col_character(),
    n_divers = col_double(),
    tempo_censo = col_double(),
    dafor = col_double(),
    
    # Legacy field; not used in the present analyses
    iar_medio = col_character(),
    
    n_trans_vis = col_double(),
    n_trans_pres = col_double(),
    dafor_id = col_double(),
    geo_id = col_character(),
    obs = col_character(),
    id_horus = col_double()
  )
) |>
  select(-any_of("...19")) |>
  mutate(
    visib_horiz = clean_num(visib_horiz)
  )
################################################################################
# Locality extent
################################################################################

df_localidade <- read_delim(
  "data/localidade_rebio2.csv",
  delim = ";",
  locale = locale(decimal_mark = ","),
  col_types = cols(
    id = col_integer(),
    localidade = col_character(),
    loc_mapa = col_character(),
    comp_m = col_double()
  )
) |>
  mutate(
    localidade = str_squish(
      str_to_upper(
        str_replace_all(localidade, "_", " ")
      )
    ),
    
    # comp_m is already expressed in metres in the source file
    extent_m = comp_m,
    
    # Number of 100-m shoreline units
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
      ) ~ "ADJACENT_REBIO",
      
      TRUE ~ "SURROUNDINGS"
    )
  )

################################################################################
# Save cleaned datasets
################################################################################

saveRDS(df_monit, "outputs/df_monit_clean.rds")
saveRDS(df_localidade, "outputs/df_localidade_clean.rds")

