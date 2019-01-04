# 4_sipp_build_ui_event_study.R further cleans SIPP data by identifying people receiving UI benefits.

pay_dd <- 0.86

# cpi cleanup
cpi <- read.csv(paste0(data_path, "cpi.csv")) %>%
  mutate(
    cpi_index = (CPIAUCSL / last(CPIAUCSL))^(-1), dt = ymd(observation_date),
    month_cal = month(dt) + (year(dt) - 2004) * 12
  )

# load data #
df_src_04 <- readRDS(paste0(data_path, "sipp/sipp04.Rdata"))

df_full_04 <- df_src_04 %>%
  mutate(
    id_part2 = ifelse(swave == 7, substring(eentaid, first = 2), eentaid),
    id_part3 = ifelse(swave == 7, substring(epppnum, first = 2), epppnum),
    id = paste0(ssuid, id_part2, id_part3),
    fam_head = as.numeric(epppnum) == efrefper,
    period_index = (swave - 1) * 4 + srefmon,
    period_index_cal = (rhcalyr - 2003) * 12 + rhcalmn,
    hfam_age_tmp = ifelse(fam_head, tage, 0),
    id_fam = paste0(ssuid, as.character(shhadid), as.character(rfid)),
    any_coll = eeducate >= 40, any_ba = eeducate >= 44
  ) %>%
  group_by(id_fam, period_index) %>%
  mutate(
    hfam_age = max(hfam_age_tmp),
    hfam_age_bin = ifelse(hfam_age < 70, floor(hfam_age / 10) * 10, 70),
    n_members = n(),
    n_adults_raw = sum(tage >= 18)
  )

df_full_04 <- df_full_04 %>% 
  ungroup() %>% 
  mutate(n_adults = ifelse(n_adults_raw < 4, n_adults_raw, 4))

df_full_04 <- left_join(df_full_04, cpi, by = c("period_index" = "month_cal")) %>%
  mutate(
    tpearn_adj = tpearn * cpi_index, 
    tfearn_adj = tfearn * cpi_index,
    tftotinc_adj = tftotinc * cpi_index, 
    tptotinc_adj = tptotinc * cpi_index,
    t05amt_adj = t05amt * cpi_index
  )

# construct ui flag and separation flag
df_full_04 <- df_full_04 %>%
  mutate(
    ui_now = t05amt > 0,
    sep = epopstat == 1 & epdjbthn == 1 & (ejobcntr > 0 & estlemp1 == 2),
    unemp = rmesr %in% c(5, 6, 7), sep_to_unemp = lag(sep) & unemp
  ) %>%
  group_by(id) %>%
  mutate(any_ui = max(ui_now) == 1, any_sep = max(sep) == 1, any_unemp = max(sep_to_unemp) == 1)

# find ui start date
df_full_04 <- df_full_04 %>%
  ungroup() %>%
  mutate(
    ui_now_period = ifelse(ui_now == TRUE, period_index, NA),
    sep_now_period = ifelse(sep == TRUE, period_index, NA),
    u_now_period = ifelse(sep_to_unemp == TRUE, period_index, NA)
  ) %>%
  group_by(id) %>%
  mutate(
    first_ui_date = min(ui_now_period, na.rm = TRUE),
    first_sep_date = min(sep_now_period, na.rm = TRUE),
    first_u_date = min(u_now_period, na.rm = TRUE),
    time_since_ui_start = period_index - first_ui_date,
    first_ui_wave = floor(first_ui_date / 4) + 1,
    t_wave = 4 * floor(time_since_ui_start / 4),
    last_ui_date = max(ui_now_period, na.rm = TRUE),
    time_since_ui_end = period_index - last_ui_date
  )


# prior personal and family income for people with UI
df_full_04 <- df_full_04 %>%
  mutate(
    in_prior_horizon = period_index >= first_ui_date - 12 & period_index < first_ui_date,
    inc_prior_sep_mo = ifelse(in_prior_horizon, tpearn_adj, 0),
    incf_prior_sep_mo = ifelse(in_prior_horizon, tftotinc_adj, 0)
  ) %>%
  group_by(id) %>%
  mutate(
    mos_in_horizon = sum(in_prior_horizon),
    inc_prior_ui = sum(inc_prior_sep_mo) / mos_in_horizon,
    incf_prior_ui = sum(incf_prior_sep_mo) / mos_in_horizon
  )

df_full_04 <-  df_full_04 %>% arrange(id,period_index)

rm(cpi, df_src_04)
