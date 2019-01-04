# 5_sipp_who_are_ui_recipients.R produces 
# - online appendix Table 5 (out/for_jpmci/at5_sipp_inc.csv), representativeness analysis in income between JPMC and SIPP. 
# - age distribution in online Appendix Figure 2 used for representativeness analysis (out/sipp_wt_age.csv).
# - distribution of income prior to UI receipt in online Appendix Figure 2 used for representativeness analysis (out/sipp_wt_inc_10k.csv).

# make representativeness table ####
df_in_rep <- df_full_04 %>%
  select(
    id, period_index, wpfinwgt, wffinwgt, tage,
    first_ui_date, first_u_date, any_ui, any_unemp,
    tpearn_adj, tfearn_adj, tftotinc_adj, t05amt_adj, tftotinc, rfpov
  )

# add date of pseudo employment
set.seed(123)
rand_dt <- df_in_rep %>%
  group_by(id) %>%
  summarise(
    last_date = max(period_index)
  ) %>%
  filter(last_date >= 16) %>%
  mutate(rand_emp_date = floor(runif(1, min = 16, max = last_date)))

system.time(rand_dt_emp <- rand_dt %>%
  inner_join(df_in_rep %>% select(id, period_index, tpearn_adj), by = c("id" = "id", "rand_emp_date" = "period_index")))
rand_dt_emp <- rand_dt_emp %>%
  mutate(rand_emp_date = ifelse(tpearn_adj > 0, rand_emp_date, NA)) %>%
  select(id, rand_emp_date)
system.time(df_in_rep <- df_in_rep %>% left_join(rand_dt_emp, by = "id"))

df_in_rep <- df_in_rep %>% group_by(id) %>% mutate(
  in_horizon_e = period_index >= rand_emp_date - 15 & period_index <= rand_emp_date - 4,
  in_horizon_u = period_index >= first_u_date - 15 & period_index <= first_u_date - 4,
  in_horizon_ui = period_index >= first_ui_date - 15 & period_index <= first_ui_date - 4,
  in_pov = tftotinc < rfpov
)

# split up into three DFs
df_in_rep_e <- df_in_rep %>%
  filter(sum(in_horizon_e) == 12) %>%
  rename(in_horizon = in_horizon_e, start_date = rand_emp_date) %>%
  select(-first_u_date, -first_ui_date, -in_horizon_u, -in_horizon_ui) %>%
  mutate(key = "Employed")
df_in_rep_u <- df_in_rep %>%
  filter(sum(in_horizon_u) == 12 & any_unemp == TRUE) %>%
  rename(in_horizon = in_horizon_u, start_date = first_u_date) %>%
  select(-rand_emp_date, -first_ui_date, -in_horizon_e, -in_horizon_ui) %>%
  mutate(key = "Unemployed: All")
df_in_rep_ui <- df_in_rep %>%
  filter(sum(in_horizon_ui) == 12 & any_ui == TRUE) %>%
  rename(in_horizon = in_horizon_ui, start_date = first_ui_date) %>%
  select(-rand_emp_date, -first_u_date, -in_horizon_u, -in_horizon_e) %>%
  mutate(key = "Unemployed: Get UI")

df_in_rep_rbind <- rbind(df_in_rep_e, df_in_rep_u, df_in_rep_ui)
df_in_rep_rbind <- df_in_rep_rbind %>%
  group_by(id, key) %>%
  mutate(
    tpearn_adj_prior = sum(tpearn_adj * in_horizon) / 12,
    tfearn_adj_prior = sum(tfearn_adj * in_horizon) / 12,
    tftotinc_adj_prior = sum(tftotinc_adj * in_horizon) / 12,
    in_pov_prior = sum(in_pov * in_horizon) / 12
  )

# now, select month of spell start
df_in_rep_rbind <- df_in_rep_rbind %>%
  filter(period_index == start_date) %>%
  mutate(
    other_earn = tfearn_adj - tpearn_adj, `Others' Earn` = tfearn_adj_prior - tpearn_adj_prior,
    `Fam Inc` = tftotinc_adj_prior, `Poverty Rate` = in_pov_prior,
    `Fam Earn` = tfearn_adj_prior, `Person Earn` = tpearn_adj_prior,
    `Other Earn > 0` = other_earn > 0, `Under 21` = tage <= 20
  )

# two offsetting forces. 60% of HHs have other earners. in those HHs, the other earner makes a lot more.
tbl_in <- df_in_rep_rbind %>%
  group_by(key) %>%
  select(
    key, `Fam Inc`, `Poverty Rate`, `Under 21`, `Fam Earn`, `Person Earn`, `Others' Earn`, wpfinwgt,
    `Other Earn > 0`
  ) %>%
  summarise_at(vars(-wpfinwgt, -key), funs(weighted.mean(., na.rm = TRUE, w = wpfinwgt)))

tbl_in_dd_adj <- df_in_rep_rbind %>%
  ungroup() %>%
  filter(key == "Unemployed: Get UI") %>%
  mutate(key = "Unemployed: Get UI, Pay DD") %>%
  group_by(key) %>%
  transmute(`Fam Inc`, `Poverty Rate`, `Under 21`, `Fam Earn`,
    `Person Earn` = `Person Earn` * pay_dd, `Others' Earn` = `Others' Earn` * pay_dd, wpfinwgt,
    `Other Earn > 0` = `Other Earn > 0` * pay_dd
  ) %>%
  summarise_at(vars(-wpfinwgt, -key), funs(weighted.mean(., na.rm = TRUE, w = wpfinwgt)))

inflow_tbl <- rbind(tbl_in, tbl_in_dd_adj)
inflow_tbl <- df_in_rep_rbind %>%
  group_by(key) %>%
  summarise(`Median Fam Inc` = weightedMedian(tftotinc_adj_prior, na.rm = TRUE, w = wpfinwgt)) %>%
  inner_join(inflow_tbl, by = "key") %>%
  select(key, `Under 21`, `Median Fam Inc`, `Fam Inc`, `Poverty Rate`, `Fam Earn`, `Person Earn`, `Other Earn > 0`, `Others' Earn`)

write.csv(inflow_tbl, paste0(out_path, "at5_sipp_inc.csv"))

# study marginal distributions

# age distributions ####
mean_age <- df_full_04 %>%
  filter(first_ui_date >= 13 & any_ui & !is.na(incf_prior_ui)) %>%
  ungroup() %>%
  summarise_at(vars(hfam_age), funs(weighted.mean(., wpfinwgt)))
test_that("mean age in SIPP is",  expect_equal(as.character(round(mean_age, 0)), "44"))

wt_age <- df_full_04 %>%
  filter(first_ui_date >= 13 & any_ui & !is.na(incf_prior_ui)) %>%
  group_by(hfam_age_bin) %>%
  distinct(id_fam, .keep_all = TRUE) %>%
  summarise(wt = sum(wpfinwgt)) %>%
  mutate(share = wt / sum(wt))
write.csv(wt_age, paste0(out_path, "for_jpmci/sipp_wt_age.csv"))

# distribution of income prior to UI receipt ####
bin_w <- 10000
df_inc_ui_prior <- df_full_04 %>%
  group_by(id) %>%
  filter(period_index >= first_ui_date - 12 & period_index < first_ui_date) %>%
  filter(max(period_index) - min(period_index) == 11) %>%
  summarise(
    mos_in_horizon = n(), wt = sum(wpfinwgt) / mos_in_horizon,
    incef_prior_ui = 12 * sum(tfearn_adj) / mos_in_horizon,
    inctf_prior_ui = 12 * sum(tftotinc_adj) / mos_in_horizon,
    ince_bin = ifelse(incef_prior_ui < 200000, floor(incef_prior_ui / bin_w) * bin_w, 200000),
    inct_bin = ifelse(inctf_prior_ui < 200000, floor(inctf_prior_ui / bin_w) * bin_w, 200000)
  )
df_inc_prior <- df_full_04 %>%
  group_by(id) %>%
  filter(period_index <= 12) %>%
  mutate(x = max(period_index) - min(period_index) == 12) %>%
  summarise(
    mos_in_horizon = n(), wt = sum(wpfinwgt) / mos_in_horizon,
    incef_prior = 12 * sum(tfearn_adj) / mos_in_horizon,
    inctf_prior = 12 * sum(tftotinc_adj) / mos_in_horizon,
    ince_bin = ifelse(incef_prior < 200000, floor(incef_prior / bin_w) * bin_w, 200000),
    inct_bin = ifelse(inctf_prior < 0, 0, ifelse(inctf_prior < 200000, floor(inctf_prior / bin_w) * bin_w, 200000))
  )

wt_inc <- cbind(
  df_inc_prior %>% group_by(ince_bin) %>% summarise(wt = sum(wt)) %>% mutate(pct_earn = wt / sum(wt)),
  df_inc_prior %>% group_by(inct_bin) %>% summarise(wt = sum(wt)) %>% mutate(pct_inc = wt / sum(wt)),
  df_inc_ui_prior %>% group_by(ince_bin) %>% summarise(wt = sum(wt)) %>% mutate(pct_earn_ui = wt / sum(wt)),
  df_inc_ui_prior %>% group_by(inct_bin) %>% summarise(wt = sum(wt)) %>% mutate(pct_inc_ui = wt / sum(wt))
)
write.csv(wt_inc, paste0(out_path, "for_jpmci/sipp_wt_inc_10k.csv"))

rm(list = ls(pattern = "^df"))
rm(list = ls(pattern = "^wt"))
rm(list = ls(pattern = "^rand"))
rm(list = ls(pattern = "^tbl"))
rm(inflow_tbl)
