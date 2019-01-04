# 2_scf.R uses SCF to produces 
# - statistics for statements in the text:
#    - "We calculate using the SCF that about 15 percent of labor income is paid by paper checks and pre-paid debit cards 
#       rather than by direct deposit."
#    - "90 percent of households reporting UI income in the past year in the SCF had a bank account at the time of the survey."
#    - "We estimate using the SCF that 64 percent of UI recipients with a bank account have an outside credit card." 
#    - "According to the Department of Labor Benefit Accuracy Measurement system, in states where the maximum potential 
#       duration of UI benefits is six months, about 15 percent of UI recipients are eligible for five months of benefits."
# - checking account balance in online Appendix Figure 2 used for representativeness analysis (out/for_jpmci/scf_ck_bal.csv).
# - online Appendix Table 6 (out/for_jpmci/at6_scf_liq_asset.csv), the distribution of all liquid assets and checking account balances for representativeness analysis.


# helpful description of summary vars:
# http://www.wealthandwant.com/issues/wealth/SCF_defs.html
# helpful summary stats (warning: medians are conditional on > 0)
# http://www.federalreserve.gov/pubs/bulletin/2012/articles/scf/scf.htm

df_src <- readRDS(paste0(data_path, "scf/p13i6.Rdata"))
sum_src <- readRDS(paste0(data_path, "scf/rscfp2013.Rdata"))

df_src_select <- df_src %>%
  transmute(
    caseid = Y1,
    bal_chk_prim = X3506, ui_any = X5715,
    emplr = X4106, emplr_spouse = X4706, any_cc = X410 == 1,
    one_employee = (emplr == 1 & emplr_spouse != 1) | (emplr != 1 & emplr_spouse == 1),
    cc_chg_1 = X412, cc_chg_2 = X420, cc_chg_3 = X426, cc_chg_4 = X429,
    cc_pay_construct = cc_chg_1 + cc_chg_2 + cc_chg_3 + cc_chg_4,
    payroll_dd = X7123,
    n_visa_mc = X411, n_store = X419, n_amex = X425, n_cc_oth = X428,
    n_cc_tot = n_visa_mc + n_store + n_amex + n_cc_oth,
    num_cc = X411 + X419 + X425 + X428,
    exact_one_cc = num_cc == 1,
    more_one_cc = num_cc > 1
  )

df <- sum_src %>%
  transmute(
    caseid = Y1,
    mortpay, liq, saving, nochk, ehchkg, wageinc, wgt, income,
    veh_pay = PAYVEH1 + PAYVEH2 + PAYVEH3 + PAYVEH4 + payvehm,
    edu_pay = PAYEDU1 + PAYEDU2 + PAYEDU3 + PAYEDU4 + PAYEDU5 + PAYEDU6 + PAYEDU7,
    retqliq
  ) %>%
  inner_join(df_src_select, by = "caseid")


# other clean up
df <- df %>%
  mutate(
    any_wage_emp = emplr == 1 | emplr_spouse == 1,
    bal_chk_prim = ifelse(is.na(bal_chk_prim), 0, bal_chk_prim),
    payroll_dd = ifelse(nochk == 1, NA, !is.na(payroll_dd) & payroll_dd == 1),
    base_samp = any_wage_emp & wageinc > 15000
  )


# stats for text ####

frac_payroll_dd2 <- df %>%
  filter(base_samp) %>%
  summarise_at(vars(payroll_dd), funs(weighted.mean(., w = wgt, na.rm = TRUE)))
test_that("fact 1 -- what fraction get any DD of payroll?", 
          expect_equal(paste0(round(frac_payroll_dd2*100,0), "%"), "85%"))

frac_ui_nochk <- df %>%
  filter(ui_any == 1) %>%
  summarise_at(vars(nochk), funs(1 - weighted.mean(., w = wgt, na.rm = TRUE)))
test_that("fact 2 -- what fraction of UI recipients have checking accounts?",
          expect_equal(paste0(round(frac_ui_nochk*100, 0), "%"), "90%"))

frac_ui_anycc <- df %>%
  filter(ui_any == 1 & nochk == 0) %>%
  summarise_at(vars(any_cc), funs(weighted.mean(., w = wgt, na.rm = TRUE)))
test_that("what fraction of UI recipients with bank account have credit cards?",
          expect_equal(paste0(round(frac_ui_anycc*100, 0), "%"), "72%"))

frac_ui_one_cc <- df %>%
  filter(ui_any == 1 & nochk == 0) %>%
  summarise_at(vars(exact_one_cc), funs(weighted.mean(., w = wgt, na.rm = TRUE)))
test_that("what fraction of UI recipients with bank account have exactly one credit card?",
          expect_equal(paste0(round(frac_ui_one_cc*100, 0), "%"), "15%"))

frac_ui_more_one_cc <- df %>%
  filter(ui_any == 1 & nochk == 0) %>%
  summarise_at(vars(more_one_cc), funs(weighted.mean(., w = wgt, na.rm = TRUE)))
test_that("what fraction of UI recipients with bank account have more than one credit card?",
          expect_equal(paste0(round(frac_ui_more_one_cc*100, 0), "%"), "57%"))

# 37% of UI recipients have a Chase credit card
frac_ui_chase_cc <- 0.37
frac_ui_chk_non_chase_cc <- frac_ui_one_cc * (1 - frac_ui_chase_cc / frac_ui_anycc) + frac_ui_more_one_cc
test_that("what fraction of UI recipients with bank account have a non-Chase credit card?",
          expect_equal(paste0(round(frac_ui_chk_non_chase_cc*100, 0), "%"), "64%"))

state_popu <- read.csv(paste0(data_path, "state_population_share.csv"), skip = 1)
state_popu_share_less_22 <- state_popu %>%
  mutate(share_less_22 = (Less.Than.10 + X10.to.14 + X15.to.19 + X20.to.21) / Total.Potential) %>% 
  summarise(avg_share_less_22 = mean(share_less_22))
test_that("what share of population eligible for less than 22 weeks of benefits?",
          expect_equal(paste0(round(state_popu_share_less_22*100, 0), "%"), "15%"))


# Fig A.2 summary stats -- checking acct balance ####
liq_bin <- df %>%
  filter(base_samp & nochk == 0 & bal_chk_prim >= 0) %>%
  select(bal_chk_prim, wgt) %>%
  mutate(asset_bin = ifelse(bal_chk_prim < 13500, floor(bal_chk_prim / 500) * 500, 13500)) %>%
  group_by(asset_bin) %>%
  summarise(n = sum(wgt)) %>%
  mutate(`SCF Employed` = n / sum(n))
write.csv(liq_bin, file = paste0(out_path, "for_jpmci/scf_ck_bal.csv"))


# Table A.6 -- distribution of all liquid assets and checking acct balance ####
liq <- df %>%
  filter(base_samp & nochk == 0) %>%
  select(liq, wgt) %>%
  summarise(
    p10 = wtd.quantile(liq, q = 0.1, weight = wgt, na.rm = TRUE),
    p50 = wtd.quantile(liq, q = 0.5, weight = wgt, na.rm = TRUE),
    p90 = wtd.quantile(liq, q = 0.9, weight = wgt, na.rm = TRUE),
    mean = weighted.mean(liq, w = wgt, na.rm = TRUE)
  ) %>%
  mutate_all(funs(round(., 0)))
bal_chk_prim <- df %>%
  filter(base_samp & nochk == 0) %>%
  select(bal_chk_prim, wgt) %>%
  summarise(
    p10 = wtd.quantile(bal_chk_prim, q = 0.1, weight = wgt, na.rm = TRUE),
    p50 = wtd.quantile(bal_chk_prim, q = 0.5, weight = wgt, na.rm = TRUE),
    p90 = wtd.quantile(bal_chk_prim, q = 0.9, weight = wgt, na.rm = TRUE),
    mean = weighted.mean(bal_chk_prim, w = wgt, na.rm = TRUE)
  ) %>%
  mutate_all(funs(round(., 0)))
rbind(liq, bal_chk_prim)
write.csv(rbind(liq, bal_chk_prim), file = paste0(out_path, "at6_scf_liq_asset.csv"))

rm(list = ls(pattern = "^df"))
rm(list = ls(pattern = "^liq"))
rm(list = ls(pattern = "^frac"))
rm(bal_chk_prim, sum_src)
rm(state_popu, state_popu_share_less_22)
