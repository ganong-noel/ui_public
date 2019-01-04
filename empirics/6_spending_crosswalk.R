# 6_spending_crosswalk.R produces spending coverage analysis:
# - online appendix Table 2 (out/at2_jpmc.csv), the comparision of monthly spending between JPMC and external CE and PCE surveys.
# - online appendix Table 3 (out/at3_cex.csv), the comparison of monthly spending between JPMC and CE surveys.
# - online appendix Table 4 (out/at4_pce.csv), the comparison of monthly spending between JPMC and PCE surveys.


# jpmc customer month
jpmc_month <- 118018

# pce population (in millions)
pce_population <- 125


# match mcc_code to spending data ####
# load mcc_code info and remove duplicate codes (choose first description)
# data comes from https://github.com/greggles/mcc-codes, which combines USDA and IRS information
# adding missings manually using Citi's MCC information:
# http://www.transactionservices.citigroup.com/transactionservices/home/sa/a2/gsasmartpay2/reference/docs/appendix_h.pdf and Google
# specific hotel and casino names also added manually
mcc_code <- read.csv(paste0(data_path, "mcc_codes.csv")) %>%
  transmute(mcc = mcc_code, mcc_merchant_type = edited_description) %>%
  group_by(mcc) %>%
  filter(mcc_merchant_type == first(mcc_merchant_type))

# load spending data and remove duplicate codes (choose the one with highest spending dollars) or those missing JPMCI group
# remove mcc 6011, which takes up 44% of cash withdrawals of jpmc_cash assigned above
spending <- read.csv(paste0(data_path, "mcc_cat_xwalk_2015_only_prim.csv"), na.strings = "") %>%
  transmute(
    mcc = at_mcccode,
    dollars = (-1) * dollars,
    spend_month = dollars / jpmc_month,
    mcc_grp_jpmci = str_trim(mcc_grp_jpmci)
  ) %>%
  filter(!is.na(mcc) & !is.na(mcc_grp_jpmci) & mcc != 6011) %>%
  group_by(mcc) %>%
  filter(dollars == max(dollars))

# load ACH data, remember 16001 for telecom, 16003 for other utilities, and 17008 for insurance premiums
spending_add <- read.csv(paste0(data_path, "mcc_cat_xwalk_2015_only_prim.csv")) %>%
  filter(is.na(at_mcccode) & !is.na(category_description)) %>%
  transmute(
    ach = at_categoryid2,
    dollars = (-1) * dollars,
    spend_month = dollars / jpmc_month
  )

telecom_ach <- spending_add$spend_month[which(spending_add$ach == 16001)]
utilities_ach <- spending_add$spend_month[which(spending_add$ach == 16003)]
insurance_ach <- spending_add$spend_month[which(spending_add$ach == 17008)]

# calculate sum of uncategorized ACH
spending_add %>%
  filter(!(ach %in% c(16001, 16003, 17008))) %>%
  summarise(sum = sum(spend_month, na.rm = TRUE))

# merge and save data
mcc_matched <- left_join(spending, mcc_code %>% select(mcc, mcc_merchant_type), by = "mcc")

write.csv(mcc_matched, file = paste0(data_path, "mcc_matched.csv"))


# load categorisation, CEX, and PCE data ####
# load categorisation info, and merge MCC match data
# categorisation sheet should have all MCC's in mcc_matched categorised

categorization <- read.csv(paste0(data_path, "mcc_xwalk.csv"), na.strings = "0") %>%
  transmute(
    mcc = `MCC`, mcc_grp_jpmci, mcc_merchant_type = MCC.Merchant.Type,
    ce_category = CE.Survey.Category, pce_category = PCE.Category,
    pce_1 = as.numeric(as.character(pce_1)),
    pce_2 = as.numeric(as.character(pce_2)),
    pce_3 = as.numeric(as.character(pce_3)),
    pce_4 = as.numeric(as.character(pce_4))
  ) %>%
  full_join(., mcc_matched %>% select(mcc, dollars, spend_month), by = "mcc")

# load options info
ce_option <- read.csv(paste0(data_path, "ce_option.csv"), nrows = 34) %>%
  transmute(
    ce_category = CE.Survey.Category,
    ce_group = Group
  ) %>%
  filter(!is.na(ce_category))

pce_option <- read.csv(paste0(data_path, "pce_option.csv"), nrows = 44) %>%
  transmute(
    pce_category = PCE.Category,
    pce_group = Group,
    pce_1, pce_2, pce_3, pce_4
  ) %>%
  filter(!is.na(pce_category))

jpmc_option <- read.csv(paste0(data_path, "Categorization of JPMCI to CE and PCE.csv")) %>%
  select(
    jpmc_category, jpmc_group,
    ce_jpmc_1, ce_jpmc_2, ce_jpmc_3, ce_jpmc_4, ce_jpmc_5,
    pce_jpmc_1, pce_jpmc_2, pce_jpmc_3, pce_jpmc_4, pce_jpmc_5, pce_jpmc_6
  ) %>%
  filter(jpmc_category != "")

# load and clean consumer expenditure survey
cex_orig <- read.csv(paste0(data_path, "cex_2015.csv"), skip = 68, nrows = 588, na.strings = "") %>%
  transmute(item = .[, 1], spend = .[, 2]) %>%
  mutate(
    item = lag(item),
    spend = gsub(",", "", as.character(spend)),
    spend = as.numeric(spend)
  ) %>%
  na.omit(cols = "item") %>%
  filter(!(item %in% c("Mean", "Share", "SE"))) %>%
  mutate(line = 1:n())

# load and clean personal consumption expenditures
pce_orig <- read.csv(paste0(data_path, "pce_2015.csv"), skip = 4, nrows = 335) %>%
  transmute(line = as.numeric(as.character(Line)), item = X., agg = X2015)


# Appendix Table 2 ####
# aggregate according to JPMCI groups
jpmc_mcc <- categorization %>%
  select(mcc, dollars, spend_month, mcc_grp_jpmci) %>%
  mutate(
    jpmc_category = fct_collapse(mcc_grp_jpmci,
                                 'Auto Repair' = c('autopartsmfr','autodealer_svcs','autodealer_repair','autodealer_dur'),
                                 'Department Stores' = 'departmentstore',
                                 'Drug Stores' = 'drugstore',
                                 'Entertainment' = c('entertainment','otherretail_pool'),
                                 'Flights' = 'travel_air',
                                 'Food Away From Home' = c('restaurant_main','restaurant_alc'),
                                 'Groceries' = c('grocery_main','grocery_alc'),
                                 'Ground Transportation' = c('transitcommute','fuel'),
                                 'Home Improvement' = c('homeimprovement_dur','homeimprovement_flowr'),
                                 'Hotels & Rental Cars' = c('travel_auto','travel_lodging','travel_svcs'),
                                 'Insurance' = 'insurance',
                                 'Medical Copay' = c('healthcare_equp_gen','healthcare_equp_oth','healthcare_phys_gen','healthcare_dntl','healthcare_phys_oth','healthcare_optl_gen','healthcare_optl_rtl','healthcare_phys_oth','healthcare_hosp','healthcare_svcs_oth'),
                                 'Miscellaneous Durables' = c('school','notforprofitorg','membershiporg','politcalorg'),
                                 'Miscellaneous Nondurables' = c('others_main','others_unpred','others_tax','electronicsappls_nondur','onlinemarket','directmktgcatalog'),
                                 'Professional & Personal Services' = c('profperslsvcs_main','profperslsvcs_home','profperslsvcs_unpred'),
                                 'Retail Durables' = c('otherretail_dur','electronicsappls_dur'),
                                 'Retail Nondurables' = c('discountstore','otherretail_nondur'),
                                 'Telecom' = 'utilities_telecomm',
                                 'Utilities' = 'utilities_main',
                                 'Not in benchmark' = c('autodealer_mobile', 'healthcare_ambl')
                                 )
    ) %>%
  group_by(jpmc_category) %>%
  summarise(jpmc_spend_month = sum(spend_month, na.rm = TRUE))

# Load UI info
ui_spend <- read.csv(paste0(data_path, "spend_ui.csv")) %>%
  transmute(jpmc_category = as.character(category), ui_spend = spend)

jpmc_mcc <- full_join(jpmc_mcc %>% mutate(jpmc_category = as.character(jpmc_category)), 
                      ui_spend, by = "jpmc_category") %>%
  filter(jpmc_category != "Discount Stores")

jpmc_ce <- jpmc_option %>%
  select(jpmc_category, ce_jpmc_1, ce_jpmc_2, ce_jpmc_3, ce_jpmc_4, ce_jpmc_5) %>%
  gather(key = "ce_line", value = "line", ce_jpmc_1:ce_jpmc_5, na.rm = TRUE) %>%
  left_join(., cex_orig %>% select(line, spend), by = "line") %>%
  group_by(jpmc_category) %>%
  summarise(ce_spend = sum(spend, na.rm = TRUE) / 12)

jpmc_pce <- jpmc_option %>%
  select(jpmc_category, pce_jpmc_1, pce_jpmc_2, pce_jpmc_3, pce_jpmc_4, pce_jpmc_5, pce_jpmc_6) %>%
  gather(key = "pce_line", value = "line", pce_jpmc_1:pce_jpmc_6, na.rm = TRUE) %>%
  left_join(., pce_orig %>% select(line, agg), by = "line") %>%
  group_by(jpmc_category) %>%
  summarise(pce_spend = sum(agg, na.rm = TRUE) / pce_population / 12)

# modify table: reallocate spending
jpmc_tax <- mcc_matched %>%
  filter(mcc == 9211 | mcc == 9222 | mcc == 9223 | mcc == 9311 | mcc == 9405) %>%
  group_by() %>%
  summarise(agg = sum(spend_month, na.rm = TRUE)) %>%
  as.numeric()
jpmc_tax_share <- jpmc_tax / jpmc_mcc$jpmc_spend_month[which(jpmc_mcc$jpmc_category == "Miscellaneous Nondurables")]
jpmc_casino <- mcc_matched %>%
  filter(mcc == 3662 | mcc == 3730 | mcc == 3731 | mcc == 3774) %>%
  group_by() %>%
  summarise(agg = sum(spend_month, na.rm = TRUE)) %>%
  as.numeric()
jpmc_casino_share <- jpmc_casino / jpmc_mcc$jpmc_spend_month[which(jpmc_mcc$jpmc_category == "Hotels & Rental Cars")]
jpmc_departmentstore <- jpmc_mcc$jpmc_spend_month[which(jpmc_mcc$jpmc_category == "Department Stores")]
jpmc_discountstore <- mcc_matched$spend_month[which(mcc_matched$mcc == 5310)]
jpmc_discountstore_share <- jpmc_discountstore / jpmc_mcc$jpmc_spend_month[which(jpmc_mcc$jpmc_category == "Retail Nondurables")]
jpmc_drugstore <- jpmc_mcc$jpmc_spend_month[which(jpmc_mcc$jpmc_category == "Drug Stores")]
jpmc_groceries <- jpmc_mcc$jpmc_spend_month[which(jpmc_mcc$jpmc_category == "Groceries")]
jpmc_wholesale <- mcc_matched$spend_month[which(mcc_matched$mcc == 5300)]
jpmc_wholesale_share <- jpmc_wholesale / jpmc_mcc$jpmc_spend_month[which(jpmc_mcc$jpmc_category == "Retail Nondurables")]

jpmc_tax <- jpmc_tax_share * jpmc_mcc$ui_spend[which(jpmc_mcc$jpmc_category == "Miscellaneous Nondurables")]
jpmc_casino <- jpmc_casino_share * jpmc_mcc$ui_spend[which(jpmc_mcc$jpmc_category == "Hotels & Rental Cars")]
jpmc_discountstore <- jpmc_discountstore_share * jpmc_mcc$ui_spend[which(jpmc_mcc$jpmc_category == "Retail Nondurables")]
jpmc_wholesale <- jpmc_wholesale_share * jpmc_mcc$ui_spend[which(jpmc_mcc$jpmc_category == "Retail Nondurables")]
jpmc_departmentstore <- jpmc_mcc$ui_spend[which(jpmc_mcc$jpmc_category == "Department Stores")]
jpmc_drugstore <- jpmc_mcc$ui_spend[which(jpmc_mcc$jpmc_category == "Drug Stores")]
jpmc_groceries <- jpmc_mcc$ui_spend[which(jpmc_mcc$jpmc_category == "Groceries")]

jpmc_mcc <- jpmc_mcc %>%
  transmute(jpmc_category, jpmc_spend_month = ui_spend)

jpmc_cash <- jpmc_mcc %>%
  filter(jpmc_category %in% c("Cash", "Online", "Miscellaneous Nondurables")) %>%
  select(jpmc_spend_month) %>%
  sum()

jpmc_mcc <- jpmc_mcc %>%
  mutate(
    jpmc_spend_month = ifelse(jpmc_category == "Department Stores", NA, jpmc_spend_month),
    jpmc_spend_month = ifelse(jpmc_category == "Drug Stores", 0.3 * jpmc_drugstore + 0.1 * jpmc_discountstore, jpmc_spend_month),
    jpmc_spend_month = ifelse(jpmc_category == "Entertainment", jpmc_spend_month + 0.55 * jpmc_casino + 0.1 * jpmc_discountstore, jpmc_spend_month),
    jpmc_spend_month = ifelse(jpmc_category == "Food Away From Home", jpmc_spend_month + 0.2 * jpmc_casino, jpmc_spend_month),
    jpmc_spend_month = ifelse(jpmc_category == "Groceries", 0.75 * jpmc_groceries + 0.5 * jpmc_discountstore + 0.6 * jpmc_wholesale, jpmc_spend_month),
    jpmc_spend_month = ifelse(jpmc_category == "Home Improvement", jpmc_spend_month + 0.1 * jpmc_departmentstore + 0.15 * jpmc_discountstore + 0.1 * jpmc_wholesale, jpmc_spend_month),
    jpmc_spend_month = ifelse(jpmc_category == "Hotels & Rental Cars", jpmc_spend_month - (1 - 0.25) * jpmc_casino, jpmc_spend_month),
    jpmc_spend_month = ifelse(jpmc_category == "Medical Copay", jpmc_spend_month + 0.05 * jpmc_wholesale, jpmc_spend_month),
    jpmc_spend_month = ifelse(jpmc_category == "Miscellaneous Nondurables", jpmc_spend_month - jpmc_tax, jpmc_spend_month),
    jpmc_spend_month = ifelse(jpmc_category == "Professional & Personal Services", jpmc_spend_month + 0.1 * jpmc_departmentstore + 0.4 * jpmc_drugstore + 0.1 * jpmc_wholesale, jpmc_spend_month),
    jpmc_spend_month = ifelse(jpmc_category == "Retail Nondurables", jpmc_spend_month + 0.8 * jpmc_departmentstore - (1 - 0.15) * jpmc_discountstore + 0.3 * jpmc_drugstore + 0.25 * jpmc_groceries - (1 - 0.15) * jpmc_wholesale, jpmc_spend_month)
  ) %>%
  na.omit(cols = "jpmc_spend_month")

ce_transportation <- cex_orig$spend[which(cex_orig$item == "Public and other transportation")] / 12
ce_tax <- jpmc_ce$ce_spend[which(jpmc_ce$jpmc_category == "Miscellaneous Nondurables")] * jpmc_tax_share
ce_casino <- jpmc_ce$ce_spend[which(jpmc_ce$jpmc_category == "Hotels & Rental Cars")] * jpmc_casino_share
ce_discountstore <- jpmc_ce$ce_spend[which(jpmc_ce$jpmc_category == "Retail Nondurables")] * jpmc_discountstore_share
ce_drugstore <- jpmc_ce$ce_spend[which(jpmc_ce$jpmc_category == "Drug Stores")]
ce_groceries <- jpmc_ce$ce_spend[which(jpmc_ce$jpmc_category == "Groceries")]
ce_wholesale <- jpmc_ce$ce_spend[which(jpmc_ce$jpmc_category == "Retail Nondurables")] * jpmc_wholesale_share

jpmc_ce <- jpmc_ce %>%
  mutate(
    ce_spend = ifelse(jpmc_category == "Drug Stores", 0.3 * ce_drugstore + 0.1 * ce_discountstore, ce_spend),
    ce_spend = ifelse(jpmc_category == "Entertainment", ce_spend + 0.55 * ce_casino + 0.1 * ce_discountstore, ce_spend),
    ce_spend = ifelse(jpmc_category == "Food Away From Home", ce_spend + 0.2 * ce_casino, ce_spend),
    ce_spend = ifelse(jpmc_category == "Groceries", 0.75 * ce_groceries + 0.5 * ce_discountstore + 0.6 * ce_wholesale, ce_spend),
    ce_spend = ifelse(jpmc_category == "Ground Transportation", ce_spend - 0.64 * ce_transportation, ce_spend),
    ce_spend = ifelse(jpmc_category == "Home Improvement", ce_spend + 0.15 * ce_discountstore + 0.1 * ce_wholesale, ce_spend),
    ce_spend = ifelse(jpmc_category == "Hotels & Rental Cars", ce_spend - (1 - 0.25) * ce_casino, ce_spend),
    ce_spend = ifelse(jpmc_category == "Medical Copay", ce_spend + 0.05 * ce_wholesale, ce_spend),
    ce_spend = ifelse(jpmc_category == "Miscellaneous Nondurables", ce_spend - ce_tax, ce_spend),
    ce_spend = ifelse(jpmc_category == "Professional & Personal Services", ce_spend + 0.4 * ce_drugstore + 0.1 * ce_wholesale, ce_spend),
    ce_spend = ifelse(jpmc_category == "Retail Nondurables", ce_spend + (1 - 0.15) * ce_discountstore + 0.3 * ce_drugstore + 0.25 * ce_groceries - (1 - 0.15) * ce_wholesale, ce_spend)
  )
jpmc_ce[nrow(jpmc_ce) + 1, ] <- list("Flights", 0.64 * ce_transportation)

pce_tax <- jpmc_pce$pce_spend[which(jpmc_pce$jpmc_category == "Miscellaneous Nondurables")] * jpmc_tax_share
pce_casino <- jpmc_pce$pce_spend[which(jpmc_pce$jpmc_category == "Hotels & Rental Cars")] * jpmc_casino_share
pce_discountstore <- jpmc_pce$pce_spend[which(jpmc_pce$jpmc_category == "Retail Nondurables")] * jpmc_discountstore_share
pce_drugstore <- jpmc_pce$pce_spend[which(jpmc_pce$jpmc_category == "Drug Stores")]
pce_groceries <- jpmc_pce$pce_spend[which(jpmc_pce$jpmc_category == "Groceries")]
pce_wholesale <- jpmc_pce$pce_spend[which(jpmc_pce$jpmc_category == "Retail Nondurables")] * jpmc_wholesale_share

jpmc_pce <- jpmc_pce %>%
  mutate(
    pce_spend = ifelse(jpmc_category == "Drug Stores", 0.3 * pce_drugstore + 0.1 * pce_discountstore, pce_spend),
    pce_spend = ifelse(jpmc_category == "Entertainment", pce_spend + 0.55 * pce_casino + 0.1 * pce_discountstore, pce_spend),
    pce_spend = ifelse(jpmc_category == "Food Away From Home", pce_spend + 0.2 * pce_casino, pce_spend),
    pce_spend = ifelse(jpmc_category == "Groceries", 0.75 * pce_groceries + 0.5 * pce_discountstore + 0.6 * pce_wholesale, pce_spend),
    pce_spend = ifelse(jpmc_category == "Home Improvement", pce_spend + 0.15 * pce_discountstore + 0.1 * pce_wholesale, pce_spend),
    pce_spend = ifelse(jpmc_category == "Hotels & Rental Cars", pce_spend - (1 - 0.25) * pce_casino, pce_spend),
    pce_spend = ifelse(jpmc_category == "Medical Copay", pce_spend + 0.05 * pce_wholesale, pce_spend),
    pce_spend = ifelse(jpmc_category == "Miscellaneous Nondurables", pce_spend - pce_tax, pce_spend),
    pce_spend = ifelse(jpmc_category == "Professional & Personal Services", pce_spend + 0.4 * pce_drugstore + 0.1 * pce_wholesale, pce_spend),
    pce_spend = ifelse(jpmc_category == "Retail Nondurables", pce_spend + (1 - 0.15) * pce_discountstore + 0.3 * pce_drugstore + 0.25 * pce_groceries - (1 - 0.15) * pce_wholesale, pce_spend)
  )

jpmc_pce_drugstore <- jpmc_pce$pce_spend[which(jpmc_pce$jpmc_category == "Drug Stores")]
jpmc_pce_medical <- jpmc_pce$pce_spend[which(jpmc_pce$jpmc_category == "Medical Copay")]

jpmc_pce <- jpmc_pce %>%
  mutate(
    pce_spend = ifelse(jpmc_category == "Drug Stores", 0.14 * jpmc_pce_drugstore, pce_spend),
    pce_spend = ifelse(jpmc_category == "Medical Copay", 0.124 * jpmc_pce_medical, pce_spend),
    pce_spend = ifelse(jpmc_category == "Not in JPMCI", pce_spend + (1 - 0.14) * jpmc_pce_drugstore + (1 - 0.124) * jpmc_pce_medical, pce_spend)
  )

jpmc_merged <- jpmc_option %>%
  select(jpmc_group, jpmc_category) %>%
  mutate(jpmc_group = as.character(jpmc_group),
         jpmc_category = as.character(jpmc_category)) %>%
  left_join(., jpmc_mcc, by = "jpmc_category") %>%
  left_join(., jpmc_ce %>% mutate(jpmc_category = as.character(jpmc_category) ), by = "jpmc_category") %>%
  left_join(., jpmc_pce %>% mutate(jpmc_category = as.character(jpmc_category)), by = "jpmc_category") %>%
  mutate(
    jpmc_spend_month = ifelse(jpmc_category == "Cash and Miscellaneous Nondurables", jpmc_cash, jpmc_spend_month),
    ratio_jpmc_ce_month = jpmc_spend_month / ce_spend,
    ratio_jpmc_pce_month = jpmc_spend_month / pce_spend
  ) %>%
  mutate_at(vars(contains("spend")), funs(round(., 0))) %>%
  mutate_at(vars(starts_with("ratio")), funs(round(., 2))) %>%
  select(jpmc_group, jpmc_category, jpmc_spend_month, ce_spend, ratio_jpmc_ce_month, pce_spend, ratio_jpmc_pce_month)

# order according to durability
jpmc_group <- jpmc_merged$jpmc_group
jpmc_merged$jpmc_group <- factor(jpmc_group, levels = c("ND", "D", "Others"))
jpmc_merged <- jpmc_merged[order(jpmc_merged$jpmc_group), ]

# omit unnecessary rows
jpmc_merged <- jpmc_merged %>%
  filter(!(jpmc_category %in% c(
    "Department Stores",
    "Miscellaneous Nondurables"
  ) |
    (jpmc_group == "Others" & !(jpmc_category == "Not in JPMCI"))))

write.csv(jpmc_merged, paste0(out_path, "at2_jpmc.csv"))

# Appendix Table 3 - CEX ####
# choose CEX items of interest
cex_item <- c(as.character(ce_option$ce_category), "Owned dwellings", "Rented dwellings")
cex <- cex_orig %>%
  filter(item %in% cex_item) %>%
  mutate(ce_category = fct_collapse(item,
                                    'Owned and rented dwellings' = c('Owned dwellings','Rented dwellings')
                                    )) %>%
  group_by(ce_category) %>%
  summarise(ce_spend = sum(spend, na.rm = TRUE) / 12)

# match JPMCI spending with CEX categories
mcc_cex <- categorization %>%
  group_by(ce_category) %>%
  summarise(jpmc_spend_month = sum(spend_month, na.rm = TRUE)) %>%
  mutate(ce_category = as.character(ce_category)) %>%
  full_join(., cex %>% select(ce_category, ce_spend) %>% mutate(ce_category = as.character(ce_category)), by = "ce_category") %>%
  full_join(., ce_option %>% select(ce_category, ce_group) %>% mutate(ce_category = as.character(ce_category)), by = "ce_category")

# modify table: reallocate spending
mcc_cex <- mcc_cex %>%
  group_by(ce_category) %>%
  summarise(
    jpmc_spend_month = sum(jpmc_spend_month, na.rm = TRUE),
    ce_spend = sum(ce_spend, na.rm = TRUE)
  )

mcc_cex <- mcc_cex %>%
  mutate(
    jpmc_spend_month = ifelse(ce_category == "Apparel and services", jpmc_spend_month - (1 - 0.8) * jpmc_departmentstore + 0.15 * jpmc_discountstore, jpmc_spend_month),
    jpmc_spend_month = ifelse(ce_category == "Cash", NA, jpmc_spend_month),
    jpmc_spend_month = ifelse(ce_category == "Drugs", 0.3 * jpmc_drugstore + 0.1 * jpmc_discountstore, jpmc_spend_month),
    jpmc_spend_month = ifelse(ce_category == "Food at home", jpmc_spend_month - (1 - 0.5) * jpmc_discountstore - (1 - 0.75) * jpmc_groceries - (1 - 0.6) * jpmc_wholesale, jpmc_spend_month),
    jpmc_spend_month = ifelse(ce_category == "Food away from home", jpmc_spend_month + 0.2 * jpmc_casino, jpmc_spend_month),
    jpmc_spend_month = ifelse(ce_category == "Household operations", jpmc_spend_month + 0.1 * jpmc_departmentstore + 0.15 * jpmc_discountstore + 0.1 * jpmc_wholesale, jpmc_spend_month),
    jpmc_spend_month = ifelse(ce_category == "Housekeeping supplies", jpmc_spend_month + 0.25 * jpmc_groceries, jpmc_spend_month),
    jpmc_spend_month = ifelse(ce_category == "Medical supplies", jpmc_spend_month + 0.05 * jpmc_wholesale, jpmc_spend_month),
    jpmc_spend_month = ifelse(ce_category == "Miscellaneous", jpmc_spend_month + 0.3 * jpmc_drugstore, jpmc_spend_month),
    jpmc_spend_month = ifelse(ce_category == "Other entertainment supplies, equipment, and services", jpmc_spend_month - (1 - 0.55) * jpmc_casino + 0.1 * jpmc_discountstore + 0.15 * jpmc_wholesale, jpmc_spend_month),
    jpmc_spend_month = ifelse(ce_category == "Other lodging", jpmc_spend_month + 0.25 * jpmc_casino, jpmc_spend_month),
    jpmc_spend_month = ifelse(ce_category == "Personal care products and services", jpmc_spend_month + 0.1 * jpmc_departmentstore + 0.4 * jpmc_drugstore + 0.1 * jpmc_wholesale, jpmc_spend_month),
    jpmc_spend_month = ifelse(ce_category == "Nondurable Cash and Online Pay", dcpc$nondurable_share * jpmc_cash, jpmc_spend_month),
    jpmc_spend_month = ifelse(ce_category == "Durable Cash and Online Pay", dcpc$durable_share * jpmc_cash, jpmc_spend_month)
  )

mcc_cex <- mcc_cex %>%
  mutate(
    jpmc_spend_month = ifelse(ce_category == "Utilities, fuels, and public services", telecom_ach + utilities_ach, jpmc_spend_month),
    jpmc_spend_month = ifelse(ce_category == "Health insurance", insurance_ach, jpmc_spend_month)
  )

# modify table: suppress and merge rows
jpmc_total_month <- sum(mcc_cex$jpmc_spend_month, na.rm = TRUE)
ce_total <- sum(mcc_cex$ce_spend, na.rm = TRUE)

mcc_cex <- mcc_cex %>%
  mutate(
    ce_category = fct_collapse(as.factor(ce_category),
                               'Medical' = c('Medical services','Medical supplies'),
                               'Not comparable' = c('Audio and visual equipment and services','Pensions and Social Security','Health insurance','Life and other personal insurance','Vehicle purchases (net outlay)','Owned and rented dwellings','Cash contributions')
                  )) %>%
  group_by(ce_category) %>%
  summarise(
    jpmc_spend_month = sum(jpmc_spend_month, na.rm = TRUE),
    ce_spend = sum(ce_spend, na.rm = TRUE)
  ) %>%
  mutate(
    share_jpmc_month = jpmc_spend_month / jpmc_total_month,
    share_ce = ce_spend / ce_total,
    ratio_jpmc_ce_month = jpmc_spend_month / ce_spend
  ) %>%
  mutate_at(vars(contains("spend")), funs(round(., 0))) %>%
  mutate_at(vars(starts_with("share")), funs(round(., 2))) %>%
  mutate_at(vars(starts_with("ratio")), funs(round(., 2))) %>%
  mutate(ce_category = as.character(ce_category)) %>%
  left_join(., ce_option %>% select(ce_category, ce_group) %>% mutate(ce_category = as.character(ce_category)), by = "ce_category") %>%
  na.omit(cols = "ce_category") %>%
  select(ce_group, ce_category, jpmc_spend_month, share_jpmc_month, ce_spend, share_ce, ratio_jpmc_ce_month)

# order according to durability
ce_group <- mcc_cex$ce_group
mcc_cex$ce_group <- factor(ce_group, levels = c("ND", "D", "Others"))
mcc_cex <- mcc_cex[order(mcc_cex$ce_group), ]

write.csv(mcc_cex, paste0(out_path, "at3_cex.csv"))

# Appendix Table 4 - PCE ####
# choose pce items of interest
pce <- pce_option %>%
  select(pce_category, pce_1, pce_2, pce_3, pce_4) %>%
  gather(key = "pce_line", value = "line", pce_1:pce_4) %>%
  left_join(., pce_orig %>% select(line, agg), by = "line") %>%
  group_by(pce_category) %>%
  summarise(pce_spend = sum(agg, na.rm = TRUE) / pce_population / 12)

# match jpmc spending with pce categories
mcc_pce <- categorization %>%
  group_by(pce_category) %>%
  summarise(jpmc_spend_month = sum(spend_month, na.rm = TRUE)) %>%
  mutate(pce_category = as.character(pce_category)) %>%
  full_join(., pce %>% select(pce_category, pce_spend) %>% mutate(pce_category = as.character(pce_category)), by = "pce_category")

# modify table: reallocate spending
mcc_pce <- mcc_pce %>%
  group_by(pce_category) %>%
  summarise(
    jpmc_spend_month = sum(jpmc_spend_month, na.rm = TRUE),
    pce_spend = sum(pce_spend, na.rm = TRUE)
  )

mcc_pce <- mcc_pce %>%
  mutate(
    jpmc_spend_month = ifelse(pce_category == "Cash", NA, jpmc_spend_month),
    jpmc_spend_month = ifelse(pce_category == "Clothing and footwear", jpmc_spend_month - (1 - 0.8) * jpmc_departmentstore + 0.15 * jpmc_discountstore, jpmc_spend_month),
    jpmc_spend_month = ifelse(pce_category == "Pharmaceutical products", 0.3 * jpmc_drugstore + 0.1 * jpmc_discountstore + 0.05 * jpmc_wholesale, jpmc_spend_month),
    jpmc_spend_month = ifelse(pce_category == "Food and nonalcoholic beverages purchased for off-premises consumption", jpmc_spend_month - (1 - 0.5) * jpmc_discountstore - (1 - 0.75) * jpmc_groceries - (1 - 0.6) * jpmc_wholesale, jpmc_spend_month),
    jpmc_spend_month = ifelse(pce_category == "Food services", jpmc_spend_month + 0.2 * jpmc_casino, jpmc_spend_month),
    jpmc_spend_month = ifelse(pce_category == "Gambling", jpmc_spend_month - (1 - 0.4) * jpmc_casino, jpmc_spend_month),
    jpmc_spend_month = ifelse(pce_category == "Hotels and motels", jpmc_spend_month + 0.25 * jpmc_casino, jpmc_spend_month),
    jpmc_spend_month = ifelse(pce_category == "Housing supplies", jpmc_spend_month + 0.1 * jpmc_departmentstore + 0.15 * jpmc_discountstore + 0.25 * jpmc_groceries + 0.1 * jpmc_wholesale, jpmc_spend_month),
    jpmc_spend_month = ifelse(pce_category == "Personal care products and services", jpmc_spend_month + 0.1 * jpmc_departmentstore + 0.4 * jpmc_drugstore + 0.15 * jpmc_wholesale, jpmc_spend_month),
    jpmc_spend_month = ifelse(pce_category == "Recreation - Other", jpmc_spend_month + 0.3 * jpmc_drugstore + 0.15 * jpmc_casino, jpmc_spend_month),
    jpmc_spend_month = ifelse(pce_category == "Video, audio, photographic, and information processing equipment, media, and services", jpmc_spend_month + 0.1 * jpmc_discountstore + 0.15 * jpmc_wholesale, jpmc_spend_month),
    jpmc_spend_month = ifelse(pce_category == "Nondurable Cash and Online Pay", dcpc$nondurable_share * jpmc_cash, jpmc_spend_month),
    jpmc_spend_month = ifelse(pce_category == "Durable Cash and Online Pay", dcpc$durable_share * jpmc_cash, jpmc_spend_month)
  )

mcc_pce <- mcc_pce %>%
  mutate(
    jpmc_spend_month = ifelse(pce_category == "Telecommunication services", telecom_ach, jpmc_spend_month),
    jpmc_spend_month = ifelse(pce_category == "Household utilities", utilities_ach, jpmc_spend_month),
    jpmc_spend_month = ifelse(pce_category == "Health insurance", insurance_ach, jpmc_spend_month)
  )

# modify table: suppress and merge rows
jpmc_total_month <- sum(mcc_pce$jpmc_spend_month, na.rm = TRUE)
pce_total <- sum(mcc_pce$pce_spend, na.rm = TRUE)

mcc_pce <- mcc_pce %>%
  mutate(
    pce_category = fct_collapse(as.factor(pce_category),
                                'Recreation - Other' = 'Gambling',
                                'Telecom' = c('Telecommunication services','Internet access'),
                                'Medical' = c('Outpatient services','Hospital and nursing home services','Other medical products'),
                                'Not comparable' = c('Health insurance','Motor vehicles','Life insurance','Net foreign spending','Housing','Package tours','Financial services')
    )) %>%
  group_by(pce_category) %>%
  summarise(
    jpmc_spend_month = sum(jpmc_spend_month, na.rm = TRUE),
    pce_spend = sum(pce_spend, na.rm = TRUE)
  )

# modify table: reallocate PCE spending
pce_drug <- mcc_pce$pce_spend[which(mcc_pce$pce_category == "Pharmaceutical products")]
pce_medical <- mcc_pce$pce_spend[which(mcc_pce$pce_category == "Medical")]

mcc_pce <- mcc_pce %>%
  mutate(
    pce_spend = ifelse(pce_category == "Pharmaceutical products", 0.14 * pce_drug, pce_spend),
    pce_spend = ifelse(pce_category == "Medical", 0.124 * pce_medical, pce_spend),
    pce_spend = ifelse(pce_category == "Not comparable", pce_spend + (1 - 0.14) * pce_drug + (1 - 0.124) * pce_medical, pce_spend),
    share_jpmc_month = jpmc_spend_month / jpmc_total_month,
    share_pce = pce_spend / pce_total,
    ratio_jpmc_pce_month = jpmc_spend_month / pce_spend
  ) %>%
  mutate_at(vars(contains("spend")), funs(round(., 0))) %>%
  mutate_at(vars(starts_with("share")), funs(round(., 2))) %>%
  mutate_at(vars(starts_with("ratio")), funs(round(., 2))) %>%
  mutate(pce_category = as.character(pce_category)) %>%
  left_join(., pce_option %>% select(pce_category, pce_group) %>% mutate(pce_category = as.character(pce_category)), by = "pce_category") %>%
  na.omit(cols = "pce_category") %>%
  select(pce_group, pce_category, jpmc_spend_month, pce_spend, ratio_jpmc_pce_month)

# order according to durability
pce_group <- mcc_pce$pce_group
mcc_pce$pce_group <- factor(pce_group, levels = c("ND", "D", "Others"))
mcc_pce <- mcc_pce[order(mcc_pce$pce_group, mcc_pce$pce_category), ]

write.csv(mcc_pce, paste0(out_path, "at4_pce.csv"))

rm(list = ls(pattern = "^jpmc"))
rm(list = ls(pattern = "^pce"))
rm(list = ls(pattern = "^mcc"))
rm(list = ls(pattern = "^cex"))
rm(list = ls(pattern = "^spending"))
rm(categorization, ce_option, ui_spend)
