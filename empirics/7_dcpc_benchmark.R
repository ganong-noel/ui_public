# 7_dcpc_benchmark.R produces online Appendix Table 21 (out/at21_dcpc_dur_share.csv), payment methods analysis. 

dcpc <- readRDS(paste0(data_path, "dcpc_2012_public.Rdata"))
# select relevant variables, use only those from October
# remove single influential outlier
dcpc_use <- dcpc %>%
  select(
    prim_key, ind_weight,
    diary_day, date,
    amnt, amnt_orig,
    pi, merch, type
  ) %>%
  mutate(date_month = format(mdy(date), "%m")) %>%
  filter(date_month == "10") %>%
  mutate(amnt = ifelse(amnt == 70000, NA, amnt))

# set population weight (from CPS)
pop_weight <- 235036000 / 114960000


# replication ####
dcpc_rep <- dcpc_use %>%
  mutate(
    category = fct_collapse(as.factor(merch),
      food = c("1", "2", "3", "10", "11", "12", "13", "14", "32"),
      housing = c("18", "20", "21", "22", "23", "24", "25", "26", "27", "28", "38", "39"),
      transport = c("4", "5", "6", "7", "8", "9", "19"),
      recreation = c("15", "16", "17", "33"),
      health = c("29", "31"),
      finance = "35",
      education = "30",
      charity = c("40", "42", "43", "44"),
      other = c("34", "36", "37", "41", "45")
    )
  )

# find aggregate sum for each category, and find ratio
dcpc_rep_sum <- dcpc_rep %>%
  group_by(category) %>%
  summarise(rep_sum = sum(amnt * ind_weight, na.rm = TRUE) * pop_weight) %>%
  na.omit() %>%
  transmute(
    category = factor(category,
      levels = c(
        "food",
        "housing",
        "transport",
        "recreation",
        "health",
        "finance",
        "education",
        "charity",
        "other"
      )
    ),
    ratio = round(rep_sum / sum(rep_sum), 3)
  )
dcpc_rep_sum <- dcpc_rep_sum[order(dcpc_rep_sum$category), ]


# analysis by durability ####
# define data, and create days individual completed survey
# convert a three-day rate to a monthly rate
dcpc_dur <- dcpc_use %>%
  mutate(
    category = fct_collapse(as.factor(merch),
      strict = c("1", "2", "3", "7", "8", "9", "11", "14", "17", "18", "19", "21", "22", "24", "27", "28", "32", "33", "34", "36", "37", "38"),
      nondurable = c("10", "12", "13", "29", "31"),
      durable = c("4", "5", "6", "15", "16", "20", "23", "25", "30", "35", "39", "40"),
      taxes = "41",
      "inter-household transfer" = c("42", "44"),
      "misc consumption" = "43",
      missing = c("26", "45")
    ),
    payment = fct_collapse(as.factor(pi),
      cash = "1",
      check = "2",
      online = "8",
      credit = "3",
      debit = c("4", "5"),
      ach = "7",
      other = c("6", "9", "11", "12", "13")
    )
  ) %>%
  group_by(prim_key) %>%
  mutate(days_active = max(diary_day) - min(diary_day) + 1) %>%
  ungroup() %>%
  mutate(amnt = amnt * (31 / days_active))

# there are 2468 unique individuals
count <- length(unique(dcpc_dur$prim_key))

# create table of sum spending
# multiply by ratio to get monthly spending for household
dcpc_dur_sum <- dcpc_dur %>%
  group_by(payment, category) %>%
  summarise(spend = sum(amnt * ind_weight, na.rm = TRUE) * pop_weight / count) %>%
  select(payment, category, spend) %>%
  filter(!is.na(payment) & !is.na(category)) %>%
  spread(., key = category, value = spend) %>%
  mutate_all(funs(round(., 2)))

# organise result
dcpc_dur_sum <- dcpc_dur_sum %>%
  ungroup() %>%
  mutate(
    payment = fct_recode(payment,
      "Cash" = "cash",
      "Check" = "check",
      "Online Pay" = "online",
      "Credit Card" = "credit",
      "Debit Card" = "debit",
      "Electronic Transfer" = "ach",
      "Other" = "other"
    ),
    payment = factor(payment,
      levels =
        c(
          "Cash",
          "Check",
          "Credit Card",
          "Debit Card",
          "Electronic Transfer",
          "Online Pay",
          "Other"
        )
    )
  ) %>%
  select(payment, strict, nondurable, durable, taxes, "inter-household transfer", "misc consumption", missing)
dcpc_dur_sum <- dcpc_dur_sum[order(dcpc_dur_sum$payment), ]

dcpc_dur_sum_total <- data.frame(payment = "Total", t(colSums(dcpc_dur_sum[, -1])))
colnames(dcpc_dur_sum_total) <- names(dcpc_dur_sum)
dcpc_dur_sum <- rbind(dcpc_dur_sum, dcpc_dur_sum_total)


# produce Appendix table 20 -- share of spending type by payment method
dcpc_dur_share <- dcpc_dur_sum %>%
  mutate(
    nondurable_share = (strict + nondurable) / (strict + nondurable + durable + taxes + `inter-household transfer`),
    durable_share = durable / (strict + nondurable + durable + taxes + `inter-household transfer`),
    consump_share = nondurable_share + durable_share,
    other_share = (1 - consump_share)
  ) %>%
  select(payment, nondurable_share, durable_share, consump_share, other_share)


dcpc_dur_share <- dcpc_dur_share %>%
  mutate(
    nondurable_share = sprintf("%.0f%%", 100 * nondurable_share),
    durable_share = sprintf("%.0f%%", 100 * durable_share),
    consump_share = sprintf("%.0f%%", 100 * consump_share),
    other_share = sprintf("%.0f%%", 100 * other_share)
  )

write.csv(dcpc_dur_share, file = paste0(out_path, "at21_dcpc_dur_share.csv"))
rm(list = ls(pattern = "^dcpc"))
