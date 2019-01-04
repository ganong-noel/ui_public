# 8_ui_benefits.R calculates average weekly UI benefits in 2015 for online Appendix Figure 2 (out/for_jpmci/avg_weekly_ui_benefit.csv).

ui_benefits <- read.csv(paste0(data_path, "state_weekly_ui_benefits.csv"), skip = 1)
ui_benefits <- ui_benefits %>%
  group_by(State) %>%
  summarise(avg_weekly_ui_benefit = mean(as.numeric(gsub("[\\$,]", "", Weekly.Benefits))))
write.csv(ui_benefits, file = paste0(out_path, "for_jpmci/avg_weekly_ui_benefit.csv"))
rm(ui_benefits)
