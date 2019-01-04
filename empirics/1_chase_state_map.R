# 1_chase_state_map.R produces online Appendix Figure 1 (out/chase_footprint.png), 
# map of states with Chase branches and direct deposit of UI benefits. 

#Source: http://eriqande.github.io/rep-res-web/lectures/making-maps-with-R.html 
states <- map_data("state") %>%
  mutate(region = factor(region))
branches <- read.csv("data/state_name_chase_branches.csv")
branches <- branches %>% select(region, branch_2)
branches <- branches %>% mutate(`Chase & UI` = ifelse(branch_2 == "Y","Yes","No"))
states <- left_join(states, branches, by="region")
ggplot(data = states) + 
  geom_polygon(aes(x = long, y = lat, fill = `Chase & UI`, group = group), color = "black") + 
  scale_fill_manual(values = c("white", cb_palette[1])) +
  coord_fixed(1.3) +
  fte_theme() +
  labs(title="States with Chase Branches and Direct Deposit of UI Benefits",x="",y="") +  
  theme(axis.text.x=element_blank(), axis.text.y=element_blank(),
        legend.position = c(1,0), legend.justification = c(1,0))
ggsave(paste0(out_path, "chase_footprint.png"), width= 6 , height = 4) 

rm(states, branches)
