
library(tidyverse)
d = read_csv("output/sensitivity.csv")
#d = read_csv("https://raw.githubusercontent.com/cesaraustralia/SpottedWingDrosophila/master/output/sensitivity.csv")
names(d)
dir.create("SWDplots", showWarnings = F)

for (column in c("meancost", "meanarea")){
  sdcol = sub("mean","sd",column)
  for (i_incursion in unique(d$incursion)) {
    d %>%
      filter(incursion == i_incursion) %>%
      mutate(eradication = factor(eradication)) %>%
      group_by(incursion, eradication, local_quarantine, juristiction_quarantine, trap_density) %>%
      summarise(meanarea = mean(area),
                meancost = mean(cost),
                sdarea = sd(area),
                sdcost = sd(cost)) %>%
      mutate(ymin = get(column) - get(sdcol)) %>%
      mutate(ymax = get(column) + get(sdcol)) %>%
      ggplot(aes(trap_density, get(column), color = eradication)) +
      geom_line() +
      geom_errorbar(aes(ymin = ymin, ymax=ymax)) +
      ylab(column) +
      theme(legend.position = "bottom") +
      facet_grid(local_quarantine ~ juristiction_quarantine , labeller = "label_both") + 
      ggtitle(i_incursion) + 
      theme_bw()
      ggsave(paste0("SWDplots/", column,"_", i_incursion,".png"), height = 9, width = 8)
  }
}
