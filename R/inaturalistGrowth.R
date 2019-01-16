
# plot gbifsnapshots 
library(dplyr)
library(purrr)

citizenSciencekeys = gbifapi::getCitizenScienceKeys() # get marie cs

D = gbifsnapshots::countsByDatasetkey # all occ counts by datasetkey 

csTotals = D %>% filter(datasetkey %in% citizenSciencekeys) %>%  # citizen science 
group_by(snapshots,created,datasetkey) %>%
summarise(totalCsCounts = sum(countByDatasetkey)) %>% 
filter(datasetkey == "50c9509d-22c7-4a22-a47d-8c48425ef4a7") %>%
as.data.frame()

csTotals

library(ggplot2)
library(ggthemes)

Title = "Rapid growth of iNaturalist - Research Grade Observations"
Subtitle = "Total number of occurrences records from iNautralist on GBIF since 2012"

ggplot(csTotals,aes(created, totalCsCounts)) +
geom_line(color="#509E2F") +
geom_point(color="#509E2F") +
scale_y_continuous(breaks = breaks,label = label) +
theme_hc() +
ggtitle(label = Title, subtitle = Subtitle) +
ylab("total number of occurrence records") +
xlab("") +
guides(colour=guide_legend(title="")) +
theme(plot.caption = element_text(hjust = 0)) +
theme(legend.position = c(.925,.12))
# 
# scale_colour_manual(values = c("#509E2F")) +
# labs(caption = paste(strwrap(caption,90), collapse="\n")) +


# csTotals




