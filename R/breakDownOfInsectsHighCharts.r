# Break Down of Insects


library(highcharter)
library(roperators)
library(dplyr)
library(purrr)
library(forcats)

load("C:/Users/ftw712/Desktop/citizen science/data/speciesKeyCounts.rda")
D = speciesKeyCounts
citizenSciencekeys = gbifapi::getCitizenScienceKeys()

D = D %>% 
  group_by(specieskeys) %>%
  summarise(occCounts = sum(occCounts)) %>%
  arrange(-occCounts) %>%
  mutate(specieskeys = fct_reorder(specieskeys, occCounts)) %>% 
  gbifapi::addTaxonInfo(specieskeys) %>% # add kingdom class phylum ect to data.frame
  as.data.frame() 
  # filter(occCounts > 50) %>%

# plot categroies 
D$plotCat = "Other"
D$plotCat[D$class == "Aves"] = "Bird"
D$plotCat[D$class == "Insecta"] = "Insect"
D$plotCat[D$class == "Mammalia"] = "Mammal"
D$plotCat[D$kingdom == "Plantae"] = "Plant"
D$plotCat[D$kingdom == "Fungi"] = "Fungi"
PD = D

PD = PD %>% filter(plotCat == "Insect")

PD


# PD$plotCat[PD$class == "Magnoliopsida"] = "flowers"

# PD %>% filter(order == "Hymenoptera") %>% group_by(species) %>% summarise(occCounts = sum(occCounts)) %>%
  # arrange(-occCounts) %>% as.data.frame() %>% tail()

# PD
# PD$order %>% unique()

