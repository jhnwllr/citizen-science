
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
  filter(occCounts > 1000) %>%
  gbifapi::addTaxonInfo(specieskeys) %>% # add kingdom class phylum ect to data.frame
  as.data.frame() 


# plot categroies 
D$plotCat = "Other"
D$plotCat[D$class == "Aves"] = "Bird"
D$plotCat[D$class == "Insecta"] = "Insect"
D$plotCat[D$class == "Mammalia"] = "Mammal"
D$plotCat[D$kingdom == "Plantae"] = "Plant"
D$plotCat[D$kingdom == "Fungi"] = "Fungi"
PD = D

PD = PD %>% filter(plotCat == "Plant")

PD$plotCat[PD$class == "Magnoliopsida"] = "flowers"
PD$plotCat[PD$class == "Liliopsida"] = "flowers"
PD$plotCat[PD$class == "Bryopsida"] = "mosses and ferns"
PD$plotCat[PD$class == "Polypodiopsida"] = "mosses and ferns"
PD$plotCat[PD$class == "Pinopsida"] = "tree"
PD$plotCat[PD$class == "Jungermanniopsida"] = "mosses and ferns"
PD$plotCat[PD$class == "Sphagnopsida"] = "mosses and ferns"
PD$plotCat[PD$class == "Florideophyceae"] = "algea"
PD$plotCat[PD$class == "Equisetopsida"] = "other"
PD$plotCat[PD$class == "Lycopodiopsida"] = "mosses and ferns"
PD$plotCat[PD$class == "Ulvophyceae"] = "algea"
PD$plotCat[PD$class == "Charophyceae"] = "algea"
PD$plotCat[PD$class == "Psilotopsida"] = "mosses and ferns"

PD = PD %>% mutate(plotCat = as.factor(plotCat))

# PD$plotCat
PD$plotCat = factor(PD$plotCat, level =c("flowers","mosses and ferns","tree","algea","other"))
color = c("#dddddd","#777777","#509E2F","#D66F27","#C2938D")
# algea flowers mosses and ferns other tree
hc = highchart() %>%
                hc_add_series(PD, "bar",
                hcaes(x=species,y=occCounts, group = plotCat),
                color = color,
                showInLegend=c(TRUE,TRUE,TRUE,TRUE,TRUE),
                visible = c(TRUE,TRUE,TRUE,TRUE,TRUE)) %>%
                hc_yAxis(title = list(text = "number of occurrence records")) %>%
                hc_xAxis(title = list(text = "index of species"))

hc


# htmlwidgets::saveWidget(hc, file="C:/Users/ftw712/Desktop/highchart.html")