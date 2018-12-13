
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

PD = PD %>% mutate(plotCat = as.factor(plotCat))
# mutate(plotCat = fct_reorder(plotCat, c("Bird","Fungi","Insect","Mammal","Other","Plant")))


PD$plotCat = factor(PD$plotCat, level =c("Bird","Plant","Insect","Mammal","Fungi","Other"))
color = c("#dddddd","#509E2F","#777777","#D66F27","#C2938D","#ff9f00")

hc = highchart() %>%
        hc_add_series(PD, "bar", 
        hcaes(x=species,y=occCounts, group = plotCat), 
        color = color, 
        showInLegend=c(TRUE,TRUE,TRUE,TRUE,TRUE,TRUE),
        visible = c(TRUE,TRUE,TRUE,TRUE,TRUE,TRUE)) %>%
        hc_yAxis(title = list(text = "number of occurrence records")) %>%
        hc_xAxis(title = list(text = "index of species"))

htmlwidgets::saveWidget(hc, file="C:/Users/ftw712/Desktop/highchart.html")
