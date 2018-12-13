
# Plot the top 800 species in citizen science with 

library(ggplot2)
library(roperators)
library(gbifapi)
library(dplyr)
library(purrr)
library(forcats)
library(ggthemes)
library(hrbrthemes)
library(extrafont) # might be necessary for hrbrthemes
loadfonts(quiet = TRUE)

load("C:/Users/ftw712/Desktop/citizen science/data/speciesKeyCounts.rda")
D = speciesKeyCounts
citizenSciencekeys = gbifapi::gbifapi("http://api.gbif.org/v1/dataset?machineTagNamespace=citizenScience.mgrosjean.gbif.org&limit=500")$results %>% map_chr(~.x$key)

D = D %>% 
group_by(specieskeys) %>%
summarise(occCounts = sum(occCounts)) %>%
arrange(-occCounts) %>%
mutate(specieskeys = fct_reorder(specieskeys, occCounts)) %>% 
head(800) %>% # get top 8000
as.data.frame() 

D$class = D$specieskeys %>% map(~ gbifapi::gbifapi("http://api.gbif.org/v1/species/" %+% .x)) %>% map_chr(~ .x$class)
D$species = D$specieskeys %>% map(~ gbifapi::gbifapi("http://api.gbif.org/v1/species/" %+% .x)) %>% map_chr(~ .x$species)
D$kingdom = D$specieskeys %>% map(~ gbifapi::gbifapi("http://api.gbif.org/v1/species/" %+% .x)) %>% map_chr(~ .x$kingdom)
D$scientificName = D$specieskeys %>% map(~ gbifapi::gbifapi("http://api.gbif.org/v1/species/" %+% .x)) %>% map_chr(~ .x$scientificName) %>% rev()

# plot categroies 
D$plotCat = NA
D$plotCat[D$class == "Aves"] = "Bird"
D$plotCat[D$class == "Insecta"] = "Insect"
D$plotCat[D$class == "Mammalia"] = "Mammal"
D$plotCat[D$class == "Liliopsida"] = "Plant"
D$plotCat[D$class == "Pinopsida"] = "Plant"
D$plotCat[D$class == "Magnoliopsida"] = "Plant"

PD = D

interestingTaxa = rbind(
c("Vanessa atalanta", "red admiral butterfly"),
c("Larus argentatus", "seagull"),
c("Corvus corax", "raven"),
c("Capreolus capreolus", "roe deer"),
c("Urtica dioica", "common nettle"),
c("Pieris rapae", "cabbage butterfly"),
c("Maniola jurtina", "meadow brown butterfly"),
c("Ischnura elegans", "blue-tailed damselfly"),
c("Quercus robur", "english oak"),
c("Ranunculus repens", "creeping buttercup flower"),
c("Danaus plexippus", "monarch butterfly"),
c("Enallagma cyathigerum", "common blue damselfly"),
c("Dactylis glomerata", "cat grass"),
c("Platalea regia", "royal spoonbill"),
c("Erinaceus europaeus", "european hedgehog"),
c("Vulpes vulpes", "red fox"),
c("Dryocopus pileatus", "pileated  woodpecker"),
c("Myiarchus crinitus","great crested flycatcher"),
c("Phasianus colchicus","ring-necked pheasant"),
c("Columba oenas","dove"),
c("Piranga rubra","summer tanager"),
c("Riparia riparia","sand martin"),
c("Anthochaera carunculata","red wattlebird"),
c("Melanitta fusca", "velvet scoter")
)

colnames(interestingTaxa) = c("sciName","commonName")
interestingTaxa = interestingTaxa %>% as.data.frame(stringsAsFactors=FALSE)

PD$barLabels = NA
for(i in 1:nrow(interestingTaxa)) PD$barLabels[PD$species == interestingTaxa$sciName[i]] = interestingTaxa$commonName[i]

barLabelsBoxesBirds = NA 
PD$barLabelsBoxesBirds[PD$plotCat == "Bird" & PD$species %in% interestingTaxa$sciName] = " "
barLabelsBoxesMammals = NA 
PD$barLabelsBoxesMammals[PD$plotCat == "Mammal" & PD$species %in% interestingTaxa$sciName] = " "
barLabelsBoxesPlants = NA 
PD$barLabelsBoxesPlants[PD$plotCat == "Plant" & PD$species %in% interestingTaxa$sciName] = " "
PD$barLabelsBoxesInsects = NA 
PD$barLabelsBoxesInsects[PD$plotCat == "Insect" & PD$species %in% interestingTaxa$sciName] = " "

# hack for adding text labels using error bars 

barLines = PD %>% select(scientificName,species,specieskeys,plotCat,occCounts) %>% 
mutate(ymax = occCounts + 0.9e6) %>%
mutate(ymin = occCounts + 0.1e6) 
barLines$ymax[!barLines$species %in% interestingTaxa$sciName] = 0
barLines$ymin[!barLines$species %in% interestingTaxa$sciName] = 0

# gbifColors = c("#175CA1", "#40BFFF", "#636FB4", "#E5FFFF", "#E8E8E8", "#D66F27", "#7D466A", "#FDAF02", "#509E2F", "#231F20")

breaks = seq(0,10e6,1e6)
label = c("0", "1M", "2M", "3M", "4M", "5M", "6M", "7M", "8M", "9M", "10M")

caption = "Source: " %+% length(citizenSciencekeys) %+% " GBIF citizen science datasets that have been both automatically and manually classified using the dataset description supplied by the publisher. Classified in 2018."

p = ggplot(PD, aes(specieskeys,occCounts, fill = plotCat)) + 
geom_bar(stat = "identity", position = "dodge",width=1) + 
scale_x_discrete(labels = rev(D$species), expand=c(0.03, 0)) + 
scale_y_continuous(breaks = breaks,label = label, limits = c(0,10e6)) + 
scale_fill_manual(values = c("#dddddd","#777777","#D66F27","#509E2F")) + 
coord_flip() + 
theme_ipsum(base_family="Myriad Pro", grid="X") + 
guides(colour=guide_legend(title=NULL)) +
theme(axis.text.y=element_text(size=0.5, vjust=0.5, margin = margin(t = 0, r = -28, b = 0, l = 0))) +
theme(legend.position="bottom") + 
guides(fill=guide_legend(title=NULL)) + 
ylab("number of occurrences (millions)") + 
xlab("top 800 species") + 
labs(caption = paste(strwrap(caption,90), collapse="\n")) + 	 
labs(title="The dominance of birds in citizen science", subtitle="The top 800 species in GBIF citizen science datasets") + 
geom_label(aes(y = (occCounts + 1e6),label=barLabels),label.padding = unit(0.1, "lines"),hjust=0,fill = "white",alpha=0.5, position=position_dodge(width=1), size=2) +
geom_label(aes(y = occCounts + 0.91e6,label=barLabelsBoxesBirds),label.padding = unit(0.1, "lines"),label.r = unit(0.1, "lines"),hjust=0,fill = "#dddddd", position=position_dodge(width=1), size=2) +
geom_label(aes(y = occCounts + 0.91e6,label=barLabelsBoxesMammals),label.padding = unit(0.1, "lines"),label.r = unit(0.1, "lines"),hjust=0,fill = "#D66F27", position=position_dodge(width=1), size=2) +
geom_label(aes(y = occCounts + 0.91e6,label=barLabelsBoxesPlants),label.padding = unit(0.1, "lines"),label.r = unit(0.1, "lines"),hjust=0,fill = "#509E2F", position=position_dodge(width=1), size=2) +
geom_label(aes(y = occCounts + 0.91e6,label=barLabelsBoxesInsects),label.padding = unit(0.1, "lines"),label.r = unit(0.1, "lines"),hjust=0,fill = "#777777", position=position_dodge(width=1), size=2) + 
geom_errorbar(aes(ymax=ymax, ymin=ymin),data = barLines,color= "#777777",width=0.001,size=0.05,stat="identity")

saveFileName = "C:/Users/ftw712/Desktop/citizen science/plots/top800speciesBarplot"

ggsave(saveFileName %+% ".pdf", plot = p, device=cairo_pdf,width=9,height=7)
ggsave(saveFileName %+% ".jpg", plot = p,width=9,height=7,units="in",dpi=900)
ggsave(saveFileName %+% ".tiff", plot = p,width=9,height=7,units="in",dpi=600)
ggsave(saveFileName %+% ".svg", plot = p,width=9,height=7)
