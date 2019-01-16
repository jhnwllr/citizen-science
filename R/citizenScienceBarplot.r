
if(FALSE) { # static 

library(gbifsnapshots)
library(gbifapi)
library(dplyr)
library(rgbif)
library(stringr)
library(ggthemes)
library(ggplot2)
library(roperators)
library(purrr)
library(forcats)

# Process Data 
citizenSciencekeys = gbifapi::gbifapi("http://api.gbif.org/v1/dataset?machineTagNamespace=citizenScience.mgrosjean.gbif.org&limit=500")$results %>% map_chr(~.x$key)

D = gbifsnapshots::countsByDatasetkey # all occ counts by datasetkey 

D = D %>% 
  group_by(snapshots,created,datasetkey) %>%
  summarise(totalCounts = sum(countByDatasetkey)) %>%
  mutate(csCategory = if_else(datasetkey %in% citizenSciencekeys,"citizen science","not citizen science")) %>%
  filter(created == "2018-09-28") %>%
  filter(totalCounts > 5e6) %>%
  mutate(datasetTitle = map_chr(datasetkey, ~ gbifapi::gbifapi("http://api.gbif.org/v1/dataset/" %+% .x)$title)) %>% 
  mutate(datasetkey = fct_reorder(datasetkey, totalCounts)) %>% 
  arrange(totalCounts) %>% 
  as.data.frame()

# Plot #####

Title = "The largest datasets on GBIF are Citizen Science datasets"
Subtitle = "A list of the largest datasets"
caption = "Based on data downloaded in 2018. Citizen Science datasets have been labeled automatically. "

shortDatasetTitle = c(
  "Southern African Bird Atlas Project", 
  "Occurrence Data of Vascular Plants ...", 
  "The vascular plants collection (MNHN) ...", 
  "iNaturalist Research-grade Observations",                                                                          
  "UK Butterfly Monitoring Scheme (UKBMS)",                                                                             
  "Macro-moth distribution records (UK) ...", 
  "Bird Ringing Centre in Sweden (NRM)",                                                                           
  "Victorian Biodiversity Atlas",                                                                                
  "Great Backyard Bird Count",                                                                                       
  "NMNH Extant Specimen Records",
  "Bird tracking - GPS tracking ...",
  "naturgucker",                                                                                   
  "Anillamiento SEO Bird ringing",
  "OEH Atlas of NSW Wildlife",                                                                                      
  "Flora von Deutschland (Phanerogamen)",
  "BirdLife Australia, Birdata",                                                                               
  "Southern African Bird Atlas ...",
  "Finnish Bird Ringing and Recovery Database",
  "Dutch Vegetation Database",                                                                         
  "Norwegian Species Observation Service",
  "DOF (Danish Ornithological Society)",                                                                              
  "INPN ...",
  "Artportalen (Swedish Species Observation System)",
  "EOD - eBird Observation Dataset")

PD = D

breaks = seq(0,500e6,100e6)
label = c("0", "100 M", "200 M", "300 M", "400 M", "500 M")

# ggplot(PD, aes(x = Service, y = Perc, fill = Service, colour=colour)) 
p = ggplot(PD, aes(datasetkey,totalCounts, fill = csCategory)) + 
  scale_x_discrete("dataset", labels = shortDatasetTitle) + 
  scale_y_continuous(breaks = breaks, label = label) + 
  ylab("number of occurrences") + 
  geom_bar(stat = "identity", position = "dodge") + 
  theme_hc() + 
  scale_fill_manual(values = c("#509E2F","#777777")) + 
  ggtitle(label = Title, subtitle = Subtitle) +
  coord_flip() + 
  guides(fill=guide_legend(title=NULL)) +
  labs(caption = paste(strwrap(caption,90), collapse="\n")) +  
  theme(legend.position = c(.85,.12)) + 
  theme(plot.caption = element_text(hjust = 0)) 

saveFileName = "C:/Users/ftw712/Desktop/citizen science/plots/citizenScienceBarplot"

# ggsave(saveFileName %+% ".pdf", plot = p, device=cairo_pdf,width=10,height=7)
ggsave(saveFileName %+% ".pdf", plot = p,width=10,height=7)
ggsave(saveFileName %+% ".jpg", plot = p,width=10,height=7,units="in",dpi=900)
ggsave(saveFileName %+% ".tiff", plot = p,width=10,height=7,units="in",dpi=600)
ggsave(saveFileName %+% ".svg", plot = p,width=10,height=7)

}

# web 
library(gbifsnapshots)
library(gbifapi)
library(dplyr)
library(rgbif)
library(stringr)
library(ggthemes)
library(ggplot2)
library(roperators)
library(purrr)
library(forcats)

# Process Data 
citizenSciencekeys = gbifapi::gbifapi("http://api.gbif.org/v1/dataset?machineTagNamespace=citizenScience.mgrosjean.gbif.org&limit=500")$results %>% map_chr(~.x$key)

D = gbifsnapshots::countsByDatasetkey # all occ counts by datasetkey 

D = D %>% 
  group_by(snapshots,created,datasetkey) %>%
  summarise(totalCounts = sum(countByDatasetkey)) %>%
  mutate(csCategory = if_else(datasetkey %in% citizenSciencekeys,"citizen science","not citizen science")) %>%
  filter(created == "2018-09-28") %>%
  filter(totalCounts > 5e6) %>%
  mutate(datasetTitle = map_chr(datasetkey, ~ gbifapi::gbifapi("http://api.gbif.org/v1/dataset/" %+% .x)$title)) %>% 
  mutate(datasetkey = fct_reorder(datasetkey, totalCounts)) %>% 
  arrange(totalCounts) %>% 
  as.data.frame()

# Plot #####

Title = "The largest datasets on GBIF are Citizen Science datasets"
Subtitle = "A list of the largest datasets"
caption = "Based on data downloaded in 2018. Citizen Science datasets have been labeled automatically. "

shortDatasetTitle = c(
  "Southern African Bird Atlas Project", 
  "Occurrence Data of Vascular Plants ...", 
  "The vascular plants collection (MNHN) ...", 
  "iNaturalist Research-grade Observations",                                                                          
  "UK Butterfly Monitoring Scheme (UKBMS)",                                                                             
  "Macro-moth distribution records (UK) ...", 
  "Bird Ringing Centre in Sweden (NRM)",                                                                           
  "Victorian Biodiversity Atlas",                                                                                
  "Great Backyard Bird Count",                                                                                       
  "NMNH Extant Specimen Records",
  "Bird tracking - GPS tracking ...",
  "naturgucker",                                                                                   
  "Anillamiento SEO Bird ringing",
  "OEH Atlas of NSW Wildlife",                                                                                      
  "Flora von Deutschland (Phanerogamen)",
  "BirdLife Australia, Birdata",                                                                               
  "Southern African Bird Atlas ...",
  "Finnish Bird Ringing and Recovery Database",
  "Dutch Vegetation Database",                                                                         
  "Norwegian Species Observation Service",
  "DOF (Danish Ornithological Society)",                                                                              
  "INPN ...",
  "Artportalen (Swedish Species Observation System)",
  "EOD - eBird Observation Dataset")

PD = D

breaks = seq(0,500e6,100e6)
label = c("0", "100 M", "200 M", "300 M", "400 M", "500 M")

# ggplot(PD, aes(x = Service, y = Perc, fill = Service, colour=colour)) 
p = ggplot(PD, aes(datasetkey,totalCounts, fill = csCategory)) + 
  scale_x_discrete("dataset", labels = shortDatasetTitle) + 
  scale_y_continuous(breaks = breaks, label = label) + 
  ylab("number of occurrences") + 
  geom_bar(stat = "identity", position = "dodge") + 
  theme_hc() + 
  scale_fill_manual(values = c("#509E2F","#777777")) + 
  coord_flip() + 
  guides(fill=guide_legend(title=NULL)) +
  theme(legend.position = c(.85,.12)) + 
  theme(plot.caption = element_text(hjust = 0)) 

# labs(caption = paste(strwrap(caption,90), collapse="\n")) + 
# ggtitle(label = Title, subtitle = Subtitle) +

saveFileName = "C:/Users/ftw712/Desktop/citizen science/plots/citizenScienceBarplotWeb"

# ggsave(saveFileName %+% ".pdf", plot = p, device=cairo_pdf,width=10,height=7)
ggsave(saveFileName %+% ".pdf", plot = p,width=10,height=7)
ggsave(saveFileName %+% ".jpg", plot = p,width=10,height=7,units="in",dpi=900)
ggsave(saveFileName %+% ".tiff", plot = p,width=10,height=7,units="in",dpi=600)
ggsave(saveFileName %+% ".svg", plot = p,width=10,height=7)

ggsave("C:/Users/ftw712/Desktop/data-blog/static/post/2018-12-10-gbif-citizen-science-data_files/citizenScienceBarplotWeb.jpg",width=10,height=7,units="in",dpi=900)

