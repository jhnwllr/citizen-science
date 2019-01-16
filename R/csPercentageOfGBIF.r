
# static 
if(FALSE) { 

# Percentage of GBIF that is citizen science 

library(gbifsnapshots)
library(dplyr)
library(rgbif)
library(stringr)
library(ggthemes)
library(ggplot2)
library(roperators)

citizenSciencekeys = gbifapi::gbifapi("http://api.gbif.org/v1/dataset?machineTagNamespace=citizenScience.mgrosjean.gbif.org&limit=500")$results %>% map_chr(~.x$key)

D = gbifsnapshots::countsByDatasetkey # all occ counts by datasetkey 

csTotals = D %>% filter(datasetkey %in% citizenSciencekeys) %>%  # citizen science 
group_by(snapshots,created) %>%
summarise(totalCsCounts = sum(countByDatasetkey)) %>% 
as.data.frame()

D = merge(csTotals,snapshotCounts,id="snapshots") %>% select(snapshots,created,totalCounts,totalCsCounts)
D$CsPercentage = (D$totalCsCounts/D$totalCounts)*100

D = D %>% select(snapshots,created,"% citizen science" = CsPercentage) # rename for plot
PD = reshape2::melt(D,id=c("snapshots","created"))

# pdf("C:/Users/ftw712/Desktop/percentageCitizenScience.pdf",width=10,height=7)

caption = "Based on data from " %+% nrow(D) %+% " saved GBIF snapshots since 2007 and " %+% length(citizenSciencekeys) %+% " automatically identified citizen science datasets. "
# captions = paste(strwrap(caption,5), collapse="\n") # process caption to fit area
Title = "The rise of Citizen Science on GBIF"
Subtitle = "The percentage of GBIF occurrences that are Citizen Science"

breaks = seq(0,100,10)
label = c("0%", "10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%", "90%", "100%")

# gbifColors = c("#175CA1", "#40BFFF", "#636FB4", "#E5FFFF", "#E8E8E8", "#D66F27", "#7D466A", "#FDAF02", "#509E2F", "#231F20")

ggplot(PD,aes(created, value,colour=variable)) + 
scale_y_continuous(breaks = breaks,label = label) + 
geom_line() + 
geom_point() + 
theme_hc() + 
scale_colour_manual(values = c("#509E2F")) + 
ggtitle(label = Title, subtitle = Subtitle) +
ylab("percentage of GBIF occurrences") + 
xlab("") + 
guides(colour=guide_legend(title="")) +  
labs(caption = paste(strwrap(caption,90), collapse="\n")) +
theme(plot.caption = element_text(hjust = 0)) + 
theme(legend.position = c(.925,.12)) 

# dev.off()

saveFileName = "C:/Users/ftw712/Desktop/citizen science/plots/percentageOfGBIFWeb"

# ggsave(saveFileName %+% ".pdf", plot = p, device=cairo_pdf,width=10,height=7)
ggsave(saveFileName %+% ".pdf", plot = p,width=10,height=7)
ggsave(saveFileName %+% ".jpg", plot = p,width=10,height=7,units="in",dpi=900)
ggsave(saveFileName %+% ".tiff", plot = p,width=10,height=7,units="in",dpi=600)
ggsave(saveFileName %+% ".svg", plot = p,width=10,height=7)

# ggsave("C:/Users/ftw712/Desktop/data-blog/static/post/2018-12-10-gbif-citizen-science-data_files/percentageOfGBIFWeb.jpg",width=10,height=7,units="in",dpi=900)



}

# Web 

library(gbifsnapshots)
library(dplyr)
library(rgbif)
library(stringr)
library(ggthemes)
library(ggplot2)
library(roperators)

citizenSciencekeys = gbifapi::gbifapi("http://api.gbif.org/v1/dataset?machineTagNamespace=citizenScience.mgrosjean.gbif.org&limit=500")$results %>% map_chr(~.x$key)

D = gbifsnapshots::countsByDatasetkey # all occ counts by datasetkey 

csTotals = D %>% filter(datasetkey %in% citizenSciencekeys) %>%  # citizen science 
  group_by(snapshots,created) %>%
  summarise(totalCsCounts = sum(countByDatasetkey)) %>% 
  as.data.frame()

D = merge(csTotals,snapshotCounts,id="snapshots") %>% select(snapshots,created,totalCounts,totalCsCounts)
D$CsPercentage = (D$totalCsCounts/D$totalCounts)*100

D = D %>% select(snapshots,created,"% citizen science" = CsPercentage) # rename for plot
PD = reshape2::melt(D,id=c("snapshots","created"))

# pdf("C:/Users/ftw712/Desktop/percentageCitizenScience.pdf",width=10,height=7)

caption = "Based on data from " %+% nrow(D) %+% " saved GBIF snapshots since 2007 and " %+% length(citizenSciencekeys) %+% " automatically identified citizen science datasets. "
# captions = paste(strwrap(caption,5), collapse="\n") # process caption to fit area
Title = "The rise of Citizen Science on GBIF"
Subtitle = "The percentage of GBIF occurrences that are Citizen Science"

breaks = seq(0,100,10)
label = c("0%", "10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%", "90%", "100%")

# gbifColors = c("#175CA1", "#40BFFF", "#636FB4", "#E5FFFF", "#E8E8E8", "#D66F27", "#7D466A", "#FDAF02", "#509E2F", "#231F20")

p = ggplot(PD,aes(created, value,colour=variable)) + 
  scale_y_continuous(breaks = breaks,label = label) + 
  geom_line() + 
  geom_point() + 
  theme_hc() + 
  scale_colour_manual(values = c("#509E2F")) + 
  ylab("percentage of GBIF occurrences") + 
  xlab("") + 
  guides(colour=guide_legend(title="")) +
  theme(legend.position = c(.925,.12)) 

# dev.off()

saveFileName = "C:/Users/ftw712/Desktop/citizen science/plots/percentageOfGBIFWeb"

# ggsave(saveFileName %+% ".pdf", plot = p, device=cairo_pdf,width=10,height=7)
ggsave(saveFileName %+% ".pdf", plot = p,width=10,height=7)
ggsave(saveFileName %+% ".jpg", plot = p,width=10,height=7,units="in",dpi=900)
ggsave(saveFileName %+% ".tiff", plot = p,width=10,height=7,units="in",dpi=600)
ggsave(saveFileName %+% ".svg", plot = p,width=10,height=7)

ggsave("C:/Users/ftw712/Desktop/data-blog/static/post/2018-12-10-gbif-citizen-science-data_files/percentageOfGBIFWeb.jpg",width=10,height=7,units="in",dpi=900)




