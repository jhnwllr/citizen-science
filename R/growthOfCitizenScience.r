
# static version 
if(FALSE) {
  
library(gbifsnapshots)
library(dplyr)
library(rgbif)
library(stringr)
library(ggthemes)
library(ggplot2)
library(roperators)
library(purrr)

D = read.table("C:/Users/ftw712/Desktop/citizen science/data/some_manually_annotated_datasets.tsv",header=TRUE)
D = D[D$CS,]

citizenSciencekeys = gbifapi::gbifapi("http://api.gbif.org/v1/dataset?machineTagNamespace=citizenScience.mgrosjean.gbif.org&limit=500")$results %>% map_chr(~.x$key)

D = gbifsnapshots::countsByDatasetkey # all occ counts by datasetkey 

csTotals = D %>% filter(datasetkey %in% citizenSciencekeys) %>%  # citizen science 
group_by(snapshots,created) %>%
summarise(totalCsCounts = sum(countByDatasetkey)) %>% 
as.data.frame()

D = merge(csTotals,snapshotCounts,id="snapshots") %>% select(snapshots,created,totalCounts,totalCsCounts)
 
D = D %>% select(snapshots,created,"all gbif" = totalCounts,"citizen science" = totalCsCounts) # rename for plot

PD = reshape2::melt(D,id=c("snapshots","created"))


# pdf("C:/Users/ftw712/Desktop/growthOfCitizenScience.pdf",width=10,height=7)

caption = "Based on data from " %+% nrow(D) %+% " saved GBIF snapshots since 2007 and " %+% length(citizenSciencekeys) %+% " automatically identified citizen science datasets. "
# captions = paste(strwrap(caption,5), collapse="\n") # process caption to fit area

breaks = seq(0,1000e6,100e6)
label = c("0", "100 M", "200 M", "300 M", "400 M", "500 M", "600 M", "700 M", "800 M", "900 M", "1 B")
# gbifColors = c("#175CA1", "#40BFFF", "#636FB4", "#E5FFFF", "#E8E8E8", "#D66F27", "#7D466A", "#FDAF02", "#509E2F", "#231F20")

p = ggplot(PD,aes(created, value,colour=variable)) + 
scale_y_continuous(breaks = breaks,label = label) + 
geom_line() + 
geom_point() + 
theme_hc() + 
scale_colour_manual(values = c("#231F20", "#509E2F")) + 
ggtitle(label = "The rise of citizen science on GBIF", subtitle = "The number of occurrences in Citizen Science and all GBIF datasets") +
ylab("number of occurrences") + 
xlab("") + 
guides(colour=guide_legend(title="")) +  
labs(caption = paste(strwrap(caption,90), collapse="\n")) +
theme(plot.caption = element_text(hjust = 0)) + 
theme(legend.position = c(.925,.1)) + 
theme(legend.background = element_rect(fill="transparent"))
# dev.off()

saveFileName = "C:/Users/ftw712/Desktop/citizen science/plots/growthOfCitizenScience"

# ggsave(saveFileName %+% ".pdf", plot = p, device=cairo_pdf,width=10,height=7)
ggsave(saveFileName %+% ".pdf", plot = p,width=10,height=7)
ggsave(saveFileName %+% ".jpg", plot = p,width=10,height=7,units="in",dpi=900)
ggsave(saveFileName %+% ".tiff", plot = p,width=10,height=7,units="in",dpi=600)
ggsave(saveFileName %+% ".svg", plot = p,width=10,height=7)
}

# web version 
# if(FALSE) {
  
  library(gbifsnapshots)
  library(dplyr)
  library(rgbif)
  library(stringr)
  library(ggthemes)
  library(ggplot2)
  library(roperators)
  library(purrr)
  
  D = read.table("C:/Users/ftw712/Desktop/citizen science/data/some_manually_annotated_datasets.tsv",header=TRUE)
  D = D[D$CS,]
  
  citizenSciencekeys = gbifapi::gbifapi("http://api.gbif.org/v1/dataset?machineTagNamespace=citizenScience.mgrosjean.gbif.org&limit=500")$results %>% map_chr(~.x$key)
  
  D = gbifsnapshots::countsByDatasetkey # all occ counts by datasetkey 
  
  csTotals = D %>% filter(datasetkey %in% citizenSciencekeys) %>%  # citizen science 
    group_by(snapshots,created) %>%
    summarise(totalCsCounts = sum(countByDatasetkey)) %>% 
    as.data.frame()
  
  D = merge(csTotals,snapshotCounts,id="snapshots") %>% select(snapshots,created,totalCounts,totalCsCounts)
  
  D = D %>% select(snapshots,created,"all gbif" = totalCounts,"citizen science" = totalCsCounts) # rename for plot
  
  PD = reshape2::melt(D,id=c("snapshots","created"))
  
  
  # pdf("C:/Users/ftw712/Desktop/growthOfCitizenScience.pdf",width=10,height=7)
  
  caption = "Based on data from " %+% nrow(D) %+% " saved GBIF snapshots since 2007 and " %+% length(citizenSciencekeys) %+% " automatically identified citizen science datasets. "
  # captions = paste(strwrap(caption,5), collapse="\n") # process caption to fit area
  
  breaks = seq(0,1000e6,100e6)
  label = c("0", "100 M", "200 M", "300 M", "400 M", "500 M", "600 M", "700 M", "800 M", "900 M", "1 B")
  # gbifColors = c("#175CA1", "#40BFFF", "#636FB4", "#E5FFFF", "#E8E8E8", "#D66F27", "#7D466A", "#FDAF02", "#509E2F", "#231F20")
  
  p = ggplot(PD,aes(created, value,colour=variable)) + 
    scale_y_continuous(breaks = breaks,label = label) + 
    geom_line() + 
    geom_point() + 
    theme_hc() + 
    scale_colour_manual(values = c("#231F20", "#509E2F")) + 
    ylab("number of occurrences") + 
    xlab("") + 
    guides(colour=guide_legend(title="")) +  
    theme(legend.position = c(.925,.1)) + 
    theme(legend.background = element_rect(fill="transparent"))
  # dev.off()
  # ggtitle(label = "The rise of citizen science on GBIF", subtitle = "The number of occurrences in Citizen Science and all GBIF datasets") +
  saveFileName = "C:/Users/ftw712/Desktop/citizen science/plots/growthOfCitizenScienceWeb"

  # theme(plot.caption = element_text(hjust = 0)) + 
    
  # ggsave(saveFileName %+% ".pdf", plot = p, device=cairo_pdf,width=10,height=7)
  ggsave(saveFileName %+% ".pdf", plot = p,width=10,height=7)
  ggsave(saveFileName %+% ".jpg", plot = p,width=10,height=7,units="in",dpi=900)
  ggsave(saveFileName %+% ".tiff", plot = p,width=10,height=7,units="in",dpi=600)
  ggsave(saveFileName %+% ".svg", plot = p,width=10,height=7)

  # and save one for blog
  ggsave("C:/Users/ftw712/Desktop/data-blog/static/post/2018-12-10-gbif-citizen-science-data_files/growthOfCitizenScienceWeb.jpg",width=10,height=7,units="in",dpi=900)

  # }
