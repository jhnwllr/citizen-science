# citizen science counts 

if(FALSE) { # get tables from registry to get identifiers for top10 CS datasets 
source("C:/Users/ftw712/Desktop/usage/usageStats/dbConnect.r")
# dbGetQuery(con,'SELECT * FROM INFORMATION_SCHEMA.TABLES')
identifier = dbGetQuery(con,"SELECT * FROM identifier")
dataset_identifier = dbGetQuery(con,"SELECT * FROM dataset_identifier")
pryr::mem_used() # check how big these tables. 

# save(identifier, file="C:/Users/ftw712/Desktop/identifier.rda")
# save(dataset_identifier, file="C:/Users/ftw712/Desktop/dataset_identifier.rda")
}

if(FALSE) { # get CS top 10 identifiers if any 
load("C:/Users/ftw712/Desktop/spark/countSnapshots/identifier.rda")
load("C:/Users/ftw712/Desktop/spark/countSnapshots/dataset_identifier.rda")
library(dplyr)
library(rgbif)

D = read.table("C:/Users/ftw712/Desktop/spark/countSnapshots/some_manually_annotated_datasets.tsv",header=TRUE)
D = D[D$CS,]

D$UUID = D$UUID %>% as.character() 
CS = sapply(D$UUID, function(x) occ_count(datasetKey = x)) %>% sort() %>% rev()
keys = CS[1:10] %>% names()

str(identifier)
str(dataset_identifier)

dataset_identifier$key = dataset_identifier$identifier_key

D = merge(identifier,dataset_identifier,id="key")
str(D)

# D$type %>% unique()
# D$identifier %>% unique()
# head(D)

# D %>% filter(dataset_key %in% keys) %>% select(key,identifier,dataset_key) %>% filter(dataset_key == "b124e1e0-4755-430f-9eab-894f25a9b59c")
# %>% filter(dataset_key == "50c9509d-22c7-4a22-a47d-8c48425ef4a7")

# %>% filter(identifier == "43")
# D[D$dataset_key %in% keys,]$dataset_key %>% unique()
# str(D)
}


if(FALSE) { # When were the top 10 CS datasets created? 

library(roperators)
library(lubridate)
library(dplyr)
library(rgbif)
library(purrr)
library(magrittr)

D = read.table("C:/Users/ftw712/Desktop/spark/countSnapshots/some_manually_annotated_datasets.tsv",header=TRUE)
D = D[D$CS,]

D$UUID = D$UUID %>% as.character() 
CS = sapply(D$UUID, function(x) occ_count(datasetKey = x)) %>% sort() %>% rev()
keys = CS[1:10] %>% names()

load("C:/Users/ftw712/Desktop/spark/countSnapshots/identifier.rda")
load("C:/Users/ftw712/Desktop/spark/countSnapshots/dataset_identifier.rda")
library(dplyr)
library(rgbif)
library(purrr)

D = read.table("C:/Users/ftw712/Desktop/spark/countSnapshots/some_manually_annotated_datasets.tsv",header=TRUE)
D = D[D$CS,]

D$UUID = D$UUID %>% as.character() 
CS = sapply(D$UUID, function(x) occ_count(datasetKey = x)) %>% sort() %>% rev()
keys = CS[1:10] %>% names() # 
}

if(FALSE) { # get datasetJson.rda and data_resource_id.rda
library(roperators)
library(lubridate)
library(dplyr)
library(rgbif)
library(purrr)

load("C:/Users/ftw712/Desktop/spark/countSnapshots/identifier.rda")
load("C:/Users/ftw712/Desktop/spark/countSnapshots/dataset_identifier.rda")

dataset_identifier$key = dataset_identifier$identifier_key

D = merge(identifier,dataset_identifier,id="key")
D$created = as.Date(D$created)

# these are the datasets that might have a mysql key  
D = D %>% select(dataset_key,identifier_key,identifier,created) %>%
arrange(dataset_key,created) %>%
filter(created < "2013-12-20") # first snapshot of hbase ver1 


# D = D[1:100,] # use less for testing 
# convert datasetKey to data resource id 
# datasetsJson = D$dataset_key %>% map(~gbifapi::gbifapi("http://api.gbif.org/v1/dataset/" %+% .))
# save(datasetsJson,file = "C:/Users/ftw712/Desktop/datasetsJson.rda") # save expensive operation 

load("C:/Users/ftw712/Desktop/datasetsJson.rda")
L = datasetsJson # this is a big list 
L = L %>% map("identifiers")
identifiers = L %>% map(flatten) %>% map(~ .[names(.) == "identifier"]) %>% map(flatten_chr)
bools = L %>% map(flatten) %>% map(~ .[names(.) == "type"]) %>% map(flatten_chr) %>% map(~ . == "GBIF_PORTAL")
data_resource_id = map2(identifiers,bools,~.x[.y]) 
data_resource_id[map_lgl(data_resource_id, ~identical(., character(0)))] = NA # replace character(0) with NA

datasetsThatHave2Bool = !map_lgl(data_resource_id,~length(.) > 1) # some datasets have 2 data_resource_id s
data_resource_id = data_resource_id[datasetsThatHave2Bool]
D = D[datasetsThatHave2Bool,]

D$data_resource_id = unlist(data_resource_id) # finally unlist 
str(D)

# save(D,file = "C:/Users/ftw712/Desktop/dataResourceIDTable.rda")
}

if(FALSE) { # save data_resource_id
library(dplyr)
load("C:/Users/ftw712/Desktop/dataResourceIDTable.rda")

D = D %>% filter(!is.na(data_resource_id)) %>% # filter out plazi datasets all created 2013-12-12, hbasev1, 2013-12-20
select(dataset_key,data_resource_id,created) %>%
rename(datasetkey = dataset_key)

data_resource_id = D 

# save(data_resource_id,file="C:/Users/ftw712/Desktop/data_resource_id.rda")
}

if(FALSE) { # Make table of all snapshots 
# hard coded snapshots 

library(dplyr)
library(lubridate)

mysql=c("20071219", "20080401", "20080627", "20081010", "20081217", "20090406", "20090617", "20090925", "20091216", "20100401", "20100726", "20101117", "20110221", "20110610", "20110905", "20120118", "20120326", "20120713", "20121031", "20121211", "20130220", "20130521", "20130709", "20130910")
hbase_v1=c("20131220", "20140328")
hbase_v2=c("20140908", "20150119", "20150409") # v3 exists only because of a tiny difference in taxonomy schema (v_order_ became v_order)
hbase_v3=c("20150703", "20151001", "20160104", "20160405", "20160704", "20161007", "20161227", "20170412", "20170724", "20171012", "20171222", "20180409", "20180711", "20180928")

names(mysql) = rep("mysql",length(mysql))
names(hbase_v1) = rep("hbase_v1",length(hbase_v1))
names(hbase_v2) = rep("hbase_v2",length(hbase_v2))
names(hbase_v3) = rep("hbase_v3",length(hbase_v3))
snapshots = c(mysql,hbase_v1,hbase_v2,hbase_v3)
D = data.frame(snapshots=snapshots,schema=names(snapshots),row.names=NULL)

D$created = lubridate::ymd(D$snapshots) # create date column
D$schema = D$schema %>% as.character()
D$snapshots = D$snapshots %>% as.character()

D = D %>% arrange(created)
snapshots = D

# write.table(snapshots,file="C:/Users/ftw712/Desktop/snapshots.csv",sep=",",quote=FALSE,row.names=FALSE)

save(snapshots,file="C:/Users/ftw712/Desktop/snapshots.rda")

# D
# str(D)
}

if(FALSE) { # get ids for citizen science datasets 
library(gbifsnapshots)
library(dplyr)
library(rgbif)

D = read.table("C:/Users/ftw712/Desktop/citizen science/some_manually_annotated_datasets.tsv",header=TRUE)
D = D[D$CS,]

D$UUID = D$UUID %>% as.character() 
CS = sapply(D$UUID, function(x) occ_count(datasetKey = x)) %>% sort() %>% rev()
keys = CS[1:10] %>% names()

str(data_resource_id)
data_resource_id %>% filter(datasetkey %in% keys) %>% pull(data_resource_id)
}


if(FALSE) { # Plot CS unique points citizen science 
# snapshots

library(reshape2)
library(dplyr)
library(gbifsnapshots)
library(ggplot2) # do not mask %+%
library(roperators)
library(stringr)
library(lubridate)

CS = read.table("C:/Users/ftw712/Desktop/spark/data/citizenScienceUniqueCounts.csv",header=TRUE,sep=",") # citizen science
SC = read.table("C:/Users/ftw712/Desktop/spark/data/snapshotCounts.csv",header=TRUE,sep=",") # snapshot counts

D = merge(CS,SC,id="snapshots") 
D$countDistinctLatLon = D$csCountDistinctLatLon + D$notCsCountDistinctLatLon
D
D = melt(D,id.vars=c("raw_snapshots"))
D$created = D$raw_snapshots %>% str_replace_all("raw_","") %>% ymd()
D = D %>% filter(!variable == "count")

library(ggplot2)
pdf("C:/Users/ftw712/Desktop/plot.pdf",width=10,height=7)
ggplot(D,aes(created, value,colour=variable)) + geom_line() + geom_point()
dev.off()

}

if(FALSE) { # Plot count total growth and citizen science 
library(dplyr)
library(reshape2)
library(lubridate)
library(stringr)

CS = read.table("C:/Users/ftw712/Desktop/spark/data/citizenScienceTotalCounts.csv",header=TRUE,sep=",")
SC = read.table("C:/Users/ftw712/Desktop/spark/data/snapshotCounts.csv",header=TRUE,sep=",") # snapshot counts

D = merge(CS,SC,id="raw_snapshots") %>% select(raw_snapshots, csTotalCounts, count) %>% melt(id="raw_snapshots")
D$created = D$raw_snapshots %>% str_replace_all("raw_","") %>% ymd()

library(ggplot2)
pdf("C:/Users/ftw712/Desktop/plot.pdf",width=10,height=7)
ggplot(D,aes(created, value,colour=variable)) + geom_line() + geom_point()
dev.off()
}

if(FALSE) { # get ids for all citizen science datasets 
library(gbifsnapshots)
library(dplyr)
library(rgbif)
library(roperators)

D = read.table("C:/Users/ftw712/Desktop/citizen science/some_manually_annotated_datasets.tsv",header=TRUE)
D = D[D$CS,]

D$UUID = D$UUID %>% as.character() 
CS = sapply(D$UUID, function(x) occ_count(datasetKey = x)) %>% sort() %>% rev()
keys = CS %>% names()
cat('\"' %+% keys %+% '\"' %+% ",") # for pasting into python 

# str(data_resource_id)
dri = data_resource_id %>% filter(datasetkey %in% keys) %>% pull(data_resource_id)
cat('\"' %+% dri %+% '\"' %+% ",") # for pasting into python 
}



if(FALSE) { # 1 . the growth of citizen science 
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


pdf("C:/Users/ftw712/Desktop/growthOfCitizenScience.pdf",width=10,height=7)

caption = "Based on data from " %+% nrow(D) %+% " saved GBIF snapshots since 2007 and " %+% length(citizenSciencekeys) %+% " automatically identified citizen science datasets. "
# captions = paste(strwrap(caption,5), collapse="\n") # process caption to fit area

breaks = seq(0,1000e6,100e6)
label = c("0", "100 M", "200 M", "300 M", "400 M", "500 M", "600 M", "700 M", "800 M", "900 M", "1 B")
# gbifColors = c("#175CA1", "#40BFFF", "#636FB4", "#E5FFFF", "#E8E8E8", "#D66F27", "#7D466A", "#FDAF02", "#509E2F", "#231F20")

ggplot(PD,aes(created, value,colour=variable)) + 
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
dev.off()

}

if(FALSE) { # 2. percentage of GBIF that is citizen science 
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

pdf("C:/Users/ftw712/Desktop/percentageCitizenScience.pdf",width=10,height=7)

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

dev.off()
}

if(FALSE) { # 3. number of cititzen science datasets through time 

library(gbifsnapshots)
library(dplyr)
library(rgbif)
library(stringr)
library(ggthemes)
library(ggplot2)
library(roperators)

D = read.table("C:/Users/ftw712/Desktop/citizen science/data/some_manually_annotated_datasets.tsv",header=TRUE)
D = D[D$CS,]

D$UUID = D$UUID %>% as.character() 
CS = sapply(D$UUID, function(x) occ_count(datasetKey = x)) %>% sort() %>% rev()
# keys = CS[1:10] %>% names()
keys = CS %>% names()

D = gbifsnapshots::countsByDatasetkey # all occ counts by datasetkey 

csTotals = D %>% filter(datasetkey %in% keys) %>%
group_by(snapshots,created) %>%
summarise(datasetCounts = n_distinct(datasetkey))

PD = reshape2::melt(csTotals,id=c("snapshots","created"))

pdf("C:/Users/ftw712/Desktop/countsOfCitizenScience.pdf",width=10,height=7)

caption = "Based on data from " %+% nrow(D) %+% " saved GBIF snapshots since 2007 and " %+% length(keys) %+% " automatically identified citizen science datasets. "
# captions = paste(strwrap(caption,5), collapse="\n") # process caption to fit area
Title = "The rise of Citizen Science on GBIF"
Subtitle = "The percentage of GBIF occurrences that are Citizen Science"

breaks = seq(0,350,50)
label = c("0", "50", "100", "150", "200", "250", "300", "350")

# gbifColors = c("#175CA1", "#40BFFF", "#636FB4", "#E5FFFF", "#E8E8E8", "#D66F27", "#7D466A", "#FDAF02", "#509E2F", "#231F20")

ggplot(PD,aes(created, value,colour=variable)) + 
scale_y_continuous(breaks = breaks,label = label) + 
geom_line() + 
geom_point() + 
theme_hc() + 
scale_colour_manual(values = c("#231F20")) + 
ggtitle(label = Title, subtitle = Subtitle) +
ylab("percentage of GBIF occurrences") + 
xlab("") + 
guides(colour=guide_legend(title="")) +  
theme(plot.caption = element_text(hjust = 0)) + 
theme(legend.position = c(.925,.12)) 

dev.off()

}

if(FALSE) { # 4. largest datasets bar plot
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

pdf("C:/Users/ftw712/Desktop/citizenScienceBarplot.pdf",width=10,height=7)

breaks = seq(0,500e6,100e6)
label = c("0", "100 M", "200 M", "300 M", "400 M", "500 M")

# ggplot(PD, aes(x = Service, y = Perc, fill = Service, colour=colour)) 
ggplot(PD, aes(datasetkey,totalCounts, fill = csCategory)) + 
scale_x_discrete("dataset", labels = shortDatasetTitle) + 
scale_y_continuous(breaks = breaks, label = label) + 
ylab("number of occurrences") + 
geom_bar(stat = "identity", position = "dodge") + 
theme_hc() + 
scale_fill_manual(values = c("#509E2F","#777777")) + 
ggtitle(label = Title, subtitle = Subtitle) +
coord_flip() + 
guides(fill=guide_legend(title=NULL)) +
labs(caption = paste(strwrap(caption,40), collapse="\n")) +  
theme(legend.position = c(.85,.12)) + 
theme(plot.caption = element_text(hjust = 0)) 

dev.off()

}

if(FALSE) { # 5. The dominance of Birds in citizen science 

library(ggplot2)
library(roperators)
library(gbifapi)
library(dplyr)
library(purrr)
library(forcats)
library(ggthemes)
library(hrbrthemes)


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
D$kingdom = D$specieskeys %>% map(~ gbifapi::gbifapi("http://api.gbif.org/v1/species/" %+% .x)) %>% map_chr(~ .x$kingdom)
D$scientificName = D$specieskeys %>% map(~ gbifapi::gbifapi("http://api.gbif.org/v1/species/" %+% .x)) %>% map_chr(~ .x$scientificName) %>% rev()

D$speices[D$class == "Aves"] = "Bird"
D$speices[D$class == "Insecta"] = "Insect"
D$speices[D$class == "Mammalia"] = "Mammal"
D$speices[D$class == "Liliopsida"] = "Plant"
D$speices[D$class == "Pinopsida"] = "Plant"
D$speices[D$class == "Magnoliopsida"] = "Plant"

PD = D

# gbifColors = c("#175CA1", "#40BFFF", "#636FB4", "#E5FFFF", "#E8E8E8", "#D66F27", "#7D466A", "#FDAF02", "#509E2F", "#231F20")

breaks = seq(0,9e6,1e6)
label = c("0", "1M", "2M", "3M", "4M", "5M", "6M", "7M", "8M", "9M")

caption = "Source: " %+% length(citizenSciencekeys) %+% " GBIF citizen science datasets that have been both automatically and manually classified using the dataset description supplied by the publisher."

# p = ggplot(PD, aes(scientificName,occCounts, fill = speices)) + 
p = ggplot(PD, aes(specieskeys,occCounts, fill = speices)) + 
geom_bar(stat = "identity", position = "dodge",width=1.1) + 
scale_x_discrete(labels = D$scientificName) + 
scale_y_continuous(breaks = breaks,label = label) + 
scale_fill_manual(values = c("#dddddd","#777777","#509E2F","#D66F27")) + 
coord_flip() + 
labs(title="The dominance of birds in citizen science", subtitle="The top 800 species in GBIF citizen science datasets") + 
labs(caption = paste(strwrap(caption,60), collapse="\n")) + 	 
theme_ipsum_rc(grid="X") + 
guides(colour=guide_legend(title=NULL)) +
theme(axis.text.y=element_text(size=0.5)) +
theme(legend.position="bottom") + 
ylab("number of occurrences (millions)") + 
xlab("top 800 species") + 
guides(fill=guide_legend(title=NULL))


ggsave("C:/Users/ftw712/Desktop/top500speciesBarplot.pdf", plot = p, device=cairo_pdf, useDingbats=FALSE)
# ggsave("C:/Users/ftw712/Desktop/top500speciesBarplot.svg", p)

}

if(FALSE) { # 6. The top 800 species in citizen science 

library(ggplot2)
library(roperators)
library(gbifapi)
library(dplyr)
library(purrr)
library(forcats)
library(ggthemes)
library(hrbrthemes)
library(extrafont)
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
c("Melanitta fusca", "Velvet scoter")
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

# hack for adding text labels 
PD$barLabelsLines = ""
PD$barLabelsLines[PD$species %in% interestingTaxa$sciName] = paste(rep("-",220),collapse="")

# PD$barLabelsLines[PD$species == "Vanessa atalanta"] = paste(rep("-",200),collapse="")
# PD$barLabelsLines[PD$species == "Larus argentatus"] = paste(rep("-",180),collapse="")
# PD$barLabelsLines[PD$species == "Maniola jurtina"] = paste(rep("-",180),collapse="")
# PD$barLabelsLines[PD$species == "Pieris rapae"] = paste(rep("-",180),collapse="")


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
labs(caption = paste(strwrap(caption,60), collapse="\n")) + 	 
labs(title="The dominance of birds in citizen science", subtitle="The top 800 species in GBIF citizen science datasets") + 
geom_label(aes(y = (occCounts + 1e6),label=barLabels),label.padding = unit(0.1, "lines"),hjust=0,fill = "white",alpha=0.5, position=position_dodge(width=1), size=2) +
geom_label(aes(y = occCounts + 0.91e6,label=barLabelsBoxesBirds),label.padding = unit(0.1, "lines"),label.r = unit(0.1, "lines"),hjust=0,fill = "#dddddd", position=position_dodge(width=1), size=2) +
geom_label(aes(y = occCounts + 0.91e6,label=barLabelsBoxesMammals),label.padding = unit(0.1, "lines"),label.r = unit(0.1, "lines"),hjust=0,fill = "#D66F27", position=position_dodge(width=1), size=2) +
geom_label(aes(y = occCounts + 0.91e6,label=barLabelsBoxesPlants),label.padding = unit(0.1, "lines"),label.r = unit(0.1, "lines"),hjust=0,fill = "#509E2F", position=position_dodge(width=1), size=2) +
geom_label(aes(y = occCounts + 0.91e6,label=barLabelsBoxesInsects),label.padding = unit(0.1, "lines"),label.r = unit(0.1, "lines"),hjust=0,fill = "#777777", position=position_dodge(width=1), size=2) +
geom_text(aes(label=barLabelsLines), position=position_dodge(width=1), size=0.2, hjust=-0.1) 


ggsave("C:/Users/ftw712/Desktop/top500speciesBarplot.pdf", plot = p, device=cairo_pdf,width=9,height=7)
# ggsave("C:/Users/ftw712/Desktop/top500speciesBarplot.svg", p)

}



if(FALSE) { # 6. The dominance of eBird and Artportalen 
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

D = read.table("C:/Users/ftw712/Desktop/citizen science/data/some_manually_annotated_datasets.tsv",header=TRUE)
D = D[D$CS,]

citizenSciencekeys = gbifapi::gbifapi("http://api.gbif.org/v1/dataset?machineTagNamespace=citizenScience.mgrosjean.gbif.org&limit=500")$results %>% map_chr(~.x$key)

D = gbifsnapshots::countsByDatasetkey # all occ counts by datasetkey 

allCs = D %>% 
filter(datasetkey %in% citizenSciencekeys) %>% 
group_by(snapshots,created) %>% 
summarise(totalCounts = sum(countByDatasetkey)) %>%
mutate(plotCat = "all citizen science") %>%
as.data.frame()

eBird = D %>% filter(datasetkey %in% "4fa7b334-ce0d-4e88-aaae-2e0c138d049e") %>% 
group_by(snapshots,created) %>% 
summarise(totalCounts = sum(countByDatasetkey)) %>%
mutate(plotCat = "eBird") %>%
as.data.frame()

artPortalen = D %>% filter(datasetkey %in% "38b4c89f-584c-41bb-bd8f-cd1def33e92f") %>% 
group_by(snapshots,created) %>% 
summarise(totalCounts = sum(countByDatasetkey)) %>%
mutate(plotCat = "artportalen") %>%
as.data.frame()

PD = rbind(allCs,eBird, artPortalen) # plot data 

cs = allCs %>% filter(created == max(allCs$created)) %>% pull(totalCounts) %>% as.numeric()
eb = eBird %>% filter(created == max(eBird$created)) %>% pull(totalCounts) %>% as.numeric()
art = artPortalen %>% filter(created == max(artPortalen$created)) %>% pull(totalCounts) %>% as.numeric()
(art + eb)/cs

Title = "The dominance of eBird and Artportalen"
Subtitle = "The two largest GBIF datasets make up >70% of the citizen science occurrence records"
caption = "Based on data downloaded in 2018. eBird is updated on GBIF around once a year, and this is why we see sudden jumps in the number of citizen
science records. "

breaks = seq(0,1000e6,100e6)
label = c("0", "100 M", "200 M", "300 M", "400 M", "500 M", "600 M", "700 M", "800 M", "900 M", "1 B")

# gbifColors = c("#175CA1", "#40BFFF", "#636FB4", "#E5FFFF", "#E8E8E8", "#D66F27", "#7D466A", "#FDAF02", "#509E2F", "#231F20")
pdf("C:/Users/ftw712/Desktop/dominanceOfeBirdAndArtportalen.pdf",width=10,height=7)

ggplot(PD,aes(created, totalCounts,colour=plotCat)) + 
geom_line() + 
geom_point() + 
theme_hc() + 
scale_y_continuous(breaks = breaks,label = label) + 
scale_colour_manual(values = c("#509E2F", "#231F20","#D66F27")) + 
ggtitle(label = Title, subtitle = Subtitle) +
ylab("number of occurrences") + 
xlab("") + 
guides(colour=guide_legend(title=NULL)) +  
labs(caption = paste(strwrap(caption,90), collapse="\n")) +
theme(plot.caption = element_text(hjust = 0)) + 
theme(legend.position = c(.75,.79)) + 
theme(legend.background = element_rect(fill="transparent"))
dev.off()

}



if(FALSE) { # What types of animals of citizen scientists observing 

library(dplyr)
library(roperators)
library(purrr)

# datasetKey = "4fa7b334-ce0d-4e88-aaae-2e0c138d049e"
# taxonKey = "212" # birds
# taxonKey = "6" # plants
# taxonKey = "359" # mammals
# taxonKey = "216" # insects
# taxonKey = "1" # all Animals
# taxonKey = "2498056" # ducks
# taxonKey = Anatidae # ducks geese swans


citizenSciencekeys = gbifapi::gbifapi("http://api.gbif.org/v1/dataset?machineTagNamespace=citizenScience.mgrosjean.gbif.org&limit=500")$results %>% map_chr(~.x$key)
birdCount = citizenSciencekeys %>% map_dbl(~ gbifapi::gbifapi("http://api.gbif.org/v1/occurrence/count?datasetKey=" %+% .x %+% "&" %+% "taxonKey=" %+% taxonKey)) %>% sum()
totalCount = citizenSciencekeys %>% map_dbl(~ gbifapi::gbifapi("http://api.gbif.org/v1/occurrence/count?datasetKey=" %+% .x)) %>% sum()

(birdCount/totalCount)*100


}

if(FALSE) { # how many mallard ducks on inaturalist? 

library(dplyr)
library(roperators)
library(purrr)

taxonKey = "9761484" # mallard duck
datasetKey = "50c9509d-22c7-4a22-a47d-8c48425ef4a7"

numDucks = gbifapi::gbifapi("http://api.gbif.org/v1/occurrence/count?datasetKey=" %+% datasetKey %+% "&" %+% "taxonKey=" %+% taxonKey)
totaliNautralist = gbifapi::gbifapi("http://api.gbif.org/v1/occurrence/count?datasetKey=" %+% datasetKey)
# 27866
numDucks
(numDucks/totaliNautralist)*100 # 0.45%
}


if(FALSE) { # taxonKeys for citizen science datasets 

library(roperators)
library(gbifapi)
library(dplyr)
library(purrr)
library(rlist)

# get all taxonKeys for citizen science 
citizenSciencekeys = gbifapi::gbifapi("http://api.gbif.org/v1/dataset?machineTagNamespace=citizenScience.mgrosjean.gbif.org&limit=500")$results %>% map_chr(~.x$key)

DL = citizenSciencekeys %>% 
map(~ gbifapi::getTaxonKeyCounts(datasetkey=.x,verbose=FALSE,Step=1000,maxPages=1)) 
D = DL[map_lgl(DL, ~ !is.null(.x))] %>% plyr::rbind.fill() # filter list and combine data 

str(D)
# save(D, file ="C:/Users/ftw712/Desktop/D.rda")
}


if(FALSE) { # citizen science top taxa not only Birds 
library(roperators)
library(dplyr)
library(gbifapi)
library(purrr)

load("C:/Users/ftw712/Desktop/D.rda")

D = D %>% group_by(taxonkeys) %>%
summarise(occCounts = sum(occCounts)) %>%
arrange(-occCounts) %>%
head(200) %>% # get top 200
as.data.frame() 

D$taxonkeys %>% map(~ gbifapi::gbifapi("http://api.gbif.org/v1/species/" %+% .x))

D$scientificName = D$taxonkeys %>% map(~ gbifapi::gbifapi("http://api.gbif.org/v1/species/" %+% .x)) %>% map_chr(~ .x$scientificName)
D$rank = D$taxonkeys %>% map(~ gbifapi::gbifapi("http://api.gbif.org/v1/species/" %+% .x)) %>% map_chr(~ .x$rank)

rankAboveBird = D %>% filter(rank %in% c("KINGDOM","PHYLUM","CLASS"))
rankBelowBird = D %>% filter(rank %in% c("ORDER", "FAMILY", "GENUS", "SPECIES"))

rankAboveBird$class = NA
rankBelowBird$class = rankBelowBird$taxonkeys %>% map(~ gbifapi::gbifapi("http://api.gbif.org/v1/species/" %+% .x)) %>% map_chr(~ .x$class)

parentsOfAvesKeys = gbifapi::gbifapi("http://api.gbif.org/v1/species/212/parents?limit=20") %>% map_chr(~ .x$key)

D = rbind(rankAboveBird, rankBelowBird) %>% 
filter(!class == "Aves" | is.na(class)) %>% 
filter(!taxonkeys %in% parentsOfAvesKeys)

interesting = c("Aves","Plantae","Tracheophyta","Insecta","Liliopsida","Fungi","Lepidoptera","Poales","Hymenoptera")

interesting
}


if(FALSE) { # make speciesKeyCounts.rda

library(roperators)
library(gbifapi)
library(dplyr)
library(purrr)


# get all taxonKeys for citizen science 
citizenSciencekeys = gbifapi::gbifapi("http://api.gbif.org/v1/dataset?machineTagNamespace=citizenScience.mgrosjean.gbif.org&limit=500")$results %>% map_chr(~.x$key)

DL = citizenSciencekeys %>% 
map(~ gbifapi::getSpeciesKeyCounts(datasetkey=.x,verbose=TRUE,Step=1000,maxPages=1)) 
D = DL[map_lgl(DL, ~ !is.null(.x))] %>% plyr::rbind.fill() # filter list and combine data 

speciesKeyCounts = D # 
save(speciesKeyCounts, file ="C:/Users/ftw712/Desktop/speciesKeyCounts.rda")


# gbifapi::getSpeciesKeyCounts

}

# Ireland number of taxa detected by citizen science 
if(FALSE) { # match checklist scientificName with GBIF backbone
library(dplyr)
library(purrr)
library(purrrlyr)

D = data.table::fread("C:/Users/ftw712/Desktop/citizen science/data/dwca-ireland-griis-gbif-v2.4/taxon.txt") %>% as.data.frame()
D = D %>% select(scientificName,taxonRank,kingdom,phylum,class,order,family) 

backboneStats = D %>% pmap(~ rgbif::name_backbone(name=..1,rank=..2,kingdom=..3,phylum=..4,class=..5,order=..6,family=..7)) %>% 
map(~ data.frame(usageKey = .x$usageKey, scientificNameApi = .x$scientificName, confidence=.x$confidence)) %>% 
plyr::rbind.fill()

D = cbind(D,backboneStats)
str(D)

# save(D,file = "C:/Users/ftw712/Desktop/D.rda")
}

if(FALSE) { # get csCounts.rda
library(roperators)
library(dplyr)
library(purrr)

load(file = "C:/Users/ftw712/Desktop/D.rda")
str(D)

citizenSciencekeys = gbifapi::getCitizenScienceKeys()

citizenSciencekeys

# "http://api.gbif.org/v1/occurrence/search?country=IE&dataset_key=f794b231-42de-4008-ba8e-809e01ee7785&taxon_key=7561279"

getCount = function(usageKey) {
	
	countsCountryDatasetKey = citizenSciencekeys %>% map(~ gbifapi::gbifapi("http://api.gbif.org/v1/occurrence/search?country=IE&dataset_key=" %+% .x %+% "&" %+% "taxon_key=" %+% usageKey)) %>% map_dbl(~ .x$count)
	
	countsCountry = citizenSciencekeys %>% map(~ gbifapi::gbifapi("http://api.gbif.org/v1/occurrence/search?country=IE" %+% "&" %+% "taxon_key=" %+% usageKey)) %>% map_dbl(~ .x$count)
	
	d = data.frame(citizenSciencekeys,countsCountry,countsCountryDatasetKey,usageKey)
	print(d)
	print(d$countsCountry)
	print(d$countsCountryDatasetKey)
	return(d) 
}

csCounts = D$usageKey %>% map(~ getCount(.x) ) %>% plyr::rbind.fill()
print(csCounts)
# save(csCounts, file = "C:/Users/ftw712/Desktop/csCounts.rda")
}

if(FALSE) { # plotting ireland citizen science 
library(dplyr)
library(purrr)
library(roperators)
library(forcats)
library(ggthemes)
library(hrbrthemes)
library(extrafont)
loadfonts(quiet = TRUE)

load("C:/Users/ftw712/Desktop/csCounts.rda")

usageKeyCounts = csCounts %>% group_by(usageKey) %>% summarise(usageKeyCounts = n()) %>% pull(usageKeyCounts)

D = csCounts %>% group_by(usageKey) %>% 
summarise(csCounts = sum(countsCountryDatasetKey), countryCounts = sum(countsCountry)) %>%
as.data.frame() 

D$countryCounts = D$countryCounts/usageKeyCounts 
D = D %>% arrange(-csCounts,-countryCounts) 
D = D %>% setNames(c("taxonKey", "csCounts", "countryCounts"))

D$species = D$taxonKey %>% map(~ gbifapi::gbifapi("http://api.gbif.org/v1/species/" %+% .x)) %>% map(~ .x$species) %>% map(~ ifelse(is.null(.x), NA, .x)) %>% flatten_chr()

D$class = D$taxonKey %>% map(~ gbifapi::gbifapi("http://api.gbif.org/v1/species/" %+% .x)) %>% map_chr(~ .x$class)
D$kingdom = D$taxonKey %>% map(~ gbifapi::gbifapi("http://api.gbif.org/v1/species/" %+% .x)) %>% map_chr(~ .x$kingdom)

D = head(D,100)

# str(D)
D = reshape2::melt(D,id=c("taxonKey","species","class","kingdom"))


str(D)

D$taxonKey = as.factor(D$taxonKey)
D$taxonKey = fct_reorder(D$taxonKey, D$value)

library(ggplot2)
p = ggplot(D,aes(as.factor(taxonKey),value, fill = variable)) +   
geom_bar(position = "dodge", stat="identity") + 
coord_flip() + 
scale_fill_manual(values = c("#D66F27","#509E2F")) + 
theme_ipsum(base_family="Myriad Pro", grid="X") +
theme(axis.text.y=element_text(size=1))

ggsave("C:/Users/ftw712/Desktop/irelandInvasivesBarplot.pdf", plot = p, device=cairo_pdf)


# Muntiacus reevesi
# Dreissena polymorpha
# Sus scrofa
# Oxyura jamaicensis
# Mustela furo
# Carpobrotus edulis
# Harmonia axyridis
# Sciurus carolinensis
# Gunnera tinctoria
# Heracleum mantegazzianum
# Rattus norvegicus
# Rattus rattus

}




