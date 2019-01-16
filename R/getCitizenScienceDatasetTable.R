
library(roperators)
library(dplyr)
library(purrr)
library(forcats)

citizenSciencekeys = gbifapi::getCitizenScienceKeys()

# citizenSciencekeys %>% map_chr(~ gbifapi::gbifapi("http://api.gbif.org/v1/occurrence/count?datasetKey=" %+% .x))

D = data.frame(datasetkey = citizenSciencekeys) %>%
mutate(datasetTitle = map_chr(datasetkey, ~ gbifapi::gbifapi("http://api.gbif.org/v1/dataset/" %+% .x)$title)) %>%
mutate(occurrenceCount = map_chr(datasetkey, ~ gbifapi::gbifapi("http://api.gbif.org/v1/occurrence/count?datasetKey=" %+% .x))) %>%
mutate(occurrenceCount = as.numeric(occurrenceCount)) %>%
arrange(-occurrenceCount) 


D

write.table(D,file="C:/Users/ftw712/Desktop/citizenScienceTable.csv",quote=FALSE,sep="\t",row.names=FALSE)
