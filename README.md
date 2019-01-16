# GBIF citizen science repository 

Here you will find files related to citizen science research at GBIF. 


You can get a json list of all citizen science datasets using: 

```

http://api.gbif.org/v1/dataset?machineTagNamespace=citizenScience.mgrosjean.gbif.org&limit=500

```

One can get a table of citizen science counts using R. Requires my gbifapi package. 

```


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

```



