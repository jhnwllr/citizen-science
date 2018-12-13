# Unique GBIF Points Graph 


D = read.table("C:/Users/ftw712/Desktop/snapshotCounts.csv",sep=",",header=TRUE)
str(D)

library(dplyr)
library(stringr)
library(lubridate)
library(reshape2)
library(ggplot2)

D$date = D$snapshot %>% str_replace_all("raw_", "") %>% ymd()

max(D$countDistinctLatLon) - min(D$countDistinctLatLon)
max(D$count) - min(D$count)

max(D$countDistinctLatLon)
# 26 M
# 662M 

D = melt(D,id=c("snapshot","date"))
D$value = D$value/1e6

D = D[D$variable == "countDistinctLatLon",]

# str(D)
pdf("C:/Users/ftw712/Desktop/plotUniqueCounts.pdf",width=10,height=5)
ggplot(D, aes(date, value, colour=variable)) + geom_line() + facet_wrap(~variable,scale="free")
dev.off()

# 255 848 753
# D$count
# D$countDistinctLatLon



