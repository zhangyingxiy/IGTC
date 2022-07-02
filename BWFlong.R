setwd("/Users/ying/Desktop/R projects")
library(haven)
library(tidyverse)
library(dplyr)

long <- read_dta("GenMod_Flavors_Long_FlavorLevel_6Jun2022.dta")
colnames(long)
nrow(long)
long %>% 
  group_by(flavors_flavor, na.rm = T) %>% 
  summarise(percent = 100 * n() / nrow(long))

long$flavors_flavor

## How many flavors were advertised?
unique(long$name_flavor)

newdf <- long[long$name_flavor!= "",]
view(newdf)
colnames(newdf)
nrow(newdf)

newdf %>% 
  group_by(flavors_flavor) %>% 
  summarise(percent = 100 * n() / nrow(newdf))

tobacco_flavored <- newdf[newdf$flavors_flavor == 1,]


sapply(tobacco_flavored[,17:62], function(x) sum(x == 1)/nrow(tobacco_flavored)*100)





