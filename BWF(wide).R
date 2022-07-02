setwd("/Users/ying/Desktop/R projects")
library(haven)
library(tidyverse)
library(dplyr)

wide <- read_dta("GenMod_Flavors_Wide_AdLevel_6Jun2022.dta")
nrow(wide)


##What brands were coded in the dataset?
unique(wide$brand)
table(wide$brand)

##Report the # of tobacco flavors advertised overall
wide_flavors <- wide %>%
  select("flavors_flavor1", "flavors_flavor2", "flavors_flavor3", 
         "flavors_flavor4", "flavors_flavor5", "flavors_flavor6", "flavors_flavor7", 
         "flavors_flavor8", "flavors_flavor9", "flavors_flavor10")
view(wide_flavors)


wide_flavors$number <- apply(wide_flavors[,1:10], MARGIN=1, function(x) sum(!is.na(x)))

wide_flavors %>% 
  group_by(number) %>% 
  summarise(percent = 100 * n() / nrow(wide_flavors))

##Blu

blu <- wide[wide$brand == 1,]
blu <- slice(blu, 1:(n()-4))     ##Delete last 4 rows (NA)


blu_flavors <- blu %>%
  select("flavors_flavor1", "flavors_flavor2", "flavors_flavor3", 
         "flavors_flavor4", "flavors_flavor5", "flavors_flavor6", "flavors_flavor7", 
         "flavors_flavor8", "flavors_flavor9", "flavors_flavor10")
view(blu_flavors)

blu_flavors$tobacco <- apply(blu_flavors, MARGIN=1, function(x) any(x == 1))

blu_flavors %>% 
  group_by(tobacco) %>% 
  summarise(percent = 100 * n() / nrow( blu_flavors))

blu_flavors$number <- apply(blu_flavors[,1:10], MARGIN=1, function(x) sum(!is.na(x)))

blu_flavors %>% 
  group_by(number) %>% 
  summarise(percent = 100 * n() / nrow(blu_flavors))

##Juul

juul <- wide[wide$brand == 2,]
juul <- slice(juul, 1:(n()-4))     ##Delete last 4 rows (NA)
view(juul)
table(juul$brand)

juul_flavors <- juul %>%
  select("flavors_flavor1", "flavors_flavor2", "flavors_flavor3", 
         "flavors_flavor4", "flavors_flavor5", "flavors_flavor6", "flavors_flavor7", 
         "flavors_flavor8", "flavors_flavor9", "flavors_flavor10")

juul_flavors$tobacco <- apply(juul_flavors, MARGIN=1, function(x) any(x == 1))


juul_flavors %>% 
  group_by(tobacco) %>% 
  summarise(percent = 100 * n() / nrow(juul_flavors))

juul_flavors$number <- apply(juul_flavors[,1:10], MARGIN=1, function(x) sum(!is.na(x)))

juul_flavors %>% 
  group_by(number) %>% 
  summarise(percent = 100 * n() / nrow(juul_flavors))


##MarkTen

MarkTen <- wide[wide$brand == 3,]
MarkTen <- slice(MarkTen, 1:(n()-4))     ##Delete last 4 rows (NA)

MarkTen_flavors <- MarkTen %>%
  select("flavors_flavor1", "flavors_flavor2", "flavors_flavor3", 
         "flavors_flavor4", "flavors_flavor5", "flavors_flavor6", "flavors_flavor7", 
         "flavors_flavor8", "flavors_flavor9", "flavors_flavor10")

MarkTen_flavors$tobacco <- apply(MarkTen_flavors, MARGIN=1, function(x) any(x == 1))


MarkTen_flavors %>% 
  group_by(tobacco) %>% 
  summarise(percent = 100 * n() / nrow(MarkTen_flavors))

MarkTen_flavors$number <- apply(MarkTen_flavors[,1:10], MARGIN=1, function(x) sum(!is.na(x)))

MarkTen_flavors %>% 
  group_by(number) %>% 
  summarise(percent = 100 * n() / nrow(MarkTen_flavors))


##NJOY

NJOY <- wide[wide$brand == 4,]
NJOY <- slice(NJOY, 1:(n()-4))     ##Delete last 4 rows (NA)

NJOY_flavors <- NJOY %>%
  select("flavors_flavor1", "flavors_flavor2", "flavors_flavor3", 
         "flavors_flavor4", "flavors_flavor5", "flavors_flavor6", "flavors_flavor7", 
         "flavors_flavor8", "flavors_flavor9", "flavors_flavor10")

NJOY_flavors$tobacco <- apply(NJOY_flavors, MARGIN=1, function(x) any(x == 1))


NJOY_flavors %>% 
  group_by(tobacco) %>% 
  summarise(percent = 100 * n() / nrow(NJOY_flavors))

NJOY_flavors$number <- apply(NJOY_flavors[,1:10], MARGIN=1, function(x) sum(!is.na(x)))

NJOY_flavors %>% 
  group_by(number) %>% 
  summarise(percent = 100 * n() / nrow(NJOY_flavors))


##Vuse

Vuse <- wide[wide$brand == 5,]
Vuse <- slice(juul, 1:(n()-4))     ##Delete last 4 rows (NA)

Vuse_flavors <- Vuse %>%
  select("flavors_flavor1", "flavors_flavor2", "flavors_flavor3", 
         "flavors_flavor4", "flavors_flavor5", "flavors_flavor6", "flavors_flavor7", 
         "flavors_flavor8", "flavors_flavor9", "flavors_flavor10")

Vuse_flavors$tobacco <- apply(Vuse_flavors, MARGIN=1, function(x) any(x == 1))


Vuse_flavors %>% 
  group_by(tobacco) %>% 
  summarise(percent = 100 * n() / nrow(Vuse_flavors))

Vuse_flavors$number <- apply(Vuse_flavors[,1:10], MARGIN=1, function(x) sum(!is.na(x)))

Vuse_flavors %>% 
  group_by(number) %>% 
  summarise(percent = 100 * n() / nrow(Vuse_flavors))

##VaporFi

VaporFi <- wide[wide$brand == 8,]
VaporFi <- slice(VaporFi, 1:(n()-4))     ##Delete last 4 rows (NA)

VaporFi_flavors <- VaporFi %>%
  select("flavors_flavor1", "flavors_flavor2", "flavors_flavor3", 
         "flavors_flavor4", "flavors_flavor5", "flavors_flavor6", "flavors_flavor7", 
         "flavors_flavor8", "flavors_flavor9", "flavors_flavor10")

VaporFi_flavors$tobacco <- apply(VaporFi_flavors, MARGIN=1, function(x) any(x == 1))


VaporFi_flavors %>% 
  group_by(tobacco) %>% 
  summarise(percent = 100 * n() / nrow(VaporFi_flavors))

VaporFi_flavors$number <- apply(VaporFi_flavors[,1:10], MARGIN=1, function(x) sum(!is.na(x)))

VaporFi_flavors %>% 
  group_by(number) %>% 
  summarise(percent = 100 * n() / nrow(VaporFi_flavors))

##Logic

Logic <- wide[wide$brand == 9,]
Logic <- slice(Logic, 1:(n()-4))     ##Delete last 4 rows (NA)

Logic_flavors <- Logic %>%
  select("flavors_flavor1", "flavors_flavor2", "flavors_flavor3", 
         "flavors_flavor4", "flavors_flavor5", "flavors_flavor6", "flavors_flavor7", 
         "flavors_flavor8", "flavors_flavor9", "flavors_flavor10")

Logic_flavors$tobacco <- apply(Logic_flavors, MARGIN=1, function(x) any(x == 1))


Logic_flavors %>% 
  group_by(tobacco) %>% 
  summarise(percent = 100 * n() / nrow(Logic_flavors))

Logic_flavors$number <- apply(Logic_flavors[,1:10], MARGIN=1, function(x) sum(!is.na(x)))

Logic_flavors %>% 
  group_by(number) %>% 
  summarise(percent = 100 * n() / nrow(Logic_flavors))

##South Beach Smoke

South_Beach_Smoke <- wide[wide$brand == 10,]
South_Beach_Smoke <- slice(South_Beach_Smoke, 1:(n()-4))     ##Delete last 4 rows (NA)


South_Beach_Smoke_flavors <- South_Beach_Smoke %>%
  select("flavors_flavor1", "flavors_flavor2", "flavors_flavor3", 
         "flavors_flavor4", "flavors_flavor5", "flavors_flavor6", "flavors_flavor7", 
         "flavors_flavor8", "flavors_flavor9", "flavors_flavor10")

South_Beach_Smoke_flavors$tobacco <- apply(South_Beach_Smoke_flavors, MARGIN=1, function(x) any(x == 1))


South_Beach_Smoke_flavors %>% 
  group_by(tobacco) %>% 
  summarise(percent = 100 * n() / nrow(South_Beach_Smoke_flavors))

South_Beach_Smoke_flavors$number <- apply(South_Beach_Smoke_flavors[,1:10], MARGIN=1, function(x) sum(!is.na(x)))

South_Beach_Smoke_flavors %>% 
  group_by(number) %>% 
  summarise(percent = 100 * n() / nrow(South_Beach_Smoke_flavors))

##Vapor4Life

Vapor4Life <- wide[wide$brand == 11,]
Vapor4Life <- slice(Vapor4Life, 1:(n()-4))     ##Delete last 4 rows (NA)


Vapor4Life_flavors <- Vapor4Life %>%
  select("flavors_flavor1", "flavors_flavor2", "flavors_flavor3", 
         "flavors_flavor4", "flavors_flavor5", "flavors_flavor6", "flavors_flavor7", 
         "flavors_flavor8", "flavors_flavor9", "flavors_flavor10")

Vapor4Life_flavors$tobacco <- apply(Vapor4Life_flavors, MARGIN=1, function(x) any(x == 1))

Vapor4Life_flavors$number <- apply(Vapor4Life_flavors, MARGIN=1, function(x) sum(!is.na(x)))
view(Vapor4Life_flavors)

Vapor4Life_flavors %>% 
  group_by(number) %>% 
  summarise(percent = 100 * n() / nrow(Vapor4Life_flavors))

Vapor4Life_flavors %>% 
  group_by(tobacco) %>% 
  summarise(percent = 100 * n() / nrow(Vapor4Life_flavors))

Vapor4Life_flavors$number <- apply(Vapor4Life_flavors[,1:10], MARGIN=1, function(x) sum(!is.na(x)))
view(Vapor4Life_flavors)

Vapor4Life_flavors %>% 
  group_by(number) %>% 
  summarise(percent = 100 * n() / nrow(Vapor4Life_flavors))




          











