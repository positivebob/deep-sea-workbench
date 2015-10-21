#Working on mapping subsets for the State of Deep Sea Corals #####
d <- d_cor

###### map 1 - Order == "Scleractinia"#####
bs_wp <- d %>%
  filter(Flag == "0", CatalogNumber != "276894", CatalogNumber != "248020", Order == "Scleractinia")  %>%
  select(Latitude, Longitude, VernacularNameCategory, Order, Genus, ScientificName)

setwd("C:/rworking/deep-sea-workbench/OutData")
write.csv(bs_wp, "bs_wp_20151006.csv")

#cleanup
rm(list = ls())

###### map 2 - Antipatharia #####

at_wp <- d %>%
  filter(Flag == "0", Order == "Antipatharia")  %>%
  select(DataProvider, SampleID, Latitude, Longitude, VernacularNameCategory, Order, Family, Genus, ScientificName)

setwd("C:/rworking/deep-sea-workbench/OutData")
write.csv(at_wp,"at_wp_2.csv")

###### map 3 - Non-gorgonian alcyonaceans #####

alcyon_wp <- d %>%
  filter(Flag == "0", Order == "Alcyonacea")  %>% 
  select(DataProvider, SampleID, Latitude, Longitude, VernacularNameCategory, Order, Suborder, Family, Genus, ScientificName)

setwd("C:/rworking/deep-sea-workbench/OutData")
write.csv(alcyon_wp,"alcyon_wp_2.csv")


###### map 4 - Gorgonian alcyonaceans – #####
# with differentcolors for suborders Holaxonia, Scleraxonia, & Calcaxonia 
# (this would trackwell with much of the region-level modeling 
#which has been done at this level.Black dots for unidentified gorgonians

gorg_wp <- d %>%
  filter(Flag == "0", CatalogNumber != "530828", Order == "Gorgonacea")  %>% 
  select(DataProvider, SampleID, Latitude, Longitude, VernacularNameCategory, Order, Suborder, Family, Genus, ScientificName)

setwd("C:/rworking/deep-sea-workbench/OutData")
write.csv(gorg_wp,"gorg_wp_20151006.csv")

###### map 5 - Pennatulacea – with different colors forsuborders #####
#Sessiliflorae & Subsessiliflorae (may be too many families tobreak it down further)
#& unidentified seapens (black dots)


pen_wp <- d %>%
  filter(Flag == "0", Order == "Pennatulacea")  %>% 
  select(DataProvider, SampleID, Latitude, Longitude, VernacularNameCategory, Order, Suborder, Family, Genus, ScientificName)

setwd("C:/rworking/deep-sea-workbench/OutData")
write.csv(pen_wp,"pen_wp_2.csv")  
  
 
###### map 6 -  Stylasterid corals and other (e.g., gold corals)#####

styl_wp <- d %>%
  filter(Flag == "0", Family == "Stylasteridae")  %>% 
  select(DataProvider, SampleID, Latitude, Longitude, VernacularNameCategory, Order, Suborder, Family, Genus, ScientificName)

setwd("C:/rworking/deep-sea-workbench/OutData")
write.csv(styl_wp,"styl_wp_2.csv")  
  

###### map 7 -  Sponges (one map with different colors for Porifera #####
# (unidentified – black dots), Demosponges, Glass sponges, and calcareous sponge

sp_wp <- d %>%
  filter(Flag == "0", Phylum == "Porifera")  %>% 
  select(DataProvider, SampleID, Latitude, Longitude, VernacularNameCategory, Order, Suborder, Family, Genus, ScientificName)

#sp_wp$Latitude <- as.numeric(sp_wp$Latitude)
#sp_wp$Longitude <- as.numeric(sp_wp$Longitude)

setwd("C:/rworking/deep-sea-workbench/OutData")
write.csv(sp_wp,"sp_wp2.csv")  




