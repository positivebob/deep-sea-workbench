##### Installation/Loading of Packages ##### 

library(knitr)

#install.packages("maptools")
library(maptools)

#install.packages("maps")

library(maps)

#("reshape")


library(reshape)

#install.packages("reshape2")


library(reshape2)


#install.packages("psych")


library(psych)


#install.packages("ggplot2")


library(ggplot2)


#install.packages("data.table")

library(data.table)


#intstall.packages("plyr")


#insatll.packags("dplyr")


library(plyr)


library(dplyr)


#install.packages("car")


library(car)


#install.packages("rgdal")


library(gdata)


library(stringr)

#install.packages("xlsx")
#library(xlsx)

#install.packages("rgdal")


library(rgdal)

library(ggmap)


library(SSOAP)

require(gdata)

##### Setting working directories to intake ##### 
setwd("C:/rworking/deep-sea-workbench/InData/IntakePipe")

##### Bringing in input datasets #####

#from csv
d<-read.csv("Join_Output.csv", header = T)
d_cor <- read.csv("Pacific_IslandsMap20150814_9-26.csv", header = T)

#from table
d <- read.table("DSCRTP_NatDB_20150626-1.txt", header = T, sep = ",")

#from xls
d <- read.xlsx("DSCRTP_NatDB_20150626-1.xlsx", sheetName = "Sheet1")

##### Subsetting according to geography #####
subset <- subset(d, as.numeric(Latitude) > 18.1509 & 
                   as.numeric(Latitude) < 30.40613 & 
                   as.numeric(Longitude) < -80.45932 &
                   as.numeric(Longitude) > -97.858208)


##### Setting various to various filters for summary of QA datasets #####

subset <- d2
  #filter(as.numeric(IndividualCount) > 200)  %>%
  #filter(DataProvider == "" )
 
x <- d %>%
  #filter(as.numeric(Flag) == 0)  %>%
  #filter(DataProvider == "California Academy of Sciences" ) %>%
  #filter(DataProvider == "California Academy of Sciences" ) %>%
  #filter(grepl("Porifera", d$Phylum) & PI == "Butler")  %>%
  #filter(PI == "Butler" 
        # & DataProvider == "NOAA, Center for Coastal Environmental Health and Biomolecular Research"
        # & ScientificName != "Porifera") #%>%
  #filter(grepl("EX1402L3", d$ImageFilePath)) %>%
  #select(Flag, FlagReason, CatalogNumber,SampleID, Latitude, Longitude, DataProvider, DataContact
   #      Vessel, VehicleName, SamplingEquipment, LocationAccuracy, PI, SurveyID, 
    #     ObservationYear, Repository, ScientificName, Citation) %>%
  group_by(Phylum, Order, Genus, Locality_1, ScientificName) %>%
  summarize(n_records = n(), 
            IndividuaCount = sum(as.numeric(IndividualCount)),
            minyear = min(as.numeric(ObservationYear)), 
            maxyear = max(as.numeric(ObservationYear)), 
            mindepth = min (as.numeric(DepthInMeters)), 
            maxdepth = max (as.numeric(DepthInMeters)))
                                                             
setwd("C:/rworking/deep-sea-workbench/OutData")
write.csv(x,"Summary_Table_with_Locations_TannerCortes_RMcguinn_20150925.csv", row.names = F, quote = T)                                 


write.csv(d,"tanner_cortes_gislocality.csv", row.names = F, quote = T)                                 

                                
                                                    



fix(x)
sum(x$n)

table(factor(x$Genus), useNA = "always")
table(factor(x$DataContact), useNA = "always")
table(factor(x$Repository), useNA = "always")
table(factor(x$Repository), useNA = "always")
#fix(x) 

pi_tbl <-
  subset %>%
  group_by(PI) %>%
  summarize(n = n()) %>%
  arrange(desc(n))
pi_tbl


cit_tbl <-
  subset %>%
  group_by(Citation) %>%
  summarize(n = n()) %>%
  arrange(desc(n))
cit_tbl

genus_tbl <-
  subset %>%
  group_by(Genus) %>%
  summarize(n = n()) %>%
  arrange(desc(n))
genus_tbl

repo_tbl <-
  subset %>%
  group_by(Repository) %>%
  summarize(n = n()) %>%
  arrange(desc(n))
repo_tbl

setwd("C:/rworking/deep-sea-workbench/OutData")
write.csv(genus_tbl, "genus_table_Gulf.csv")
write.csv(cit_tbl, "Citation_JHS.csv")
write.csv(order_tbl, "Order_table_JHS.csv")


subset$ObservationYear[subset$ObservationYear == -999] <- NA
summary(as.numeric(subset$ObservationYear), na.rm = T)

fix(subset)

length(subset$DataProvider)
sum(x$n)
sum(x$n_coral)
sum(x$n_sponges)

write.csv(x,"DSCRTP_20150626-1_DatasetID_nrows3785.csv", row.names = F, quote = T)

##### This is a way to split a dataset up into its constituent parts #####

lst1 <- split(x, with(x, interaction(Order,Locality)), drop = TRUE)
lapply(seq_along(lst1),function(i) write.csv(lst1[[i]],file=paste0(names(lst1[i]),".csv"),row.names=FALSE)) 


##### Exporting taxonomy table #####
x <- d %>%
  filter(as.numeric(Flag) == 0)  %>%
  group_by(VernacularNameCategory, ScientificName, TaxonRank, AphiaID, Phylum, Class, Subclass, Order, Suborder, Family, 
           Subfamily, Genus, Subgenus, Species, Subspecies) %>%
  summarize(n = n()) 

setwd("C:/rworking/deep-sea-workbench/OutData")
write.csv(x,"unique_taxonomy_table_from_DSCRTP_NatDB_20150714-0_.csv", row.names = F, quote = T)
          
##### Exporting dataset metadata ##### 
x <- d %>%
  #filter(VehicleName == "Delta")  %>%
  #select(DataProvider, Repository, PI, DataContact, FlagReason) 
  group_by(DataProvider, Citation, SurveyID, Vessel, Repository, PI, DataContact) %>%
  summarize(n = n(), n_coral = length(Phylum[Phylum == "Cnidaria"]), 
            n_sponges = length(Phylum[Phylum == "Porifera"]),
            start = min(as.numeric(ObservationYear[ObservationYear != -999])), 
            end = max(as.numeric(ObservationYear[ObservationYear != -999])), 
            noflag = length(Flag[Flag == "0"]), 
            flagged = length(Flag[Flag == "1"]), 
            n_orders = length(unique(Order)),
            imagefilepath = length(ImageFilePath[is.na(ImageFilePath) == F]),
            imageurl = length(ImageURL[is.na(ImageURL) == F])
) 
  
setwd("C:/rworking/deep-sea-workbench/OutData")
write.csv(x,"datasetID_NatDB_20150807-1-summary.csv", row.names = F, quote = T)
write.csv(geosub,"cortes_tanner_natDB.csv", row.names = F, quote = T)


##### Taxonomic Table ##### 

x <- d %>%
  filter(as.numeric(Flag) == 0)  %>%
  group_by(VernacularNameCategory, ScientificName, TaxonRank, AphiaID, Phylum, Class, Subclass, Order, Suborder, Family, 
           Subfamily, Genus, Subgenus, Species, Subspecies) %>%
  summarize(n = n()) 

setwd("C:/rworking/deep-sea-workbench/OutData")
write.csv(x,"unique_taxonomy_table_from_DSCRTP_NatDB_20150714-0_.csv", row.names = F, quote = T)

##### Exporting dataset metadata basic taxonomy and depth distribution ##### 
x <- d %>%  
filter(DataProvider == "NOAA, Alaska Fisheries Science Center" & Flag != "1")  %>%
    group_by(FlagReason,Order, ScientificName) %>%
    summarize(n = n(), 
              Min_Depth = min(as.numeric(DepthInMeters[DepthInMeters != -999])), 
              Max_Depth = max(as.numeric(DepthInMeters[DepthInMeters != -999])), 
              Min_Lat = min(as.numeric(Latitude[Latitude != -999])), 
              Max_Lat = max(as.numeric(Latitude[Latitude != -999])),
              Min_Long = min(as.numeric(Longitude[Longitude != -999])), 
              Max_Long = max(as.numeric(Longitude[Longitude != -999])),        
              noflag = length(Flag[Flag == "0"]), 
              flagged = length(Flag[Flag == "1"]))

plot(a,b, cex = 0.5)
hpts <- chull(a,b)
hpts <- c(hpts, hpts[1])
lines([hpts, ]) 
             

setwd("C:/rworking/deep-sea-workbench/OutData")
write.csv(x,"datasetID_NatDB_20150807_tax_summary_AFSF.csv", row.names = F, quote = T)

##### 
unique(factor(d_cor$DataProvider))
unique(factor(d_cor$DataProvider))
