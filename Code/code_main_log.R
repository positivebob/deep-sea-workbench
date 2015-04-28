##### Header #####
#Author: Robert McGuinn
#Date: This file started 20140731
#Abstract: Editing version 20140731-1 
#Institution:  NOAA - CCEHBR
#Place: Charleston, SC

##### Installation/Loading of Packages #####
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
library(dplyr)
#install.packages("car")
library(car)
#install.packages("rgdal")
library(gdata)
library(stringr)
#install.packages("rgdal")
library(rgdal)
library(ggmap)
library(plyr)
library(SSOAP)

##### ____________ NEWDATA: Loading database version 201407311 #####

##### Reading in Original Data #####
options(stringsAsFactors=FALSE)
d<-read.csv("DSC-RTP_Nat_DBS_20140731-1.csv")

##### _____OUTPUT: "d" ######

##### ____________ GeoSubsetting #####

##### Geographic subsetting to Gulf of Mexico (R code for data distribution to Katie Crane) ##### 
geosub<- subset(d, as.numeric(latitude) > 20 & 
                           as.numeric(latitude) < 35 & 
                           as.numeric(longitude) < -75 &
                           as.numeric(longitude) > -97, )
##### Subsetting to only flagged fields = 0
geosub<- subset(geosub, flag == 0, )

#####_____ OUTPUT: "DSC-RTP_Nat_DBS_GoMexSubset_20140731-1.csv"
#### Writing data for release #####
write.csv(d2, "DSC-RTP_Nat_DBS_GoMexSubset_20140731-1.csv")

##### Checking data and GIS component GOMEX 
unique(d2$fishcouncilregion)
gis<-subset(d2, ,c(latitude, longitude, dataprovider))
write.csv(gis, "gis.csv")
x<-subset(d2, dataprovider == "NOAA, Northeast Fisheries Science Center", c(catalognumber, dataprovider))
write.csv(x, "test.csv")

##### _____ OUTPUT:"DSC-RTP_Nat_DBS_GoMexSubset_20140731-1.csv" #####

##### ____________ Working on Bugs #####

##### Looking at some problems in navtype ##### 
subset(d, dataprovider == "NOAA, Cordell Bank National Marine Sanctuary", c(catalognumber, navtype))

##### Working on bug#2 #####
class(d$observationdate)
class(d6$ObservationDate)
head(d$observationdate)
head(d6$ObservationDate)
table(d$entrydate, useNA = "always")
table(d6$EntryDate, useNA = "always")
table(d$modified, useNA = "always")
table(d6$Modified, useNA = "always")
table(d$identificationdate, useNA = "always")
table(d6$IdentificationDate, useNA = "always")
length(subset(d, is.na(observationdate) == T, c(observationdate))$observationdate)
length(subset(d6, is.na(ObservationDate) == T, c(ObservationDate))$ObservationDate)
length(subset(d, is.na(entrydate) == T, c(entrydate))$entrydate)
length(subset(d6, is.na(EntryDate) == T, c(EntryDate))$EntryDate)

##### Working on bug #5 - zooxanthellate corals ##### 
subset(d, taxongenus == "Montastrea", c(taxongenus))
subset(d, taxongenus == "Eunicea", c(flag, taxongenus))
d[d$taxongenus == "Eunicea" & is.na(d$taxongenus) == F,]$flag <- 1
d[d$taxongenus == "Eunicea" & is.na(d$taxongenus) == F,]$flagreason <- "Eunicea is zooxanthellate - remove from database"

##### Working on bug #82- catagorical abundance is messed up in some ##### 
subset(d, catalognumber == "418744", c(categoricalabundance, datacontact, reporter))
subset(d6, CatalogNumber == "418744", c(CategoricalAbundance, DataContact, Reporter))

subset(d, categoricalabundance == "25-Jun", c(catalognumber, categoricalabundance, datacontact, reporter))
subset(d6, CatalogNumber == "419364", c(CategoricalAbundance, DataContact, Reporter))

# Recoding the offending variables.  
d$categoricalabundance<-recode(d$categoricalabundance, '"25-Jun" = "6-25"')
d$categoricalabundance<-recode(d$categoricalabundance, '"5-Jan" = "1-5"')
unique(d$categoricalabundance)

##### Fixing bug 83 imagefilepath issues #####
d$imagefilepath<-gsub("n/a;", "", d$imagefilepath)
d$imagefilepath<-gsub(";;", "", d$imagefilepath)

##### Fixing bug 84 checking positive longitudes above 150.#####  
subset(d, longitude > 150, c(longitude))

##### Fixing bug 85 checking on missing data, verified that these records removed from the database #####
subset(d, catalognumber == "418053")

##### Fixing - need GIS fix for these ##### 
subset(d, catalognumber == "276813", c(latitude, longitude,catalognumber, fishcouncilregion, gisregion))
unique(d$gisregion)
unique(d$fishcouncilregion)
unique(d$gisregion)

subset(d, d$gisregion == "Pacific Islands", c(gisregion, fishcouncilregion))
subset(d, d$gisregion == "Pacific Islands", c(gisregion, fishcouncilregion, locality, gislocality))

subset(d, catalognumber == "110902", c(country, gisregion, fishcouncilregion, locality, gislocality, gisocean, ocean))
subset(d, catalognumber == "110898", c(country, gisregion, fishcouncilregion, locality, gislocality, gisocean, ocean))
subset(d, catalognumber == "110859", c(country, gisregion, fishcouncilregion, locality, gislocality, gisocean, ocean))
subset(d, catalognumber == "110910", c(country, gisregion, fishcouncilregion, locality, gislocality, gisocean, ocean))
subset(d, catalognumber == "110863", c(country, gisregion, fishcouncilregion, locality, gislocality, gisocean, ocean))

##### Fixing bug number  - not in Russia ##### 
subset(d, catalognumber == "109121", c(flag, vessel, dataprovider, country, gisregion, fishcouncilregion, locality, gislocality, gisocean, ocean, latitude, longitude))
# change longitude to negative
d[d$catalognumber == "109121",]$longitude <- -168.04425

##### Fixing bug   #####
subset(d, catalognumber == "247755", c(country, gisregion, fishcouncilregion, locality, gislocality, gisocean, ocean))
#Flagging because of geographic issues
d[d$catalognumber == "247755",]$flag <- 1
class(d$flag)
d[d$catalognumber == "247755",]$flagreason <- "problems with latitude and longitude"

##### Fixing bug - checking if on land and flagging. 
subset(d, catalognumber == "348", c(flag, flagreason))
subset(d, catalognumber == "168", c(flag, flagreason))
subset(d, catalognumber == "12611", c(flag, flagreason))
subset(d, catalognumber == "9913", c(flag, flagreason))
subset(d, catalognumber == "2640", c(flag, flagreason, gisdepth))

subset(d, catalognumber == "9367", c(flag, flagreason, gisdepth))
d[d$catalognumber =="9367", ]$flag <- 1
d[d$catalognumber =="9367", ]$flagreason <- "possibly intersects land"

subset(d, catalognumber == "4084", c(flag, flagreason, gisdepth))
d[d$catalognumber =="4084", ]$flag <- 1
d[d$catalognumber =="4084", ]$flagreason <- "possibly intersects land"

subset(d, catalognumber == "5401", c(flag, flagreason, gisdepth))
subset(d, catalognumber == "5402", c(flag, flagreason, gisdepth))

subset(d, catalognumber == "10240", c(flag, flagreason, gisdepth))
d[d$catalognumber =="10240", ]$flag <- 1
d[d$catalognumber =="10240", ]$flagreason <- "possibly intersects land"

subset(d, catalognumber == "1823", c(flag, flagreason, gisdepth))
subset(d, catalognumber == "6855", c(flag, flagreason, gisdepth))
subset(d, catalognumber == "2703", c(flag, flagreason, gisdepth))
subset(d, catalognumber == "168", c(flag, flagreason, gisdepth))
subset(d, catalognumber == "420", c(flag, flagreason, gisdepth))

subset(d, catalognumber == "419", c(flag, flagreason, gisdepth))
d[d$catalognumber =="419", ]$flag <- 1
d[d$catalognumber =="419", ]$flagreason <- "possibly intersects land"

subset(d, catalognumber == "663", c(flag, flagreason, gisdepth))
subset(d, catalognumber == "3242", c(flag, flagreason, gisdepth))

subset(d, catalognumber == "121", c(flag, flagreason, gisdepth))
d[d$catalognumber =="121", ]$flag <- 1
d[d$catalognumber =="121", ]$flagreason <- "possibly intersects land"

subset(d, catalognumber == "2926", c(flag, flagreason, gisdepth))
subset(d, catalognumber == "9192", c(flag, flagreason, gisdepth))
subset(d, catalognumber == "5770", c(flag, flagreason, gisdepth))
subset(d, catalognumber == "6819", c(flag, flagreason, gisdepth))
subset(d, catalognumber == "10337", c(flag, flagreason, gisdepth))
subset(d, catalognumber == "10361", c(flag, flagreason, gisdepth))
subset(d, catalognumber == "10360", c(flag, flagreason, gisdepth))
subset(d, catalognumber == "10359", c(flag, flagreason, gisdepth))
subset(d, catalognumber == "10357", c(flag, flagreason, gisdepth))
subset(d, catalognumber == "68", c(flag, flagreason, gisdepth))

##### Fix bug  #####
subset(d, catalognumber == "12648", c(flag, flagreason, latitude, longitude, locationcomment, gisdepth))
#change had already been made 
#changes to LocationComment for same
d[d$catalognumber =="12648", ]$locationcomment <- "changed latitude from 32.7811 to 31.7811 per Tom Hourigan comment, error in original"

##### Fix bug  ##### 
subset(d, catalognumber == "7793", c(flag, flagreason, gisdepth, vernacularname))
subset(d, catalognumber == "7794", c(flag, flagreason, gisdepth, vernacularname))
subset(d, catalognumber == "8723", c(flag, flagreason, gisdepth, vernacularname))
#changes requested were verified, change had already been made per Tom Hourigan

##### Fix bug 69 ##### 
subset(d, catalognumber == "10159", c(flag, flagreason, gisdepth, vernacularname))

##### Fix bug 37 ##### 
subset(d, catalognumber == "1850", c(flag, flagreason, depthinmeters, gisdepth, vernacularname))
d[d$catalognumber =="1850", ]$flagreason <- "location does not match recorded depth"

##### Fix bug  #####
subset(d, catalognumber == "11622", c(flag, flagreason, gisdepth, vernacularname))

##### Fix bug  #####
subset(d, catalognumber == "2560", c(flag, flagreason, gisdepth, vernacularname))

##### Fix bug  ##### 
subset(d, catalognumber == "2885", c(flag, flagreason, gisdepth, vernacularname))

##### Fix bug  #####
subset(d, catalognumber == "7977", c(flag, flagreason, gisdepth, vernacularname))

##### Fix bug  #####
subset(d, catalognumber == "2703", c(flag, flagreason, gisdepth, vernacularname))
d[d$catalognumber =="2703", ]$flagreason <- "intersects land, missing depth"

##### Fix bug  #####
subset(d, catalognumber == "7966", c(flag, flagreason, gisdepth, vernacularname))
d[d$catalognumber =="7966", ]$flag <- 1
d[d$catalognumber =="7966", ]$flagreason <- "location and depth do not match Alvin dives"

subset(d, catalognumber == "6479", c(flag, flagreason, gisdepth, vernacularname))
d[d$catalognumber =="6479", ]$flag <- 1
d[d$catalognumber =="6479", ]$flagreason <- "location and depth do not match Alvin dives"

##### Fix bug  ##### 
subset(d, catalognumber == "9465", c(flag, flagreason, gisdepth, vernacularname))
d[d$catalognumber =="9465", ]$flag <- 1
d[d$catalognumber =="9465", ]$flagreason <- "too shallow"

subset(d, catalognumber == "9467", c(flag, flagreason, gisdepth, vernacularname))
d[d$catalognumber =="9467", ]$flag <- 1
d[d$catalognumber =="9467", ]$flagreason <- "too shallow"

d[d$catalognumber =="9468", ]$flag <- 1
d[d$catalognumber =="9468", ]$flagreason <- "too shallow"

d[d$catalognumber =="9913", ]$flag <- 1
d[d$catalognumber =="9913", ]$flagreason <- "too shallow"

d[d$catalognumber =="9914", ]$flag <- 1
d[d$catalognumber =="9914", ]$flagreason <- "too shallow"

d[d$catalognumber =="9812", ]$flag <- 1
d[d$catalognumber =="9812", ]$flagreason <- "too shallow"

##### Fix bug  #####
d[d$catalognumber =="4338", ]$flag <- 1
d[d$catalognumber =="4338", ]$flagreason <- "wrong locality or wrong lat/lan, not in Tortugas"

##### bug 71 ##### 
subset(d, catalognumber == "10377", c(flag, flagreason, gisdepth, vernacularname))

##### bug 73 #####
subset(d, catalognumber == "2915", c(flag, flagreason, gisdepth, vernacularname))
subset(d, catalognumber == "4597", c(flag, flagreason, gisdepth, vernacularname))
subset(d, catalognumber == "11401", c(flag, flagreason, gisdepth, vernacularname))
subset(d, catalognumber == "11470", c(flag, flagreason, gisdepth, vernacularname))

##### bug 74 - Oculina diffusa
subset(d, scientificname == "Oculina diffusa", c(flag, flagreason, gisdepth, vernacularname))

##### bug 75 - Madracis auretenra
subset(d, scientificname == "Madracis auretenra", c(flag, flagreason, gisdepth, vernacularname))

##### bug 76 - Verified flag
subset(d, scientificname == "Madracis decactis", c(flag, flagreason, gisdepth, vernacularname))

##### bug 77 - Verified flag
subset(d, scientificname == "Madracis formosa", c(flag, flagreason, gisdepth, vernacularname))

##### bug 78 - Verified flag
subset(d, scientificname == "Madracis pharensis luciphila", c(flag, flagreason, gisdepth, vernacularname))

##### bug 79 - verified flag
subset(d, catalognumber == "116199", c(flag, flagreason, locality, gislocality, vernacularname))

##### answering Aaron's (Pew) question.  
geo<-subset(d, as.numeric(latitude) > 29 & 
              as.numeric(latitude) < 34 & 
              as.numeric(longitude) < -77 &
              as.numeric(longitude) > -81, 
            c(latitude, longitude, dataprovider, samplingprotocol, vessel, observationdate))
write.csv(geo, "geo.csv")

##### subset to get rid of objectid
d<-d[,2:103]
names(d)

#writing out tab delimited version of db
write.table(d,"DSCRTP_NatDB_20140804-1.txt", row.names = F, quote = F, sep = "\t")

##### ____________NEWDATA: 20140808-1 ##### 

# Starting from  version 20140804-1 going to version 20140808-1

##### bug 72 - flagged #####
subset(d, catalognumber == "114013", c(flag, flagreason, gisdepth, vernacularname))
d[d$catalognumber =="114013", ]$flag <- 1
d[d$catalognumber =="114013", ]$flagreason <- "latitude appears to be incorrect"

##### bug 3 - Stylatula elongata (resolved) ##### 
subset(d, scientificname == "Stylatula elongata", c(flag, flagreason, minimumdepthinmeters, gisdepth, vernacularname))
#sent this to peter for analysis 
write.csv(subset(d, scientificname == "Stylatula elongata" ,
                 c(flag, flagreason, catalognumber,dataprovider,observationdate,vessel, scientificname, minimumdepthinmeters, gisdepth))
          , "Stylatula_elongata_subset_20140804-1.csv")

#flagging records 
d[d$scientificname == "Stylatula elongata",]$flag <- 1  
d[d$scientificname == "Stylatula elongata",]$flagreason <- "all Stylatula elongata flagged - shallow water scecies"

##### bug 159 -  OBIS release fields review #####
# published new version of schema with OBIS indications. 
# see email chain of review from 20140806.  

##### bug 160 null field for worms ID should be NA (resolved) ##### 
#make all -999 be NA.  
d[d$wormsid == "-999", ]$wormsid <- NA
table(d$wormsid, useNA = "always")

##### looking at depthmethod for Tom's review ##### 
write.table(table(d$depthmethod, useNA = "always"), "depthmethod.txt")
# create bug for standardizing depthmethod to the valid vocabulary

##### bug 1 - schema changes ##### 
names(d)

##### Read in the correct field order from schema, call it "fo" (field order) #####
fo <- read.csv("fo_20140806-1.csv", header = T)
#strip off the names of fo as a vector
fo <- as.vector(names(fo))
#transform fo into class character
fo <- c(as.character(fo))

##### make field order match by replacing uppercase to lowercase.  ##### 
fo2<-tolower(fo)
fo2 
fofile<-names(d)

##### reconcile matching issues ##### 
##### trying to change the columnnames the file to match those in the schema.   
setdiff(fofile, fo2)
setdiff(fo2, fofile)
colnames(d)[75] <- "samplingequipment"
names(d)

d <-rename(d, c("taxonphylum" = "phylum"))
d <-rename(d, c("taxonclass" = "class"))
d <-rename(d, c("taxonsubclass"="subclass"))
d <-rename(d, c("taxonorder"="order"))
d <-rename(d, c("taxonsuborder"="suborder"))
d <-rename(d, c("taxonfamily"="family"))
d <-rename(d, c("taxonsubfamily"="subfamily"))
d <-rename(d, c("taxongenus"="genus"))
d <-rename(d, c("taxonsubgenus"="subgenus"))
d <-rename(d, c("taxonspecies"="species"))
d <-rename(d, c("taxonsubspecies"="subspecies"))
d <-rename(d, c("sizecategory"="size"))

##### test for differences in column names on db and those in the shcema (lowercased) ###### 
setdiff (fo2, names(d))
setdiff (names(d), fo2)
fo2
names(d)

##### stopped here and saved image #####
save.image("H:/rworking/20140731-1/.RData")

##### Assign new field order based on order in the database schema  #####
d <- d[,c(fo2)]

##### bug 163 - fixing a problem with data provider ##### 
d[d$dataprovider == "Texas A&amp;M University",]$dataprovider <- "key"
table(d$dataprovider)
d$dataprovider <- recode(d$dataprovider, '"key" = "Texas AM University"')
table(d$dataprovider)

###### saving image ######
save.image("H:/rworking/20140731-1/.RData")

##### bug 161 - fix Ocean ##### 
unique(d$ocean)
write.table(table(d$ocean, useNA = "always"), "ocean_table.txt")
write.csv(subset(d, is.na(ocean) == T, c(flag, flagreason, dataprovider, latitude, longitude)), "noocean.csv")

# flag additional land intersections by using the ocean variable which was GIS based
#IHO seas layer. 
d[is.na(d$ocean) == T,]$flag <- 1
d[is.na(d$ocean) == T,]$flagreason <- "possibly intersects land"

#exploring reordering tables as a class
tab<-table(d$ocean, useNA = "always")
class(tab)
names(tab)
tab2<-c(names(tab))
class(tab)
class(tab2)

#Recoding the ocean variable to get rid of the finer divisions (seas)
d$ocean <-recode(d$ocean, '"Beaufort Sea" = "Arctic Ocean"')
d$ocean <-recode(d$ocean, '"Caribbean Sea" = "North Atlantic"')
d$ocean <-recode(d$ocean, '"Davis Strait" = "Arctic Ocean"')
d$ocean <-recode(d$ocean, '"Gulf of California" = "North Pacific"')
d$ocean <-recode(d$ocean, '"Labrador Sea" = "Arctic Ocean"')
d$ocean <-recode(d$ocean, '"The Coastal Waters of Southeast Alaska and British Columbia" = "North Pacific"')
d$ocean <-recode(d$ocean, '"Bay of Fundy" = "North Atlantic"')
d$ocean <-recode(d$ocean, '"Bering Sea" = "North Pacific"')
d$ocean <-recode(d$ocean, '"Gulf of Alaska" = "North Pacific"')
d$ocean <-recode(d$ocean, '"Chukchi Sea" = "Arctic Ocean"')
d$ocean <-recode(d$ocean, '"Gulf of Mexico" = "North Atlantic"')
d$ocean <-recode(d$ocean, '"North Atlantic Ocean" = "North Atlantic"')
d$ocean <-recode(d$ocean, '"South Pacific Ocean" = "South Pacific"')
d$ocean <-recode(d$ocean, '"North Pacific Ocean" = "North Pacific"')
table(d$ocean)

##### bug 19 - fixing entries in vernacularnamecategory (resolved) ######
# assigning NA to all entries with "Flag-do not release..."
d$vernacularnamecategory[grepl("Flag-", d$vernacularnamecategory)] <- NA
#checking
#d[grepl("Flag-do not", d$flagreason), c("flag", "flagreason", "vernacularnamecategory")]

###### saving image ######
save.image("H:/rworking/20140731-1/.RData")

###### bug 20 - MBARI Records from 8/24-26/2002, 8/6-11/2009 ######
#Country/Region should be "Canada/BC," not "US/West Coast" 
#table(d$dataprovider)

#view the dates of interest  
table(d[d$dataprovider == "Monterey Bay Aquarium Research Institute" & 
         grepl("2002-08-24", d$observationdate)|
          grepl("2002-08-25", d$observationdate)|
          grepl("2002-08-26", d$observationdate), c("observationdate", "country")])

#make the correct assignment 

d$country[d$dataprovider == "Monterey Bay Aquarium Research Institute" & 
    grepl("2002-08-24", d$observationdate)|
    grepl("2002-08-25", d$observationdate)|
    grepl("2002-08-26", d$observationdate)] <- "Canada/BC"

#Check 
# > table(d[d$dataprovider == "Monterey Bay Aquarium Research Institute" & 
#             +          grepl("2002-08-24", d$observationdate)|
#             +           grepl("2002-08-25", d$observationdate)|
#             +           grepl("2002-08-26", d$observationdate), c("observationdate", "country")])
# country
# observationdate Canada/BC
# 2002-08-24       192
# 2002-08-25        95
# 2002-08-26       119

#view the dates of interest  
table(d[d$dataprovider == "Monterey Bay Aquarium Research Institute" & 
          grepl("2009-08-06", d$observationdate)|
          grepl("2009-08-07", d$observationdate)|
          grepl("2009-08-08", d$observationdate)|
          grepl("2009-08-09", d$observationdate)|
          grepl("2009-08-10", d$observationdate)|
          grepl("2009-08-11", d$observationdate), c("observationdate", "country")])

#make the correct assignment 

d$country[d$dataprovider == "Monterey Bay Aquarium Research Institute" & 
            grepl("2009-08-06", d$observationdate)|
            grepl("2009-08-07", d$observationdate)|
            grepl("2009-08-08", d$observationdate)|
            grepl("2009-08-09", d$observationdate)|
            grepl("2009-08-10", d$observationdate)|
            grepl("2009-08-11", d$observationdate)] <- "Canada/BC"

#Check 
table(d[d$dataprovider == "Monterey Bay Aquarium Research Institute" & 
          grepl("2009-08-06", d$observationdate)|
          grepl("2009-08-07", d$observationdate)|
          grepl("2009-08-08", d$observationdate)|
          grepl("2009-08-09", d$observationdate)|
          grepl("2009-08-10", d$observationdate)|
          grepl("2009-08-11", d$observationdate), c("observationdate", "country")])

###### saving image ######
save.image("H:/rworking/20140731-1/.RData")

##### bug 21 - AFSC Records ######
#5/10-11/89, 8/27/1995, 8/17/2001 among others - Country/Region should be "Canada/BC," not "US/West Coast"
# make assignments the same way as in bug 20 above.  check out the notes and additional code
#from that session
table(d$dataprovider)
d$country[d$dataprovider == "NOAA, Alaska Fisheries Science Center" & 
            grepl("1989-05-10", d$observationdate)|
            grepl("1989-05-11", d$observationdate)|
            grepl("1995-08-27", d$observationdate)|
            grepl("2001-08-17", d$observationdate)] <- "Canada/BC"

#check results
table(d$country[d$dataprovider == "NOAA, Alaska Fisheries Science Center" & 
            grepl("1989-05-10", d$observationdate)|
            grepl("1989-05-11", d$observationdate)|
            grepl("1995-08-27", d$observationdate)|
            grepl("2001-08-17", d$observationdate)])

##### bug 22 -  276813 – Lat or Region appears wrong
d$flag[d$catalognumber == "276813"]<-1
d$flagreason[d$catalognumber == "276813"]<-"Lat or region appears wrong"

######bug 23 110902 – not “Gulf of Alaska” – Bowers Ridge  #####
d[d$catalognumber == "110902", c("country", "locality", "ocean", "fishcouncilregion", "latitude", "longitude", "gislocality")]

#change locality to Petrel Spur
d$locality[d$catalognumber == "110902"]<-"Petrel Spur"

##### bug 24 - 110898 – not “Gulf of Alaska” – Aleutian Islands #####
d[d$catalognumber == "110898", c("country", "locality", "ocean", "fishcouncilregion", "latitude", "longitude", "gislocality")]

##### bug 26 - 110910 – not “Gulf of Alaska” or should be negative Lon.  #####
d[d$catalognumber == "110910", c("country", "locality", "ocean", "fishcouncilregion", "latitude", "longitude", "gislocality")]

###### saving image ######
save.image("H:/rworking/20140731-1/.RData")

##### Writing DSCRTP_NatDB_20140808-1.txt"
write.table(d,"DSCRTP_NatDB_20140808-1.txt", row.names = F, quote = F, sep = "\t")

######Troubleshooting problem with field order and naming.  
d <-rename(d, c("phylum" = "taxonphylum"))
d <-rename(d, c("class" = "taxonclass"))
d <-rename(d, c("subclass"="taxonsubclass"))
d <-rename(d, c("order"="taxonorder"))
d <-rename(d, c("suborder"="taxonsuborder"))
d <-rename(d, c("family"="taxonfamily"))
d <-rename(d, c("subfamily"="taxonsubfamily"))
d <-rename(d, c("genus"="taxongenus"))
d <-rename(d, c("subgenus"="taxonsubgenus"))
d <-rename(d, c("species"="taxonspecies"))
d <-rename(d, c("subspecies"="taxonsubspecies"))
d <-rename(d, c("size"="sizecategory"))
d <-rename(d, c("samplingequipment" = "samplingprotocol"))

# changing field order back to old field order.  

setdiff(fofile, names(d))
d <- d[,c(fofile)]

######write and save new table (date: 20140812 at 11AM)######
write.table(d,"DSCRTP_NatDB_20140808-1_orderchange.txt", row.names = F, quote = F, sep = "\t")
save.image("H:/rworking/20140731-1/.RData")

#checking 
# > setdiff(fofile, fo2)
# [1] "taxonphylum"      "taxonclass"       "taxonsubclass"    "taxonorder"       "taxonsuborder"    "taxonfamily"     
# [7] "taxonsubfamily"   "taxongenus"       "taxonsubgenus"    "taxonspecies"     "taxonsubspecies"  "sizecategory"    
# [13] "samplingprotocol"
# > setdiff(fo2, fofile)
# [1] "phylum"            "class"             "subclass"          "order"             "suborder"          "family"           
# [7] "subfamily"         "genus"             "subgenus"          "species"           "subspecies"        "size"             
# [13] "samplingequipment"
# > setdiff(fofile, names(d))
# character(0)
# > setdiff(names(d), fofile)
# character(0)

##### Experiment for running down bug number 91 - encoding issues for special characters.  #####
write.table(d,"DSCRTP_NatDB_20140808-1_orderchange_encoding.txt", row.names = F, quote = F, sep = "\t", fileEncoding = "UTF-16LE")

table(Encoding(data$DataProvider))
table(Encoding(d$citation))
table(Encoding(d$scientificnameauthorship))
subset(data, grepl("Kölliker", data$ScientificNameAuthorship), c(CatalogNumber, ScientificNameAuthorship))

write.table(subset(data, grepl("Kölliker", data$scientificnameauthorship), c(CatalogNumber, ScientificNameAuthorship)), "encodingtest.txt", row.names = F, quote = F, sep = "\t")

##### #Bug 4 - missing vernacularnamecategory issues #####
# writing out a version of the file for Tom to review
write.csv(subset(d, ,c(catalognumber, scientificname, taxonorder, taxonfamily, taxongenus, taxonspecies, vernacularname, vernacularnamecategory)),"vernacularnamecategory.csv")

#reading in corrected data from Tom Hourigan
th<-read.csv("VernacularNameCategory_subset_NA_from_20140804-1-TH.csv", header = T)

# #Example of replacing data from a partial corrected data fram.  
# original <- data.frame( Name = c("joe","john") , Id = c( 1 , 2) , Value1 = c(1.2,NA), Value2 = c(NA,9.2) )
# replacement <- data.frame( Name = c("john") , Id = 2 , Value1 = 2.2 , value2 = 5.9)
# goal <- data.frame( Name = c("joe","john") , Id = c( 1 , 2) , Value1 = c(1.2,2.2), Value2 = c(NA,5.9) )
# 
# rownames(original) <- original$Id
# rownames(replacement) <- replacement$Id
# original[rownames(replacement), c("Value2", "Value1") ] <- replacement[,c("value2", "Value1")]

# make sure everything is a character string, factors will break this process. 
d <- data.frame(lapply(d, as.character), stringsAsFactors=FALSE)
th <- data.frame(lapply(th, as.character), stringsAsFactors=FALSE)

# create an index variable by setting the catalognumber as the key
rownames(d) <- d$catalognumber
rownames(th) <- th$catalognumber

# this is the key step to assign all of the replacement data back to the original using the new rownames key (which is set as catalognumber)
d[rownames(th), c("scientificname", "taxonorder", "taxonfamily", "taxongenus", "taxonspecies", "vernacularname", "vernacularnamecategory")] <- th[,c("scientificname", "taxonorder", "taxonfamily", "taxongenus", "taxonspecies", "vernacularname", "vernacularnamecategory")]          

# checking
x<-417604
subset(d, catalognumber == x, c(flag, catalognumber, dataprovider, taxonorder, scientificname, vernacularname, vernacularnamecategory))

# setting flags and flag reasons per Tom's changes
d[d$catalognumber == "417186",]$flag <- 1
d[d$catalognumber == "417186",]$flagreason <- "Delete per Tom Hourigan 20140813"

#write and save new table for Tom's review (date: 20140813 at 4PM)
write.table(d,"DSCRTP_NatDB_20140813-1.txt", row.names = F, quote = F, sep = "\t")
save.image("H:/rworking/20140731-1/.RData")

###### Distribution to John Manderson of NOAA ##### 
# Picking only unflagged records
dflag<-subset(d, flag == 0, )
# Write the table.  
write.table(dflag,"DSCRTP_NatDB_20140813-1_unflagged.txt", row.names = F, quote = F, sep = "\t")
save.image("H:/rworking/20140731-1/.RData")

##### Distribution to Laurie Bauer, Marine Ecologist CSS-Dynamac
# Write the table.

dflag_geo<-subset(dflag, as.numeric(latitude) > 15 & 
                    as.numeric(latitude) < 26 & 
                    as.numeric(longitude) < -151 &
                    as.numeric(longitude) > -164, )

write.table(dflag_geo,"DSCRTP_NatDB_20140813-1_unflagged_geosubset.txt", row.names = F, quote = F, sep = "\t")
save.image("H:/rworking/20140731-1/.RData")

##### # Bug 164:exploring issue with missing fishcouncilregion "South-Atlantic" ######
names(d)
unique(d$fishcouncilregion)
length(subset(d,gisregion == "South-Atlantic", c(gisregion))$gisregion)
subset(d,gisregion == "South-Atlantic", c(gisregion,fishcouncilregion))

#making assignments of South-Atlantic based on gisregion
d[d$gisregion == "South-Atlantic" 
  & is.na(d$fishcouncilregion) == T
  & is.na(d$gisregion) == F, c("fishcouncilregion", "gisregion")]$fishcouncilregion <- "South-Atlantic"



##### #write and save new table. his table (date: 20140929 at 4PM) ###### 
write.table(d,"DSCRTP_NatDB_20140929-1.txt", row.names = F, quote = F, sep = "\t")

##### _____ OUTPUT: "DSCRTP_NatDB_20140929-1.txt" ######

##### ____________NEWDATA: from Tom review #####

##### read in Tom's dataset #####
tdata<-read.delim("DSCRTP_NatDB_20140813-2.txt", header = TRUE, sep = "\t", quote = "\"",
           dec = ".", fill = TRUE, comment.char = "")
class(tdata)

tdata[tdata$gisregion == "South-Atlantic" 
  & is.na(tdata$FishCouncilRegion) == T
  & is.na(tdata$gisregion) == F, c("FishCouncilRegion", "gisregion")]#$FishCouncilRegion <- "South-Atlantic"

###### _____________ Frequency table and ordered bar chart module #####
z="StationID"
x<-which(colnames(tdata)==z)

xout <- as.data.frame(table(tdata[x], useNA="always"))
xout <- transform(xout, relative = prop.table(Freq))
xout <-xout[order(xout$Freq),] 
head(xout[order(-xout$Freq), ], n=100)

par(mar=c(10,20,5,10))
y <- barplot(unique(sort(table(tdata[x], useNA="always"))), 
             las=2, cex.names=.5, horiz=TRUE, main=z)

##### ____________ fixing more bugs ######

##### Bug 167 - missing Phylum ##### 
tdata[is.na(tdata$Phylum) == T, c("CatalogNumber", "Phylum", "ScientificName", "ScientificNameAuthorship")]$Phylum <- "Cnidaria"

#checking
tdata[tdata$CatalogNumber == "417461" |
        tdata$CatalogNumber == "417462" |
        tdata$CatalogNumber == "417463", 
      c("CatalogNumber", "Phylum", "ScientificName", "ScientificNameAuthorship")] 

##### Bug 171 - problem with NavType##### 
#Problems with NavType - it has some numbers that look like Latitudes  171
#Assigning NA to all problem records
tdata[grepl("3", tdata$NavType),]$NavType <- NA

# #checking
# tdata[grepl("3", tdata$NavType), c("NavType", "ScientificName")]

##### Bug 173 Many records have no sampleID ##### 
# #checking
# Var1  Freq     relative
# 82826     <NA> 43270 0.1954381210
# 54511 T0628-05  2123 0.0095889792
# 10543 2.01E+11  1908 0.0086178862

tdata[is.na(tdata$SampleID) == T, c("DataProvider", "SurveyID", "SampleID", "TrackingID")]
table(tdata[is.na(tdata$SampleID) == T,]$Flag)
length(tdata[is.na(tdata$SampleID) == T,]$Flag)
tdata[is.na(tdata$SampleID) == T,]$Flag <- "1"
tdata$FlagReason <- as.character(tdata$FlagReason)
tdata[is.na(tdata$SampleID) == T & is.na(tdata$FlagReason) == T ,]$FlagReason <- "No SampleID"
table(tdata[is.na(tdata$SampleID) == T,]$FlagReason)

##### Bug 174 working on date issue #####
#Checking
setdiff(as.character(tdata$CatalogNumber), as.character(d$catalognumber))
setdiff(as.character(d$catalognumber), as.character(tdata$CatalogNumber))
head(d$catalognumber)
tail(d$catalognumber)
head(tdata$CatalogNumber)
tail(tdata$CatalogNumber)
tdata$CatalogNumber <- as.character(tdata$CatalogNumber)
class(tdata$CatalogNumber)
head(d$observationdate)
head(tdata$ObservationDate)
head(d$modified)
head(tdata$Modified)
length(d$observationdate)
length(tdata$ObservationDate)

#fixing
tdata$ObservationDate <- d$observationdate
tdata$Modified <- d$modified
tdata$EntryDate <- d$entrydate

##### Flag west coast groundfish trawl records as do not release   Bug 182 ##### 
unique(tdata$DataContact)
tdata[tdata$DataContact == "Curt Whitmire" & 
        is.na(tdata$DataContact) == F,]
unique(tdata$Repository)
unique(tdata[grepl("Groundfish", tdata$Citation),]$Citation)

tdata[grepl("Groundfish", tdata$Citation),]$Flag <- "1"
tdata[grepl("Groundfish", tdata$Citation),]$FlagReason <- "Embargo these records per Curt Whitmire"

##### Flag sampleID == 0  183#####
#subset(tdata, individualcount == 1500, c(dataprovider, sampleid, individualcount))
tdata[tdata$SampleID == "0" & tdata$DataProvider == "Texas AM University", ]$FlagReason

##### Retooling the column names. 
#strip off the oldnames
oldnames<-as.vector(names(tdata))
oldnames<-c(as.character(oldnames))
write.csv(oldnames,"oldnames.csv")

#Read in field order for OBIS release from field definitions sheet order 
fo<-read.csv("obisorder.csv")
fo <- as.vector(names(fo))
fo <- c(as.character(fo))

#read in matching CamelText field names (used for changing)
capsnames<-read.csv("capsnames.csv")
capsnames<-as.vector(names(capsnames))
capsnames<-c(as.character(capsnames))

setdiff(fo,capsnames)

#change all of the columnnames the capsnames
colnames(tdata)<-c(capsnames)

##### Apply the OBIS field order based on schema and choose only unflagged records #####
obis <- tdata[tdata$Flag == "0",c(fo)]

#### Write out the latest version of the full database ##### 
colnames(tdata)<-c(oldnames)
write.table(tdata,"DSCRTP_NatDB_20141002-1.txt", row.names = F, quote = F, sep = "\t")

##### Write out a version of the file for OBIS ##### 
names(obis)
length(obis$CatalogNumber)
write.table(obis,"DSCRTP_NatDB_20141002-1_OBIS_subset.txt", row.names = F, quote = F, sep = "\t")

###### Split up obis by latitude and publish. #####
#create a test subsets
geosub <- subset(obis, as.numeric(Latitude) > 19  & 
               as.numeric(Latitude) <  32 & 
              as.numeric(Longitude) < -78 &
              as.numeric(Longitude) > -99, )
length(geosub$CatalogNumber)

geosub <- subset(geosub,,c(Latitude, Longitude, VernacularNameCategory))
                 
Writing data for release
write.table(geosub,"maptest.txt", row.names = F, quote = F, sep = "\t")

##### getting out racebace subset for QA purposes. #####  
racebase <-subset(tdata, grepl("RACEBASE", tdata$Citation,),)
length(racebase$DataContact)
unique(racebase$DataProvider)
unique(racebase$Citation)
write.table(racebase,"DSCRTP_NatDB_20141002-1_RACEBASE_subset.txt", row.names = F, quote = F, sep = "\t")

##### doing some QA on the Falkor data ##### 
falkor <-subset(tdata, grepl("Falkor", tdata$Vessel,),)
length(falkor$DataContact)
unique(falkor$DataProvider)
unique(falkor$ObservationDate)
unique(falkor$Latitude)
write.table(falkor,"maptest.txt", row.names = F, quote = F, sep = "\t")
rm(falkor)
rm(racebase)
rm(geosub)

##### working on Export for Ed Bowlby QA ##### 
bowlby <-subset(tdata, grepl("Olympic", tdata$DataProvider,),)
unique(bowlby$DataProvider)
unique(bowlby$DataContact)
unique(bowlby$ObservationDate)
write.table(bowlby,"DSCRTP_NatDB_20141002-1_Ed_Bowlby_subset.txt", row.names = F, quote = F, sep = "\t")

##### ___________ NEW DATA: Observer Program data (Merideth Everett) ##### 

##### Bringing new data Merideth Everett 2014-12-01  ##### 
options(stringsAsFactors=FALSE)
op<-read.table("Sea_Pens_DSC_DB-Updated_MEverett_ObserverProgram_20141124_rmcguinn.txt", sep = "\t", header = T)

##### Changing column names in imported data to make sure they match the main dataset  #####
#checking which names don't match
setdiff(names(op), names(d2))
#[1] "IdentifiedBy.Verified" "DecimalLatitude"       "DecimalLongitude"     

#finding and changing the column names in the new dataset that do not match.  
z = "IdentifiedBy.Verified"
x <-which(colnames(op) == z)
colnames(op)[x] <- "IdentifiedBy"

z = "DecimalLatitude"
x <-which(colnames(op) == z)
colnames(op)[x] <- "Latitude"

z = "DecimalLongitude"
x <-which(colnames(op) == z)
colnames(op)[x] <- "Longitude"

# #Verifing if the procedure worked.  
# setdiff(names(op), names(tdata))
# character(0)

###### Making all variables in new dataset be character strings  ###### 
op <- data.frame(lapply(op, as.character), stringsAsFactors=FALSE)

##### Checking the values #####
names(op)
length(op$ScientificName)
unique(op$TaxonRank)
unique(op$WormsID)
unique(op$IdentifiedBy)
unique(op$IdentificationQualifier)
unique(op$GenBankID) #no genbankID?
table(op$ObservationDate, useNA = c("always"))
unique(op$ObservationTime)
unique(op$Latitude)
unique(op$Longitude)
table(op$DepthInMeters, useNA = c("always")) # 13 NA values
names(op)
table(op$SurveyID, useNA = c("always"))
table(op$EventID, useNA = c("always"))
table(op$SampleID, useNA = c("always"))
table(op$ObservationDate, useNA = c("always"))
table(op$ObservationTime, useNA = c("always"))
table(op$Latitude, useNA = c("always")) # 13 records have no lat or long
table(op$Longitude, useNA = c("always"))
fix(op)

##### Filling in where ObservationDate is missing with NA values  ######
op$ObservationDate[op$ObservationDate == ""] <- NA

##### Changing ObservationDate to the right date format  #####

#trimming white space
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
op$ObservationDate <- trim(op$ObservationDate)

#changing ObservationDate to a true date class
op$ObservationDate <- as.Date(op$ObservationDate, "%m/%d/%Y")
table(op$ObservationDate, useNA = c("always"))

##### Fixing ObservationTime  #####
op$ObservationTime<-substr(strptime(op$ObservationTime, "%I:%M:%S %p"),12,22) # use %I to make it work
tail(op$ObservationTime, n=10)
op
#testing a way to get back to GMT.  
# y<-z
# y
# y<-paste(y, "-0800" ,sep = "")
# y
# 
# test<-paste(op$ObservationDate, y, sep = " ")
# test
# test2<-strptime(test, "%Y-%m-%d %I:%M:%S%z")
# test2
# tail(test, n=10)
# tail(test2, n=10)
# 
# y<-substr(strptime(y, "%I:%M:%S%z"),12,100) 
# #y<-strptime(y, "%I:%M:%S%z")
# y
# 
# tail(op$ObservationTime, n=10)
# tail(y, n=10)
# 
# class(y)
# as.Date(y)

##### Assign AphiaID and Species List ##### 
Load library, process WSDL and prepare R SOAP functions
install.packages("XML", dependencies = TRUE)
download.file("http://www.omegahat.org/Prerelease/XMLSchema_0.8-0.tar.gz", "XMLSchema")
install.packages("XMLSchema", type="source", repos = NULL)
download.file("http://www.omegahat.org/Prerelease/SSOAP_0.91-0.tar.gz", "SSOAP")
install.packages("SSOAP", type="source", repos = NULL)
library(SSOAP)
w = processWSDL("http://www.marinespecies.org/aphia.php?p=soap&wsdl=1")
iface = genSOAPClientInterface(, w)

# Create your specieslist
MySpecies<-c(unique(op$ScientificName))
MySpecies<-data.frame(MySpecies)
MySpecList<-data.frame(unique(MySpecies))
MySpecList

# Get original AphiaID's for specieslist 
AphiaMatch <- function(x) { 
  result<-NULL
  for (i in 1:length(x)) {
    AphiaRecord <- iface@functions$getAphiaID(x[i],1,('http://www.marinespecies.org/aphia.php?p=soap')) 
    result<-c(result, AphiaRecord)
  }
  return(result)
}
MySpecList$OrigTaxID<-AphiaMatch(MySpecList$MySpecies)
MySpecList

# Get accepted synonym AphiaID's for specieslist 

SynResolv <- function(x) { 
  result<-NULL
  for (i in 1:length(x)) {
    AphiaRecord <- iface@functions$getAphiaRecordByID(x[i],('http://www.marinespecies.org/aphia.php?p=soap')) 
    result<-c(result, slot(AphiaRecord, "valid_AphiaID"))
  }
  return(result)
}
MySpecList$AccTaxID<-SynResolv(MySpecList$OrigTaxID)
MySpecList

# Add full record information (classification, ranking, authority,...)

getFullRecord <- function(x) { 
  result<-NULL
  for (i in 1:length(x)) {
    AphiaRecord <- iface@functions$getAphiaRecordByID(x[i],('http://www.marinespecies.org/aphia.php?p=soap')) 
    slotnames <- slotNames(AphiaRecord)
    slotlist <- data.frame(rbind(1:length(slotnames)))
    names(slotlist) <- slotnames
    for(y in slotnames) {
      #R cannot handle a slot name "class"
      if (y == "CLASS") {slotlist[1,y] <- '(empty)'}
      else {slotlist[1, y] <- slot(AphiaRecord,  y)}
    }
    result<-rbind(result, slotlist)
  }
  return(result)
}
AphiaRecords<-getFullRecord(MySpecList$AccTaxID)
names(AphiaRecords)

# Assign full information back to species list
# MySpecList<-cbind(MySpecList, AphiaRecords)
# MySpecList

#join Worms records back to original data
join <- merge(x = op, y = AphiaRecords, by.x = "ScientificName", by.y = "scientificname", all.x = TRUE)
join <- join %>% 
  mutate(WormsID = AphiaID, 
         ScientificNameAuthorship = valid_authority, 
         Phylum = phylum,
         Class = CLASS, 
         Order = order,
         Family = family,
         Genus = genus,
         TaxonRank = rank) %>% 
  select((ScientificName:SampleID), 
         ScientificNameAuthorship, Phylum, Class, 
         Order, Family, Genus, TaxonRank, -(AphiaID:modified))
names(join)
table(join$ScientificNameAuthorship)
op<-join

#testing master 
join <- merge(x = op, y = taxmaster, by.x = "ScientificName", by.y = "ScientificName", all.x = T)

length(join$Genus.x)
length(join$Genus.y)
names(join)

##### Flagging ####
names(op)
op$Flag <- "0"
save(op, file = "op.RData")

#checking
op$Flag

#####fixing names ##### 
names(d)
setdiff(names(d), names(op))
setdiff(names(op), names(d))

#####_____Sub Module: getting subclass and suborder infomration from Worms to add to OP #####

##### defining a function to get the full taxonomic listing from World Registry of Marine Species (THIS IS BROKEN) #####
getFullList <- function(x) {
  iface@functions$getAphiaClassificationByID(x,('http://www.marinespecies.org/aphia.php?p=soap')) 
}

##### use lapply to feed an integer vector of AphiaID's to the taxonomic #####
classificationObject<-lapply(FUN = getFullList, MySpecList$AccTaxID)

##### define a function to extract only the information that you need from the "classificationObject" #####
extractList <- function(x){
  a <-c(x@scientificname, x@rank)
  b <-c(x@child@scientificname, x@child@rank)
  c <-c(x@child@child@scientificname, x@child@child@rank)
  d <-c(x@child@child@child@scientificname, x@child@child@child@rank)
  e <-c(x@child@child@child@child@scientificname, x@child@child@child@child@rank)
  f <-c(x@child@child@child@child@child@scientificname, x@child@child@child@child@child@rank)
  g <-c(x@child@child@child@child@child@child@scientificname, x@child@child@child@child@child@child@rank)
  h <-c(x@child@child@child@child@child@child@child@scientificname, x@child@child@child@child@child@child@child@rank)
  i <-c(x@child@child@child@child@child@child@child@child@scientificname, x@child@child@child@child@child@child@child@child@rank)
  j <-c(x@child@child@child@child@child@child@child@child@child@scientificname, x@child@child@child@child@child@child@child@child@child@rank)
  vector<-c(a,b,c,d,e,f,g,h,i,j)
  return(vector)
}

##### feed the classificationObject to the extractList function to actually get just the taxonomic string that you need #####
taxstring <- lapply(FUN = extractList, classificationObject)

##### row bind (with fill) the list of taxonomic character vectors by first making them transposed data frames in turn using lapply #####
df<-rbind.fill(lapply(taxstring, function(X) data.frame(t(X))))
df

#select the columns I need
df<-df %>% 
  select(X1,X3,X5,X7,X9,X11,X13,X15,X17,X19)

##### Creating a names string #####
names(df) <- c("Superdomain","Kingdom","Phylum","Class","Subclass","Order","Suborder","Family","Genus","Species")

##### Go out to manual fixes of df, problem with alignment of row entries with taxonomic level headings ####
fix(df)

##### Add the aphia ID numbers back to to the taxonomic dataframe in preparation for the merge function #####
table(op$ScientificName)
table(op$WormsID)
a<-op$WormsID
names(MySpecList)
b<-MySpecList$AccTaxID
di
df
df$WormsID <- MySpecList$AccTaxID
df
names(df)

##### now selection only the fields from df that I want to merge back to OP ##### 
df <- df %>% 
  select(WormsID, Subclass, Suborder)
df
setdiff(names(df), names(op))

##### merging the new taxonomic inofmation in df with our op table.  #####
op<-merge(op, df, by = "WormsID", all.x = T)
names(op)

##### _____ OUTPUT: "op"
save(op, file = "./OutData/op.RData")

##### ____________ geographic subsetting #####

##### Geographic subsetting to Gulf of Mexico (R code for data distribution to John Reed) ##### 
tdataGOMEX<- subset(tdata, as.numeric(Latitude) > 20 & 
              as.numeric(Latitude) < 35 & 
              as.numeric(Longitude) < -75 &
              as.numeric(Longitude) > -97, )
# Subsetting to only flagged fields = 0
tdataGOMEX <- subset(tdataGOMEX, Flag == 0, )

# Writing data for release
write.table(tdataGOMEX,"DSCRTP_NatDB_20141002-1_GOMEXsubset.txt", row.names = F, quote = F, sep = "\t")

##### __________ NEW DATA: Pacific Sponge Data #####

##### Bringing in Pacific sponge data ##### 
options(stringsAsFactors=FALSE)
sponge<-read.csv("NEPacific_Sponges12-15-2014.csv", header = T, na.strings = "")

#Changing column names
#testing variable name difference between the main data (tdata) and the new sponge data (sponge)
setdiff(names(sponge), names(tdata))
setdiff(names(tdata), names(sponge))

z = "WoRMSID"
x <-which(colnames(sponge) == z)
colnames(sponge)[x] <- "WormsID"

z = "taxonphylum"
x <-which(colnames(sponge) == z)
colnames(sponge)[x] <- "Phylum"

z = "taxonclass"
x <-which(colnames(sponge) == z)
colnames(sponge)[x] <- "Class"

z = "taxonsubclass"
x <-which(colnames(sponge) == z)
colnames(sponge)[x] <- "Subclass"

setdiff(names(sponge), names(tdata))
setdiff(names(tdata), names(sponge))

z = "taxonorder"
x <-which(colnames(sponge) == z)
colnames(sponge)[x] <- "Order"

z = "taxonsuborder"
x <-which(colnames(sponge) == z)
colnames(sponge)[x] <- "Suborder"

setdiff(names(sponge), names(tdata))
setdiff(names(tdata), names(sponge))

z = "taxonfamily"
x <-which(colnames(sponge) == z)
colnames(sponge)[x] <- "Family"

z = "taxonsubfamily"
x <-which(colnames(sponge) == z)
colnames(sponge)[x] <- "Subfamily"

z = "taxonsubfamily"
x <-which(colnames(sponge) == z)
colnames(sponge)[x] <- "Subfamily"

setdiff(names(sponge), names(tdata))
setdiff(names(tdata), names(sponge))

z = "taxongenus"
x <-which(colnames(sponge) == z)
colnames(sponge)[x] <- "Genus"

z = "taxonSubgenus"
x <-which(colnames(sponge) == z)
colnames(sponge)[x] <- "Subgenus"

z = "taxonspecies"
x <-which(colnames(sponge) == z)
colnames(sponge)[x] <- "Species"

z = "taxonsubspecies"
x <-which(colnames(sponge) == z)
colnames(sponge)[x] <- "Subspecies"

z = "GenbankID"
x <-which(colnames(sponge) == z)
colnames(sponge)[x] <- "GenBankID"

z = "Categoricalabundance"
x <-which(colnames(sponge) == z)
colnames(sponge)[x] <- "CategoricalAbundance"

z = "pH"
x <-which(colnames(sponge) == z)
colnames(sponge)[x] <- "Ph"

z = "pHScale"
x <-which(colnames(sponge) == z)
colnames(sponge)[x] <- "PhScale"

z = "pCO2"
x <-which(colnames(sponge) == z)
colnames(sponge)[x] <- "PCO2"

z = "ta"
x <-which(colnames(sponge) == z)
colnames(sponge)[x] <- "TA"

z = "dic"
x <-which(colnames(sponge) == z)
colnames(sponge)[x] <- "DIC"

z = "startlatitude"
x <-which(colnames(sponge) == z)
colnames(sponge)[x] <- "StartLatitude"

z = "dic"
x <-which(colnames(sponge) == z)
colnames(sponge)[x] <- "DIC"

z = "startlongitude"
x <-which(colnames(sponge) == z)
colnames(sponge)[x] <- "StartLongitude"

z = "endlatitude"
x <-which(colnames(sponge) == z)
colnames(sponge)[x] <- "EndLatitude"

z = "endlongitude"
x <-which(colnames(sponge) == z)
colnames(sponge)[x] <- "EndLongitude"

z = "endlatitude"
x <-which(colnames(sponge) == z)
colnames(sponge)[x] <- "EndLatitude"

z = "WebSite"
x <-which(colnames(sponge) == z)
colnames(sponge)[x] <- "Website"

z = "ImageFile"
x <-which(colnames(sponge) == z)
colnames(sponge)[x] <- "ImageFilePath"

z = "observationyear"
x <-which(colnames(sponge) == z)
colnames(sponge)[x] <- "ObservationYear"


setdiff(names(sponge), names(tdata))
setdiff(names(tdata), names(sponge))

##### Make sponge variable names conform to the variable names of tdata #####
names(tdata)
stays <- setdiff(names(tdata), names(sponge))
goes <- setdiff(names(sponge), names(tdata))
#trim leading and trailing white space
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
stays<-trim(stays)
goes<-trim(goes)
#sorting
stays<-sort(stays)
goes<-sort(goes)

goes<-goes[-1]
goes<-goes[-13]

#defining column changes function
col.change <- function(goes, stays){
  z = goes
  x <-which(colnames(sponge) == z)
  colnames(sponge)[x] <<- stays
}

#feed the two lists to the col.change function using mapply
mapply(col.change, goes, stays)

#checking
setdiff(names(tdata), names(sponge))
setdiff(names(sponge), names(tdata))

##### Adding information from Preparation to OccurenceRemarks  #####

sponge$OccurrenceRemarks <- paste(sponge$OccurrenceRemarks, sponge$Preparation, sep = ":")
unique(sponge$OccurrenceRemarks, useNA = "always")

##### cleaning up OccurrenceRemarks after paste operation #####                     
sponge[sponge$OccurrenceRemarks == " NA               :NA",]$OccurrenceRemarks <- NA                    

sponge[sponge$OccurrenceRemarks == "NA:NA" &
         is.na(sponge$OccurrenceRemarks) == F,]$OccurrenceRemarks <- NA

##### Adding CatalogNumber to sponge dataset#####
summary(as.numeric(tdata$CatalogNumber), digits = 20)
tail(tdata$CatalogNumber)

x<-sponge$CatalogNumber
#create a variable that is the length of the NA entries
y<-length(x[is.na(x)])
#Assign an integer series to all of the NA values in CatalogNumber
x[is.na(x)]<-423013:(423013+(y-1))
#checks
x[is.na(x)]
head(x)
tail(x)
#Put the corrected column back in the sponge dataset
sponge$CatalogNumber<-x

#checks
head(sponge$CatalogNumber)
tail(sponge$CatalogNumber)

##### Copying Accession.Number to TrackingID #####
sponge$TrackingID <- sponge$Accession.Number

##### Getting rid of Accession.Number and Preparation from the sponge data #####
sponge <- sponge[,1:102]
names(sponge)

###### ____________ Schema Adherence Modifications to "tdata" #####

##### Bringing in a list of correctly named and ordred variable name from the schema spreadsheet #####
template <- read.csv("FullTemplate.csv", header = F, stringsAsFactors = FALSE)
template <- as.character(template)
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
template <- trim(template)
template

#defining lists of differences in column names beween the schema and the current dataset
stays<-setdiff(template, names(tdata))
goes<-setdiff(names(tdata), template)
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
stays<-trim(stays)
goes<-trim(goes)

#stays and goes must be the same length and must be pairwise matches to work properly in the function and in mapply
stays<-sort(stays)
goes<-sort(goes)

##### defining column changes function for "tdata" #####
col.change <- function(goes, stays){
z = goes
x <-which(colnames(tdata) == z)
colnames(tdata)[x] <<- stays
}

##### Mapply the pairwise variable change lists to tdata using the col.change function within mapply####
mapply(col.change, goes, stays)

#checking
setdiff(template, names(tdata))
setdiff(names(tdata), template)
setdiff(names(sponge), names(tdata))  

##### Make tdata conform to the variable order of the full schema template #####
tdata<-tdata[,c(template)]

#checking
names(tdata)

######______________ Null Assignment Module ###### ######

##### The steps in this code section allow for proper null assignment for numeric and character variables #####

#Important! Set each of the datasets that are going to be joined go through the null assignment module module and call it "a" 
#the steps below must be executed for both of the datasets being added together (in this case
#sponge and tdata). Outputs are tdata2 and sponge2 So steps must be executed twice, once for tdata
#and once for. 

a <- tdata 

# Bringing in the numerical variable list

numeric <- read.csv("numeric.csv", header = F, stringsAsFactors = FALSE)
numeric <- as.character(numeric)
numeric <-trim(numeric)
numeric

# Bringing in the character variable list
character <- read.csv("character.csv", header = F, stringsAsFactors = FALSE)
character <- as.character(character)
character <-trim(character)
character

#make everything a character
a <- data.frame(lapply(a, as.character), stringsAsFactors=FALSE)

# 1 - Defining the trim function 
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
trimall <- function(x){
  dat<-trim(a[,c(x)])
  a[,c(x)] <<- dat
}



# 2 - Trim all white space from beginning and end of "a" data 
lapply(numeric, FUN = trimall)
lapply(character, FUN = trimall)

# 3 - Create -999 for all missing numerical values in "a" data  
NumericNull<-function(x){
  dat <- a[,c(x)]
  a[is.na(dat) == T | dat == "" | dat == "NA", c(x)] <<- -999
}

lapply(numeric, FUN = NumericNull)

# 4 - Create NA for all missing factor and character variables in "a" data 
#define function for creating NA values.
FactorNull<-function(x){
  dat <- a[,c(x)]
  a[is.na(dat) == T | dat == "" | dat == "NA", c(x)] <<- NA
}

lapply(character, FUN = FactorNull)

# 5 - getting the changed set assigned back to a new file.  
#Again, this is done to create tdata2 and sponge2, so steps 1-5 are executed twice.
tdata2 <- a

##### _____OUTPUT: "sponge2" AND "tdata2" RUN TWICE!!! #####

#####____________ Adding tdata and sponge data together #####

##### checking for missing lat and long on outputs from the null assignment module above #####
table(sponge2[is.na(sponge2$Longitude) == T , c("Longitude")], useNA = "always")
table(tdata2[is.na(tdata2$Longitude) == T , c("Longitude")], useNA = "always")
table(sponge2[is.na(sponge2$Latitude) == T , c("Latitude")], useNA = "always")
table(tdata2[is.na(tdata2$Latitude) == T , c("Latitude")], useNA = "always")

##### Checking for missing Depth ######
table(sponge2[is.na(sponge2$MinimumDepthInMeters) == T , c("MinimumDepthInMeters")], useNA = "always")
table(tdata2[is.na(tdata2$MinimumDepthInMeters) == T , c("MinimumDepthInMeters")], useNA = "always")
table(sponge2[is.na(sponge2$MaximumDepthInMeters) == T , c("MaximumDepthInMeters")], useNA = "always")
table(tdata2[is.na(tdata2$MaximumDepthInMeters) == T , c("MaximumDepthInMeters")], useNA = "always")

##### Adding datasets together at the end #####
library(plyr)
d <- rbind.fill(tdata2, sponge2)
fix(d)

###### _____ OUTPUT: "d"#####

##### ____________ FishCouncilRegion Bugfix ##### 

##### Fixing problem with FishCouncilRegion with d ######

d[d$FishCouncilRegion == "South Atlantic/Gulf of Mexico" &
    is.na(d$FishCouncilRegion) == F, ]$FishCouncilRegion <- "Gulf of Mexico"

d[d$FishCouncilRegion == "South Atlantic; Gulf of Mexico" &
    is.na(d$FishCouncilRegion) == F, ]$FishCouncilRegion <- "Gulf of Mexico"

#checks
table(d$FishCouncilRegion, useNA = "always")

##### ____________ Write Table for NCDDC #####

##### Write out the quick release version for NCDDC. Can't Repeat This Command #####
# write.table(d,"DSCRTP_NatDB_20141219-0.txt", row.names = F, quote = F, sep = "\t")

##### ____________ GIS enhancement module ###### 

##### GIS Modules to populate all gis variables #####

##### ...reading in the rgdal library #####
# using the rgdal library
library(rgdal)

##### ...reading in the world seas polygon and the geomorhic spatial data polygons #####
worldseas <- readOGR(".", "World_Seas")
abyss <- readOGR(".", "Abyss")
abyssal_class <- readOGR(".", "Abyssal_Classification")
basins <- readOGR(".", "Basins")
bridges <- readOGR(".", "Bridges")
canyons <- readOGR(".", "Canyons")
escarpments <- readOGR(".", "Escarpments")
fans <- readOGR(".", "Fans")
glacial_troughs <- readOGR(".", "Glacial_troughs")
guyots <- readOGR(".", "Guyots")
hadal <- readOGR(".", "Hadal")
plateaus <- readOGR(".", "Plateaus")
ridges <- readOGR(".", "Ridges")
rift_valleys <- readOGR(".", "Rift_valleys")
rises <- readOGR(".", "Rises")
seamounts <- readOGR(".", "Seamounts")
shelf <- readOGR(".", "Shelf")
shelf_class <- readOGR(".", "Shelf_Classification")
shelf_valleys <- readOGR(".", "Shelf_valleys")
sills <- readOGR(".", "Sills")
slope <- readOGR(".", "Slope")
spreading_ridges <- readOGR(".", "Spreading_ridges")
terraces <- readOGR(".", "Terraces")
trenches <- readOGR(".", "Trenches")
troughs <- readOGR(".", "Troughs")

##### ...reading in FishManagement Council Regions #####
caribbean <- readOGR(".", "Caribbean")
westernpacific <- readOGR(".", "WesternPacific")
midatlantic <- readOGR(".", "MidAtlantic")
newengland <- readOGR(".", "NewEngland")
northpacific <- readOGR(".", "NorthPacific")
pacific <- readOGR(".", "Pacific")
southatlantic <- readOGR(".", "SouthAtlantic")
gulfofmexico <- readOGR(".", "GOMEX")

##### ...make sure the projections all match for the FMC Regions #####
proj4string(spdf)
# "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

proj4string(caribbean)
# "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
caribbean<-spTransform(caribbean, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

proj4string(gulfofmexico)
# "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
gulfofmexico<-spTransform(gulfofmexico, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

proj4string(midatlantic)
# "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
midatlantic<-spTransform(midatlantic, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

proj4string(newengland)
# "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
newengland<-spTransform(newengland, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

proj4string(northpacific) # this one already matches
# "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

proj4string(pacific)
# "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
pacific<-spTransform(pacific, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

proj4string(southatlantic)
# "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
southatlantic<-spTransform(southatlantic, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

proj4string(westernpacific) # this one already matches
# "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

##### ...save all of the newly projected FMC files to save space in RAM #####
save(caribbean, file = "caribbean.RData")
save(gulfofmexico, file = "gulfofmexico.RData")
save(midatlantic, file = "midatlantic.RData")
save(newengland, file = "newengland.RData")
save(northpacific, file = "northpacific.RData")
save(pacific, file = "pacific.RData")
save(southatlantic, file = "southatlantic.RData")
save(westernpacific, file = "westernpacific.RData")

##### ...save images of each of the gis files to save space in RAM#####
save(d, file = "d.RData")
rm(d)
save(data, file = "data.RData")
rm(data)
save(sponge, file = "sponge.RData")
rm(sponge)
save(sponge2, file = "sponge2.RData")
rm(sponge2)
save(tdata, file = "tdata.RData")
rm(tdata)
save(tdata2, file = "tdata2.RData")
rm(tdata2)
save(abyss, file = "abyss.RData")
rm(abyss)
save(abyssal_class, file = "abyssal_class.RData")
rm(abyssal_class)
save(basins, file = "basins.RData")
rm(basins)
save(bridges, file = "bridges.RData")
rm(bridges)
save(canyons, file = "canyons.RData")
rm(canyons)
save(escarpments, file = "escarpments.RData")
rm(escarpments)
save(etopo, file = "etopo.RData")
rm(etopo)
save(fans, file = "fans.RData")
rm(fans)
save(glacial_troughs, file = "glacial_troughs.RData")
rm(glacial_troughs)
save(guyots, file = "guyots.RData")
rm(guyots)
save(hadal, file = "hadal.RData")
rm(hadal)
save(ngia, file = "ngia.RData")
rm(ngia)
save(ridges, file = "ridges.RData")
rm(ridges)
save(rift_valleys, file = "rift_valleys.RData")
rm(rift_valleys)
save(rises, file = "rises.RData")
rm(rises)
save(safmc, file = "safmc.RData")
rm(safmc)
save(seamounts, file = "seamounts.RData")
rm(seamounts)
save(shelf, file = "shelf.RData")
rm(shelf)
save(shelf_class, file = "shelf_class.RData")
rm(shelf_class)
save(shelf_valleys, file = "shelf_valleys.RData")
rm(shelf_valleys)
save(sills, file = "sills.RData")
rm(sills)
save(slope, file = "slope.RData")
rm(slope)
save(spdf, file = "spdf.RData")
rm(spdf)
save(spreading_ridges, file = "spreading_ridges.RData")
rm(spreading_ridges)
save(terraces, file = "terraces.RData")
rm(terraces)
save(trenches, file = "trenches.RData")
rm(trenches)
save(troughs, file = "troughs.RData")
rm(troughs)
save(worldseas, file = "worldseas.RData")
rm(worldseas)
save(zones, file = "zones.RData")
rm(zones)

##### ...example of reprojecting polygon to match world seas #####
proj4string(worldseas)
safmc<-spTransform(safmc, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
proj4string(safmc)

##### Spatial transformation of the north pacific and western pacific #####
spdft <- spTransform(spdf, CRS("+proj=eqc +lat_ts=0 +lat_0=0 
+lon_0=-65 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")) 

northpacifict <- spTransform(northpacific, CRS("+proj=eqc +lat_ts=0 +lat_0=0 
+lon_0=-65 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))

westernpacifict <- spTransform(westernpacific, CRS("+proj=eqc +lat_ts=0 +lat_0=0 
+lon_0=-65 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))

##### ...read in point file (NGIA placenames) layer ######
ngia <- readOGR(".", "undersea") #note: to make this work I needed to modify the undersea file in QGIS (vector/multipoint to single point conversion)
proj4string(ngia)

##### ...reading in the rasters #####
require(sp)
require(rgdal)
require(raster)

#loading raster from geotiff
etopo <- raster("ETOPO1_Ice_g_geotiff.tif")
#assigning the correct projection (note: this is only assignment of existing projection, like define projection in ArcGIS)
proj4string(etopo) <- "+proj=longlat +datum=WGS84 +no_defs"

##### ...SPDF Subset: Create a subset of the points to create SpatialPointsDataFrame #####
#defining a subset of the data to create the SPDF.  
# query <- d$Latitude != "-999" &
#   d$VernacularNameCategory == "black coral" &
#   is.na(d$VernacularNameCategory) == F

# OR

#defining a subset of data from which to create the SPDF which does not include -999 for lat 
query <- d$Latitude != "-999"

#defining xy variable
xy <- c("Longitude", "Latitude")

#define which fields to return from query (don't need this part if not selecting speific fields)
#fields <- c(xy,"DataProvider","Genus", "MinimumDepthInMeters","gisDepth","gisOcean", "gisLocality")

#defining coordinates for just the results of the query 
coords<-d[query, c(xy)]

#making coords vectors numeric
coords$Latitude<-as.numeric(coords$Latitude)
coords$Longitude<-as.numeric(coords$Longitude)

# #defining subset of variables to use
# data <-d[query, c(fields)]

# OR

#defining subset of d to use, keeping the original data frame intact (this is a subset of d)
data <-d[query,]

#checking the names
names(data)

#creating SpatialPointsDataFrame from the subset. 
spdf<-SpatialPointsDataFrame(coords, data, 
                       proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"), 
                       match.ID = TRUE)

names(spdf)

##### ...ETOPO (gisDepth: Raster extract coordinates from etopo) #####
ex<-extract(etopo, coords)
#names(data)
data$gisDepth<-ex*-1

# #checking that the assignment worked.
# tail(data[,c("gisDepth","MinimumDepthInMeters")], n=100)
# plot(data$gisDepth, data$MinimumDepthInMeters)

# NOTE:  Here I could flag data but I will not just yet because I need better than ETOPO1
# flagging any data where gisDepth is < 0, or on land (recall that depth is expressed as a positive in our scheme)
# data[data$gisDepth < 0 & data$Flag == 0, c("FlagReason", "gisDepth", "MinimumDepthInMeters")]$FlagReason <- "may intersect land"
  
# you could paste the information into FlagReason
# paste(data[data$gisDepth < 0 & data$Flag == 0,]$FlagReason, "test", sep = ";")

# assigning flag
# data[data$gisDepth < 0 & data$Flag == 0,]$Flag

# checking where data is less than 0 and greater -999

data[as.numeric(data$MinimumDepthInMeters) < 0 &
     as.numeric(data$MinimumDepthInMeters) > -999,
     c("CatalogNumber", "Flag", "FlagReason", "gisDepth","MinimumDepthInMeters")]

#change negative depth value to positive value
data[data$CatalogNumber == "278201", c("MinimumDepthInMeters")] <- "474"

#...Plot gisDepth vs. observed depth #
# #checking
plot(data$MinimumDepthInMeters, 
     data$gisDepth,
     #xlim = c(0,5000),
     #ylim = c(0,5000),
     )

# create a new variable called checkDpeth which is the absolute
# difference between gisDepth and MinimumDepthInMeters
data$checkDepth <- abs(as.numeric(data$MinimumDepthInMeters) - as.numeric(data$gisDepth))

summary(data$checkDepth)

# below is a threshold function that can be used to test where the checkDepth variable is greater than some threshhold
# excluding places where MinimumDeptheInMeters no equal to "-999"

data[data$checkDepth > 3000 & data$MinimumDepthInMeters != "-999", 
     c("Flag", "FlagReason", "checkDepth", "MinimumDepthInMeters", "gisDepth")]

##### ...NGIA (gisLocality) Point to point intersection with NGIA (long run time for big sets)#####
 
#  Define these vectors, used in the loop. 
# the closestSiteVector is the vector of ID's from the ngia layer that was ID'd as the closest
# named locality.  the minDistVec gives the actual distance between the occurrence point and the name (ngia) point
closestSiteVec <- vector(mode = "numeric",length = nrow(spdf))
minDistVec     <- vector(mode = "numeric",length = nrow(spdf))

# Warning: this can take long time with the whole dataset. Much faster for subsets. 
for (i in 1 : nrow(spdf))
{
  distVec <- spDistsN1(ngia,spdf[i,],longlat = TRUE) 
  #units of distance are in kilometers Great Circle Distance
  minDistVec[i] <- min(distVec)
  closestSiteVec[i] <- which.min(distVec)
}

# #checking the names that were generated
# names(ngia)
# names(spdf)

# defining the NGIA names vector
# this pulls out the FULL_NAME from the ngia spatial points 
# data frame based on the closestSiteVec calculated in the loop above
gisNGIA <- as(ngia[closestSiteVec,]$FULL_NAME,"character")


# Assign the Full Name vector back to the data in the appropriate place
data$gisLocality <- gisNGIA

# Assign the minDistVec to a new variable called gisLocalityDist (units = km).  Adding this 
# variable allows us to threshhold the distance
# units of distance are in kilometers Great Circle Distance

data$gisLocalityDist <- minDistVec

# right here we need an additional step to threshold the 
# distance for gisLocality assignment (1km vs. 10km vs. 20km?)

# checking the new variables 

tail(data[,c("gisLocalityDist", "gisLocality")], n = 1000)

#checks
#table(data$gisLocality, useNA = "always")
#head(data[,c("Locality", "gisLocality")], n=100)
#tail(data[,c("Locality", "gisLocality")], n=1000)

##### ...checking the results of the NGIA gisLocality assignments #####
table(data$gisLocality, useNA = "always")

# looks like some trailing white space issues are evident.
# thesholding the gisLocality is assignment is very important.

head(data[,c("Locality", "gisLocality")], n=100)
tail(data[,c("Locality", "gisLocality")], n=100)

##### ...IHO World Seas (gisOcean) polygon names to point (specify zones at top) #####

# establish zone (poly) layer to be used 
# (i am using a variable name so I can generalize this process)

zones<-worldseas

# checking
# head(worldseas)

# intersect the coral and sponge points with the world seas (zones) SpatialPolygonsDataFrame
# assign the results of the intersection to a variable (z). variable z is of class = data.frame
z<-over(spdf, zones)

# checking
# > class(z)
# [1] "data.frame"
# > names(z)
# [1] "NAME"       "ID"         "Gazetteer_"
# > 

# ... and assigning the intersect values back to the coral and sponge data file
data$gisOcean <- z$NAME

###### ...Checks for IHO worldseas assignment to gisOcean #####
table(factor(data[,c("gisOcean")]), useNA = "always")
table(factor(data[,c("gisLME")]), useNA = "always")


# note from the results below that there are 502 records that are NA
# these are usually from coastline mismatch issues.  

# table(factor(data[,c("gisOcean")]), useNA = "always")
# 
# Arctic Ocean 
# 4 
# Bay of Fundy 
# 5 
# Beaufort Sea 
# 6 
# Bering Sea 
# 29435 
# Caribbean Sea 
# 2181 
# Chukchi Sea 
# 60 
# Davis Strait 
# 19 
# Gulf of Alaska 
# 5494 
# Gulf of California 
# 56 
# Gulf of Mexico 
# 22576 
# Labrador Sea 
# 10 
# North Atlantic Ocean 
# 8602 
# North Pacific Ocean 
# 217333 
# South Pacific Ocean 
# 54 
# The Coastal Waters of Southeast Alaska and British Columbia 
# 732 
# <NA> 
#   502 

# head(data[,c("gisOcean", "Ocean", "gisLocality")], n = 1000)
# tail(data[,c("MinimumDepthInMeters", "gisDepth")])

##### Analysis of overlap with Geomorphology #####
# Results are a logical vector of (True or False) that indicates 
# Whether the point is present in a given feature

# Canyons 
load("H:/rworking/DeepSeaData20150108/canyons.RData")

z<-over(spdf, canyons)
data$gisCanyons <- !(is.na(z$Geomorphic))

# #checking
# table(factor(data[,c("gisCanyons")]), useNA = "always")
# rm(z)
# rm(canyons)
# save(data, file = "data.RData")

# Escarpments
load("H:/rworking/DeepSeaData20150108/escarpments.RData")
z<-over(spdf, escarpments)
names(z)
data$gisEscarpments <- !(is.na(z$Geomorphic))
save(data, file = "data.RData")

# #checking
# table(factor(data[,c("gisEscarpments")]), useNA = "always")
# rm(z)
# rm(escarpments)
# save(data, file = "data.RData")

##### Analysis of overlap with Fisheries Management Councils #####
# Results are a logical vector of (True or False) that indicates 
# Whether the point is present in a given feature
z<-over(spdf, caribbean)
names(z)
data$gisCaribbeanFMC <- !(is.na(z$Region))

# #checking
# table(factor(z[,c("Region")]), useNA = "always")
# table(factor(data[,c("gisCaribbeanFMC")]), useNA = "always")
# rm(z)
# rm(canyons)
# save(data, file = "data.RData")

z<-over(spdf, gulfofmexico)
names(z)
table(z$Region)

data$gisGulfofMexicoFMC <- !(is.na(z$Region))

z<-over(spdf, midatlantic)
names(z)
table(z$Region)
data$gisMidAtlanticFMC <- !(is.na(z$Region))

z<-over(spdf, newengland)
names(z)
table(z$Region)
data$gisNewEnglandFMC <- !(is.na(z$Region))

load("H:/rworking/DeepSeaData20150108/spdf.RData")
load("H:/rworking/DeepSeaData20150108/northpacific.RData")
load("H:/rworking/DeepSeaData20150108/d.RData")

# note: in this case I had to reproject (see above where spdft and
# northpacifict is created) because the North Pacific
# crosses over the "seam" at -180.  This gives the "over" command trouble
# this particular analysis requires almost all of the 8gigs of 
# ram that I have requires lots on 
library(rgdal)
z<-over(spdft, northpacifict)
names(z)
table(z$Country)
rm(northpacific)
rm(northpacifict)
rm(spdf)
rm(spdft)
rm(EPSG)
load("H:/rworking/DeepSeaData20150108/data.RData")
data$gisNorthPacificFMC <- !(is.na(z$Country))
save(data, file = "data.RData")


load("H:/rworking/DeepSeaData20150108/spdf.RData")
spdft <- spTransform(spdf, CRS("+proj=eqc +lat_ts=0 +lat_0=0 
+lon_0=-65 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")) 
rm(spdf)
z<-over(spdft, westernpacifict)
names(z)
table(z$Sovereign)
load("H:/rworking/DeepSeaData20150108/data.RData")
data$gisWesternPacificFMC <- !(is.na(z$Sovereign))
save(data, file = "data.RData")
rm(data)
rm(spdft)
rm(westernpacifict)
rm(z)

load("H:/rworking/DeepSeaData20150108/spdf.RData")
load("H:/rworking/DeepSeaData20150108/southatlantic.RData")
z<-over(spdf, southatlantic)
names(z)
table(z$AreaName)
rm(southatlantic)
rm(spdf)
load("H:/rworking/DeepSeaData20150108/data.RData")
data$gisSouthAtlanticFMC <- !(is.na(z$AreaName))
save(data, file = "data.RData")
rm(data)
rm(z)

load("H:/rworking/DeepSeaData20150108/spdf.RData")
load("H/rworking/DeepSeaData20150108/pacific.RData")
z<-over(spdf, pacific)
names(z)
table(z$Region)
rm(pacific)
rm(spdf)
load("H:/rworking/DeepSeaData20150108/data.RData")
data$gisPacificFMC <- !(is.na(z$Region))
save(data, file = "data.RData")
rm(data)
rm(z)

##### Bounding boxes for different regions #####

# #Southeast
# xlim = c(-80,-75),
# ylim = c(24,35),
# 
# #GOMEX
# xlim = c(-98,-80),
# ylim = c(22,29),
# 
# #SoCAL
# xlim = c(-123,-113),
# ylim = c(26,35),
# 
# #Pacific
# xlim = c(-124,-120),
# ylim = c(35,50),

##### Removing the extra gis variables that don't matter any more ##### 
data <- data[,-c(94:97)]

##### ____________ Mapping Module ######

##### Experiment Using GGPlot2 to make a scalable map ##### 
mp1 <- fortify(map(fill=TRUE, plot=FALSE))
mp2 <- mp1
mp2$long <- mp2$long + 360
mp2$group <- mp2$group + max(mp2$group) + 1
mp <- rbind(mp1, mp2)
ggplot(aes(x = long, y = lat, group = group), data = mp) + 
  geom_path() + 
  scale_x_continuous(limits = c(100, 360)) + #use this to scale 
  scale_y_continuous(limits = c(-70, 70))

###### Make some maps ######
load("H:/rworking/DeepSeaData20150108/caribbean.RData")
load("H:/rworking/DeepSeaData20150108/southatlantic.RData")
load("H:/rworking/DeepSeaData20150108/spdf.RData")
load("H:/rworking/DeepSeaData20150108/data.RData")
plot(southatlantic)
plot(caribbean)
points(spdf, pch=20, cex=0.2, col = "red")

load("H:/rworking/DeepSeaData20150108/data.RData")

##### ____________ Looking @ orig. Pacific Grounfish S. Data ######

#####  Exploring Problem with existing original Groundfish survey data (Curt Whitmire) Data ##### 

curt <- data %>% 
  filter(DataProvider == "NOAA, Northwest Fisheries Science Center", 
         DataContact == "Curt Whitmire") %>% 
  select(Flag, FlagReason, CatalogNumber, SampleID, Citation, DataContact, DataProvider, SamplingEquipment, SampleID)

table(curt$FlagReason, useNA = "always")
table(curt$SamplingEquipment, useNA = "always")
table(curt$DataProvider, useNA = "always")
table(curt$DataContact, useNA = "always")
table(curt$Citation, useNA = "always")
table(curt$SampleID, useNA = "always")

##### ____________ NEWDATA: Pacific Groundfish Survey from (Curt Whitmire) #####

##### read in new Curt Whitmire data ##### 
options(stringsAsFactors=FALSE)
whitmire<-read.csv("./InData/WCGBTS_2001_13_DSCS_forDSCRTP.csv")

# make sure the names match
setdiff(names(d), names(whitmire))
setdiff(names(whitmire), names(d))

#change unmatching column names
z = "Order_"
x <-which(colnames(whitmire) == z)
colnames(whitmire)[x] <- "Order"

setdiff(names(d), names(whitmire))
setdiff(names(whitmire), names(d))

# get rid of OBJECTID variable
whitmire <- whitmire %>%
  select(-OBJECTID)

names(whitmire)

#assign NA to CatalogNumber
whitmire$CatalogNumber <- NA
table(whitmire$CatalogNumber)

#set Flag = 0
whitmire$Flag <- "0"
table(whitmire$Flag)

##### _____ OUTPUT: "whitmire"
save(whitmire, file = "./OutData/whitmire.RData")

##### ____________ Fixing flag problem with "d"
##### Fixing problem with an errant Flag indicator #####
load("C:/rworking/deep-sea-workbench/OutData/d.RData")
d[d$Flag == "l" & is.na(d$Flag) == F,]$Flag <- "1"
save(d, file = "d.RData")

##### _____OUTPUT: "whitmire"

##### ____________ binding "whitmire" to the dbase ##### 

#NOTE: Creating D2 which is a temporary file.
# binding rows together
load("C:/rworking/deep-sea-workbench/OutData/d2.RData")
d2 <- rbind(d, whitmire)
table(d2$Flag)
save(d2, file = "d2.RData")

#####_____OUTPUT: "d2" #####

#####____________ Adding Pacific Observer Program Data to the dbase #####

##### Adding the Merideth's Observer Program data to the mix ######
load("C:/rworking/deep-sea-workbench/OutData/op.RData")
setdiff(names(op), names(d2))
d3 <- rbind.fill(d2, op)

#####_____OUTPUT: "d3"##### 

##### save d3 #####
save(d3,file = "./OutData/d3.RData")

##### ____________ Adding "osp" to main data frame

d4 <- rbind.fill(d3, osp)

#####_____ OUTPUT: "d4"#####

##### ____________ Preparing data for Geographic Enhancement #####

##### Flag lats and longs with NA or other problems #####
table(d4$Flag, useNA = "always")
table(d4[is.na(d4$Flag) == T, c("Latitude")], useNA = "always")
d4$Flag[is.na(d4$Flag) == T]<-"0"


##### Creating a new spatial points data frame ##### 
d4unflag<-d4[d4$Flag == "0",]
d4unflag$Latitude <- as.numeric(d4unflag$Latitude)
d4unflag$Longitude <- as.numeric(d4unflag$Longitude)
d4unflag<-d4unflag[is.na(d4unflag$Latitude) == F,]
d4unflag<-d4unflag[is.na(d4unflag$Longitude) == F,]

xy <- c("Longitude", "Latitude")
coords<-d4unflag[, c(xy)]
class(coords)


#making coords vectors numeric
coords$Latitude<-as.numeric(coords$Latitude)
coords$Longitude<-as.numeric(coords$Longitude)

#creating SpatialPointsDataFrame from the subset. 
spdf<-SpatialPointsDataFrame(coords, d4unflag, 
                             proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"), 
                             match.ID = TRUE)

names(spdf)

##### ____________ Assigning GIS Region (for Pacific Only )

##### Creating a new variable called gisPacificFMC ##### 
load("C:/rworking/deep-sea-workbench/SpatialDFs/pacific.RData")
z<-over(spdf, pacific)
names(z)
table(z$Region)
d4unflag$gisPacificFMC <- !(is.na(z$Region))
table(d4unflag$gisPacificFMC, useNA = "always")
save(d4unflag, file = "d4unflag.RData")

##### ____________ Creating Mapping Subsets for SoDSCE report #####

#Working on mapping subsets for the State of Deep Sea Corals #####
###### map 1 - Order == "Scleractinia"#####
bs <- d4unflag %>%
    filter(Order == "Scleractinia", gisPacificFMC == T)  %>%
    select(Latitude, Longitude, VernacularNameCategory, Order, Genus, ScientificName)

write.csv(bs, "./OutData/bs.csv")

###### map 2 - Antipatharia #####

at <- d4unflag %>%
  filter(Order == "Antipatharia", 
         gisPacificFMC == T
         )  %>%
  select(DataProvider, SampleID, Latitude, Longitude, VernacularNameCategory, Order, Family, Genus, ScientificName)

write.csv(at,"./OutData/at.csv")

###### map 3 - Non-gorgonian alcyonaceans #####

alcyon <- d4unflag %>%
  filter(Order == "Alcyonacea", 
         gisPacificFMC == T)  %>%
  select(DataProvider, SampleID, Latitude, Longitude, VernacularNameCategory, Order, Suborder, Family, Genus, ScientificName)


write.csv(alcyon,"./OutData/alcyon.csv")

table(alcyon$Suborder, useNA = "always")

###### map 4 - Gorgonian alcyonaceans – #####
# with differentcolors for suborders Holaxonia, Scleraxonia, & Calcaxonia 
# (this would trackwell with much of the region-level modeling 
#which has been done at this level.Black dots for unidentified gorgonians

gorg <- d4unflag %>%
  filter(Order == "Gorgonacea", 
         gisPacificFMC == T)  %>%
  select(DataProvider, SampleID, Latitude, Longitude, VernacularNameCategory, Order, Suborder, Family, Genus, ScientificName)

write.csv(gorg,"./OutData/gorg.csv")


###### map 5 - Pennatulacea – with different colors forsuborders #####
#Sessiliflorae & Subsessiliflorae (may be too many families tobreak it down further)
#& unidentified seapens (black dots)

table(d4unflag$Order)

pen <- d4unflag %>%
  filter(Order == "Pennatulacea", 
         gisPacificFMC == T)  %>%
  select(DataProvider, SampleID, Latitude, Longitude, VernacularNameCategory, Order, Suborder, Family, Genus, ScientificName)

table(pen$Family, useNA = "always")

write.csv(pen, "./OutData/pen.csv")


###### map 6 -  Stylasterid corals and other (e.g., gold corals)#####

table(d4unflag$Order)

styl <- d4unflag %>%
  filter(Family == "Stylasteridae", 
         gisPacificFMC == T)  %>%
  select(DataProvider, SampleID, Latitude, Longitude, VernacularNameCategory, Order, Suborder, Family, Genus, ScientificName)

table(styl$Order)

write.csv(styl, "./OutData/styl.csv")


###### map 7 -  Sponges (one map with different colors for Porifera #####
# (unidentified – black dots), Demosponges, Glass sponges, and calcareous sponge

table(d4unflag$Phylum, useNA = "always")

sp <- d4unflag %>%
  filter(Phylum == "Porifera", 
         gisPacificFMC == T)  %>%
  select(DataProvider, SampleID, Latitude, Longitude, VernacularNameCategory, Order, Suborder, Family, Genus, ScientificName)

table(sp$VernacularNameCategory, useNA = "always")

write.csv(sp, "./OutData/sp.csv")


#####____________ NEWDATA: "./InData/tax_master.csv" ##### 


##### Adding the taxonomic master list #####
options(stringsAsFactors=FALSE)
taxmaster<-read.csv("./InData/tax_master.csv", header = T, na.strings = "")


#####____________ Bug fixing ##### 

##### fuzzy searching ##### 

agrep(pattern, 
      x, 
      max.distance = 0.1, 
      costs = NULL,
      ignore.case = FALSE, 
      value = FALSE, 
      fixed = TRUE,
      useBytes = FALSE)

getwd()

oculina <- d3[agrepl("Oculina", 
      d3$ScientificName, 
      max.distance = 0.01, 
      costs = NULL,
      ignore.case = T, 
      fixed = F,
      useBytes = T),]
##### taxonomic filtering #####

taxa<-"Enallopsammia"
filter <- d3[grepl(taxa, d3$ScientificName,),]
unique(filter$ScientificName)
write.csv(filter, paste(taxa, ".csv", sep=""))
table<-table(filter$ScientificName)
write.csv(table, paste(taxa, "FrequencyTable.csv", sep=""))


#####____________ Alaska Subset Mapping Work #####

##### Geographic subsetting to Alaska Region ##### 
geosub<- subset(d3, as.numeric(Latitude) > 40 & 
                  as.numeric(Latitude) < 60 & 
                  as.numeric(Longitude) < -115 |
                  as.numeric(Longitude) > 145)

##### Taking care of unflagging the RACEBASE trawl sample (Recall that they were flagged and needing unflagged for mapping##### 
geosub[geosub$FlagReason == "No SampleID" & is.na(geosub$FlagReason) == F,]$Flag <- "0"   

#checking
table(geosub[geosub$FlagReason == "No SampleID",]$Flag, useNA = "always")

#filtering for only unflagged records
geosub <- geosub %>% 
  filter(Flag == "0")

#subsetting to just the variables of interest
geosub <- geosub %>%
  select(Flag, FlagReason, DataProvider, SampleID, Latitude, Longitude, VernacularNameCategory, Order, Suborder, Family, Genus, ScientificName)
                 
###### map 1 - Order == "Scleractinia"#####
#checking
table(geosub$Order, useNA = "always")

table(geosub %>% 
  filter(is.na(Order)) %>% 
  select(ScientificName))

#making the subset for mapping
bs_ak <- geosub %>%
  filter(Order == "Scleractinia")
 
write.csv(bs_ak, "./OutData/bs_ak.csv")

###### map 4 - Gorgonian alcyonaceans – #####
# with differentcolors for suborders Holaxonia, Scleraxonia, & Calcaxonia 
# (this would trackwell with much of the region-level modeling 
#which has been done at this level.Black dots for unidentified gorgonians

gorg_ak <- geosub %>%
  filter(Order == "Gorgonacea")  %>%
  select(Flag, FlagReason, DataProvider, SampleID, Latitude, Longitude, VernacularNameCategory, Order, Suborder, Family, Genus, ScientificName)

write.csv(gorg_ak,"./OutData/gorg_ak.csv")


##### ____________ OBIS subset release on 20150402 from DB version 20141219-0 #####

#reading in the fields required for OBIS as a list
template <- read.csv("indata/obistemplate.csv", header = F, stringsAsFactors = FALSE)
template <- as.character(template)
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
template <- trim(template)
template

#checking set differences between names
setdiff(template, names(d))

# changing variable names where needed to match current schema
d.obis <-rename(d, c("WormsID" = "AphiaID"))
d.obis <- rename(d.obis, c("IdentificationRemarks" = "IdentificationComments"))
d.obis <- rename(d.obis, c("OccurrenceRemarks" = "OccurrenceComments"))
d.obis <- rename(d.obis, c("LocationComment" = "LocationComments"))

# addin ImageURL field
d.obis$ImageURL <- "NA"

# checking the set differences between names to make sure all is fixed
setdiff(template, names(d.obis))

# selecting only the unflagged records and applying the OBIS field template
obis<-d.obis[d.obis$Flag == "0" & is.na(d.obis$Flag) == F, c(template)]

# checking the field names on the file
names(obis)

# writing out the new set
write.table(obis,"OutData/DSCRTP_NatDB_20141219-0_OBIS_subset.txt", row.names = F, quote = F, sep = "\t")

#####____________Writing out the Pacific Species List for Tom #####
SpeciesList <- d4unflag %>%
  filter(gisPacificFMC == T)  %>%
  select(DataProvider, ScientificName)

SpeciesList<-unique(SpeciesList$ScientificName)

write.csv(SpeciesList, "./OutData/PacificSpeciesList.csv")






