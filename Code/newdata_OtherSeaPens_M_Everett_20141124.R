#####___________ NEW DATA: Other sea pen data from Merideth Everett #####

##### loading the data #####
options(stringsAsFactors=FALSE)
osp<-read.table("./InData/Sea_Pens_DSC_DB-Updated_MEverett_20141124_rmcguinn_seapens_other_sources.txt", sep = "\t", header = T)

##### #finding and changing the column names in the new dataset that do not match. ##### 
setdiff(names(osp), names(d2))
names(osp)

z = "IdentifiedBy.Verified"
x <-which(colnames(osp) == z)
colnames(osp)[x] <- "IdentifiedBy"

z = "DecimalLatitude"
x <-which(colnames(osp) == z)
colnames(osp)[x] <- "Latitude"

z = "DecimalLongitude"
x <-which(colnames(osp) == z)
colnames(osp)[x] <- "Longitude"

###### Making all variables in new dataset be character strings  ###### 
osp <- data.frame(lapply(osp, as.character), stringsAsFactors=FALSE)

##### Filling in where ObservationDate is missing with NA values  ######
osp$ObservationDate[op$ObservationDate == ""] <- NA

##### Changing ObservationDate to the right date format  #####

#trimming white space
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
osp$ObservationDate <- trim(osp$ObservationDate)

#making some manual changes
fix(osp)

#changing ObservationDate to a true date class
test<-
  osp$ObservationDate <- as.Date(osp$ObservationDate, "%m/%d/%Y")
table(osp$ObservationDate, useNA = c("always"))

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

#recode any problems with osp ScientificName
osp$ScientificName[osp$ScientificName == "Halipteris sp."] <- "Halipteris"
osp$ScientificName[osp$ScientificName == "Halipteris sp"] <- "Halipteris"
osp$ScientificName[osp$ScientificName == "Umbellula sp."] <- "Umbellula"

# Create your specieslist
MySpecies<-c(unique(osp$ScientificName))
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
join <- merge(x = osp, y = AphiaRecords, by.x = "ScientificName", by.y = "scientificname", all.x = TRUE)
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
osp<-join

##### Flagging ####
names(osp)
osp$Flag <- "0"
save(osp, file = "./OutData/osp.RData")

#checking
osp$Flag

##### Fixing names ##### 
names(d)
setdiff(names(d), names(osp))
setdiff(names(osp), names(d))