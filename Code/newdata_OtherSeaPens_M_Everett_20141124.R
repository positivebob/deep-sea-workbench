#####___________ NEW DATA: Other sea pen data from Merideth Everett #####

##### loading the data #####
options(stringsAsFactors=FALSE)
osp<-read.table("./InData/Sea_Pens_DSC_DB-Updated_MEverett_20141124_rmcguinn_seapens_other_sources.txt", sep = "\t", header = T)

###### Setting all variables in new dataset be character strings  ###### 
osp <- data.frame(lapply(osp, as.character), stringsAsFactors=FALSE)

#####Trim all leading and trailing whitespace in new data frame
osp <- as.data.frame(apply(osp,2,function(x)gsub("^\\s+|\\s+$","",x)))

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

##### Recode problems with ScientificName #####

#table(osp$ScientificName)
osp$ScientificName[osp$ScientificName == "Halipteris sp."] <- "Halipteris"
osp$ScientificName[osp$ScientificName == "Halipteris sp"] <- "Halipteris"
osp$ScientificName[osp$ScientificName == "Umbellula sp."] <- "Umbellula"

#changing individual ObservationDate with inconsistent formats
osp$ObservationDate[osp$ObservationDate == "7/22/07"] <- "7/22/2007"
osp$ObservationDate[osp$ObservationDate == "7/8/09"] <- "7/8/2009"
#checking
#table(osp$ObservationDate, useNA = c("always"))

#changing ObservationDate to the schema format
osp$ObservationDate <- as.Date(osp$ObservationDate, "%m/%d/%Y")
#checking
#table(osp$ObservationDate, useNA = c("always"))

##### Assign AphiaID and Species List ##### 
library(SSOAP)
w = processWSDL("http://www.marinespecies.org/aphia.php?p=soap&wsdl=1")
iface = genSOAPClientInterface(, w)


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

#Add missing AphiaID for Umbellula (this seems like a problem with the SSOAP service)
MySpecList$OrigTaxID[MySpecList$OrigTaxID == "-999"] <- "128499"

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

AphiaRecord <- iface@functions$getAphiaChildrenByID("128491",('http://www.marinespecies.org/aphia.php?p=soap')) 

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

##### Fixing names ##### 
names(d)
setdiff(names(d), names(osp))
setdiff(names(osp), names(d))
