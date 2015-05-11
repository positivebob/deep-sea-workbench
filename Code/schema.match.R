###### ____________ Schema Match Module (schema.match) #####

##### Module switch inbound #####
# outdata from last module becomes indata for this module, 
# another switch is added at the enc to ) 
indata <- outdata

##### Define FUNCTIONS and load libraries Libraries: Defining any functions needed in the module #####
# FUNCTIONS
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

# Libraries

##### ENVIRONMENT settings #####
# set environment variables
setwd("C:/rworking/deep-sea-workbench/OutData")

##### INPUTS: Other inputs besides indata that will be needed in the module ######
# Bringing in a list of correctly named and ordered variables from the schema
template <- read.csv("../InData/Schema/FullTemplate.csv", header = F, stringsAsFactors = FALSE)
template <- as.character(template)
template <- trim(template)

##### Defining column name changes for "indata" #####
# assign a column number to a specific variable name that needs replaced.  
# use that number to assign a replacement. 

x <-which(colnames(indata) == "CruiseComments")
colnames(indata)[x] <- "SurveyComments"

x <-which(colnames(indata) == "WormsID")
colnames(indata)[x] <- "AphiaID"

x <-which(colnames(indata) == "IdentificationRemarks")
colnames(indata)[x] <- "IdentificationComments"

x <-which(colnames(indata) == "LocationComment")
colnames(indata)[x] <- "LocationComments"

x <-which(colnames(indata) == "GenBankID")
colnames(indata)[x] <- "AssociatedSequences"

x <-which(colnames(indata) == "GenBankID")
colnames(indata)[x] <- "AssociatedSequences"

x <-which(colnames(indata) == "OccurrenceRemarks")
colnames(indata)[x] <- "OccurrenceComments"

#####Erasing any variable from indata that has "gis" in the name ##### 
indata <- indata[,grepl("gis", names(indata)) == F]

##### Adding any needed non GIS variables from the template #####
indata$ImageURL <- NA

# checking
setdiff(names(indata), template)
setdiff(template, names(indata))

# adding the remainingg variables that are in the template but not in indata
namevector<-setdiff(template, names(indata))

for(i in namevector)
  indata[,i] <- NA

# names(indata)

##### Make indata conform to the variable order of the full schema template #####
indata<-indata[,c(template)]

# checking
# names(indata)

##### Module switch outbound #####
# indata from the changes in this module become outdata for entry into the next module, 
outdata <- indata

##### cleanup ##### 
rm(indata)
