##### ____________ NULL assignment module #####
# The steps in this code section allow for proper null assignment for numeric and character variables

##### Module switch inbound #####
# outdata from last module becomes indata for this module, 
# another switch is added at the end to change back to outdata, ready for the next module
indata <- outdata

#### Define FUNCTIONS and load libraries Libraries: Defining any functions needed in the module #####
# FUNCTIONS
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

# Libraries

##### ENVIRONMENT settings #####
# set environment variables
setwd("C:/rworking/deep-sea-workbench/OutData")

##### Bringing in the numerical variable list from latest schema #####
numeric <- read.csv("C:/rworking/deep-sea-workbench/InData/Schema/numeric.csv", header = F, stringsAsFactors = FALSE)
numeric <- as.character(numeric)
numeric <- trim(numeric)

# # checking
# numeric

##### Bringing in the character variable list #####
character <- read.csv("C:/rworking/deep-sea-workbench/InData/Schema/character.csv", header = F, stringsAsFactors = FALSE)
character <- as.character(character)
character <- trim(character)

# # checking
# character

##### Set all cells in the dataframe as.character #####
indata <- data.frame(lapply(indata, as.character), stringsAsFactors=FALSE)

##### 1 - Defining the trim function #####
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
trimall <- function(x){
  dat<-trim(indata[,c(x)])
  indata[,c(x)] <- dat
}

##### 2 - Trim all white space from beginning and end of indata #####
lapply(numeric, FUN = trimall)
lapply(character, FUN = trimall)

##### 3 - Create -999 for all missing numerical values in indata #####
NumericNull<-function(x){
  dat <- indata[,c(x)]
  indata[is.na(dat) == T | dat == "" | dat == "NA", c(x)] <<- -999
}

lapply(numeric, FUN = NumericNull)

##### 4 - Create NA for all missing factor and character variables in indata  #####
#  define function for creating NA values.
FactorNull<-function(x){
  dat <- indata[,c(x)]
  indata[is.na(dat) == T | dat == "" | dat == "NA", c(x)] <<- NA
}

# applying the function of the list of character variables.  
lapply(character, FUN = FactorNull)

##### Module switch outbound #####
# indata from the changes in this module become outdata for entry into the next module, 

outdata <- indata

##### cleanup ##### 
rm(indata)
