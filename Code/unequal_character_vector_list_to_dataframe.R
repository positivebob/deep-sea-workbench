library(SSOAP)
w = processWSDL("http://www.marinespecies.org/aphia.php?p=soap&wsdl=1")
iface = genSOAPClientInterface(, w)

##### defining a function to get the full taxonomic listing from World Registry of Marine Species #####
iface@functions$getAphiaClassificationByID(289324,('http://www.marinespecies.org/aphia.php?p=soap'))

getFullList <- function(x) {
  iface@functions$getAphiaClassificationByID(x,('http://www.marinespecies.org/aphia.php?p=soap')) 
  }
##### use lapply to feed an integer vector of AphiaID's to the taxonomic #####
classificationObject<-lapply(FUN = getFullList, MySpecList$AccTaxID)

##### define a function to extract only the information that you need from the "classificationObject" #####
extractList <- function(x){
          a <-x@scientificname
          b <-x@child@scientificname
          c <-x@child@child@scientificname
          d <-x@child@child@child@scientificname
          e <-x@child@child@child@child@scientificname
          f <-x@child@child@child@child@child@scientificname
          g <-x@child@child@child@child@child@child@scientificname
          h <-x@child@child@child@child@child@child@child@scientificname
          i <-x@child@child@child@child@child@child@child@child@scientificname
          j <-x@child@child@child@child@child@child@child@child@child@scientificname
          vector<-c(a,b,c,d,e,f,g,h,i,j)
          return(vector)
}

##### feed the classificationObject to the extractList function to actually get just the taxonomic string that you need #####
taxstring <- lapply(FUN = extractList, classificationObject)

##### row bind (with fill) the list of taxonomic character vectors by first making them transposed data frames in turn using lapply #####
table<-rbind.fill(lapply(taxstring, function(X) data.frame(t(X))))

