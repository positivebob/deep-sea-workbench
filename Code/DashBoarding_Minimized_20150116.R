# Dashboarding Script
# by: Robert McGuinn
# date started: 2014-11-25
# purpose: to provide a constistent and repeatable set of database summaries for the 
# ...DSCRTP database.  

##### Load the latest data #####
load("C:/rworking/DeepSeaData20150128/data.RData")

#### Total number of unflagged records currently in the database ####
length(data$CatalogNumber) 

##### pick the variable and region you want to analyze #####
#set the factor variable you wish to summarize by
# z = "VernacularNameCategory"
# z = "ScientificName"
z = "DataProvider"
region = "Caribbean"
##### setting some other variables #####
# gisvars<-names(data[,c(grep("gis",names(data)))])
# gisFMC<-grep("FMC", gisvars)
# gisvars[gisFMC]

#make the region correspond to the actual regional variable name
regionvar = paste("gis",region,"FMC", sep="")

#set the path to save the resulting work into
path <- paste("C:/rworking/DeepSeaData20150128", region, sep = '/') 
filepath <- paste(path,z,sep='/')

#figure out the column number of the region variable
x <-which(colnames(data) == regionvar)

#figure out the column number of the factor variable you wish to summarize by
y <-which(colnames(data) == z)
##### subsetting data by region var ##### 
dsub <- data[data$Flag == 0 & data[x] == T, ]
##### creating an ordered frequency table and output a CSV #####
xout <- as.data.frame(table(dsub[y], useNA="always"))
xout <- transform(xout, relative = prop.table(Freq))
xout <-xout[order(xout$Freq),] 
table<-head(xout[order(-xout$Freq), ], n=100)
colnames(table) <- c(z, "Count", "Proportion")
write.csv(table, row.names = F, quote = T, paste(filepath,"csv", sep = '.'))
##### create an ordered bar plot of the first 30 rows of the ordered frequency table above #####
table <- table[1:30,]
sort <-table[order(table$Proportion),]
pdf(paste(filepath,"pdf", sep = '.'),width=10,height=7)
par(mar=c(8,20,5,5))
par(mgp=c(4, 1, 0))
barplot(sort$Count, names.arg = sort[,1], las=2, cex.names=.75, horiz=TRUE, main=paste(z,region,sep = " - "), xlab = "Number of Records") 
dev.off()

######__________________________________________________#####

##### dplyr summaries #####
#install.packages("dplyr")
library(ggplot2)
library(dplyr)
tbl <- tbl_df(data)

##### Create cool tables ##### 
#find all the variable names with gis in them
gisvars<-names(data[,c(grep("gis",names(data)))])
#find all gis variable names with FMC in them
gisFMC<-grep("FMC", gisvars)
gisvars[gisFMC]

# x <- "gisCaribbeanFMC"      
# x <- "gisGulfofMexicoFMC"  
# x <- "gisMidAtlanticFMC"
# x <- "gisNewEnglandFMC"    
# x <- "gisNorthPacificFMC"
# x <- "gisWesternPacificFMC"
# x <- "gisSouthAtlanticFMC"
x <- "gisCaribbeanFMC"
library(dplyr)
new_tbl<- 
  tbl %>% 
  #select(DataContact, VernacularNameCategory, MinimumDepthInMeters) %>%
  filter(!is.na(MinimumDepthInMeters),
         as.numeric(MinimumDepthInMeters) != 999,
         as.numeric(MinimumDepthInMeters) != -999,
         !is.na(VernacularNameCategory),
         gisSouthAtlanticFMC == T) %>%
  group_by(DataProvider, VernacularNameCategory) %>%
  summarize(
    min.Depth = min(as.numeric(MinimumDepthInMeters)),
    max.Depth = max(as.numeric(MinimumDepthInMeters)), 
            mean.Depth = mean(as.numeric(MinimumDepthInMeters)),
            mean.DepthGIS = mean(as.numeric(gisDepth)),
            n = n()) %>%
  arrange(mean.Depth)

write.csv(new_tbl, paste(x, "DataProvider_by_VernacularNameCategory.csv", sep = ""))

##### Some Graphing using ggplot2 #####
library(ggplot2)

##### Filter the data ##### 
new_tbl<- 
  tbl %>% 
  #select(Order,Vessel,DataProvider, DataContact, Genus, VernacularNameCategory, MinimumDepthInMeters) %>%
  filter(!is.na(MinimumDepthInMeters),
         as.numeric(MinimumDepthInMeters) != 999,
         as.numeric(MinimumDepthInMeters) != -999,
         #as.numeric(MinimumDepthInMeters) < 2000,
         !is.na(VernacularNameCategory), 
         #grepl("Thesea",ScientificName), 
         gisCaribbeanFMC == T)
         #Family == "Primnoidae")

##### Kernel density plots #####
# grouped by X (indicated by color)
qplot(as.numeric(MinimumDepthInMeters), data=new_tbl, geom="histogram", alpha=I(.5), fill = Order,
      main="Depth Distribution in Carribean FMC", xlab="Depth (Meters)", 
      ylab="Number of Database Records", binwidth = 50) +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.05)) +
  scale_x_continuous(limits = c(0, 3500))

##### Jitter #####
qplot(gisLocality, DataProvider, data=new_tbl,
      geom="jitter",alpha=I(.5), colour=VernacularNameCategory, main="New England FMC (All Points)",
      xlab="DataProvider", ylab="ScientificName") +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.05))# +
# scale_x_continuous(limits = c(0, 3000))

##### Box Plots #####

qplot(DataContact, as.numeric(DepthInMeters), data=new_tbl, geom=c("boxplot"),alpha=I(.5), 
      main="Depth Distribution of All Taxa in ",
      xlab="Data Provider", ylab="Depth") +
theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.05)) +
  scale_y_continuous(limits = c(0, 3000))

##### knit 2 html #####
library(knitr)
knit2html("DSCRTP Data Dashboarding.Rmd")

##### cut the depth vector and then barplot (varying bins) #####
groups = cut(data$gisDepth, breaks=c(0,50,100,200,500,1000,5000), include.lowest=TRUE)
groups
#bygroup = tapply(data$gisDepth, groups)
table<-table(groups)
table<-as.data.frame(table)
table <- table[rev(order(row.names(table))),]
table

names.arg = c("0-50","50-100","100-200", "200-500", "500-1000", ">1000")
rev.names.arg= rev(names.arg)

barplot(table$Freq, 
        horiz=T, 
        las=2,
        xlab="Number of Records",
        ylab="Depth in Meters",
        names.arg = rev.names.arg
)

##### Take a random sample of the data frame#####
randomSample = function(df,n) { 
  return (df[sample(nrow(df), n),])
}
testdf<-randomSample(data, 10000)
