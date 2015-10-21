# Dashboarding Script
# by: Robert McGuinn
# date started: 2014-11-25

##### DataProvider #####
z = "DataProvider"
x <-which(colnames(tdata) == z)

xout <- as.data.frame(table(tdata[x], useNA="always"))
xout <- transform(xout, relative = prop.table(Freq))
xout <-xout[order(xout$Freq),] 
table<-head(xout[order(-xout$Freq), ], n=100)
colnames(table) <- c("DataProvider", "Count", "Proportion")
write.csv(table[,c("DataProvider", "Count", "Proportion")], 
          row.names = F, quote = T,"DataProvider.csv")

table <- table[1:30,c("DataProvider", "Count", "Proportion")]
sort <-table[order(table$Proportion),]
pdf("DataProvider.pdf",width=10,height=7)
par(mar=c(8,20,5,5))
par(mgp=c(4, 1, 0))
barplot(sort$Count, names.arg = sort$DataProvider, las=2, cex.names=.75, horiz=TRUE, main=z, xlab = "Number of Records") 
dev.off()

##### SamplingEquipment #####
z = "SamplingEquipment"
x <-which(colnames(tdata) == z)

xout <- as.data.frame(table(tdata[x], useNA="always"))
xout <- transform(xout, relative = prop.table(Freq))
xout <-xout[order(xout$Freq),] 
table<-head(xout[order(-xout$Freq), ], n=100)
colnames(table) <- c("SamplingEquipment", "Count", "Proportion")
write.csv(table[,c("SamplingEquipment", "Count", "Proportion")], 
          row.names = F, quote = T,"SamplingEquipment.csv")

table <- table[1:30,c("SamplingEquipment", "Count", "Proportion")]
sort <-table[order(table$Proportion),]
pdf("SamplingEquipment.pdf",width=10,height=7)
par(mar=c(8,20,5,5))
par(mgp=c(4, 1, 0))
barplot(sort$Count, names.arg = sort$SamplingEquipment, las=2, cex.names=.75, horiz=TRUE, main=z, xlab = "Number of Records") 
dev.off()

##### Vessel #####
z = "Vessel"
x <-which(colnames(tdata) == z)

xout <- as.data.frame(table(tdata[x], useNA="always"))
xout <- transform(xout, relative = prop.table(Freq))
xout <-xout[order(xout$Freq),] 
table<-head(xout[order(-xout$Freq), ], n=100)
colnames(table) <- c("Vessel", "Count", "Proportion")
write.csv(table[,c("Vessel", "Count", "Proportion")], 
          row.names = F, quote = T,"Vessel.csv")

table <- table[1:30,c("Vessel", "Count", "Proportion")]
sort <-table[order(table$Proportion),]
pdf("Vessel.pdf",width=10,height=7)
par(mar=c(8,20,5,5))
par(mgp=c(4, 1, 0))
barplot(sort$Count, names.arg = sort$Vessel, las=2, cex.names=.75, horiz=TRUE, main=z, xlab = "Number of Records") 
dev.off()

##### SurveyID #####
z = "SurveyID"
x <-which(colnames(tdata) == z)

xout <- as.data.frame(table(tdata[x], useNA="always"))
xout <- transform(xout, relative = prop.table(Freq))
xout <-xout[order(xout$Freq),] 
table<-head(xout[order(-xout$Freq), ], n=100)
colnames(table) <- c("SurveyID", "Count", "Proportion")
write.csv(table[,c("SurveyID", "Count", "Proportion")], 
          row.names = F, quote = T,"SurveyID.csv")

table <- table[1:30,c("SurveyID", "Count", "Proportion")]
sort <-table[order(table$Proportion),]
pdf("SurveyID.pdf",width=10,height=7)
par(mar=c(8,20,5,5))
par(mgp=c(4, 1, 0))
barplot(sort$Count, names.arg = sort$SurveyID, las=2, cex.names=.75, horiz=TRUE, main=z, xlab = "Number of Records") 
dev.off()

##### EventID #####
z = "EventID"
x <-which(colnames(tdata) == z)

xout <- as.data.frame(table(tdata[x], useNA="always"))
xout <- transform(xout, relative = prop.table(Freq))
xout <-xout[order(xout$Freq),] 
table<-head(xout[order(-xout$Freq), ], n=100)
colnames(table) <- c("EventID", "Count", "Proportion")
write.csv(table[,c("EventID", "Count", "Proportion")], 
          row.names = F, quote = T,"EventID.csv")

table <- table[1:30,c("EventID", "Count", "Proportion")]
sort <-table[order(table$Proportion),]
pdf("EventID.pdf",width=10,height=7)
par(mar=c(8,20,5,5))
par(mgp=c(4, 1, 0))
barplot(sort$Count, names.arg = sort$EventID, las=2, cex.names=.75, horiz=TRUE, main=z, xlab = "Number of Records") 
dev.off()

##### Citation #####
z = "Citation"
x <-which(colnames(tdata) == z)

xout <- as.data.frame(table(tdata[x], useNA="always"))
xout <- transform(xout, relative = prop.table(Freq))
xout <-xout[order(xout$Freq),] 
table<-head(xout[order(-xout$Freq), ], n=100)
colnames(table) <- c("Citation", "Count", "Proportion")
write.csv(table[,c("Citation", "Count", "Proportion")], 
          row.names = F, quote = T,"Citation.csv")

table <- table[1:30,c("Citation", "Count", "Proportion")]
sort <-table[order(table$Proportion),]
pdf("Citation.pdf",width=10,height=7)
par(mar=c(8,40,5,5))
par(mgp=c(4, 1, 0))
barplot(sort$Count, names.arg = sort$Citation, las=2, cex.names=.45, horiz=TRUE, main=z, xlab = "Number of Records") 
dev.off()

##### ScientificName #####
z = "ScientificName"
x <-which(colnames(tdata) == z)

xout <- as.data.frame(table(tdata[x], useNA="always"))
xout <- transform(xout, relative = prop.table(Freq))
xout <-xout[order(xout$Freq),] 
table<-head(xout[order(-xout$Freq), ], n=100)
colnames(table) <- c("ScientificName", "Count", "Proportion")
write.csv(table[,c("ScientificName", "Count", "Proportion")], 
          row.names = F, quote = T,"ScientificName.csv")

table <- table[1:30,c("ScientificName", "Count", "Proportion")]
sort <-table[order(table$Proportion),]
pdf("ScientificName.pdf",width=10,height=7)
par(mar=c(8,20,5,5))
par(mgp=c(4, 1, 0))
barplot(sort$Count, names.arg = sort$ScientificName, las=2, cex.names=.75, horiz=TRUE, main=z, xlab = "Number of Records") 
dev.off()

##### Genus #####
z = "Genus"
x <-which(colnames(tdata) == z)

xout <- as.data.frame(table(tdata[x], useNA="always"))
xout <- transform(xout, relative = prop.table(Freq))
xout <-xout[order(xout$Freq),] 
table<-head(xout[order(-xout$Freq), ], n=100)
colnames(table) <- c("Genus", "Count", "Proportion")
write.csv(table[,c("Genus", "Count", "Proportion")], 
          row.names = F, quote = T,"Genus.csv")

table <- table[1:30,c("Genus", "Count", "Proportion")]
sort <-table[order(table$Proportion),]
pdf("Genus.pdf",width=10,height=7)
par(mar=c(8,20,5,5))
par(mgp=c(4, 1, 0))
barplot(sort$Count, names.arg = sort$Genus, las=2, cex.names=.75, horiz=TRUE, main=z, xlab = "Number of Records") 
dev.off()

##### Family #####
z = "Family"
x <-which(colnames(tdata) == z)

xout <- as.data.frame(table(tdata[x], useNA="always"))
xout <- transform(xout, relative = prop.table(Freq))
xout <-xout[order(xout$Freq),] 
table<-head(xout[order(-xout$Freq), ], n=100)
colnames(table) <- c("Family", "Count", "Proportion")
write.csv(table[,c("Family", "Count", "Proportion")], 
          row.names = F, quote = T,"Family.csv")

table <- table[1:30,c("Family", "Count", "Proportion")]
sort <-table[order(table$Proportion),]
pdf("Family.pdf",width=10,height=7)
par(mar=c(8,20,5,5))
par(mgp=c(4, 1, 0))
barplot(sort$Count, names.arg = sort$Family, las=2, cex.names=.75, horiz=TRUE, main=z, xlab = "Number of Records") 
dev.off()

##### Order #####
z = "Order"
x <-which(colnames(tdata) == z)

xout <- as.data.frame(table(tdata[x], useNA="always"))
xout <- transform(xout, relative = prop.table(Freq))
xout <-xout[order(xout$Freq),] 
table<-head(xout[order(-xout$Freq), ], n=100)
colnames(table) <- c("Order", "Count", "Proportion")
write.csv(table[,c("Order", "Count", "Proportion")], 
          row.names = F, quote = T,"Order.csv")

table <- table[1:30,c("Order", "Count", "Proportion")]
sort <-table[order(table$Proportion),]
pdf("Order.pdf",width=10,height=7)
par(mar=c(8,20,5,5))
par(mgp=c(4, 1, 0))
barplot(sort$Count, names.arg = sort$Order, las=2, cex.names=.75, horiz=TRUE, main=z, xlab = "Number of Records") 
dev.off()

##### TaxonRank #####
z = "TaxonRank"
x <-which(colnames(tdata) == z)

xout <- as.data.frame(table(tdata[x], useNA="always"))
xout <- transform(xout, relative = prop.table(Freq))
xout <-xout[order(xout$Freq),] 
table<-head(xout[order(-xout$Freq), ], n=100)
colnames(table) <- c("TaxonRank", "Count", "Proportion")
write.csv(table[,c("TaxonRank", "Count", "Proportion")], 
          row.names = F, quote = T,"TaxonRank.csv")

table <- table[1:30,c("TaxonRank", "Count", "Proportion")]
sort <-table[order(table$Proportion),]
pdf("TaxonRank.pdf",width=10,height=7)
par(mar=c(8,20,5,5))
par(mgp=c(4, 1, 0))
barplot(sort$Count, names.arg = sort$TaxonRank, las=2, cex.names=.75, horiz=TRUE, main=z, xlab = "Number of Records") 
dev.off()

##### Class #####
z = "Class"
x <-which(colnames(tdata) == z)

xout <- as.data.frame(table(tdata[x], useNA="always"))
xout <- transform(xout, relative = prop.table(Freq))
xout <-xout[order(xout$Freq),] 
table<-head(xout[order(-xout$Freq), ], n=100)
colnames(table) <- c("Class", "Count", "Proportion")
write.csv(table[,c("Class", "Count", "Proportion")], 
          row.names = F, quote = T,"Class.csv")

table <- table[1:30,c("Class", "Count", "Proportion")]
sort <-table[order(table$Proportion),]
pdf("Class.pdf",width=10,height=7)
par(mar=c(8,20,5,5))
par(mgp=c(4, 1, 0))
barplot(sort$Count, names.arg = sort$Class, las=2, cex.names=.75, horiz=TRUE, main=z, xlab = "Number of Records") 
dev.off()

##### Flag #####
z = "Flag"
x <-which(colnames(tdata) == z)

xout <- as.data.frame(table(tdata[x], useNA="always"))
xout <- transform(xout, relative = prop.table(Freq))
xout <-xout[order(xout$Freq),] 
table<-head(xout[order(-xout$Freq), ], n=100)
colnames(table) <- c("Flag", "Count", "Proportion")
write.csv(table[,c("Flag", "Count", "Proportion")], 
          row.names = F, quote = T,"Flag.csv")

table <- table[1:30,c("Flag", "Count", "Proportion")]
sort <-table[order(table$Proportion),]
pdf("Flag.pdf",width=10,height=7)
par(mar=c(8,20,5,5))
par(mgp=c(4, 1, 0))
barplot(sort$Count, names.arg = sort$Flag, las=2, cex.names=.75, horiz=TRUE, main=z, xlab = "Number of Records") 
dev.off()

##### FlagReason #####
z = "FlagReason"
x <-which(colnames(tdata) == z)

xout <- as.data.frame(table(tdata[x], useNA="always"))
xout <- transform(xout, relative = prop.table(Freq))
xout <-xout[order(xout$Freq),] 
table<-head(xout[order(-xout$Freq), ], n=100)
colnames(table) <- c("FlagReason", "Count", "Proportion")
write.csv(table[,c("FlagReason", "Count", "Proportion")], 
          row.names = F, quote = T,"FlagReason.csv")

table <- table[1:30,c("FlagReason", "Count", "Proportion")]
sort <-table[order(table$Proportion),]
pdf("FlagReason.pdf",width=10,height=7)
par(mar=c(8,35,5,5))
par(mgp=c(4, 1, 0))
barplot(sort$Count, names.arg = sort$FlagReason, las=2, cex.names=.75, horiz=TRUE, main=z, xlab = "Number of Records") 
dev.off()

##### LocationAccuracy #####
z = "LocationAccuracy"
x <-which(colnames(tdata) == z)

xout <- as.data.frame(table(tdata[x], useNA="always"))
xout <- transform(xout, relative = prop.table(Freq))
xout <-xout[order(xout$Freq),] 
table<-head(xout[order(-xout$Freq), ], n=100)
colnames(table) <- c("LocationAccuracy", "Count", "Proportion")
write.csv(table[,c("LocationAccuracy", "Count", "Proportion")], 
          row.names = F, quote = T,"LocationAccuracy.csv")

table <- table[1:30,c("LocationAccuracy", "Count", "Proportion")]
sort <-table[order(table$Proportion),]
pdf("LocationAccuracy.pdf",width=10,height=7)
par(mar=c(8,15,5,5))
par(mgp=c(4, 1, 0))
barplot(sort$Count, names.arg = sort$LocationAccuracy, las=2, cex.names=.75, horiz=TRUE, main=z, xlab = "Number of Records") 
dev.off()

##### FishCouncilRegion #####
z = "FishCouncilRegion"
x <-which(colnames(tdata) == z)

xout <- as.data.frame(table(tdata[x], useNA="always"))
xout <- transform(xout, relative = prop.table(Freq))
xout <-xout[order(xout$Freq),] 
table<-head(xout[order(-xout$Freq), ], n=100)
colnames(table) <- c("FishCouncilRegion", "Count", "Proportion")
write.csv(table[,c("FishCouncilRegion", "Count", "Proportion")], 
          row.names = F, quote = T,"FishCouncilRegion.csv")

table <- table[1:30,c("FishCouncilRegion", "Count", "Proportion")]
sort <-table[order(table$Proportion),]
pdf("FishCouncilRegion.pdf",width=10,height=7)
par(mar=c(8,15,5,5))
par(mgp=c(4, 1, 0))
barplot(sort$Count, names.arg = sort$FishCouncilRegion, las=2, cex.names=.75, horiz=TRUE, main=z, xlab = "Number of Records") 
dev.off()

##### VernacularNameCategory #####
z = "VernacularNameCategory"
x <-which(colnames(tdata) == z)

xout <- as.data.frame(table(tdata[x], useNA="always"))
xout <- transform(xout, relative = prop.table(Freq))
xout <-xout[order(xout$Freq),] 
table<-head(xout[order(-xout$Freq), ], n=100)
colnames(table) <- c("VernacularNameCategory", "Count", "Proportion")
write.csv(table[,c("VernacularNameCategory", "Count", "Proportion")], 
          row.names = F, quote = T,"VernacularNameCategory.csv")

table <- table[1:30,c("VernacularNameCategory", "Count", "Proportion")]
sort <-table[order(table$Proportion),]
pdf("VernacularNameCategory.pdf",width=10,height=7)
par(mar=c(8,15,5,5))
par(mgp=c(4, 1, 0))
barplot(sort$Count, names.arg = sort$VernacularNameCategory, las=2, cex.names=.75, horiz=TRUE, main=z, xlab = "Number of Records") 
dev.off()

##### VernacularName #####
z = "VernacularName"
x <-which(colnames(tdata) == z)

xout <- as.data.frame(table(tdata[x], useNA="always"))
xout <- transform(xout, relative = prop.table(Freq))
xout <-xout[order(xout$Freq),] 
table<-head(xout[order(-xout$Freq), ], n=100)
colnames(table) <- c("VernacularName", "Count", "Proportion")
write.csv(table[,c("VernacularName", "Count", "Proportion")], 
          row.names = F, quote = T,"VernacularName.csv")

table <- table[1:30,c("VernacularName", "Count", "Proportion")]
sort <-table[order(table$Proportion),]
pdf("VernacularName.pdf",width=10,height=7)
par(mar=c(8,15,5,5))
par(mgp=c(4, 1, 0))
barplot(sort$Count, names.arg = sort$VernacularName, las=2, cex.names=.75, horiz=TRUE, main=z, xlab = "Number of Records") 
dev.off()

##### NavType #####
z = "NavType"
x <-which(colnames(tdata) == z)

xout <- as.data.frame(table(tdata[x], useNA="always"))
xout <- transform(xout, relative = prop.table(Freq))
xout <-xout[order(xout$Freq),] 
table<-head(xout[order(-xout$Freq), ], n=100)
colnames(table) <- c("NavType", "Count", "Proportion")
write.csv(table[,c("NavType", "Count", "Proportion")], 
          row.names = F, quote = T,"NavType.csv")

table <- table[1:30,c("NavType", "Count", "Proportion")]
sort <-table[order(table$Proportion),]
pdf("NavType.pdf",width=10,height=7)
par(mar=c(8,25,5,5))
par(mgp=c(4, 1, 0))
barplot(sort$Count, names.arg = sort$NavType, las=2, cex.names=.75, horiz=TRUE, main=z, xlab = "Number of Records") 
dev.off()

##### PI #####
z = "PI"
x <-which(colnames(tdata) == z)

xout <- as.data.frame(table(tdata[x], useNA="always"))
xout <- transform(xout, relative = prop.table(Freq))
xout <-xout[order(xout$Freq),] 
table<-head(xout[order(-xout$Freq), ], n=100)
colnames(table) <- c("PI", "Count", "Proportion")
write.csv(table[,c("PI", "Count", "Proportion")], 
          row.names = F, quote = T,"PI.csv")

table <- table[1:30,c("PI", "Count", "Proportion")]
sort <-table[order(table$Proportion),]
pdf("PI.pdf",width=10,height=7)
par(mar=c(8,20,5,5))
par(mgp=c(4, 1, 0))
barplot(sort$Count, names.arg = sort$PI, las=2, cex.names=.75, horiz=TRUE, main=z, xlab = "Number of Records") 
dev.off()

##### Substrate #####
z = "Substrate"
x <-which(colnames(tdata) == z)

xout <- as.data.frame(table(tdata[x], useNA="always"))
xout <- transform(xout, relative = prop.table(Freq))
xout <-xout[order(xout$Freq),] 
table<-head(xout[order(-xout$Freq), ], n=100)
colnames(table) <- c("Substrate", "Count", "Proportion")
write.csv(table[,c("Substrate", "Count", "Proportion")], 
          row.names = F, quote = T,"Substrate.csv")

table <- table[1:30,c("Substrate", "Count", "Proportion")]
sort <-table[order(table$Proportion),]
pdf("Substrate.pdf",width=10,height=7)
par(mar=c(8,20,5,5))
par(mgp=c(4, 1, 0))
barplot(sort$Count, names.arg = sort$Substrate, las=2, cex.names=.75, horiz=TRUE, main=z, xlab = "Number of Records") 
dev.off()

##### Habitat #####
z = "Habitat"
x <-which(colnames(tdata) == z)

xout <- as.data.frame(table(tdata[x], useNA="always"))
xout <- transform(xout, relative = prop.table(Freq))
xout <-xout[order(xout$Freq),] 
table<-head(xout[order(-xout$Freq), ], n=100)
colnames(table) <- c("Habitat", "Count", "Proportion")
write.csv(table[,c("Habitat", "Count", "Proportion")], 
          row.names = F, quote = T,"Habitat.csv")

table <- table[1:30,c("Habitat", "Count", "Proportion")]
sort <-table[order(table$Proportion),]
pdf("Habitat.pdf",width=10,height=7)
par(mar=c(8,25,5,5))
par(mgp=c(4, 1, 0))
barplot(sort$Count, names.arg = sort$Habitat, las=2, cex.names=.75, horiz=TRUE, main=z, xlab = "Number of Records") 
dev.off()

##### RecordType #####
z = "RecordType"
x <-which(colnames(tdata) == z)

xout <- as.data.frame(table(tdata[x], useNA="always"))
xout <- transform(xout, relative = prop.table(Freq))
xout <-xout[order(xout$Freq),] 
table<-head(xout[order(-xout$Freq), ], n=100)
colnames(table) <- c("RecordType", "Count", "Proportion")
write.csv(table[,c("RecordType", "Count", "Proportion")], 
          row.names = F, quote = T,"RecordType.csv")

table <- table[1:30,c("RecordType", "Count", "Proportion")]
sort <-table[order(table$Proportion),]
pdf("RecordType.pdf",width=10,height=7)
par(mar=c(8,25,5,5))
par(mgp=c(4, 1, 0))
barplot(sort$Count, names.arg = sort$RecordType, las=2, cex.names=.75, horiz=TRUE, main=z, xlab = "Number of Records") 
dev.off()

##### Repository #####
z = "Repository"
x <-which(colnames(tdata) == z)

xout <- as.data.frame(table(tdata[x], useNA="always"))
xout <- transform(xout, relative = prop.table(Freq))
xout <-xout[order(xout$Freq),] 
table<-head(xout[order(-xout$Freq), ], n=100)
colnames(table) <- c("Repository", "Count", "Proportion")
write.csv(table[,c("Repository", "Count", "Proportion")], 
          row.names = F, quote = T,"Repository.csv")

table <- table[1:30,c("Repository", "Count", "Proportion")]
sort <-table[order(table$Proportion),]
pdf("Repository.pdf",width=10,height=7)
par(mar=c(8,35,5,5))
par(mgp=c(4, 1, 0))
barplot(sort$Count, names.arg = sort$Repository, las=2, cex.names=.75, horiz=TRUE, main=z, xlab = "Number of Records") 
dev.off()

##### Ocean #####
z = "Ocean"
x <-which(colnames(tdata) == z)

xout <- as.data.frame(table(tdata[x], useNA="always"))
xout <- transform(xout, relative = prop.table(Freq))
xout <-xout[order(xout$Freq),] 
table<-head(xout[order(-xout$Freq), ], n=100)
colnames(table) <- c("Ocean", "Count", "Proportion")
write.csv(table[,c("Ocean", "Count", "Proportion")], 
          row.names = F, quote = T,"Ocean.csv")

table <- table[1:30,c("Ocean", "Count", "Proportion")]
sort <-table[order(table$Proportion),]
pdf("Ocean.pdf",width=10,height=7)
par(mar=c(8,20,5,5))
par(mgp=c(4, 1, 0))
barplot(sort$Count, names.arg = sort$Ocean, las=2, cex.names=.75, horiz=TRUE, main=z, xlab = "Number of Records") 
dev.off()

##### Locality #####
z = "Locality"
x <-which(colnames(tdata) == z)

xout <- as.data.frame(table(tdata[x], useNA="always"))
xout <- transform(xout, relative = prop.table(Freq))
xout <-xout[order(xout$Freq),] 
table<-head(xout[order(-xout$Freq), ], n=100)
colnames(table) <- c("Locality", "Count", "Proportion")
write.csv(table[,c("Locality", "Count", "Proportion")], 
          row.names = F, quote = T,"Locality.csv")

table <- table[1:30,c("Locality", "Count", "Proportion")]
sort <-table[order(table$Proportion),]
pdf("Locality.pdf",width=10,height=7)
par(mar=c(8,20,5,5))
par(mgp=c(4, 1, 0))
barplot(sort$Count, names.arg = sort$Locality, las=2, cex.names=.75, horiz=TRUE, main=z, xlab = "Number of Records") 
dev.off()

##### SizeCategory #####
z = "SizeCategory"
x <-which(colnames(tdata) == z)

xout <- as.data.frame(table(tdata[x], useNA="always"))
xout <- transform(xout, relative = prop.table(Freq))
xout <-xout[order(xout$Freq),] 
table<-head(xout[order(-xout$Freq), ], n=100)
colnames(table) <- c("SizeCategory", "Count", "Proportion")
write.csv(table[,c("SizeCategory", "Count", "Proportion")], 
          row.names = F, quote = T,"SizeCategory.csv")

table <- table[1:30,c("SizeCategory", "Count", "Proportion")]
sort <-table[order(table$Proportion),]
pdf("SizeCategory.pdf",width=10,height=7)
par(mar=c(8,20,5,5))
par(mgp=c(4, 1, 0))
barplot(sort$Count, names.arg = sort$SizeCategory, las=2, cex.names=.75, horiz=TRUE, main=z, xlab = "Number of Records") 
dev.off()

##### Condition #####
z = "Condition"
x <-which(colnames(tdata) == z)

xout <- as.data.frame(table(tdata[x], useNA="always"))
xout <- transform(xout, relative = prop.table(Freq))
xout <-xout[order(xout$Freq),] 
table<-head(xout[order(-xout$Freq), ], n=100)
colnames(table) <- c("Condition", "Count", "Proportion")
write.csv(table[,c("Condition", "Count", "Proportion")], 
          row.names = F, quote = T,"Condition.csv")

table <- table[1:30,c("Condition", "Count", "Proportion")]
sort <-table[order(table$Proportion),]
pdf("Condition.pdf",width=10,height=7)
par(mar=c(8,20,5,5))
par(mgp=c(4, 1, 0))
barplot(sort$Count, names.arg = sort$Condition, las=2, cex.names=.75, horiz=TRUE, main=z, xlab = "Number of Records") 
dev.off()

##### VehicleName #####
z = "VehicleName"
x <-which(colnames(tdata) == z)

xout <- as.data.frame(table(tdata[x], useNA="always"))
xout <- transform(xout, relative = prop.table(Freq))
xout <-xout[order(xout$Freq),] 
table<-head(xout[order(-xout$Freq), ], n=100)
colnames(table) <- c("VehicleName", "Count", "Proportion")
write.csv(table[,c("VehicleName", "Count", "Proportion")], 
          row.names = F, quote = T,"VehicleName.csv")

table <- table[1:30,c("VehicleName", "Count", "Proportion")]
sort <-table[order(table$Proportion),]
pdf("VehicleName.pdf",width=10,height=7)
par(mar=c(8,20,5,5))
par(mgp=c(4, 1, 0))
barplot(sort$Count, names.arg = sort$VehicleName, las=2, cex.names=.75, horiz=TRUE, main=z, xlab = "Number of Records") 
dev.off()

##### Synonyms #####
z = "Synonyms"
x <-which(colnames(tdata) == z)

xout <- as.data.frame(table(tdata[x], useNA="always"))
xout <- transform(xout, relative = prop.table(Freq))
xout <-xout[order(xout$Freq),] 
table<-head(xout[order(-xout$Freq), ], n=100)
colnames(table) <- c("Synonyms", "Count", "Proportion")
write.csv(table[,c("Synonyms", "Count", "Proportion")], 
          row.names = F, quote = T,"Synonyms.csv")

table <- table[1:30,c("Synonyms", "Count", "Proportion")]
sort <-table[order(table$Proportion),]
pdf("Synonyms.pdf",width=10,height=7)
par(mar=c(8,20,5,5))
par(mgp=c(4, 1, 0))
barplot(sort$Count, names.arg = sort$Synonyms, las=2, cex.names=.75, horiz=TRUE, main=z, xlab = "Number of Records") 
dev.off()

##### gislocality #####
z = "gislocality"
x <-which(colnames(tdata) == z)

xout <- as.data.frame(table(tdata[x], useNA="always"))
xout <- transform(xout, relative = prop.table(Freq))
xout <-xout[order(xout$Freq),] 
table<-head(xout[order(-xout$Freq), ], n=100)
colnames(table) <- c("gislocality", "Count", "Proportion")
write.csv(table[,c("gislocality", "Count", "Proportion")], 
          row.names = F, quote = T,"gislocality.csv")

table <- table[1:30,c("gislocality", "Count", "Proportion")]
sort <-table[order(table$Proportion),]
pdf("gislocality.pdf",width=10,height=7)
par(mar=c(8,20,5,5))
par(mgp=c(4, 1, 0))
barplot(sort$Count, names.arg = sort$gislocality, las=2, cex.names=.75, horiz=TRUE, main=z, xlab = "Number of Records") 
dev.off()

##### ObservationYear #####
z = "ObservationYear"
x <-which(colnames(tdata) == z)

xout <- as.data.frame(table(tdata[x], useNA="always"))
xout <- transform(xout, relative = prop.table(Freq))
xout <-xout[order(xout$Freq),] 
table<-head(xout[order(-xout$Freq), ], n=100)
colnames(table) <- c("ObservationYear", "Count", "Proportion")
write.csv(table[,c("ObservationYear", "Count", "Proportion")], 
          row.names = F, quote = T,"ObservationYear.csv")

table <- table[1:30,c("ObservationYear", "Count", "Proportion")]
sort <-table[order(table$Proportion),]
pdf("ObservationYear.pdf",width=10,height=7)
par(mar=c(8,20,5,5))
par(mgp=c(4, 1, 0))
barplot(sort$Count, names.arg = sort$ObservationYear, las=2, cex.names=.75, horiz=TRUE, main=z, xlab = "Number of Records") 
dev.off()

pdf("ObservationDateHist.pdf",width=10,height=7)
par(mar=c(8,20,5,5))
par(mgp=c(4, 1, 0))
hist(as.numeric(tdata$ObservationDate)) 
dev.off()

##### gisregion #####
z = "gisregion"
x <-which(colnames(tdata) == z)

xout <- as.data.frame(table(tdata[x], useNA="always"))
xout <- transform(xout, relative = prop.table(Freq))
xout <-xout[order(xout$Freq),] 
table<-head(xout[order(-xout$Freq), ], n=100)
colnames(table) <- c("gisregion", "Count", "Proportion")
write.csv(table[,c("gisregion", "Count", "Proportion")], 
          row.names = F, quote = T,"gisregion.csv")

table <- table[1:30,c("gisregion", "Count", "Proportion")]
sort <-table[order(table$Proportion),]
pdf("gisregion.pdf",width=10,height=7)
par(mar=c(8,20,5,5))
par(mgp=c(4, 1, 0))
barplot(sort$Count, names.arg = sort$gisregion, las=2, cex.names=.75, horiz=TRUE, main=z, xlab = "Number of Records") 
dev.off()

##### gisdepth #####
z = "gisdepth"
x <-which(colnames(tdata) == z)

xout <- as.data.frame(table(tdata[x], useNA="always"))
xout <- transform(xout, relative = prop.table(Freq))
xout <-xout[order(xout$Freq),] 
table<-head(xout[order(-xout$Freq), ], n=100)
colnames(table) <- c("gisdepth", "Count", "Proportion")
write.csv(table[,c("gisdepth", "Count", "Proportion")], 
          row.names = F, quote = T,"gisdepth.csv")

table <- table[1:30,c("gisdepth", "Count", "Proportion")]
sort <-table[order(table$Proportion),]
pdf("gisdepth.pdf",width=10,height=7)
par(mar=c(8,20,5,5))
par(mgp=c(4, 1, 0))
barplot(sort$Count, names.arg = sort$gisdepth, las=2, cex.names=.75, horiz=TRUE, main=z, xlab = "Number of Records") 
dev.off()

pdf("gisdepthHist.pdf",width=10,height=7)
par(mar=c(8,20,5,5))
par(mgp=c(4, 1, 0))
hist(as.numeric(tdata$gisdepth)) 
dev.off()

##### MinimumDepthInMeters #####
z = "MinimumDepthInMeters"
x <-which(colnames(tdata) == z)

xout <- as.data.frame(table(tdata[x], useNA="always"))
xout <- transform(xout, relative = prop.table(Freq))
xout <-xout[order(xout$Freq),] 
table<-head(xout[order(-xout$Freq), ], n=100)
colnames(table) <- c("MinimumDepthInMeters", "Count", "Proportion")
write.csv(table[,c("MinimumDepthInMeters", "Count", "Proportion")], 
          row.names = F, quote = T,"MinimumDepthInMeters.csv")

table <- table[1:30,c("MinimumDepthInMeters", "Count", "Proportion")]
sort <-table[order(table$Proportion),]
pdf("MinimumDepthInMeters.pdf",width=10,height=7)
par(mar=c(8,20,5,5))
par(mgp=c(4, 1, 0))
barplot(sort$Count, names.arg = sort$MinimumDepthInMeters, las=2, cex.names=.75, horiz=TRUE, main=z, xlab = "Number of Records") 
dev.off()

pdf("MinimumDepthInMetersHist.pdf",width=10,height=7)
par(mar=c(8,20,5,5))
par(mgp=c(4, 1, 0))
hist(as.numeric(tdata$MinimumDepthInMeters)) 
dev.off()

##### MaximumDepthInMeters #####
z = "MaximumDepthInMeters"
x <-which(colnames(tdata) == z)

xout <- as.data.frame(table(tdata[x], useNA="always"))
xout <- transform(xout, relative = prop.table(Freq))
xout <-xout[order(xout$Freq),] 
table<-head(xout[order(-xout$Freq), ], n=100)
colnames(table) <- c("MaximumDepthInMeters", "Count", "Proportion")
write.csv(table[,c("MaximumDepthInMeters", "Count", "Proportion")], 
          row.names = F, quote = T,"MaximumDepthInMeters.csv")

table <- table[1:30,c("MaximumDepthInMeters", "Count", "Proportion")]
sort <-table[order(table$Proportion),]
pdf("MaximumDepthInMeters.pdf",width=10,height=7)
par(mar=c(8,20,5,5))
par(mgp=c(4, 1, 0))
barplot(sort$Count, names.arg = sort$MaximumDepthInMeters, las=2, cex.names=.75, horiz=TRUE, main=z, xlab = "Number of Records") 
dev.off()

pdf("MaximumDepthInMetersHist.pdf",width=10,height=7)
par(mar=c(8,20,5,5))
par(mgp=c(4, 1, 0))
hist(as.numeric(tdata$MaximumDepthInMeters)) 
dev.off()

##### gisocean #####
z = "gisocean"
x <-which(colnames(tdata) == z)

xout <- as.data.frame(table(tdata[x], useNA="always"))
xout <- transform(xout, relative = prop.table(Freq))
xout <-xout[order(xout$Freq),] 
table<-head(xout[order(-xout$Freq), ], n=100)
colnames(table) <- c("gisocean", "Count", "Proportion")
write.csv(table[,c("gisocean", "Count", "Proportion")], 
          row.names = F, quote = T,"gisocean.csv")

table <- table[1:30,c("gisocean", "Count", "Proportion")]
sort <-table[order(table$Proportion),]
pdf("gisocean.pdf",width=10,height=7)
par(mar=c(8,20,5,5))
par(mgp=c(4, 1, 0))
barplot(sort$Count, names.arg = sort$gisocean, las=2, cex.names=.75, horiz=TRUE, main=z, xlab = "Number of Records") 
dev.off()

##### SampleID #####
z = "SampleID"
x <-which(colnames(tdata) == z)

xout <- as.data.frame(table(tdata[x], useNA="always"))
xout <- transform(xout, relative = prop.table(Freq))
xout <-xout[order(xout$Freq),] 
table<-head(xout[order(-xout$Freq), ], n=100)
colnames(table) <- c("SampleID", "Count", "Proportion")
write.csv(table[,c("SampleID", "Count", "Proportion")], 
          row.names = F, quote = T,"SampleID.csv")

table <- table[1:30,c("SampleID", "Count", "Proportion")]
sort <-table[order(table$Proportion),]
pdf("SampleID.pdf",width=10,height=7)
par(mar=c(8,20,5,5))
par(mgp=c(4, 1, 0))
barplot(sort$Count, names.arg = sort$SampleID, las=2, cex.names=.75, horiz=TRUE, main=z, xlab = "Number of Records") 
dev.off()

##### TrackingID #####
z = "TrackingID"
x <-which(colnames(tdata) == z)

xout <- as.data.frame(table(tdata[x], useNA="always"))
xout <- transform(xout, relative = prop.table(Freq))
xout <-xout[order(xout$Freq),] 
table<-head(xout[order(-xout$Freq), ], n=100)
colnames(table) <- c("TrackingID", "Count", "Proportion")
write.csv(table[,c("TrackingID", "Count", "Proportion")], 
          row.names = F, quote = T,"TrackingID.csv")

table <- table[1:30,c("TrackingID", "Count", "Proportion")]
sort <-table[order(table$Proportion),]
pdf("TrackingID.pdf",width=10,height=7)
par(mar=c(8,20,5,5))
par(mgp=c(4, 1, 0))
barplot(sort$Count, names.arg = sort$TrackingID, las=2, cex.names=.75, horiz=TRUE, main=z, xlab = "Number of Records") 
dev.off()

##### CatalogNumber #####
z = "CatalogNumber"
x <-which(colnames(tdata) == z)

xout <- as.data.frame(table(tdata[x], useNA="always"))
xout <- transform(xout, relative = prop.table(Freq))
xout <-xout[order(xout$Freq),] 
table<-head(xout[order(-xout$Freq), ], n=100)
colnames(table) <- c("CatalogNumber", "Count", "Proportion")
write.csv(table[,c("CatalogNumber", "Count", "Proportion")], 
          row.names = F, quote = T,"CatalogNumber.csv")

table <- table[1:30,c("CatalogNumber", "Count", "Proportion")]
sort <-table[order(table$Proportion),]
pdf("CatalogNumber.pdf",width=10,height=7)
par(mar=c(8,20,5,5))
par(mgp=c(4, 1, 0))
barplot(sort$Count, names.arg = sort$CatalogNumber, las=2, cex.names=.75, horiz=TRUE, main=z, xlab = "Number of Records") 
dev.off()

##### LargeMarineEcosystem #####
z = "LargeMarineEcosystem"
x <-which(colnames(tdata) == z)

xout <- as.data.frame(table(tdata[x], useNA="always"))
xout <- transform(xout, relative = prop.table(Freq))
xout <-xout[order(xout$Freq),] 
table<-head(xout[order(-xout$Freq), ], n=100)
colnames(table) <- c("LargeMarineEcosystem", "Count", "Proportion")
write.csv(table[,c("LargeMarineEcosystem", "Count", "Proportion")], 
          row.names = F, quote = T,"LargeMarineEcosystem.csv")

table <- table[1:30,c("LargeMarineEcosystem", "Count", "Proportion")]
sort <-table[order(table$Proportion),]
pdf("LargeMarineEcosystem.pdf",width=10,height=7)
par(mar=c(8,20,5,5))
par(mgp=c(4, 1, 0))
barplot(sort$Count, names.arg = sort$LargeMarineEcosystem, las=2, cex.names=.75, horiz=TRUE, main=z, xlab = "Number of Records") 
dev.off()

##### LargeMarineEcosystem #####
z = "LargeMarineEcosystem"
x <-which(colnames(tdata) == z)

xout <- as.data.frame(table(tdata[x], useNA="always"))
xout <- transform(xout, relative = prop.table(Freq))
xout <-xout[order(xout$Freq),] 
table<-head(xout[order(-xout$Freq), ], n=100)
colnames(table) <- c("LargeMarineEcosystem", "Count", "Proportion")
write.csv(table[,c("LargeMarineEcosystem", "Count", "Proportion")], 
          row.names = F, quote = T,"LargeMarineEcosystem.csv")

table <- table[1:30,c("LargeMarineEcosystem", "Count", "Proportion")]
sort <-table[order(table$Proportion),]
pdf("LargeMarineEcosystem.pdf",width=10,height=7)
par(mar=c(8,20,5,5))
par(mgp=c(4, 1, 0))
barplot(sort$Count, names.arg = sort$LargeMarineEcosystem, las=2, cex.names=.75, horiz=TRUE, main=z, xlab = "Number of Records") 
dev.off()

##### gislme #####
z = "gislme"
x <-which(colnames(tdata) == z)

xout <- as.data.frame(table(tdata[x], useNA="always"))
xout <- transform(xout, relative = prop.table(Freq))
xout <-xout[order(xout$Freq),] 
table<-head(xout[order(-xout$Freq), ], n=100)
colnames(table) <- c("gislme", "Count", "Proportion")
write.csv(table[,c("gislme", "Count", "Proportion")], 
          row.names = F, quote = T,"gislme.csv")

table <- table[1:30,c("gislme", "Count", "Proportion")]
sort <-table[order(table$Proportion),]
pdf("gislme.pdf",width=10,height=7)
par(mar=c(8,20,5,5))
par(mgp=c(4, 1, 0))
barplot(sort$Count, names.arg = sort$gislme, las=2, cex.names=.75, horiz=TRUE, main=z, xlab = "Number of Records") 
dev.off()

##### AssociatedTaxa #####
z = "AssociatedTaxa"
x <-which(colnames(tdata) == z)

xout <- as.data.frame(table(tdata[x], useNA="always"))
xout <- transform(xout, relative = prop.table(Freq))
xout <-xout[order(xout$Freq),] 
table<-head(xout[order(-xout$Freq), ], n=100)
colnames(table) <- c("AssociatedTaxa", "Count", "Proportion")
write.csv(table[,c("AssociatedTaxa", "Count", "Proportion")], 
          row.names = F, quote = T,"AssociatedTaxa.csv")

table <- table[1:30,c("AssociatedTaxa", "Count", "Proportion")]
sort <-table[order(table$Proportion),]
pdf("AssociatedTaxa.pdf",width=10,height=7)
par(mar=c(8,20,5,5))
par(mgp=c(4, 1, 0))
barplot(sort$Count, names.arg = sort$AssociatedTaxa, las=2, cex.names=.75, horiz=TRUE, main=z, xlab = "Number of Records") 
dev.off()

##### LocationComment #####
z = "LocationComment"
x <-which(colnames(tdata) == z)

xout <- as.data.frame(table(tdata[x], useNA="always"))
xout <- transform(xout, relative = prop.table(Freq))
xout <-xout[order(xout$Freq),] 
table<-head(xout[order(-xout$Freq), ], n=100)
colnames(table) <- c("LocationComment", "Count", "Proportion")
write.csv(table[,c("LocationComment", "Count", "Proportion")], 
          row.names = F, quote = T,"LocationComment.csv")

table <- table[1:30,c("LocationComment", "Count", "Proportion")]
sort <-table[order(table$Proportion),]
pdf("LocationComment.pdf",width=10,height=7)
par(mar=c(8,40,5,5))
par(mgp=c(4, 1, 0))
barplot(sort$Count, names.arg = sort$LocationComment, las=2, cex.names=.75, horiz=TRUE, main=z, xlab = "Number of Records") 
dev.off()

##### LocationComment #####
z = "LocationComment"
x <-which(colnames(tdata) == z)

xout <- as.data.frame(table(tdata[x], useNA="always"))
xout <- transform(xout, relative = prop.table(Freq))
xout <-xout[order(xout$Freq),] 
table<-head(xout[order(-xout$Freq), ], n=100)
colnames(table) <- c("LocationComment", "Count", "Proportion")
write.csv(table[,c("LocationComment", "Count", "Proportion")], 
          row.names = F, quote = T,"LocationComment.csv")

table <- table[1:30,c("LocationComment", "Count", "Proportion")]
sort <-table[order(table$Proportion),]
pdf("LocationComment.pdf",width=10,height=7)
par(mar=c(8,40,5,5))
par(mgp=c(4, 1, 0))
barplot(sort$Count, names.arg = sort$LocationComment, las=2, cex.names=.75, horiz=TRUE, main=z, xlab = "Number of Records") 
dev.off()

##### OccurenceRemarks #####
z = "OccurenceRemarks"
x <-which(colnames(tdata) == z)

xout <- as.data.frame(table(tdata[x], useNA="always"))
xout <- transform(xout, relative = prop.table(Freq))
xout <-xout[order(xout$Freq),] 
table<-head(xout[order(-xout$Freq), ], n=100)
colnames(table) <- c("OccurenceRemarks", "Count", "Proportion")
write.csv(table[,c("OccurenceRemarks", "Count", "Proportion")], 
          row.names = F, quote = T,"OccurenceRemarks.csv")

table <- table[1:30,c("OccurenceRemarks", "Count", "Proportion")]
sort <-table[order(table$Proportion),]
pdf("OccurenceRemarks.pdf",width=10,height=7)
par(mar=c(8,40,5,5))
par(mgp=c(4, 1, 0))
barplot(sort$Count, names.arg = sort$OccurenceRemarks, las=2, cex.names=.75, horiz=TRUE, main=z, xlab = "Number of Records") 
dev.off()

##### Reporter #####
z = "Reporter"
x <-which(colnames(tdata) == z)

xout <- as.data.frame(table(tdata[x], useNA="always"))
xout <- transform(xout, relative = prop.table(Freq))
xout <-xout[order(xout$Freq),] 
table<-head(xout[order(-xout$Freq), ], n=100)
colnames(table) <- c("Reporter", "Count", "Proportion")
write.csv(table[,c("Reporter", "Count", "Proportion")], 
          row.names = F, quote = T,"Reporter.csv")

table <- table[1:30,c("Reporter", "Count", "Proportion")]
sort <-table[order(table$Proportion),]
pdf("Reporter.pdf",width=10,height=7)
par(mar=c(8,20,5,5))
par(mgp=c(4, 1, 0))
barplot(sort$Count, names.arg = sort$Reporter, las=2, cex.names=.75, horiz=TRUE, main=z, xlab = "Number of Records") 
dev.off()

##### ReporterEmail #####
z = "ReporterEmail"
x <-which(colnames(tdata) == z)

xout <- as.data.frame(table(tdata[x], useNA="always"))
xout <- transform(xout, relative = prop.table(Freq))
xout <-xout[order(xout$Freq),] 
table<-head(xout[order(-xout$Freq), ], n=100)
colnames(table) <- c("ReporterEmail", "Count", "Proportion")
write.csv(table[,c("ReporterEmail", "Count", "Proportion")], 
          row.names = F, quote = T,"ReporterEmail.csv")

table <- table[1:30,c("ReporterEmail", "Count", "Proportion")]
sort <-table[order(table$Proportion),]
pdf("ReporterEmail.pdf",width=10,height=7)
par(mar=c(8,20,5,5))
par(mgp=c(4, 1, 0))
barplot(sort$Count, names.arg = sort$ReporterEmail, las=2, cex.names=.75, horiz=TRUE, main=z, xlab = "Number of Records") 
dev.off()

##### Website #####
z = "Website"
x <-which(colnames(tdata) == z)

xout <- as.data.frame(table(tdata[x], useNA="always"))
xout <- transform(xout, relative = prop.table(Freq))
xout <-xout[order(xout$Freq),] 
table<-head(xout[order(-xout$Freq), ], n=100)
colnames(table) <- c("Website", "Count", "Proportion")
write.csv(table[,c("Website", "Count", "Proportion")], 
          row.names = F, quote = T,"Website.csv")

table <- table[1:30,c("Website", "Count", "Proportion")]
sort <-table[order(table$Proportion),]
pdf("Website.pdf",width=10,height=7)
par(mar=c(8,20,5,5))
par(mgp=c(4, 1, 0))
barplot(sort$Count, names.arg = sort$Website, las=2, cex.names=.75, horiz=TRUE, main=z, xlab = "Number of Records") 
dev.off()

##### EntryDate #####
z = "EntryDate"
x <-which(colnames(tdata) == z)

xout <- as.data.frame(table(tdata[x], useNA="always"))
xout <- transform(xout, relative = prop.table(Freq))
xout <-xout[order(xout$Freq),] 
table<-head(xout[order(-xout$Freq), ], n=100)
colnames(table) <- c("EntryDate", "Count", "Proportion")
write.csv(table[,c("EntryDate", "Count", "Proportion")], 
          row.names = F, quote = T,"EntryDate.csv")

table <- table[1:30,c("EntryDate", "Count", "Proportion")]
sort <-table[order(table$Proportion),]
pdf("EntryDate.pdf",width=10,height=7)
par(mar=c(8,20,5,5))
par(mgp=c(4, 1, 0))
barplot(sort$Count, names.arg = sort$EntryDate, las=2, cex.names=.75, horiz=TRUE, main=z, xlab = "Number of Records") 
dev.off()

##### CruiseComments #####
z = "CruiseComments"
x <-which(colnames(tdata) == z)

xout <- as.data.frame(table(tdata[x], useNA="always"))
xout <- transform(xout, relative = prop.table(Freq))
xout <-xout[order(xout$Freq),] 
table<-head(xout[order(-xout$Freq), ], n=100)
colnames(table) <- c("CruiseComments", "Count", "Proportion")
write.csv(table[,c("CruiseComments", "Count", "Proportion")], 
          row.names = F, quote = T,"CruiseComments.csv")

table <- table[1:30,c("CruiseComments", "Count", "Proportion")]
sort <-table[order(table$Proportion),]
pdf("CruiseComments.pdf",width=10,height=7)
par(mar=c(8,35,5,5))
par(mgp=c(4, 1, 0))
barplot(sort$Count, names.arg = sort$CruiseComments, las=2, cex.names=.75, horiz=TRUE, main=z, xlab = "Number of Records") 
dev.off()

##### Purpose #####
z = "Purpose"
x <-which(colnames(tdata) == z)

xout <- as.data.frame(table(tdata[x], useNA="always"))
xout <- transform(xout, relative = prop.table(Freq))
xout <-xout[order(xout$Freq),] 
table<-head(xout[order(-xout$Freq), ], n=100)
colnames(table) <- c("Purpose", "Count", "Proportion")
write.csv(table[,c("Purpose", "Count", "Proportion")], 
          row.names = F, quote = T,"Purpose.csv")

table <- table[1:30,c("Purpose", "Count", "Proportion")]
sort <-table[order(table$Proportion),]
pdf("Purpose.pdf",width=10,height=7)
par(mar=c(8,40,5,5))
par(mgp=c(4, 1, 0))
barplot(sort$Count, names.arg = sort$Purpose, las=2, cex.names=.75, horiz=TRUE, main=z, xlab = "Number of Records") 
dev.off()

##### Station #####
z = "Station"
x <-which(colnames(tdata) == z)

xout <- as.data.frame(table(tdata[x], useNA="always"))
xout <- transform(xout, relative = prop.table(Freq))
xout <-xout[order(xout$Freq),] 
table<-head(xout[order(-xout$Freq), ], n=100)
colnames(table) <- c("Station", "Count", "Proportion")
write.csv(table[,c("Station", "Count", "Proportion")], 
          row.names = F, quote = T,"Station.csv")

table <- table[1:30,c("Station", "Count", "Proportion")]
sort <-table[order(table$Proportion),]
pdf("Station.pdf",width=10,height=7)
par(mar=c(8,20,5,5))
par(mgp=c(4, 1, 0))
barplot(sort$Count, names.arg = sort$Station, las=2, cex.names=.75, horiz=TRUE, main=z, xlab = "Number of Records") 
dev.off()

##### WeightInKg #####
z = "WeightInKg"
x <-which(colnames(tdata) == z)

xout <- as.data.frame(table(tdata[x], useNA="always"))
xout <- transform(xout, relative = prop.table(Freq))
xout <-xout[order(xout$Freq),] 
table<-head(xout[order(-xout$Freq), ], n=100)
colnames(table) <- c("WeightInKg", "Count", "Proportion")
write.csv(table[,c("WeightInKg", "Count", "Proportion")], 
          row.names = F, quote = T,"WeightInKg.csv")

table <- table[1:30,c("WeightInKg", "Count", "Proportion")]
sort <-table[order(table$Proportion),]
pdf("WeightInKg.pdf",width=10,height=7)
par(mar=c(8,20,5,5))
par(mgp=c(4, 1, 0))
barplot(sort$Count, names.arg = sort$WeightInKg, las=2, cex.names=.75, horiz=TRUE, main=z, xlab = "Number of Records") 
dev.off()

##### Country #####
z = "Country"
x <-which(colnames(tdata) == z)

xout <- as.data.frame(table(tdata[x], useNA="always"))
xout <- transform(xout, relative = prop.table(Freq))
xout <-xout[order(xout$Freq),] 
table<-head(xout[order(-xout$Freq), ], n=100)
colnames(table) <- c("Country", "Count", "Proportion")
write.csv(table[,c("Country", "Count", "Proportion")], 
          row.names = F, quote = T,"Country.csv")

table <- table[1:30,c("Country", "Count", "Proportion")]
sort <-table[order(table$Proportion),]
pdf("Country.pdf",width=10,height=7)
par(mar=c(8,20,5,5))
par(mgp=c(4, 1, 0))
barplot(sort$Count, names.arg = sort$Country, las=2, cex.names=.75, horiz=TRUE, main=z, xlab = "Number of Records") 
dev.off()

##### IndividualCount #####
z = "IndividualCount"
x <-which(colnames(tdata) == z)

xout <- as.data.frame(table(tdata[x], useNA="always"))
xout <- transform(xout, relative = prop.table(Freq))
xout <-xout[order(xout$Freq),] 
table<-head(xout[order(-xout$Freq), ], n=100)
colnames(table) <- c("IndividualCount", "Count", "Proportion")
write.csv(table[,c("IndividualCount", "Count", "Proportion")], 
          row.names = F, quote = T,"IndividualCount.csv")

table <- table[1:30,c("IndividualCount", "Count", "Proportion")]
sort <-table[order(table$Proportion),]
pdf("IndividualCount.pdf",width=10,height=7)
par(mar=c(8,20,5,5))
par(mgp=c(4, 1, 0))
barplot(sort$Count, names.arg = sort$IndividualCount, las=2, cex.names=.75, horiz=TRUE, main=z, xlab = "Number of Records") 
dev.off()

##### Latitude #####
z = "Latitude"
x <-which(colnames(tdata) == z)

xout <- as.data.frame(table(tdata[x], useNA="always"))
xout <- transform(xout, relative = prop.table(Freq))
xout <-xout[order(xout$Freq),] 
table<-head(xout[order(-xout$Freq), ], n=100)
colnames(table) <- c("Latitude", "Count", "Proportion")
write.csv(table[,c("Latitude", "Count", "Proportion")], 
          row.names = F, quote = T,"Latitude.csv")

table <- table[1:30,c("Latitude", "Count", "Proportion")]
sort <-table[order(table$Proportion),]
pdf("Latitude.pdf",width=10,height=7)
par(mar=c(8,20,5,5))
par(mgp=c(4, 1, 0))
barplot(sort$Count, names.arg = sort$Latitude, las=2, cex.names=.75, horiz=TRUE, main=z, xlab = "Number of Records") 
dev.off()

pdf("LatitudeHist.pdf",width=10,height=7)
par(mar=c(8,20,5,5))
par(mgp=c(4, 1, 0))
hist(as.numeric(tdata$Latitude)) 
dev.off()

##### StartLatitude #####
z = "StartLatitude"
x <-which(colnames(tdata) == z)

xout <- as.data.frame(table(tdata[x], useNA="always"))
xout <- transform(xout, relative = prop.table(Freq))
xout <-xout[order(xout$Freq),] 
table<-head(xout[order(-xout$Freq), ], n=100)
colnames(table) <- c("StartLatitude", "Count", "Proportion")
write.csv(table[,c("StartLatitude", "Count", "Proportion")], 
          row.names = F, quote = T,"StartLatitude.csv")

table <- table[1:30,c("StartLatitude", "Count", "Proportion")]
sort <-table[order(table$Proportion),]
pdf("StartLatitude.pdf",width=10,height=7)
par(mar=c(8,20,5,5))
par(mgp=c(4, 1, 0))
barplot(sort$Count, names.arg = sort$StartLatitude, las=2, cex.names=.75, horiz=TRUE, main=z, xlab = "Number of Records") 
dev.off()

pdf("StartLatitudeHist.pdf",width=10,height=7)
par(mar=c(8,20,5,5))
par(mgp=c(4, 1, 0))
hist(as.numeric(tdata$StartLatitude)) 
dev.off()

##### StartLongitude #####
z = "StartLongitude"
x <-which(colnames(tdata) == z)

xout <- as.data.frame(table(tdata[x], useNA="always"))
xout <- transform(xout, relative = prop.table(Freq))
xout <-xout[order(xout$Freq),] 
table<-head(xout[order(-xout$Freq), ], n=100)
colnames(table) <- c("StartLongitude", "Count", "Proportion")
write.csv(table[,c("StartLongitude", "Count", "Proportion")], 
          row.names = F, quote = T,"StartLongitude.csv")

table <- table[1:30,c("StartLongitude", "Count", "Proportion")]
sort <-table[order(table$Proportion),]
pdf("StartLongitude.pdf",width=10,height=7)
par(mar=c(8,20,5,5))
par(mgp=c(4, 1, 0))
barplot(sort$Count, names.arg = sort$StartLongitude, las=2, cex.names=.75, horiz=TRUE, main=z, xlab = "Number of Records") 
dev.off()

pdf("StartLongitudeHist.pdf",width=10,height=7)
par(mar=c(8,20,5,5))
par(mgp=c(4, 1, 0))
hist(as.numeric(tdata$StartLongitude)) 
dev.off()

##### Temperature #####
z = "Temperature"
x <-which(colnames(tdata) == z)

xout <- as.data.frame(table(tdata[x], useNA="always"))
xout <- transform(xout, relative = prop.table(Freq))
xout <-xout[order(xout$Freq),] 
table<-head(xout[order(-xout$Freq), ], n=100)
colnames(table) <- c("Temperature", "Count", "Proportion")
write.csv(table[,c("Temperature", "Count", "Proportion")], 
          row.names = F, quote = T,"Temperature.csv")

table <- table[1:30,c("Temperature", "Count", "Proportion")]
sort <-table[order(table$Proportion),]
pdf("Temperature.pdf",width=10,height=7)
par(mar=c(8,20,5,5))
par(mgp=c(4, 1, 0))
barplot(sort$Count, names.arg = sort$Temperature, las=2, cex.names=.75, horiz=TRUE, main=z, xlab = "Number of Records") 
dev.off()

pdf("TemperatureHist.pdf",width=10,height=7)
par(mar=c(8,20,5,5))
par(mgp=c(4, 1, 0))
hist(as.numeric(tdata$Temperature[tdata$Temperature != -999])) 
dev.off()

##### Salinity #####
z = "Salinity"
x <-which(colnames(tdata) == z)

xout <- as.data.frame(table(tdata[x], useNA="always"))
xout <- transform(xout, relative = prop.table(Freq))
xout <-xout[order(xout$Freq),] 
table<-head(xout[order(-xout$Freq), ], n=100)
colnames(table) <- c("Salinity", "Count", "Proportion")
write.csv(table[,c("Salinity", "Count", "Proportion")], 
          row.names = F, quote = T,"Salinity.csv")

table <- table[1:30,c("Salinity", "Count", "Proportion")]
sort <-table[order(table$Proportion),]
pdf("Salinity.pdf",width=10,height=7)
par(mar=c(8,20,5,5))
par(mgp=c(4, 1, 0))
barplot(sort$Count, names.arg = sort$Salinity, las=2, cex.names=.75, horiz=TRUE, main=z, xlab = "Number of Records") 
dev.off()

pdf("SalinityHist.pdf",width=10,height=7)
par(mar=c(8,20,5,5))
par(mgp=c(4, 1, 0))
hist(as.numeric(tdata$Salinity[tdata$Salinity != -999 & tdata$Salinity != 0])) 
dev.off()



#####pie charting#####

# Pie Chart from data frame with Appended Sample Sizes
par(mar=c(0,0,0,0))
par(mgp=c(0, 0, 0))
mytable <- table(data_unfl$VernacularNameCategory, useNA = "always")
lbls <- paste(names(mytable), "\n", mytable, sep="")
pie(mytable, labels = lbls, cex = 0.3,
    )

 