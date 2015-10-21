# cut the depth vector and then barplot (varying bins) 
groups = cut(data_unfl$ObservationYear, breaks=c(1800,1980, 1990, 2000, 2010,2020), include.lowest=TRUE)
                                          
                                              

#bygroup = tapply(data$gisDepth, groups)
table<-table(groups)
table<-as.data.frame(table)

names.arg = c("1800-1980", "1981-1990", "1991-2000", "2001-2010", ">2010")


par(mar=c(13,13.5,5,5))
par(mgp=c(11, 2, 1))

barplot(table$Freq, 
        horiz=T,
        las=2,
        main = "Temporal Distribution",
        cex.main = 3.0,
        xlab="Number of Records",
        ylab="Observation Year",
        cex.axis = 2.0,
        cex.names = 2.0,
        cex.lab = 3.0,
        col="blue",
        names.arg = names.arg
)