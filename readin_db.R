rm(list = ls(all = TRUE))
library(ggplot2)
library(tidyverse)
library(plotly)

#cygnetdb_temp <- read.csv("D:/RD_Projekte/Visi/db12376021-cygnetuser_datatable2.sql", header=FALSE, quote="'", na.strings="(")
cygnetdb_temp <- read.csv("D:/RD_Projekte/Visi/db12376021-cygnetuser_datatable2_shorttest.sql", header=FALSE, quote="'", na.strings="(")

cygnetdb = cygnetdb_temp[!grepl("INSERT", cygnetdb_temp$V1),]
cygnetdb = cygnetdb[!grepl("null|rd_err", cygnetdb$V4),]
cygnetdb = cygnetdb[!grepl("`Serial_number`", cygnetdb$V3),]
cygnetdb = cygnetdb[!grepl("null", cygnetdb$V9),]

# Puts together Sw verison into new column and removes all blanks
cygnetdb$swversion <- gsub(" ", "", paste(cygnetdb$V4,".",cygnetdb$V5,".",cygnetdb$V6,".",cygnetdb$V7), fixed = TRUE)
swversion_table <- table(cygnetdb$swversion)
swversion_prop <- prop.table(swversion_table)

#date_db = gsub("[()]", "", cygnetdb_temp$V1)
#hist(as.numeric(as.character(cygnetdb$V4)))
#hist(as.numeric(cygnetdb$swversion))


### BarPlot Section

#h <- barplot(table(cygnetdb$swversion),main="Cygnet Version",ylab="Freqency",las=2)
#h <- barplot(prop.table(table(cygnetdb$swversion)),main="Cygnet Version",ylab="Freqency",las=2)
#tp <- ggplot(data = cygnetdb, mapping = aes(swversion)) + geom_histogram(stat = "count") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
#tp
#h <- barplot(table(cygnetdb$V13),main="Cygnet Version",ylab="Freqency",las=2)



#rotate_x <- function(data, labels_vec, rot_angle) {
#  plt <- barplot(data, col='steelblue', xaxt="n")
#  text(plt, par("usr")[3], labels = labels_vec, srt = rot_angle, adj = c(1.0,1.1), xpd = TRUE, cex=0.9) 
#}


#rotate_x(table(cygnetdb$V13), row.names(table(cygnetdb$V13)), 45)



df <-  as.data.frame(swversion_table)
p <- ggplot(data=df, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity", fill="steelblue") +
  geom_text(aes(label=""), vjust=-0.3, size=3.5) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(breaks = row.names(swversion_table)[seq(1, length(row.names(swversion_table)), by = 5)])+
  scale_y_continuous(labels=function(n){format(n, big.mark = ",", scientific = FALSE)})+
  ggtitle("Cygnet versions") + xlab("") + ylab("Number of Sessions")
ggplotly(p)



# Pie Chart with Percentages
slices <- as.vector(swversion_table)
sub <- subset(swversion_prop, slices > 18000)
lbls <- row.names(sub)
pct <- round(as.vector(sub)*100,digits=1)
lbls <- paste(lbls,"\n", pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(as.vector(sub),labels = lbls, col=rainbow(length(lbls)),
    main="Cygnet Versions \n (only sessions > 18,000) ")


