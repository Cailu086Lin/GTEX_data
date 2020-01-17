dat<-read.table("qry_pheno_Cotwin_421.csv" , header=T, sep=",", na.strings = "NA")

tab<-aggregate(dat[,2:207],by=list(dat$UniqueID),FUN = mean, na.rm=TRUE)
write.csv(tab, "tw2_avg.csv", row.names = F)

               
dat2<-read.table("Query1.csv" , header=T, sep=",", na.strings = "NA")

tab2<-aggregate(dat2[,2:207],by=list(dat2$UniqueID),FUN = mean, na.rm=TRUE)
write.csv(tab2, "tw1_avg.csv", row.names = F)


#heatmap


pacman::p_load(ggplot2, dplyr, reshape2)
d<-read.csv("test.csv", header=T)
d1 <-melt(d, id="ID")
p <- ggplot(d1, aes(variable, ID))+geom_tile(aes(fill=value),  colour = "grey")+ylab("")+xlab("")+ guides(fill=FALSE)+theme_bw()
P1<-p+scale_fill_manual(values=c("white", "blue"))