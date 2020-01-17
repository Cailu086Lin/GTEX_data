###headmap
data=dat[, c(3:27)]
data_matrix<- aggregate(data[, 2:25], list(data$Tissue), mean)
rownames(data_matrix)<-data_matrix[,1]
d1<-data_matrix[,-1]
d2<-as.matrix(d1)

require(gplots)
heatmap.2(d2, key=TRUE, trace="none")

##GTEx_data_for_estee_chemo_recepotors in skin
dat<-read.csv('GTEx_skin_for_estee.csv')
data=dat[c(6,965:991)]
data_matrix<- aggregate(data[, 2:28], list(data$Tissue), mean)
rownames(data_matrix)<-data_matrix[,1]
d1<-data_matrix[,-1]
d2<-as.matrix(d1)
rownames(d2)<-c("non-exposed skin", "sun-exposed skin")

require(gplots)
par(mar=c(2,4,4,2))
heatmap.2(t(d2), key=TRUE, trace="none", cexCol = 1.2, srtCol=0, adjCol=0.5, colCol=c("red", "blue"))
###check sex effect
dat_sex=dat[,c(1:4, 6, 965:991)]

melt <- melt(dat_sex, id.var=c(1:5))

ggplot(melt, aes(x = variable, y = value, colour = SEX )) +
  geom_boxplot() +facet_grid(. ~ Tissue) + coord_flip()+
  ylab("Gene expreion, RPKM") +xlab("")

####check race
ggplot(melt, aes(x = variable, y = value, colour = RACE )) +
  geom_boxplot() +facet_grid(. ~ Tissue) + coord_flip()+
  ylab("Gene expreion, RPKM") +xlab("")

write.csv(melt, "GTEx_estee_sweet.bitter.csv", row.names=F)

###Check correlation with Age
d_notsun=subset(melt, Tissue=="Skin - Not Sun Exposed (Suprapubic)")
d_sun=subset(melt, Tissue=="Skin - Sun Exposed (Lower leg)")


multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}



p1 <- ggplot(d_notsun, aes(AGE,  value)) + geom_point(shape=1)+ geom_smooth(method=lm) +
  facet_grid(. ~ variable) + labs(x="", y="Expression, RPKM")+scale_x_continuous(breaks = c(20, 50))+
  ggtitle("Skin - Not Sun Exposed (Suprapubic)") + theme(plot.title=element_text(hjust=0.5))
p2 <- ggplot(d_sun, aes(AGE,  value)) + geom_point(shape=1)+ geom_smooth(method=lm) +
  facet_grid(. ~ variable) + labs(x="Age", y="Expression, RPKM")+scale_x_continuous(breaks = c(20, 50))+
  ggtitle("Skin - Sun Exposed (Lower leg)") +theme(plot.title=element_text(hjust=0.5))

multiplot(p1,p2)