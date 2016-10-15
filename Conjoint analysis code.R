setwd("/Users/suhangyao／segmentations")
library(foreign)

## A Priori Segmentation
filnm = "Session 05 - SegmentRegExample"
spssdatalab <- read.spss(paste(filnm,".sav",sep=""),to.data.frame=TRUE,use.value.labels=TRUE,trim_values=TRUE)
spssdata <- read.spss(paste(filnm,".sav",sep=""),to.data.frame=TRUE,use.value.labels=FALSE)

lm1 = lm(SpendingOnTraining~ResearchEmployees+Ads, data = spssdata)
## same as lm1 = lm(spssdata$SpendingOnTraining~spssdata$Ads+spssdata$ResearchEmployees) but save effort
summary(lm1)
##now add in the interaction between company type and ad
lm2 = lm(SpendingOnTraining~ResearchEmployees+Ads*(Consumer+B2B), data = spssdata)
summary(lm2)
##check if there is an improvement in the model fit after adding the additional terms
anova(lm1,lm2)
##the answer is yes
##now run the regression differently
lm3 = lm(SpendingOnTraining~0+ResearchEmployees+Ads:(Consumer+B2B+ServiceOrg)+Consumer+B2B+ServiceOrg, data = spssdata)
summary(lm3)


## Post Hoc Segmentation (cluster analysis)
filnm = "Session 05 - Cluster Analysis Comedy Data"
spssdatalab <- read.spss(paste(filnm,".sav",sep=""),to.data.frame=TRUE,use.value.labels=TRUE,trim_values=TRUE)
spssdata <- read.spss(paste(filnm,".sav",sep=""),to.data.frame=TRUE,use.value.labels=FALSE)
attr(spssdata, "variable.labels")


install.packages("cluster")
install.packages("fpc")
library(cluster) 
library(fpc)


set.seed(123456)   # set random number seed before doing cluster analysis
toclust = spssdata[,1:19]    # select the relevant data for clustering
wss <- (nrow(toclust)-1)*sum(apply(toclust,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(toclust, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")


##use pamk() to determine the optimal number of clusters 
pm1 = pamk(toclust,scaling=TRUE)
pm1$nc

km1 = kmeans(toclust,3,iter.max = 20, nstart=2)
km2 = kmeans(toclust,2,iter.max = 20, nstart=2)

##function turns min into 0, max into 1 and scales rest linearly
rscale = function(x){(x-min(x))/(max(x)-min(x));}

km1m = aggregate(toclust,by=list(km1$cluster),FUN=mean)
km2m = aggregate(toclust,by=list(km2$cluster),FUN=mean); #calculate profile means
km1ms = apply(km1m[,2:ncol(km1m)],2,rscale); #rescale profile means for easy presentation
par(mar=c(8.1,4.1,4.1,2.1)); #setting margins to give room for x axis labels
matplot(t(km1ms),col=c(1,4,2),ylab="Mean Value (Range Normalized)",xaxt="n")
axis(side=1,at=1:19,labels=names(toclust),las=2); #putting x axis labels on plot
##plots the three clusters with 1 max and 0 min for each variable
##notice need to know what the variables are!

percsize = paste(1:3," = ",format(km1$size/sum(km1$size)*100,digits=2),"%",sep="")
pie(km1$size,labels=percsize)
##plots the relative size of each group

clusplot(toclust, km1$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0); #plot clusters against principal components
##idea of this plot is identify the clusters 
##the axes are the principal components, which are sort of a mixture 
##  of all of the variables used in the clustering

# Centroid Plot against 1st 2 discriminant functions
plotcluster(toclust, km1$cluster); #plot against discriminant functions ()
##idea of this plot is that the axes are the two best 
##  functions to distinguish the clusters
