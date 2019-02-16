#vegetation data - all

# Clean dataset 
rm(list=ls()) ##Clear memory
require(dplyr)
require(tidyr)
require(lubridate)
require(plyr)

# First import raw data
options(stringsAsFactors = FALSE) # import not as factors
veg=read.csv("C://Data/PhD/Rawdata/Vegetation/Vegmap_gradient_means.csv",header=TRUE)

head(veg)
str(veg)

#veg[,1] <- as.factor(veg[,1])

veg$Site[veg$Site=="CH-400"] <- "xf.MIN-C"
veg$Site[veg$Site=="T1-650"] <- "xd.CCR-B"
veg$Site[veg$Site=="T10-100"] <- "xe.MXD-B"
veg$Site[veg$Site=="T2-2150"] <- "xe.MXD-A"
veg$Site[veg$Site=="T2-800"] <- "xd.CCR-A"
veg$Site[veg$Site=="T3-1800"] <- "xe.MXD-C"
veg$Site[veg$Site=="T5-250"] <- "xd.CCR-C"
veg$Site[veg$Site=="T6-850"] <- "xf.MIN-B"
veg$Site[veg$Site=="T7-1350"] <- "xf.MIN-A"
veg$Site[veg$Site=="AF-A"] <- "xb.AF-A"
veg$Site[veg$Site=="AF-B"] <- "xb.AF-B"
veg$Site[veg$Site=="AF-C"] <- "xb.AF-C"
veg$Site[veg$Site=="BA-A"] <- "xa.BA-A"
veg$Site[veg$Site=="BA-B"] <- "xa.BA-B"
veg$Site[veg$Site=="BA-C"] <- "xa.BA-C"
veg$Site[veg$Site=="SF-A"] <- "xc.SF-A"
veg$Site[veg$Site=="SF-B"] <- "xc.SF-B"
veg$Site[veg$Site=="SF-C"] <- "xc.SF-C"


str(veg)
head(veg)

# calculate means per site from quadrat data
mean.veg <- aggregate(veg,list(veg$Site),mean,na.rm=T)
sort(mean.veg$Group.1)
mean.veg <- mean.veg[order(mean.veg$Group.1),]
# format df 

#Quad <- paste(veg$Site,veg$Point,sep="_")
remove <- c(1,2,3,10,17) #columns to remove
Site <- mean.veg[,1]
veg1 <- mean.veg[,-remove]
veg1[,1] <- as.numeric(veg1[,1])
veg1[,2] <- as.numeric(veg1[,2])
veg1[,3] <- as.numeric(veg1[,3])
veg1[,4] <- as.numeric(veg1[,4])
veg1[,5] <- as.numeric(veg1[,5])
veg1[,6] <- as.numeric(veg1[,6])
veg1[,7] <- as.numeric(veg1[,7])
veg1[,8] <- as.numeric(veg1[,8])
veg1[,9] <- as.numeric(veg1[,1])
veg1[,10] <- as.numeric(veg1[,10])
veg1[,11] <- as.numeric(veg1[,11])
veg1[,12] <- as.numeric(veg1[,12])

veg.mat <- as.matrix(veg1)
rownames(veg.mat) <- Site



###############################
##
# Vegetation PCA
################################

# 2.1.2: PCA using vegan
library(vegan)

# calculate rda of veg.mat
rda.out <- rda(veg.mat,scale=T) # default scale option =F 
# the rda (redundancy analysis) function calculates the PCA and can also be used for canonical RDA
# scale = F means the data should be centred by columns but not standardised

summary(rda.out) # shows the two axes of the PCA for each site
# scaling = 1 preserves Euclidean distance among objects
# scaling = 2 preserves correlation between descriptors

# PCA w scaling 2
veg_pca2 <- summary(rda.out, scaling=1) # Eigenvalues, species scores, site scores 
biplot(rda.out, scaling=2) # Function biplot.rda() of vegan is used 
scrs <- scores(rda.out,display = c("sites", "species"), scaling =1) #extract sepecies and site scores from rda summary
sort(scrs$species[,1])

veg_pc1 <- veg_pca2$sites[,1] #PC1 axis
veg_pc2 <- veg_pca2$sites[,2] #PC2 axis

veg_pca_out <- as.data.frame(cbind(veg_pc1,veg_pc2))
#write.csv(veg_pca_out,"C://Data/PhD/Processed_data/Vegetation/veg_pca_outOct18.csv")



# custom PCA plot ----
str(veg.mat)
rda.out <- rda(veg.mat,scale=T)
scrs <- scores(rda.out,display = c("sites", "species"), scaling =2) #extract sepecies and site scores from rda summary
summary(rda.out)
max(scrs$sites)
#png(file="C://Data/PhD/Outputs/Vegetation/PCA_veg_custom.png",width=10,height=7.5,units="in",res=300,pointsize = 18)  
#tiff(file="C://Data/PhD/Outputs/Vegetation/PCA_veg_custom.tiff",width=140,height=140,units="mm",res=1000, pointsize=10)  
tiff(file="C://Data/PhD/Outputs/Dungbeetles/Biotropica figures/FigS6_PCA_veg.tiff",width=140,height=140,units="mm",res=1000, pointsize=10)  
# C://Data/PhD/Outputs/Orchidbees/Figures/Ecolind/FigS4_1-5columns-PCAvegetation.tiff"
par(mar=c(1,1,1,1))
par(mfrow=c(1,1))
plot.new()
plot.window(xlim = c(-2,2), ylim = c(-2.5,2), asp = 1)
axis(side=1, at=c(-2,-1.5,-1,-0.5,0.5,1,1.5,2), lty="solid", pos=c(0,0),padj=-1,cex.axis=0.7)
axis(side=2, at=c(-2.5,-2,-1.5,-1,-0.5,0.5,1,1.5,2), lty="solid", pos=c(0,0),padj=1,cex.axis=0.7)
#colvec <- c('darkred','darkred','darkred','firebrick2','firebrick2','firebrick2','darksalmon','darksalmon','darksalmon','darkslategray3','darkslategray3','darkslategray3','dodgerblue2','dodgerblue2','dodgerblue2','navyblue','navyblue','navyblue')
colvec <- c('#d73027','#d73027','#d73027','#fc8d59','#fc8d59','#fc8d59','#fee090','#fee090','#fee090','#e0f3f8','#e0f3f8','#e0f3f8','#91bfdb','#91bfdb','#91bfdb','#4575b4','#4575b4','#4575b4')
#points(scrs$sites, col="black", bg= colvec, pch = 21,cex=3)
symvec <- c(rep("1",3),rep("2",3),rep("3",3),rep("4",3),rep("5",3),rep("6",3))
points(scrs$sites, col=colvec,pch = 1,cex=4, lwd=4.5)
#points(scrs$sites, col="black",pch = 1,cex=5, lwd=0.5)
points(scrs$sites, col=colvec, bg= "grey",pch = symvec,cex=1.5,lwd=1)

# ## For bw-friendly option:
# symvec <- c(rep("1",3),rep("2",3),rep("3",3),rep("4",3),rep("5",3),rep("6",3))
# points(scrs$sites, col=colvec,pch = 1,cex=4, lwd=5)
# points(scrs$sites, col="black",pch = 1,cex=5, lwd=0.5)
# points(scrs$sites, col="black", bg= "grey",pch = symvec,cex=1.5,lwd=1.5)
# plot arrows for each environental variable
arrows(x0=0, y0=0, scrs$species[1,1], scrs$species[1,2], code=2, lwd=1, col="black",length=0.1)
arrows(x0=0, y0=0, scrs$species[2,1], scrs$species[2,2], code=2, lwd=1, col="black",length=0.1)
arrows(x0=0, y0=0, scrs$species[3,1], scrs$species[3,2], code=2, lwd=1, col="black",length=0.1)
arrows(x0=0, y0=0, scrs$species[4,1], scrs$species[4,2], code=2, lwd=1, col="black",length=0.1)
arrows(x0=0, y0=0, scrs$species[5,1], scrs$species[5,2], code=2, lwd=1, col="black",length=0.1)
arrows(x0=0, y0=0, scrs$species[6,1], scrs$species[6,2], code=2, lwd=1, col="black",length=0.1)
arrows(x0=0, y0=0, scrs$species[7,1], scrs$species[7,2], code=2, lwd=1, col="black",length=0.1)
arrows(x0=0, y0=0, scrs$species[8,1], scrs$species[8,2], code=2, lwd=1, col="black",length=0.1)
arrows(x0=0, y0=0, scrs$species[9,1], scrs$species[9,2], code=2, lwd=1, col="black",length=0.1)
arrows(x0=0, y0=0, scrs$species[10,1], scrs$species[10,2], code=2, lwd=1, col="black",length=0.1)
arrows(x0=0, y0=0, scrs$species[11,1], scrs$species[11,2], code=2, lwd=1, col="black",length=0.1)
arrows(x0=0, y0=0, scrs$species[12,1], scrs$species[12,2], code=2, lwd=1, col="black",length=0.1)

text(x=scrs$species[1,1]+0.4, y= scrs$species[1,2]-0.05, labels = "Canopy height", cex=1,col="black")
text(x=scrs$species[2,1], y= scrs$species[2,2]+0.1, labels = "Canopy cover", cex=1,col="black")
text(x=scrs$species[3,1]+0.3, y= scrs$species[3,2], labels = "Mid-canopy height", cex=1,col="black")
text(x=scrs$species[4,1], y= scrs$species[4,2]+0.1, labels = "Mid-canopy cover", cex=1,col="black")
text(x=scrs$species[5,1], y= scrs$species[5,2], labels = "Leaf-litter depth", cex=1,col="black")
text(x=scrs$species[6,1]-0.2, y= scrs$species[6,2]+0.2, labels = "DBH largest trees", cex=1,col="black")
text(x=scrs$species[7,1], y= scrs$species[7,2], labels = "n trees DBH>5", cex=1,col="black")
text(x=scrs$species[8,1]+0.1, y= scrs$species[8,2]+0.1, labels = "Shrub layer", cex=1,col="black")
text(x=scrs$species[9,1]+0.2, y= scrs$species[9,2]+0.1, labels = "Herb layer", cex=1,col="black")
text(x=scrs$species[10,1], y= scrs$species[10,2], labels = "Herb abundance", cex=1,col="black")
text(x=scrs$species[11,1], y= scrs$species[11,2], labels = "Bare ground", cex=1,col="black")
text(x=scrs$species[12,1], y= scrs$species[12,2], labels = "Woody debris", cex=1,col="black")
text(x=1.8, y=0.1,labels = c("PCA1"),cex=1)
text(x=0.1, y=-2,labels = c("PCA2"),srt=90,cex=1)
legend(x=0.5,y=-1.3,bty="n",pch=21,pt.cex=1,cex=1,pt.bg=c('#d73027','#fc8d59','#fee090','#e0f3f8','#91bfdb','#4575b4'),legend=c("1.Banana (most disturbed)","2.Agroforestry","3.Disturbed secondary","4.Cleared regenerating","5.Mixed history","6.Minimally disturbed"))
         #c('darkred','firebrick2','darksalmon','darkslategray3','dodgerblue2','navyblue'),
        
dev.off()

###########################

## Vegetation PCA excluding canopy height ----
head(veg.mat)
veg.mat <- veg.mat[,-c(1,3)]
rda.out <- rda(veg.mat,scale=T)
scrs <- scores(rda.out,display = c("sites", "species"), scaling =2) #extract sepecies and site scores from rda summary
summary(rda.out)

#tiff(file="C://Data/PhD/Outputs/Orchidbees/Figures/Ecolind/FigS2_1-5columns-PCAvegetation_revised.tiff",width=140,height=140,units="mm",res=1000, pointsize=10)  
par(mar=c(1,1,1,1))
par(mfrow=c(1,1))
plot.new()
plot.window(xlim = c(-2,2), ylim = c(-2.5,2), asp = 1)
axis(side=1, at=c(-2,-1.5,-1,-0.5,0.5,1,1.5,2), lty="solid", pos=c(0,0),padj=-1,cex.axis=0.7)
axis(side=2, at=c(-2.5,-2,-1.5,-1,-0.5,0.5,1,1.5,2), lty="solid", pos=c(0,0),padj=1,cex.axis=0.7)
colvec <- c('darkred','darkred','darkred','firebrick2','firebrick2','firebrick2','darksalmon','darksalmon','darksalmon','darkslategray3','darkslategray3','darkslategray3','dodgerblue2','dodgerblue2','dodgerblue2','navyblue','navyblue','navyblue')
points(scrs$sites, col="black", bg= colvec, pch = 21,cex=3)

# plot arrows for each environental variable
arrows(x0=0, y0=0, scrs$species[1,1], scrs$species[1,2], code=2, lwd=1, col="black",length=0.1)
arrows(x0=0, y0=0, scrs$species[2,1], scrs$species[2,2], code=2, lwd=1, col="black",length=0.1)
arrows(x0=0, y0=0, scrs$species[3,1], scrs$species[3,2], code=2, lwd=1, col="black",length=0.1)
arrows(x0=0, y0=0, scrs$species[4,1], scrs$species[4,2], code=2, lwd=1, col="black",length=0.1)
arrows(x0=0, y0=0, scrs$species[5,1], scrs$species[5,2], code=2, lwd=1, col="black",length=0.1)
arrows(x0=0, y0=0, scrs$species[6,1], scrs$species[6,2], code=2, lwd=1, col="black",length=0.1)
arrows(x0=0, y0=0, scrs$species[7,1], scrs$species[7,2], code=2, lwd=1, col="black",length=0.1)
arrows(x0=0, y0=0, scrs$species[8,1], scrs$species[8,2], code=2, lwd=1, col="black",length=0.1)
arrows(x0=0, y0=0, scrs$species[9,1], scrs$species[9,2], code=2, lwd=1, col="black",length=0.1)
arrows(x0=0, y0=0, scrs$species[10,1], scrs$species[10,2], code=2, lwd=1, col="black",length=0.1)

text(x=scrs$species[1,1]+0.4, y= scrs$species[1,2]-0.15, labels = "Canopy cover", cex=1,col="black")
text(x=scrs$species[2,1], y= scrs$species[2,2]+0.15, labels = "Mid-canopy cover", cex=1,col="black")
text(x=scrs$species[3,1], y= scrs$species[3,2], labels = "Leaf-litter depth", cex=1,col="black")
text(x=scrs$species[4,1]-0.2, y= scrs$species[4,2]+0.2, labels = "DBH largest trees", cex=1,col="black")
text(x=scrs$species[5,1], y= scrs$species[5,2], labels = "n trees DBH>5", cex=1,col="black")
text(x=scrs$species[6,1]+0.1, y= scrs$species[6,2]+0.1, labels = "Shrub layer", cex=1,col="black")
text(x=scrs$species[7,1]+0.2, y= scrs$species[7,2]+0.1, labels = "Herb layer", cex=1,col="black")
text(x=scrs$species[8,1], y= scrs$species[8,2], labels = "Herb abundance", cex=1,col="black")
text(x=scrs$species[9,1], y= scrs$species[9,2], labels = "Bare ground", cex=1,col="black")
text(x=scrs$species[10,1], y= scrs$species[10,2], labels = "Woody debris", cex=1,col="black")
text(x=1.8, y=0.1,labels = c("PCA1"),cex=1)
text(x=0.1, y=-2,labels = c("PCA2"),srt=90,cex=1)
legend(x=0.5,y=-1.3,bty="n",pch=21,pt.cex=1,cex=1,pt.bg=c('darkred','firebrick2','darksalmon','darkslategray3','dodgerblue2','navyblue'),legend=c("1.Banana (most disturbed)","2.Agroforestry","3.Disturbed secondary","4.Cleared regenerating","5.Mixed history","6.Minimally disturbed"))
dev.off()

rank <- c(1,1,1,2,2,2,3,3,3,4,4,4,5,5,5,6,6,6)
vegrank <- as.data.frame(cbind(scrs$sites,rank))

res <- cor.test(vegrank$PC1, vegrank$rank, method = "spearman",exact=F) # used exact = F to supress warning about ties - this 
cbind(res$statistic,res$p.value,res$estimate)
#
# bootstrapped 95% CI
quantile(
  replicate(10000, {
    boot.i <- sample(length(vegrank$rank), replace = TRUE)
    cor(vegrank$rank[boot.i],vegrank$PC1[boot.i], method = "spearman")
  }), 
  c(0.025, 0.975))

min(vegrank$PC1)
tiff(file="C://Data/PhD/Outputs/Orchidbees/Figures/Ecolind/Fig1-1column-vegpc1-rank.tiff",width=90,height=100,units="mm",res=1000, pointsize=9)  
par(mar=c(4,4,1,1),mfrow=c(1,1))
plot(vegrank$PC1~vegrank$rank,bty="n",pch=21,cex=1.5,lwd=1.2,cex.axis=1,cex.lab=1,col='black',ylab="Vegetation structure PC1",xlab="Disturbance Rank",ylim=c(-2,2.5))
dev.off()

