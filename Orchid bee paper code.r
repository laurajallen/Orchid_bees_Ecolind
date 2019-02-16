# Are Orchid bees useful indicators of human disturbance in tropical forest?
# Code to accompany paper
# Laura Allen
# 16 February 2019
## ////////////////////////////////////////


## Libraries ----


## Vegetation data ----


## Diversity calculation - rdiversity ----


## Diversity with iNEXT ----


## Beta diversity with vegan ----


## Pollination ----


## Statistical models ----


## Plots ----





# alpha diversity ----
# libraries and sources ----
rm(list=ls()) ##Clear memory
setwd("C:/Data/PhD/Analysis/Orchidbees")

source("C://Data/PhD/Analysis/Butterflies/general/diversity.r")
source("C://Data/PhD/Analysis/Diversity/diversityplots.r")
require(ggplot2)
require(reshape2)
require(BiodiversityR)
require(vegan)
require(plotrix)
library(knitr)

## observed hills numbers diversity ----
ob.mat <- read.csv("C://Data/PhD/Processed_data/Orchidbees/OBcountmat.csv",row.names=1,check.names = F) 
head(ob.mat)
#remove '.' from colnames
#names(ob.mat) <- gsub(".", "", names(ob.mat), fixed = TRUE)
str(ob.mat)


p1 <- ob.mat/sum(ob.mat) #turned counts into proportions
# qs1 <- c(0,seq(from = 0.5, to = 0.95,by=0.05),seq(from = 1, to = 2,by=0.1),Inf) # for calculating 
# qs2 <- c(0,seq(from = 2, to = 4,by=0.1),6) # for plotting
#plot colours:
cf <- c('darkred','darkred','darkred','firebrick2','firebrick2','firebrick2','darksalmon','darksalmon','darksalmon','darkslategray3','darkslategray3','darkslategray3','dodgerblue2','dodgerblue2','dodgerblue2','navyblue','navyblue','navyblue')
cf1 <- c('darkred','firebrick2','darksalmon','darkslategray3','dodgerblue2','navyblue')

# qs1 <- c(0,seq(from = 0.5, to = 0.9,by=0.1),seq(from = 1, to = 2,by=0.2),Inf) # for calculating 
# qs2 <- c(0,2,2.516129,2.7741935,2.9032258,2.9677419,3,3.516129,3.7741935,3.9032258,3.96774196,4,6) # for plotting
# qs2 <- c(0,2,2.03225806,2.09677419,2.2258065,2.483871,3,3.03225806,3.09677419,3.2258065,3.483871,4,6) # for plotting

qs1 <- c(0,seq(from = 0.5, to = 2,by=0.1),Inf) # for calculating 
#qs2 <- c(0,exp(seq(log(0.5),log(2),length.out=16)),2.5) # should the tick marcks be between 0.5-2 or 1-2??
#qs2 <- c(0,exp(seq(log(1),log(2),length.out=16)),3) #

#current favourite:
#qs2 <- c(0.5,exp(seq(log(1),log(2),length.out=16)),2.5) #make the broken axes shorter
qs2 <- c(1,seq(from = 2, to = 8,by=0.4),16) # for plotting

#qs2 <- c(0,exp(seq(log(2),log(4),length.out=16)),6) # option with 0.5,1,2 in centre
#qs2 <- c(0,exp(seq(log(1),log(3),length.out=16)),5) # option with 0.5,1,2 in centre

cbind(qs1,qs2)
length(qs2)

gradplot <- function(measure,y,colgrad=cf,legcol=cf1,legpos="topright",title="",ys=c(min(measure),max(measure))){
  plot(rep(0,length(qs1))~qs1,type="l",col="white",ylim=ys,xlim=c(0,2.8),bty='L',xlab=c("q"),
       ylab=y,cex.lab=1,cex.axis=1,xaxt="n",main=title)
  axis(1, at=c(log(1),log(2),log(4),log(8),log(16)), labels=c("0","0.5","1","2",expression(infinity)), las=0, cex.axis=1)
  axis.break(axis=1,breakpos=0.35,pos=NA,bgcol="white",breakcol="black",
             style="zigzag",brw=0.05)
  axis.break(axis=1,breakpos=2.5,pos=NA,bgcol="white",breakcol="black",
             style="zigzag",brw=0.05)
  for(rw in 1:18){
    lines(measure[rw,c(1,2)]~log(qs2[1:2]),col=colgrad[rw],lty="dotted",lwd=3)
    lines(measure[rw,c(17,18)]~log(qs2[17:18]),col=colgrad[rw],lty="dotted",lwd=3)
    lines(measure[rw,c(2:17)]~log(qs2[2:17]),col=colgrad[rw],lwd=3)
    #points(measure[rw,]~log(qs2),col=colgrad[rw],pch=16,cex=0.7)
  }
  abline(v=c(log(1),log(2),log(4),log(8),log(16)),lty="dotted",col="black")
  legend(legpos,legend= c("1.Banana (most disturbed)","2.Agroforestry","3.Disturbed secondary","4.Cleared regenerating","5.Mixed history","6.Minimally disturbed"),
         cex=1,pch=21,pt.bg=(col=legcol),pt.cex=1.7,bty="n") #Added key later (after submission) to explain subgroups.
}
#alpha hills nos
alpha <- as.matrix(subcommunity.alpha.bar(populations=p1,qs=qs1))
#png(filename="C://Data/PhD/Outputs/Orchidbees/Diversity/noq3/alphab_gradient.png", type="cairo",units="in", width=10, height=8, pointsize=12,res=96)
#tiff("C://Data/PhD/Outputs/Orchidbees/Figures/alphab_gradient_inf-hr.tiff",width=190,height=150,units="mm",res=1000, pointsize=12)  
#png(filename="C://Data/PhD/Outputs/Orchidbees/Figures/alphab_gradient_smooth05-shorterends.png", type="cairo",units="in", width=10, height=7, pointsize=18,res=300)
tiff(file="C://Data/PhD/Outputs/Orchidbees/Figures/Ecolind/Fig2_2column_alphaprofile.tiff",width=190,height=110,units="mm",res=1000, pointsize=9)  
par(mar=c(4.5,4,1,1))
gradplot(measure=alpha,y=c("Effective number of species"),colgrad=cf,legcol=cf1)
dev.off()

beta_b<- as.matrix(subcommunity.beta.bar(populations=p1,qs=qs1))
png(filename="C://Data/PhD/Outputs/Orchidbees/Diversity/noq3/betabar_gradient.png", type="cairo",units="in", width=10, height=8, pointsize=12,res=96)
gradplot(measure=beta_b,y=c("Effective number of distinct subcommunities"),colgrad=cf,legcol=cf1)
dev.off()

beta<- as.matrix(subcommunity.beta(populations=p1,qs=qs1))
png(filename="C://Data/PhD/Outputs/Orchidbees/Diversity/noq3/beta_gradient.png", type="cairo",units="in", width=10, height=8, pointsize=12,res=96)
gradplot(measure=beta,y=c("Distinctiveness of subcommunity"),colgrad=cf,legcol=cf1)
dev.off()

rho<- as.matrix(subcommunity.rho(populations=p1,qs=qs1))
png(filename="C://Data/PhD/Outputs/Orchidbees/Diversity/noq3/rho_gradient.png", type="cairo",units="in", width=10, height=8, pointsize=12,res=96)
gradplot(measure=rho, y=c("redundancy of subcommunity"),colgrad=cf,legcol=cf1)
dev.off()

rho_b<- as.matrix(subcommunity.rho.bar(populations=p1,qs=qs1))
png(filename="C://Data/PhD/Outputs/Orchidbees/Diversity/noq3/rhobar_gradient.png", type="cairo",units="in", width=10, height=8, pointsize=12,res=96)
gradplot(measure=rho_b,y=c("Representativeness of subcommunity"),colgrad=cf,legcol=cf1)
dev.off()

gamma<- as.matrix(subcommunity.gamma(populations=p1,qs=qs1))
png(filename="C://Data/PhD/Outputs/Orchidbees/Diversity/noq3/gamma_gradient.png", type="cairo",units="in", width=10, height=8, pointsize=12,res=96)
gradplot(measure=gamma,y=c("contribution per individual toward metacommunity diversity"),colgrad=cf,legcol=cf1)
dev.off()