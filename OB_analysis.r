
# Analysing orchid bee data ----

# libraries ----
library(ape) # for moran's i 
rm(list=ls())
# import data ----
#ob <- read.csv("C://Data/PhD/Processed_data/Orchidbees/OB_div_allvars.csv") # originally used this, but div was at estimated at 95%SC instead of EqSS

ob <- read.csv("C://Data/PhD/Processed_data/Orchidbees/OB_div_allvars_EqSS.csv")

str(ob)

# Obs Alpha diversity along gradient ----

# visual exploration of data ----
plot(ob$q0~ob$Rank)
plot(ob$q1~ob$Rank)
plot(ob$q2~ob$Rank)
plot(ob$q3~ob$Rank)
plot(ob$qInf~ob$Rank)
plot(ob$e95q0~ob$Rank)
plot(ob$asEst.q1~ob$Rank)
plot(ob$asEst.q2~ob$Rank)
plot(ob$e95q3~ob$Rank)
plot(ob$rq0~ob$Rank) #redundancy
plot(ob$rbq0~ob$Rank) #representaiveness
plot(ob$Visits~ob$Rank)
plot(ob$Visits~ob$q0)
plot(ob$Visits~ob$e95q0)
plot(ob$abund~ob$Rank)
head(ob)
meanRs <- cbind(ob$rq1,ob$rbq1,ob$Rank)
mean(meanRs[c(1:6,16:18),2]) 

## grpahical abstract plot

# scale data

##png(file="C://Data/PhD/Outputs/Orchidbees/Graphicalabstract.png",width=200,height=150,units="mm",res=300, pointsize=12) 
plot(scale(ob$abund)~ob$Rank,bty="n",pch=19,cex.axis=1.2,cex.lab=1.2,col='white',ylab="Orchid bee response",xlab="Disturbance Rank", ylim=c(-1,1))
abline(lm(scale(ob$abund)~ob$Rank),col="deepskyblue2", lwd=5)
#abline(lm(scale(ob$q0)~ob$Rank),col="yellow", lwd=3)
abline(lm(scale(ob$Est0_ss)~ob$Rank),col="midnightblue", lwd=5)
abline(lm(scale(ob$rq0)~ob$Rank),col="orangered1", lwd=5)
dev.off()

# compare linear abundance rank relationship with possible curve

lin <- lm(log(ob$abund)~ob$Rank)
AIC(lin)
pol <- lm(log(ob$abund)~poly(ob$Rank,2))
AIC(pol)-AIC(lin)

plot(ob$abund~ob$Rank )
lines(sort(ob$Rank), fitted(lin)[order(ob$Rank)], col='red', type='l') 
lines(sort(ob$Rank), fitted(sq)[order(ob$Rank)], col='red', type='l') 
# plots -----

rank <- c(1,1,1,2,2,2,3,3,3,4,4,4,5,5,5,6,6,6)
##png(file="C://Data/PhD/Outputs/Orchidbees/OBabudance.png",width=7,height=5,units="in",res=250)  
#tiff(file="C://Data/PhD/Outputs/Orchidbees/Figures/Abundance-rank-hr.tiff",width=140,height=140,units="mm",res=1000, pointsize=14)  
#png(file="C://Data/PhD/Outputs/Orchidbees/Figures/Abundance-rank-abline-hr.png",width=140,height=140,units="mm",res=300, pointsize=12)  
##png(file="C://Data/PhD/Outputs/Orchidbees/Diversity/poster/OBabudance_line.png",width=10,height=7,units="in",pointsize=18,res=300)  
tiff(file="C://Data/PhD/Outputs/Orchidbees/Figures/Ecolind/Fig4-1column-Abundance-rank.tiff",width=90,height=100,units="mm",res=1000, pointsize=9)  
plot(ob$abund~ob$Rank,bty="n",pch=21,cex=1.5,lwd=1.2,cex.axis=1,cex.lab=1,col='black',ylab="Raw Abundance",xlab="Disturbance Rank",ylim=c(0,250))
dev.off()

##png(file="C://Data/PhD/Outputs/Orchidbees/Diversity/poster/OBrhoq1_line.png",width=10,height=7,units="in",pointsize=18,res=300)  
#png(file="C://Data/PhD/Outputs/Orchidbees/Figures/redundancy-rank-abline-hr.png",width=140,height=150,units="mm",res=300, pointsize=12)  
tiff(file="C://Data/PhD/Outputs/Orchidbees/Figures/Ecolind/Fig7-2column-redundancy.tiff",width=190,height=100,units="mm",res=1000, pointsize=12)  
par(mar=c(5,4.2,2,1),mfrow=c(1,2))
plot(ob$rq1~ob$Rank,bty="n",pch=21,cex=1.5,lwd=1.2,cex.axis=1,cex.lab=1,col='black',ylab=expression(paste("Site redundancy (",rho,")")),xlab="Disturbance Rank",ylim=c(0,60))
mtext(side = 3, line = 1,adj=0,"a",font=2,cex=1.2)
plot(ob$rbq1~ob$Rank,bty="n",pch=21,cex=1.5,lwd=1.2,cex.axis=1,cex.lab=1,col='black',ylab=expression(paste("Site representativeness (",bar(rho),")")),xlab="Disturbance Rank",ylim=c(0.4,1))
mtext(side = 3, line = 1,adj=0,"b",font=2,cex=1.2)
dev.off()


#png(file="C://Data/PhD/Outputs/Orchidbees/Figures/Pollination-abline.png",width=190,height=100,units="mm",res=500, pointsize=12)  
#tiff(file="C://Data/PhD/Outputs/Orchidbees/Figures/Pollination-lr.tiff",width=190,height=100,units="mm",res=300, pointsize=12)  
tiff(file="C://Data/PhD/Outputs/Orchidbees/Figures/Ecolind/Fig8-2column-pollination.tiff",width=190,height=100,units="mm",res=1000, pointsize=12)  
par(mar=c(5,4.2,2,1),mfrow=c(1,2))
plot(ob$Visits~ob$Rank,bty="n",pch=21,cex=1,cex.axis=1,cex.lab=1,col='black',ylab="Insect visits",xlab="Disturbance Rank")
#abline(lm(ob$Visits~ob$Rank))
mtext(side = 3, line = 1,adj=0,"a",font=2,cex=1.2)
plot(ob$Visits~ob$q0,bty="n",pch=21,cex=1,cex.axis=1,cex.lab=1,col='black',ylab="",xlab="Observed Euglossine species richness")
#abline(lm(ob$Visits~ob$q0))
mtext(side = 3, line = 1,adj=0,"b",font=2,cex=1.2)
dev.off()
#
#
# Moran's i test for spatial autocorrelation
morans <- function(model){
  resids <- model$residuals 
  resdf <- cbind.data.frame(ob$Site,resids,ob$UTM_Coords_S,ob$UTM_Coords_E)
  ob.dists <- as.matrix(dist(cbind(ob$UTM_Coords_E,ob$UTM_Coords_S))) # create distance matrix of site coordinates
  ob.dists.inv <- 1/ob.dists # inverse dist matrix
  diag(ob.dists.inv) <- 0 # replace diagonal with 0s
  ob.dists.inv[1:5, 1:5] # view
  as.data.frame(Moran.I(resdf$resids, ob.dists.inv))}


#correlaton tests, then glms to control for elevation and river dist

# abundance ----
res <- cor.test(ob$Rank, ob$abund, method = "spearman",exact=F) # used exact = F to supress warning about ties - this 
cbind(res$statistic,res$p.value,res$estimate)
#
# bootstrapped 95% CI
quantile(
  replicate(10000, {
    boot.i <- sample(length(ob$Rank), replace = TRUE)
    cor(ob$Rank[boot.i],ob$abund[boot.i], method = "spearman")
  }), 
  c(0.025, 0.975))

m0 <- lm(log(abund)~Rank,data=ob)

m1 <- lm(log(abund)~Rank+Elevation,data=ob) 
anova(m0,m1) 

m2 <- lm(log(abund)~Rank+River_dist,data=ob) 
anova(m0,m2)

rbind(logLik(m0),logLik(m1),logLik(m2))
summary(m2)
morans(m2)



# q0 ----
res <- cor.test(ob$Rank, ob$q0, method = "spearman",exact=F) # used exact = F to supress warning about ties - this 
cbind(res$statistic,res$p.value,res$estimate)

# bootstrapped 95% CI
quantile(
  replicate(10000, {
    boot.i <- sample(length(ob$Rank), replace = TRUE)
    cor(ob$Rank[boot.i],ob$q0[boot.i], method = "spearman")
  }), 
  c(0.025, 0.975))

m0 <- lm(log(q0)~Rank,data=ob)
m1 <- lm(log(q0)~Rank+Elevation,data=ob) 
anova(m0,m1) 
m2 <- lm(log(q0)~Rank+River_dist,data=ob) 
anova(m0,m2) 

rbind(logLik(m0),logLik(m1),logLik(m2))
summary(m0)
morans(m0)


# q1 ----
res <- cor.test(ob$Rank, ob$q1, method = "spearman",exact=F) # used exact = F to supress warning about ties - this 
cbind(res$statistic,res$p.value,res$estimate)

# bootstrapped 95% CI
quantile(
  replicate(10000, {
    boot.i <- sample(length(ob$Rank), replace = TRUE)
    cor(ob$Rank[boot.i],ob$q1[boot.i], method = "spearman")
  }), 
  c(0.025, 0.975))

m0 <- lm(log(q1)~Rank,data=ob)

m1 <- lm(log(q1)~Rank+Elevation,data=ob) 
anova(m0,m1) 

m2 <- lm(log(q1)~Rank+River_dist,data=ob) 
anova(m0,m2) 

rbind(logLik(m0),logLik(m1),logLik(m2))
summary(m0)
morans(m0)

# q2 ----
res <-cor.test(ob$Rank, ob$q2, method = "spearman",exact=F) # used exact = F to supress warning about ties - this 
cbind(res$statistic,res$p.value,res$estimate)
# bootstrapped 95% CI
quantile(
  replicate(10000, {
    boot.i <- sample(length(ob$Rank), replace = TRUE)
    cor(ob$Rank[boot.i],ob$q2[boot.i], method = "spearman")
  }), 
  c(0.025, 0.975))

m0 <- lm(log(q2)~Rank,data=ob)

m1 <- lm(log(q2)~Rank+Elevation,data=ob) 
anova(m0,m1) 

m2 <- lm(log(q2)~Rank+River_dist,data=ob) 
anova(m0,m2) 

rbind(logLik(m0),logLik(m1),logLik(m2))
summary(m0)
morans(m0)

# q3 -----
res <-cor.test(ob$Rank, ob$q3, method = "spearman",exact=F) # used exact = F to supress warning about ties - this 
cbind(res$statistic,res$p.value,res$estimate)
# bootstrapped 95% CI
quantile(
  replicate(10000, {
    boot.i <- sample(length(ob$Rank), replace = TRUE)
    cor(ob$Rank[boot.i],ob$q3[boot.i], method = "spearman")
  }), 
  c(0.025, 0.975))

m0 <- lm(log(q3)~Rank,data=ob)

m1 <- lm(log(q3)~Rank+Elevation,data=ob) 
anova(m0,m1) 

m2 <- lm(log(q3)~Rank+River_dist,data=ob) 
anova(m0,m2) 

rbind(logLik(m0),logLik(m1),logLik(m2))
summary(m0)
morans(m0)

# qInf ----
res <-cor.test(ob$Rank, ob$qInf, method = "spearman",exact=F) # used exact = F to supress warning about ties - this 
cbind(res$statistic,res$p.value,res$estimate)
# bootstrapped 95% CI
quantile(
  replicate(10000, {
    boot.i <- sample(length(ob$Rank), replace = TRUE)
    cor(ob$Rank[boot.i],ob$qInf[boot.i], method = "spearman")
  }), 
  c(0.025, 0.975))

m0 <- lm(log(qInf)~Rank,data=ob)

m1 <- lm(log(qInf)~Rank+Elevation,data=ob) 
anova(m0,m1) 

m2 <- lm(log(qInf)~Rank+River_dist,data=ob) 
anova(m0,m2) 

rbind(logLik(m0),logLik(m1),logLik(m2))
summary(m0)
morans(m0)

# est q0 ----
str(ob)
res <-cor.test(ob$Rank, ob$Est2_ss, method = "spearman",exact=F) # used exact = F to supress warning about ties - this 
cbind(res$statistic,res$p.value,res$estimate)
# bootstrapped 95% CI
quantile(
  replicate(10000, {
    boot.i <- sample(length(ob$Rank), replace = TRUE)
    cor(ob$Rank[boot.i],ob$Est2_ss[boot.i], method = "spearman")
  }), 
  c(0.025, 0.975))

m0 <- lm(log(Est2_ss)~Rank,data=ob)

m1 <- lm(log(Est2_ss)~Rank+Elevation,data=ob) 
anova(m0,m1) 

m2 <- lm(log(Est2_ss)~Rank+River_dist,data=ob) 
anova(m0,m2) 

rbind(logLik(m0),logLik(m1),logLik(m2))
summary(m0)
morans(m0)


# est q1 95%SC ----
res <-cor.test(ob$Rank, ob$e95q1, method = "spearman",exact=F) # used exact = F to supress warning about ties - this 
cbind(res$statistic,res$p.value,res$estimate)
# bootstrapped 95% CI
quantile(
  replicate(10000, {
    boot.i <- sample(length(ob$Rank), replace = TRUE)
    cor(ob$Rank[boot.i],ob$e95q1[boot.i], method = "spearman")
  }), 
  c(0.025, 0.975))

m0 <- lm(log(e95q1)~Rank,data=ob)

m1 <- lm(log(e95q1)~Rank+Elevation,data=ob) 
anova(m0,m1) 

m2 <- lm(log(e95q1)~Rank+River_dist,data=ob) 
anova(m0,m2) 

rbind(logLik(m0),logLik(m1),logLik(m2))
summary(m0)
morans(m0)

# est q2 ----
res <-cor.test(ob$Rank, ob$e95q2, method = "spearman",exact=F) # used exact = F to supress warning about ties - this 
cbind(res$statistic,res$p.value,res$estimate)
# bootstrapped 95% CI
quantile(
  replicate(10000, {
    boot.i <- sample(length(ob$Rank), replace = TRUE)
    cor(ob$Rank[boot.i],ob$e95q2[boot.i], method = "spearman")
  }), 
  c(0.025, 0.975))

m0 <- lm(log(e95q2)~Rank,data=ob)

m1 <- lm(log(e95q2)~Rank+Elevation,data=ob) 
anova(m0,m1) 

m2 <- lm(log(e95q2)~Rank+River_dist,data=ob) 
anova(m0,m2) 

rbind(logLik(m0),logLik(m1),logLik(m2))
summary(m0)
morans(m0)



# est q1 ----
res <-cor.test(ob$Rank, ob$asEst.q1, method = "spearman",exact=F) # used exact = F to supress warning about ties - this 
cbind(res$statistic,res$p.value,res$estimate)
# bootstrapped 95% CI
quantile(
  replicate(10000, {
    boot.i <- sample(length(ob$Rank), replace = TRUE)
    cor(ob$Rank[boot.i],ob$asEst.q1[boot.i], method = "spearman")
  }), 
  c(0.025, 0.975))

m0 <- lm(log(asEst.q1)~Rank,data=ob)

m1 <- lm(log(asEst.q1)~Rank+Elevation,data=ob) 
anova(m0,m1) 

m2 <- lm(log(asEst.q1)~Rank+River_dist,data=ob) 
anova(m0,m2) 

rbind(logLik(m0),logLik(m1),logLik(m2))
summary(m0)
morans(m0)

# est q2 ----
res <-cor.test(ob$Rank, ob$asEst.q2, method = "spearman",exact=F) # used exact = F to supress warning about ties - this 
cbind(res$statistic,res$p.value,res$estimate)
# bootstrapped 95% CI
quantile(
  replicate(10000, {
    boot.i <- sample(length(ob$Rank), replace = TRUE)
    cor(ob$Rank[boot.i],ob$asEst.q2[boot.i], method = "spearman")
  }), 
  c(0.025, 0.975))

m0 <- lm(log(asEst.q2)~Rank,data=ob)

m1 <- lm(log(asEst.q2)~Rank+Elevation,data=ob) 
anova(m0,m1) 

m2 <- lm(log(asEst.q2)~Rank+River_dist,data=ob) 
anova(m0,m2) 

rbind(logLik(m0),logLik(m1),logLik(m2))
summary(m0)
morans(m0)

# est q3 ----
res <-cor.test(ob$Rank, ob$e95q3, method = "spearman",exact=F) # used exact = F to supress warning about ties - this 
cbind(res$statistic,res$p.value,res$estimate)
# bootstrapped 95% CI
quantile(
  replicate(10000, {
    boot.i <- sample(length(ob$Rank), replace = TRUE)
    cor(ob$Rank[boot.i],ob$e95q3[boot.i], method = "spearman")
  }), 
  c(0.025, 0.975))

m0 <- lm(log(e95q3)~Rank,data=ob)

m1 <- lm(log(e95q3)~Rank+Elevation,data=ob) 
anova(m0,m1) 

m2 <- lm(log(e95q3)~Rank+River_dist,data=ob) 
anova(m0,m2) 

rbind(logLik(m0),logLik(m1),logLik(m2))
summary(m0)
morans(m0)

# redundancy rho q1 ----
res <-cor.test(ob$Rank, ob$rq1, method = "spearman",exact=F) # used exact = F to supress warning about ties - this 
cbind(res$statistic,res$p.value,res$estimate)
# bootstrapped 95% CI
quantile(
  replicate(10000, {
    boot.i <- sample(length(ob$Rank), replace = TRUE)
    cor(ob$Rank[boot.i],ob$rq0[boot.i], method = "spearman")
  }), 
  c(0.025, 0.975))

m0 <- lm(log(rq1)~Rank,data=ob)

m1 <- lm(log(rq1)~Rank+Elevation,data=ob) 
anova(m0,m1) 

m2 <- lm(log(rq1)~Rank+River_dist,data=ob) 
anova(m0,m2) 

rbind(logLik(m0),logLik(m1),logLik(m2))
summary(m0)
morans(m0)

# redundancy rho q2 ----
res <-cor.test(ob$Rank, ob$rq2, method = "spearman",exact=F) # used exact = F to supress warning about ties - this 
cbind(res$statistic,res$p.value,res$estimate)
# bootstrapped 95% CI
quantile(
  replicate(10000, {
    boot.i <- sample(length(ob$Rank), replace = TRUE)
    cor(ob$Rank[boot.i],ob$rq2[boot.i], method = "spearman")
  }), 
  c(0.025, 0.975))

m0 <- lm(log(rq2)~Rank,data=ob)

m1 <- lm(log(rq2)~Rank+Elevation,data=ob) 
anova(m0,m1) 

m2 <- lm(log(rq2)~Rank+River_dist,data=ob) 
anova(m0,m2) 

rbind(logLik(m0),logLik(m1),logLik(m2))
summary(m0)
morans(m0)

# representativeness rho bar q0 ----
res <-cor.test(ob$Rank, ob$rbq1, method = "spearman",exact=F) # used exact = F to supress warning about ties - this 
cbind(res$statistic,res$p.value,res$estimate)
# bootstrapped 95% CI
quantile(
  replicate(10000, {
    boot.i <- sample(length(ob$Rank), replace = TRUE)
    cor(ob$Rank[boot.i],ob$rbq1[boot.i], method = "spearman")
  }), 
  c(0.025, 0.975))

m0 <- lm(log(rbq1)~Rank,data=ob)

m1 <- lm(log(rbq1)~Rank+Elevation,data=ob) 
anova(m0,m1) 

m2 <- lm(log(rbq1)~Rank+River_dist,data=ob) 
anova(m0,m2) 

rbind(logLik(m0),logLik(m1),logLik(m2))
summary(m2)
morans(m2)

# representativeness rho q2 ----
res <-cor.test(ob$Rank, ob$rbq2, method = "spearman",exact=F) # used exact = F to supress warning about ties - this 
cbind(res$statistic,res$p.value,res$estimate)
# bootstrapped 95% CI
quantile(
  replicate(10000, {
    boot.i <- sample(length(ob$Rank), replace = TRUE)
    cor(ob$Rank[boot.i],ob$rbq2[boot.i], method = "spearman")
  }), 
  c(0.025, 0.975))

m0 <- lm(log(rbq2)~Rank,data=ob)

m1 <- lm(log(rbq2)~Rank+Elevation,data=ob) 
anova(m0,m1) 

m2 <- lm(log(rbq2)~Rank+River_dist,data=ob) 
anova(m0,m2) 

rbind(logLik(m0),logLik(m1),logLik(m2))
summary(m2)
morans(m2)

# pollinator visits ~ disturbance ----
res <-cor.test(ob$Rank, ob$Visits, method = "spearman",exact=F) # used exact = F to supress warning about ties - this 
cbind(res$statistic,res$p.value,res$estimate)
# bootstrapped 95% CI
quantile(
  replicate(10000, {
    boot.i <- sample(length(ob$Rank), replace = TRUE)
    cor(ob$Rank[boot.i],ob$Visits[boot.i], method = "spearman")
  }), 
  c(0.025, 0.975))

m0 <- lm(log(Visits)~Rank,data=ob)

m1 <- lm(log(Visits)~Rank+Elevation,data=ob) 
anova(m0,m1) 

m2 <- lm(log(Visits)~Rank+River_dist,data=ob) 
anova(m0,m2) 

rbind(logLik(m0),logLik(m1),logLik(m2))
summary(m0)
morans(m0)

# pollinator visits ~ q0 ----
res <-cor.test(ob$q0, ob$Visits, method = "spearman",exact=F) # used exact = F to supress warning about ties - this 
cbind(res$statistic,res$p.value,res$estimate)
# bootstrapped 95% CI
quantile(
  replicate(10000, {
    boot.i <- sample(length(ob$q0), replace = TRUE)
    cor(ob$q0[boot.i],ob$Visits[boot.i], method = "spearman")
  }), 
  c(0.025, 0.975))

m0 <- lm(log(Visits)~q0,data=ob)

m1 <- lm(log(Visits)~q0+Elevation,data=ob) 
anova(m0,m1) 

m2 <- lm(log(Visits)~q0+River_dist,data=ob) 
anova(m0,m2) 

rbind(logLik(m0),logLik(m1),logLik(m2))
summary(m0)
morans(m0)

# pollinator visits ~ q1 ----
res <-cor.test(ob$q1, ob$Visits, method = "spearman",exact=F) # used exact = F to supress warning about ties - this 
cbind(res$statistic,res$p.value,res$estimate)

# bootstrapped 95% CI
quantile(
  replicate(10000, {
    boot.i <- sample(length(ob$q1), replace = TRUE)
    cor(ob$q1[boot.i],ob$Visits[boot.i], method = "spearman")
  }), 
  c(0.025, 0.975))

# pollinator visits ~ q2 ----
res <-cor.test(ob$q2, ob$Visits, method = "spearman",exact=F) # used exact = F to supress warning about ties - this 
cbind(res$statistic,res$p.value,res$estimate)
# bootstrapped 95% CI
quantile(
  replicate(10000, {
    boot.i <- sample(length(ob$q2), replace = TRUE)
    cor(ob$q2[boot.i],ob$Visits[boot.i], method = "spearman")
  }), 
  c(0.025, 0.975))

#////////////////

# Beta diversity -----
rm(list=ls()) 
# libraries ----
# for ordinations
library(vegan)
library(ape)

# + for beta diversity, LCBD etc and plotting
library(adespatial)
require(vegan)  
require(adegraphics)
require(ade4)
require(RColorBrewer)


ob <- read.csv("C://Data/PhD/Processed_data/Orchidbees/OBcountmat.csv",row.names = 1)

for(r in 1:18){
  ob[,r] <- as.numeric(as.character(ob[,r]))}
str(obm)
obm <- as.matrix(ob)
obm <- t(obm)
rownames(obm)

# PCA, with hellinger tranformation
ob.transf = decostand(obm,"hel")
rda.out <- rda(ob.transf) # default scale option =F # pc1 = 0.10514 pc2=0.03947 
summary(rda.out,scaling=1) 
#custom pca plot
scrs <- scores(rda.out,display = c("sites", "species"), scaling =1) #extract sepecies and site scores from rda summary
##png(file="C://Data/PhD/Outputs/Orchidbees/Beta/OB_PCA_community.png",width=10,height=7.5,units="in",res=180)  
xlim <- with(scrs, range((scrs$species[,1])/1.8, scrs$sites[,1]))
ylim <- with(scrs, range((scrs$species[,2])/1.8, scrs$sites[,2]))
par(mar=c(1,1,1,1))
plot.new()
plot.window(xlim = xlim, ylim = ylim, asp = 1)
abline(h = 0, lty = "dotted")
abline(v = 0, lty = "dotted")
colvec <- c('#d73027','#d73027','#d73027','#fc8d59','#fc8d59','#fc8d59','#fee090','#fee090','#fee090','#e0f3f8','#e0f3f8','#e0f3f8','#91bfdb','#91bfdb','#91bfdb','#4575b4','#4575b4','#4575b4')
points(scrs$sites, col="black", bg= colvec, pch = 21,cex=4)
legend("bottomleft",bty="n",pch=21,pt.cex=2,cex=1.5,pt.bg=c('#d73027','#fc8d59','#fee090','#e0f3f8','#91bfdb','#4575b4'),legend=c("1.Banana (most disturbed)","2.Agroforestry","3.Disturbed secondary","4.Cleared regenerating","5.Mixed history","6.Minimally disturbed"))
#dev.off()


#tiff(file="C://Data/PhD/Outputs/Orchidbees/Figures/PCA_community_comp-col-hr.tiff",width=140,height=140,units="mm",res=1000,pointsize=12)  
par(mfrow=c(1,1))
xlim <- with(scrs, range((scrs$species[,1])/1.8, scrs$sites[,1]))
ylim <- with(scrs, range((scrs$species[,2])/1.8, scrs$sites[,2]))
par(mar=c(1,1,1,1))
plot.new()
plot.window(xlim = xlim, ylim = ylim, asp = 1)
abline(h = 0, lty = "dotted")
abline(v = 0, lty = "dotted")
colvec <- c('#d73027','#d73027','#d73027','#fc8d59','#fc8d59','#fc8d59','#fee090','#fee090','#fee090','#e0f3f8','#e0f3f8','#e0f3f8','#91bfdb','#91bfdb','#91bfdb','#4575b4','#4575b4','#4575b4')
#points(scrs$sites, col="black", bg= colvec,pch = 21,cex=4)
legend("bottomleft",bty="n",pch=21,pt.cex=2,cex=1,pt.bg=c('#d73027','#fc8d59','#fee090','#e0f3f8','#91bfdb','#4575b4'),legend=c("1.Banana (most disturbed)","2.Agroforestry","3.Disturbed secondary","4.Cleared regenerating","5.Mixed history","6.Minimally disturbed"))
text(0.01,0.31,"PC2",srt=90,font=2)
text(-0.4,-0.01,"PC1",font=2)
#dev.off()

##bw friendly version - with numbers instead of symbols
#tiff(file="C://Data/PhD/Outputs/Orchidbees/Figures/PCA_community_comp-bwcol.tiff",width=140,height=140,units="mm",res=1000,pointsize=12)  
xlim <- with(scrs, range((scrs$species[,1])/1.8, scrs$sites[,1]))
ylim <- with(scrs, range((scrs$species[,2])/1.8, (scrs$sites[,2])+0.02))
par(mar=c(1,1,1,1))
plot.new()
plot.window(xlim = xlim, ylim = ylim, asp = 1)
abline(h = 0, lty = "dotted")
abline(v = 0, lty = "dotted")
symvec <- c(rep("1",3),rep("2",3),rep("3",3),rep("4",3),rep("5",3),rep("6",3))
points(scrs$sites, col=colvec,pch = 1,cex=5, lwd=6)
points(scrs$sites, col="black",pch = 1,cex=6, lwd=0.5)
points(scrs$sites, col="black", bg= "grey",pch = symvec,cex=1.5,lwd=1.5)
text(0.015,0.33,"PC2",srt=90,font=2)
text(-0.4,-0.015,"PC1",font=2)
legend("bottomleft",bty="n",pch=21,pt.cex=2,cex=1,pt.bg=c('#d73027','#fc8d59','#fee090','#e0f3f8','#91bfdb','#4575b4'),legend=c("1.Banana (most disturbed)","2.Agroforestry","3.Disturbed secondary","4.Cleared regenerating","5.Mixed history","6.Minimally disturbed"))
#dev.off()

## OB RDA----
## use ob.transf as before
## add enviornmetnal vars
env  <- read.csv("C:/Data/PhD/Processed_data/Site_data/sites_elev_dist_coords.csv",row.names=1) #column 1 has row names
## filter only variables of interest:
sel <- c(1,3,10) # distance to river, elevation and disturbance rank
env <- env[,sel]
env <- as.matrix(env)
rda.out <- rda(ob.transf,env) ## same results whether or not yu scale env variables in advance

#summary(summary(rda.out)) # STRUCTURE OF SUMMARY FILE
summary(rda.out) #summary of rda.out

plot(rda.out, display=c("sp","sites","bp")) 
scrs <- scores(rda.out,display = c("sp","sites","bp")) #extract sepecies and site scores from rda summary
#png(file="C://Data/PhD/Outputs/Orchidbees/OB_RDA_community.png",width=10,height=7.5,units="in",res=180)  
tiff(file="C://Data/PhD/Outputs/Orchidbees/Figures/Ecolind/Fig5_1-5columns-RDAcommunity.tiff",width=140,height=120,units="mm",res=1000, pointsize=10)  
xlim <- c(-1,1)#with(scrs, range((scrs$sp[,1])/1.8, scrs$sites[,1]))
ylim <- c(-1.5,1.5)#with(scrs, range((scrs$sp[,2])/1.8, scrs$sites[,2]))
par(mar=c(1,1,1,1))
plot.new()
plot.window(xlim = xlim, ylim = ylim, asp = 1)
axis(side=1, at=c(-1.5,-1,-0.5,0.5,1,1.5), lty="solid", pos=c(0,0),padj=-1,cex.axis=0.7)
axis(side=2, at=c(-1.5,-1,-0.5,0.5,1,1.5), lty="solid", pos=c(0,0),padj=1,cex.axis=0.7)
#abline(h = 0, lty = "dotted")
#abline(v = 0, lty = "dotted")
colvec <- c('darkred','darkred','darkred','firebrick2','firebrick2','firebrick2','darksalmon','darksalmon','darksalmon','darkslategray3','darkslategray3','darkslategray3','dodgerblue2','dodgerblue2','dodgerblue2','navyblue','navyblue','navyblue')
#colvec <- c('#d73027','#d73027','#d73027','#fc8d59','#fc8d59','#fc8d59','#fee090','#fee090','#fee090','#e0f3f8','#e0f3f8','#e0f3f8','#91bfdb','#91bfdb','#91bfdb','#4575b4','#4575b4','#4575b4')
#points(scrs$sites, col="black", bg= colvec, pch = 21,cex=4)
## For bw-friendly option:
symvec <- c(rep("1",3),rep("2",3),rep("3",3),rep("4",3),rep("5",3),rep("6",3))
 points(scrs$sites, col=colvec,pch = 1,cex=4, lwd=4.5)
 points(scrs$sites, col="black",pch = 1,cex=5, lwd=0.5)
 points(scrs$sites, col="black", bg= "grey",pch = symvec,cex=1.5,lwd=1)
# plot arrows for each environental variable
arrows(x0=0, y0=0, scrs$biplot[1,1], scrs$biplot[1,2], code=2, lwd=1.5, col="black")
arrows(x0=0, y0=0, scrs$biplot[2,1], scrs$biplot[2,2], code=2, lwd=1.5, col="black")
arrows(x0=0, y0=0, scrs$biplot[3,1], scrs$biplot[3,2], code=2, lwd=1.5, col="black")
text(x=scrs$biplot[1,1]-0.3, y=scrs$biplot[1,2]+0.1,labels = c("Distance to river"))
text(x=scrs$biplot[2,1]-0.1, y=scrs$biplot[2,2]+0.1,labels = c("Elevation"))
text(x=scrs$biplot[3,1]-0.3, y=scrs$biplot[3,2]-0.15,labels = c("Disturbance rank"))
text(x=1.5, y=0.05,labels = c("RDA1"),cex=1)
text(x=0.05, y=-1.4,labels = c("RDA2"),cex=1,srt=90)
legend("bottomleft",bty="n",pch=21,pt.cex=1,cex=1,pt.bg=c('darkred','firebrick2','darksalmon','darkslategray3','dodgerblue2','navyblue'),
       legend=c("1.Banana (most disturbed)","2.Agroforestry","3.Disturbed secondary","4.Cleared regenerating","5.Mixed history","6.Minimally disturbed"))
dev.off()
#max(scrs$sites) ##cols c('#d73027','#fc8d59','#fee090','#e0f3f8','#91bfdb','#4575b4')





# --------------
#PCA w scaling 1
summary(rda.out, scaling=1) # Eigenvalues, species scores, site scores 
biplot(rda.out, scaling=1) # Function biplot.rda() of vegan is used  
biplot(rda.out, scaling=1,display=c("sites","species"))

# SCBD ----
head(obm)
res = beta.div(obm, "hellinger", nperm=999)

par(las=2) # make label text perpendicular to axis
par(mar=c(5,12,4,2)) # increase y-axis margin.
barplot(rev(sort(res$SCBD,decreasing=T)[1:10]),horiz=T,
        main="Species with highest 
        contributions to beta diversity",
        xlab="SCBD value")
top5SCBD<- sort(res$SCBD,decreasing=T)[c(1:5,14,15)]
sp5<- names(top5SCBD)

ob.sp <- t(obm)
head(ob.sp)

BA <- ob.sp[,1]+ob.sp[,2]+ob.sp[,3]
AF <- ob.sp[,4]+ob.sp[,5]+ob.sp[,6]
SF <- ob.sp[,7]+ob.sp[,8]+ob.sp[,9]
CC <- ob.sp[,10]+ob.sp[,11]+ob.sp[,12]
MX <- ob.sp[,13]+ob.sp[,14]+ob.sp[,15]
MD <- ob.sp[,16]+ob.sp[,17]+ob.sp[,18]

obsp <-cbind.data.frame(BA,AF,SF,CC,MX,MD)
names(obsp) <- c("1.Banana","2.Agroforestry","3.Secondary","4.Cleared regen.","5.Mixed","6.Minimal dist.")
obsp <- as.matrix(obsp)
rows5<- which(rownames(obsp) %in% sp5)
ob5 <- obsp[rows5,] # select the rows with the species with top SCBD
rownames(ob5) <- c("d) Euglossa chalybeata","a) Euglossa despecta","e) Euglossa occidentalis","b) Euglossa orellana","o) Eulaema bombiformis","c) Eulaema meriana","n) Eulaema mocsaryi")
par(mar=c(5,4,4,2)) # increase y-axis margin.
sp5names <- c(rownames(ob5))
cols5 <- c('#8c510a','#d8b365','#f6e8c3','#c7eae5','#5ab4ac','#01665e','#01665e')
#png(file="C://Data/PhD/Outputs/Orchidbees/Beta/SCBD_abund.png",width=10,height=7.5,units="in",res=180)  
ob5 <- ob5[order(row.names(ob5)),]
barplot(ob5,beside=T,ylab="No. of Individuals",xlab="Disturbance Rank",cex.axis=1.2,cex.lab=1.5,col=cols5)
legend("topleft",legend=sp5names,pch=21,pt.cex=2,cex=1,col=c("black"),pt.bg=cols5,bty="n")
dev.off()

blue10 <- brewer.pal(6,"BrBG")
tiff(file="C://Data/PhD/Outputs/Orchidbees/Figures/SCBD10_abund-hr.tiff",width=190,height=100,units="mm",res=1000, pointsize=9)  
barplot(ob5,beside=T,ylab="No. of Individuals",xlab="Disturbance Rank",cex.axis=1.2,cex.lab=1.5,ylim=c(0,120),col=blue10,names.arg = c("1","2","3","4","5","6"))
legend(0,120,legend=sp5names,pch=21,pt.cex=1.5,cex=1,col=c("black"),pt.bg=blue10,bty="n")
dev.off()

# greyscale graph
gray10 <- c("gray1","gray20","gray40","gray60","gray80","white","white")
bw10names <- rownames(ob5) #c("Euglossa augaspis (a)","Euglossa chalybeata (b)","Euglossa despecta (c)","Euglossa occidentalis (d)","Euglossa orellana (e)","Eulaema meriana (f)")

tiff(file="C://Data/PhD/Outputs/Orchidbees/Figures/Ecolind/Fig6-2columns-SCBD10_abund-grey.tiff",width=190,height=100,units="mm",res=1000, pointsize=9)  
par(mar=c(4.5,4.5,2.1,0.2))
bp <- barplot(ob5,beside=T,ylab="No. of Individuals",xlab="Disturbance Rank",ylim=c(-10,130),cex.axis=1,cex.lab=1,col=gray10,names.arg = c("1","2","3","4","5","6"))
legend(0,130,legend=bw10names,pch=21,pt.cex=1.5,text.font=3,cex=1,col=c("black"),pt.bg=gray10,bty="n")
text(bp,y=1, labels=c("a","b","c","d","e","n","o"),pos=1)
dev.off()


# Beta diversity partitioning: richness vs replacement ----
# Compute and partition a matrix of %difference indices (quantitative form of S?rensen index)
#out2 = beta.div.comp(ob.transf, coef="J", quant=F)
out2 = beta.div.comp(ob.transf, coef="S", quant=TRUE)
out2$part # Most of beta diversity in this dataset is due to species richness change 
#R he 5 values are:
#BDtotal (total beta diversity) = sum(D.ij)/(n*(n-1)) (Legendre & De C?ceres 2013). This is equal to sum(d.ij^2)/(n*(n-1)) where d.ij = sqrt(D.ij). The dissimilarities are square-rooted because the Jaccard, S?rensen, Ruzicka and %difference indices are not Euclidean.
#Total replacement diversity.
#Total richness difference diversity (or nestedness).
#Total replacement diversity/Total beta diversity.
#Total richness difference diversity (or nestedness)/Total beta diversity.

#//// Beta diversity across the gradient is 68% due to a change in richness, 32% turnover

# All abundace plot----
# plot abundance of all species at all habitats
#fake rownames 
rows <- cbind(obsp,"zeros"=rep(0,length(obsp[,1])))
png(file="C://Data/PhD/Outputs/Orchidbees/Abundance_all.png",width=14,height=25,units="in",res=300)  
par(mfrow=c(7,1))
par(mar=c(2,3,2.1,2.1))
barplot(obsp[,1],main="1. Banana",names.arg=c(1:31),ylim=c(0,100))
barplot(obsp[,2],main="2. Agroforestry",names.arg=c(1:31),ylim=c(0,100))
barplot(obsp[,3],main="3. Secondary",names.arg=c(1:31),ylim=c(0,100))
barplot(obsp[,4],main="4. Cleared Regenerating",names.arg=c(1:31),ylim=c(0,100))
barplot(obsp[,5],main="5. Mixed Use",names.arg=c(1:31),ylim=c(0,100))
barplot(obsp[,6],main="6. Minimally disturbed",names.arg=c(1:31),ylim=c(0,100))
par(mar=c(12,3,0,2.1))
#barplot(rows[,7],names.arg=c(1:31),axes=NULL,las=2)
dev.off()


# Orchid bee diversity permutations -----

# check for a continuous pattern across q=0,1,2,and inf by permuting the
# results at each value of q, and then combining the p values.

head(ob)

#DB$Rank,  DB$q0, DB$q0.5, DB$q1, DB$q2, DB$qInf

# mini function to make cortest shorter
co <- function(ia,ib){
  res <- cor.test(ia, ib, method = "spearman",exact=F)
  cbind(res$p.value)}

co(ob$Rank,ob$q0.5)

#Observed Alpha
# then re-run with re-ordered ranking, 1000 times.
perms <- c(NULL)
for(s in 1:10000){
  rs <- sample(rep(1:6,3))
  cr0 <- co(rs,ob$q0)
  cr05 <- co(rs,ob$q0.5)
  cr1 <- co(rs,ob$q1)
  cr2 <- co(rs,ob$q2)
  crInf <- co(rs,ob$qInf)# correlation result
  logcr <- log(cr0)+log(cr05)+log(cr1)+log(cr2)+log(crInf) #combined p vals
  run <- cbind(s,cr0,cr05,cr1,cr2,crInf,logcr)
  perms <- rbind(perms,run)
}
head(perms)
perms <- as.data.frame(perms)
names(perms) <- c("Run","p_q0","p_q05","p_q1","p_q2","p_qInf","p_logcr")

# compare permuted results with real correlation
cr0 <- co(ob$Rank,ob$q0)
cr05 <- co(ob$Rank,ob$q0.5)
cr1 <- co(ob$Rank,ob$q1)
cr2 <- co(ob$Rank,ob$q2)
crInf <- co(ob$Rank,ob$qInf)# correlation result
logcr <- log(cr0)+log(cr05)+log(cr1)+log(cr2)+log(crInf)
real<- as.data.frame(cbind(0,cr0,cr05,cr1,cr2,crInf,logcr)) #Run 0 is the real values
names(real) <- c("Run","p_q0","p_q05","p_q1","p_q2","p_qInf","p_logcr")
comb <- rbind(perms,real)
head(comb)
tail(comb)

# check if real (Run 0) is in bottom 5% of pvals for each
pq0 <- rank(comb$p_q0)[10001]/length(comb[,1]) #divide the rank of the real p val by the total number to ge the p value of the probability of getting that result by chance
pq05 <- rank(comb$p_q05)[10001]/length(comb[,1])
pq1 <- rank(comb$p_q1)[10001]/length(comb[,1])
pq2 <- rank(comb$p_q2)[10001]/length(comb[,1])
pqI <- rank(comb$p_qInf)[10001]/length(comb[,1])
pqlog <- rank(comb$p_logcr)[10001]/length(comb[,1])

pvals_perm <- cbind(pq0,pq05,pq1,pq2,pqI,pqlog)
pvals_perm

#Estimated Alpha
# then re-run with re-ordered ranking, 1000 times.
perms <- c(NULL)
for(s in 1:10000){
  rs <- sample(rep(1:6,3))
  cr0 <- co(rs,ob$Est0_ss)
  cr05 <- co(rs,ob$Est05_ss)
  cr1 <- co(rs,ob$Est1_ss)
  cr2 <- co(rs,ob$Est2_ss)
  logcr <- log(cr0)+log(cr05)+log(cr1)+log(cr2) #combined p vals
  run <- cbind(s,cr0,cr05,cr1,cr2,logcr)
  perms <- rbind(perms,run)
}
head(perms)
perms <- as.data.frame(perms)
names(perms) <- c("Run","p_q0","p_q05","p_q1","p_q2","p_logcr")

# compare permuted results with real correlation
cr0 <- co(ob$Rank,ob$Est0_ss)
cr05 <- co(ob$Rank,ob$Est05_ss)
cr1 <- co(ob$Rank,ob$Est1_ss)
cr2 <- co(ob$Rank,ob$Est2_ss)
logcr <- log(cr0)+log(cr05)+log(cr1)+log(cr2)
real<- as.data.frame(cbind(0,cr0,cr05,cr1,cr2,logcr)) #Run 0 is the real values
names(real) <- c("Run","p_q0","p_q05","p_q1","p_q2","p_logcr")
comb <- rbind(perms,real)
head(comb)
tail(comb)

# check if real (Run 0) is in bottom 5% of pvals for each
pq0 <- rank(comb$p_q0)[10001]/length(comb[,1]) #divide the rank of the real p val by the total number to ge the p value of the probability of getting that result by chance
pq05 <- rank(comb$p_q05)[10001]/length(comb[,1])
pq1 <- rank(comb$p_q1)[10001]/length(comb[,1])
pq2 <- rank(comb$p_q2)[10001]/length(comb[,1])
pqlog <- rank(comb$p_logcr)[10001]/length(comb[,1])

pvals_perm <- cbind(pq0,pq05,pq1,pq2,pqlog)
pvals_perm


## Rho 
# then re-run with re-ordered ranking, 1000 times.
perms <- c(NULL)
for(s in 1:10000){
  rs <- sample(rep(1:6,3))
  cr0 <- co(rs,ob$rq0)
  cr05 <- co(rs,ob$rq05)
  cr1 <- co(rs,ob$rq1)
  cr2 <- co(rs,ob$rq2)
  crInf <- co(rs,ob$rqInf)# correlation result
  logcr <- log(cr0)+log(cr05)+log(cr1)+log(cr2)+log(crInf) #combined p vals
  run <- cbind(s,cr0,cr05,cr1,cr2,crInf,logcr)
  perms <- rbind(perms,run)
}
head(perms)
perms <- as.data.frame(perms)
names(perms) <- c("Run","p_q0","p_q05","p_q1","p_q2","p_qInf","p_logcr")

# compare permuted results with real correlation
cr0 <- co(ob$Rank,ob$rq0)
cr05 <- co(ob$Rank,ob$rq05)
cr1 <- co(ob$Rank,ob$rq1)
cr2 <- co(ob$Rank,ob$rq2)
crInf <- co(ob$Rank,ob$rqInf)# correlation result
logcr <- log(cr0)+log(cr05)+log(cr1)+log(cr2)+log(crInf)
real<- as.data.frame(cbind(0,cr0,cr05,cr1,cr2,crInf,logcr)) #Run 0 is the real values
names(real) <- c("Run","p_q0","p_q05","p_q1","p_q2","p_qInf","p_logcr")
comb <- rbind(perms,real)
head(comb)
tail(comb)

# check if real (Run 0) is in bottom 5% of pvals for each
pq0 <- rank(comb$p_q0)[10001]/length(comb[,1]) #divide the rank of the real p val by the total number to ge the p value of the probability of getting that result by chance
pq05 <- rank(comb$p_q05)[10001]/length(comb[,1])
pq1 <- rank(comb$p_q1)[10001]/length(comb[,1])
pq2 <- rank(comb$p_q2)[10001]/length(comb[,1])
pqI <- rank(comb$p_qInf)[10001]/length(comb[,1])
pqlog <- rank(comb$p_logcr)[10001]/length(comb[,1])

pvals_perm <- cbind(pq0,pq05,pq1,pq2,pqI,pqlog)
pvals_perm

## Rho bar
# then re-run with re-ordered ranking, 1000 times.
perms <- c(NULL)
for(s in 1:10000){
  rs <- sample(rep(1:6,3))
  cr0 <- co(rs,ob$rbq0)
  cr05 <- co(rs,ob$rbq05)
  cr1 <- co(rs,ob$rbq1)
  cr2 <- co(rs,ob$rbq2)
  crInf <- co(rs,ob$rbqInf)# correlation result
  logcr <- log(cr0)+log(cr05)+log(cr1)+log(cr2)+log(crInf) #combined p vals
  run <- cbind(s,cr0,cr05,cr1,cr2,crInf,logcr)
  perms <- rbind(perms,run)
}
head(perms)
perms <- as.data.frame(perms)
names(perms) <- c("Run","p_q0","p_q05","p_q1","p_q2","p_qInf","p_logcr")

# compare permuted results with real correlation
cr0 <- co(ob$Rank,ob$rbq0)
cr05 <- co(ob$Rank,ob$rbq05)
cr1 <- co(ob$Rank,ob$rbq1)
cr2 <- co(ob$Rank,ob$rbq2)
crInf <- co(ob$Rank,ob$rbqInf)# correlation result
logcr <- log(cr0)+log(cr05)+log(cr1)+log(cr2)+log(crInf)
real<- as.data.frame(cbind(0,cr0,cr05,cr1,cr2,crInf,logcr)) #Run 0 is the real values
names(real) <- c("Run","p_q0","p_q05","p_q1","p_q2","p_qInf","p_logcr")
comb <- rbind(perms,real)
head(comb)
tail(comb)

# check if real (Run 0) is in bottom 5% of pvals for each
pq0 <- rank(comb$p_q0)[10001]/length(comb[,1]) #divide the rank of the real p val by the total number to ge the p value of the probability of getting that result by chance
pq05 <- rank(comb$p_q05)[10001]/length(comb[,1])
pq1 <- rank(comb$p_q1)[10001]/length(comb[,1])
pq2 <- rank(comb$p_q2)[10001]/length(comb[,1])
pqI <- rank(comb$p_qInf)[10001]/length(comb[,1])
pqlog <- rank(comb$p_logcr)[10001]/length(comb[,1])

pvals_perm <- cbind(pq0,pq05,pq1,pq2,pqI,pqlog)
pvals_perm

### Test spatial/river boundary effects on model results----

#split sides of river and test separately
obE <- ob[ob$Rank<3.5,]
obW <- ob[ob$Rank>3.5,]

m1e <- lm(log(abund)~Rank+River_dist, data=obE)
m1w <- lm(log(abund)~Rank+River_dist, data=obW)
summary(m1e)
summary(m1w)

m2e <- lm(log(q0)~Rank,data=obE)
m2w <- lm(log(q0)~Rank,data=obW)
summary(m2e)
summary(m2w)

m2e <- lm(log(rq1)~Rank,data=obE)
m2w <- lm(log(rq1)~Rank,data=obW)
summary(m2e)
summary(m2w)

m2e <- lm(log(rbq1)~Rank+River_dist,data=obE)
m2w <- lm(log(rbq1)~Rank+River_dist,data=obW)
summary(m2e)
summary(m2w)

str(ob)


#include e-w coordinates in models
m1 <- lm(log(q0)~Rank+UTM_Coords_E,data=ob)
summary(m1)

m1 <- lm(log(rq1)~Rank+UTM_Coords_E,data=ob)
summary(m1)

m1 <- lm(log(rbq1)~Rank+UTM_Coords_E,data=ob)
summary(m1)

m2 <- lm(log(abund)~Rank+River_dist+UTM_Coords_E,data=ob)
summary(m2)

str(ob)


## what about if I looked at veg structure instead of rank?
m1 <- lm(log(q0)~veg_pc1+UTM_Coords_E,data=ob)
summary(m1)

m1 <- lm(log(rq1)~veg_pc1+UTM_Coords_E,data=ob)
summary(m1)

m1 <- lm(log(rbq1)~veg_pc1+UTM_Coords_E,data=ob)
summary(m1)

m2 <- lm(log(abund)~veg_pc1+River_dist+UTM_Coords_E,data=ob)
summary(m2)

##
m1e <- lm(log(abund)~veg_pc1, data=obE)
m1w <- lm(log(abund)~veg_pc1, data=obW)
summary(m1e)
summary(m1w)

m2e <- lm(log(q0)~veg_pc1,data=obE)
m2w <- lm(log(q0)~veg_pc1,data=obW)
summary(m2e)
summary(m2w)

m2e <- lm(log(rq1)~veg_pc1,data=obE)
m2w <- lm(log(rq1)~veg_pc1,data=obW)
summary(m2e)
summary(m2w)

m2e <- lm(log(rbq1)~veg_pc1+River_dist,data=obE)
m2w <- lm(log(rbq1)~veg_pc1+River_dist,data=obW)
summary(m2e)
summary(m2w)


## and veg instead of rank in general?

m1 <- lm(log(q0)~veg_pc1,data=ob)
summary(m1)

m1 <- lm(log(rq1)~veg_pc1,data=ob)
summary(m1)

m1 <- lm(log(rbq1)~veg_pc1+River_dist,data=ob)
summary(m1)

m2 <- lm(log(abund)~veg_pc1+River_dist,data=ob)
summary(m2)
