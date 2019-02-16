
#start ----
rm(list=ls()) ##Clear memory
require(dplyr)
require(tidyr)
require(lubridate)
require(plyr)


# combining datasheets ----
# to form complete orchid bee database - revisions of chalybeata 
# First import raw data - almost complete database
options(stringsAsFactors = FALSE) # import not as factors
OBJan=read.csv("C://Data/PhD/Rawdata/Orchidbees/orchidbee_specimens_JAN17.csv",header=TRUE)
str(OBJan)

#revised chalybeata IDs
OBchal=read.csv("C://Data/PhD/Rawdata/Orchidbees/REVISION-chalybeata.csv",header=TRUE)
str(OBchal)

OBcoords=read.csv("C://Data/PhD/Rawdata/Orchidbees/orchidbee_sitecoords.csv",header=TRUE)
str(OBcoords)

OB_merge1 <- merge(OBJan,OBcoords,by="Site",all.x=TRUE)
OB_merge2 <- merge(OB_merge1,OBchal,by="Specimen_code",all.x=TRUE)
str(OB_merge2)

#write.csv(OB_merge2, "C://Data/PhD/Rawdata/Orchidbees/OB_data_combined.csv")

# Now import complete dataset (everything combined including species revision)
OBdata=read.csv("C://Data/PhD/Processed_data/Orchidbees/OB_data_COMPLETE.csv",header=TRUE)
str(OBdata)

head(OBdata)
# select relevant columns for analysis
# column Nos
cols <- c(1,2,3,7,8,9,10,18,19,20,21,22,23)
ob <- OBdata[,cols]
head(ob)

summary(ob)
str(ob)

#convert from character to factors
ob$Site <- as.factor(ob$Site)
ob$Genus <- as.factor(ob$Genus)
ob$SPECIES <- as.factor(ob$SPECIES)
ob$AM_PM <- as.factor(ob$AM_PM)
ob$Bait <- as.factor(ob$Bait)
ob$Date_collected <- as.factor(ob$Date_collected)
ob$Specimen_code <-as.character(ob$Specimen_code)

# Fix species names

data.frame(table(ob$SPECIES)) #frequencies of each species

# some misspelling
fix=which(ob$SPECIES=="Eufriesea pulcra")
cat("# of misspellings=",length(fix))
ob[fix,"SPECIES"]="Eufriesea pulchra"
sort(unique(ob$SPECIES))

fix=which(ob$SPECIES=="Euglossa gaianni")
cat("# of misspellings=",length(fix))
ob[fix,"SPECIES"]="Euglossa gaianii"
sort(unique(ob$SPECIES))

# fix=which(ob$SPECIES=="Euglossa gaianii")
# cat("# of misspellings=",length(fix))
# ob[fix,"SPECIES"]="Euglossa hemichlora"
# sort(unique(ob$SPECIES))

# fix=which(ob$SPECIES=="Eufriesea magretti")
# cat("# of misspellings=",length(fix))
# ob[fix,"SPECIES"]="Eufriesea magrettii"
# sort(unique(ob$SPECIES))

fix=which(ob$SPECIES=="Eufriesea purpurata")
cat("# of misspellings=",length(fix))
ob[fix,"SPECIES"]="Eufriesea rufocauda"
sort(unique(ob$SPECIES))

# there are a few euglossa "sps", but they are spelt slightly differently 
# should convert to sp1 etc, but can't tell if all different or not. Includes unidentifiable females.
# Only about 10 soecimens total - so removed from data.
#Also removing blank species rows.

excl.spec<-c("Euglossa sp", "Euglossa sp.", "Eulaema sp female", "")

fix=which(ob$SPECIES %in% excl.spec)
cat("# of misspellings=",length(fix))
ob <- ob[-fix,] # remove blank species rows
sort(unique(ob$SPECIES))

summary(ob)
str(ob)

########
# fix site names
sort(unique(ob$Site))
data.frame(table(ob$Site)) #frequencies of each site
#remove blanks and extra survey locations (eg pini)
excl.site<-c("PINI-PINI 1121", "PINI-PINI 1020", "PINI-PINI 950", "PINI-PINI 1147", "PINI-PINI", "unknown", 
               "WETLAND", "MLC", "")

fix=which(ob$Site %in% excl.site)
cat("# of misspellings=",length(fix))
ob <- ob[-fix,] # remove blank species rows
unique(ob$Site)

######
#fixing dates
data.frame(table(ob$Date_collected)) #There is a date from 2014, same date as one from 2015.

fix=which(ob$Date_collected =="04/08/2014")
cat("# of misspellings=",length(fix))
ob[fix,"Date_collected"]="04/08/2015"
data.frame(table(ob$Date_collected))

######
##fixing am_pm
data.frame(table(ob$AM_PM))

fix=which(ob$AM_PM =="AM ")
cat("# of misspellings=",length(fix))
ob[fix,"AM_PM"]="AM"
data.frame(table(ob$AM_PM))

#########
##fixing genus

data.frame(table(ob$SPECIES, ob$Genus))#Some empty cases and some problems with species with wrong genus.
#make a genus column. Gives a warning, but cannot see that it causes any issue.
ob2= ob %>%separate(SPECIES,c("genus","specname"),remove=FALSE) %>%
select(-specname, -Genus)#to take out the dummy variable and the original genus variable.

ob <- ob2[-c(156,247,783),] # these rows are euglossa sp females - error comes up because there are 3 parts to the name, but I actually dont want these species included anywya, so removing them.
data.frame(table(ob$SPECIES, ob$genus))

######
##checking other variables
#Not sure if specimen_code is important.
dups<-data.frame(table(ob$Specimen_code))#There are a few also starting with an a, as they are probably duplications
ob[ob$Specimen_code %in% dups$Var1[dups$Freq > 1],]  #A few cases with two records, not sure what to do with that.
#This will add an .1 to the second record.Tried other solutions, but cannot make them work.
make.unique(ob$Specimen_code)

data.frame(table(ob$Bait))#seems ok, not fully sure if there needs to be any exclusion (TT?)

table(ob$UTM_Zone)#ok
data.frame(table(ob$Elevation))#ok - why only 16 elevations? - checked, there are sites with same elevation, so it's fine
data.frame(table(ob$DD.Easting))#ok
data.frame(table(ob$UTM_Coords_E))#ok
data.frame(table(ob$UTM_Coords_S))#ok

# add rank to site name so its in corret order
ob$Site <- as.factor(ob$Site)
levels(ob$Site)[levels(ob$Site)=="BA-A"] <- "1.BA-A"
levels(ob$Site)[levels(ob$Site)=="BA-B"] <- "1.BA-B"
levels(ob$Site)[levels(ob$Site)=="BA-C"] <- "1.BA-C"
levels(ob$Site)[levels(ob$Site)=="AF-A"] <- "2.AF-A"
levels(ob$Site)[levels(ob$Site)=="AF-B"] <- "2.AF-B"
levels(ob$Site)[levels(ob$Site)=="AF-C"] <- "2.AF-C"
levels(ob$Site)[levels(ob$Site)=="SF-A"] <- "3.SF-A"
levels(ob$Site)[levels(ob$Site)=="SF-B"] <- "3.SF-B"
levels(ob$Site)[levels(ob$Site)=="SF-C"] <- "3.SF-C"
levels(ob$Site)[levels(ob$Site)=="CCR-A"] <- "4.CCR-A"
levels(ob$Site)[levels(ob$Site)=="CCR-B"] <- "4.CCR-B"
levels(ob$Site)[levels(ob$Site)=="CCR-C"] <- "4.CCR-C"
levels(ob$Site)[levels(ob$Site)=="MXD-A"] <- "5.MXD-A"
levels(ob$Site)[levels(ob$Site)=="MXD-B"] <- "5.MXD-B"
levels(ob$Site)[levels(ob$Site)=="MXD-C"] <- "5.MXD-C"
levels(ob$Site)[levels(ob$Site)=="MIN-A"] <- "6.MIN-A"
levels(ob$Site)[levels(ob$Site)=="MIN-B"] <- "6.MIN-B"
levels(ob$Site)[levels(ob$Site)=="MIN-C"] <- "6.MIN-C"

#write.csv(ob, "C://Data/PhD/Processed_data/Orchidbees/OB_data_clean.csv")

# diversity ----
# data now needs to be combined with site info - habitat and disturbance rank

ob <- read.csv("C://Data/PhD/Processed_data/Orchidbees/OB_data_clean.csv")

 
head(ob)
str(ob)
keepcols <- c(2,4,5,6,7,8,9,10,12,13,14,15)

ob <- ob[,keepcols]
ob$Site <- as.factor(ob$Site)
ob$SPECIES <- as.factor(ob$SPECIES)
ob$genus <- as.factor(ob$genus)
ob$AM_PM <- as.factor(ob$AM_PM)
ob$Bait <- as.factor(ob$Bait)




# create sp x site matrix ----
ob.mat<- tapply(as.factor(ob$SPECIES),list(as.factor(ob$SPECIES),ob$Site), length)  
ob.mat[is.na(ob.mat)] <- 0
head(ob.mat)

#write.csv(ob.mat,"C://Data/PhD/Processed_data/Orchidbees/OBcountmat.csv")

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

# export diversity values ----
#
 # write.csv(alpha,"C://Data/PhD/Processed_data/Orchidbees/Diversity/obalpha.csv")
 # write.csv(beta,"C://Data/PhD/Processed_data/Orchidbees/Diversity/obbeta.csv")
 # write.csv(beta_b,"C://Data/PhD/Processed_data/Orchidbees/Diversity/obbeta_b.csv")
 # write.csv(rho,"C://Data/PhD/Processed_data/Orchidbees/Diversity/obrho.csv")
 # write.csv(rho_b,"C://Data/PhD/Processed_data/Orchidbees/Diversity/obrho_b.csv")
 # write.csv(gamma,"C://Data/PhD/Processed_data/Orchidbees/Diversity/obgamma.csv")

# 
# plot just rho at q=1 ----
Rank <- c(1,1,1,2,2,2,3,3,3,4,4,4,5,5,5,6,6,6)
rhoq1rank <- as.data.frame(cbind(Rho=rho[,2],Rank))
#png(file="C://Data/PhD/Outputs/Orchidbees/rhoq1.png",width=7,height=5,units="in",res=250)  
tiff("C://Data/PhD/Outputs/Orchidbees/Figures/rhoq1_lr.tiff",width=140,height=140,units="mm",res=300, pointsize=12)  
#plot(rhoq1rank$Rho~rhoq1rank$Rank)
png(file="C://Data/PhD/Outputs/Orchidbees/Figures/OBrhoq1_abline.png",width=10,height=7,units="in",pointsize=18,res=300)  
plot(rhoq1rank$Rho~jitter(rhoq1rank$Rank),bty="n",pch=19,cex=1.5,cex.axis=1.2,cex.lab=1.2,col='black',ylab=expression(paste("Site redundancy (",rho,")")),xlab="Disturbance Rank (jittered)",ylim=c(0,60))
abline(lm(rhoq1rank$Rho~rhoq1rank$Rank))
dev.off()
# ###

# plot just rhobar at q=1 ----
Rank <- c(1,1,1,2,2,2,3,3,3,4,4,4,5,5,5,6,6,6)
rhobarq1rank <- as.data.frame(cbind(Rhobar=rho_b[,2],Rank))
#png(file="C://Data/PhD/Outputs/Orchidbees/rhoq1.png",width=7,height=5,units="in",res=250)  
tiff("C://Data/PhD/Outputs/Orchidbees/Figures/rhobarq1_abline.tiff",width=140,height=140,units="mm",res=300, pointsize=12)  
plot(rhobarq1rank$Rhobar~jitter(rhobarq1rank$Rank),bty="n",pch=19,cex=1.5,cex.axis=1.2,cex.lab=1.2,col='black',ylab=expression(paste("Site representativeness (",bar(rho),")")),xlab="Disturbance Rank (jittered)",ylim=c(0.4,1))
abline(lm(rhobarq1rank$Rho~rhoq1rank$Rank))
dev.off()
# ###

##/////////////
#### iNEXT diversity estimates ----

library(iNEXT)
library(ggplot2)

ob.mat <- as.matrix(read.csv("C://Data/PhD/Processed_data/Orchidbees/OBcountmat.csv",row.names=1,check.names = F))
 
head(ob.mat)
colSums(ob.mat)
str(ob.mat)
summary(ob.mat) 


iN012 <- iNEXT(ob.mat, q=c(0,0.5,1,2), datatype="abundance", size=NULL, endpoint=50, knots=25, se=TRUE, conf=0.95, nboot=100)

#extract the dataframes for each site
estq123_BAA <- as.data.frame(iN012$iNextEst$`1.BA-A`)
estq123_BAB <- as.data.frame(iN012$iNextEst$`1.BA-B`)
estq123_BAC <- as.data.frame(iN012$iNextEst$`1.BA-C`)
estq123_AFA <- as.data.frame(iN012$iNextEst$`2.AF-A`)
estq123_AFB <- as.data.frame(iN012$iNextEst$`2.AF-B`)
estq123_AFC <- as.data.frame(iN012$iNextEst$`2.AF-C`)
estq123_SFA <- as.data.frame(iN012$iNextEst$`3.SF-A`)
estq123_SFB <- as.data.frame(iN012$iNextEst$`3.SF-B`)
estq123_SFC <- as.data.frame(iN012$iNextEst$`3.SF-C`)
estq123_CCRA <- as.data.frame(iN012$iNextEst$`4.CCR-A`)
estq123_CCRB <- as.data.frame(iN012$iNextEst$`4.CCR-B`)
estq123_CCRC <- as.data.frame(iN012$iNextEst$`4.CCR-C`)
estq123_MXDA <- as.data.frame(iN012$iNextEst$`5.MXD-A`)
estq123_MXDB <- as.data.frame(iN012$iNextEst$`5.MXD-B`)
estq123_MXDC <- as.data.frame(iN012$iNextEst$`5.MXD-C`)
estq123_MINA <- as.data.frame(iN012$iNextEst$`6.MIN-A`)
estq123_MINB <- as.data.frame(iN012$iNextEst$`6.MIN-B`)
estq123_MINC <- as.data.frame(iN012$iNextEst$`6.MIN-C`)

r <- which(estq123_BAA$m==50) #select the end rows for equal sample size estimates
BAA <- estq123_BAA[r,]
r <- which(estq123_BAB$m==50) #select the end rows for equal sample size estimates
BAB <- estq123_BAB[r,]
r <- which(estq123_BAC$m==50) #select the end rows for equal sample size estimates
BAC <- estq123_BAC[r,]
r <- which(estq123_AFA$m==50) #select the end rows for equal sample size estimates
AFA <- estq123_AFA[r,]
r <- which(estq123_AFB$m==50) #select the end rows for equal sample size estimates
AFB<- estq123_AFB[r,]
r <- which(estq123_AFC$m==50) #select the end rows for equal sample size estimates
AFC<- estq123_AFC[r,]
r <- which(estq123_SFA$m==50) #select the end rows for equal sample size estimates
SFA <- estq123_SFA[r,]
r <- which(estq123_SFB$m==50) #select the end rows for equal sample size estimates
SFB<- estq123_SFB[r,]
r <- which(estq123_SFC$m==50) #select the end rows for equal sample size estimates
SFC<- estq123_SFC[r,]
r <- which(estq123_CCRA$m==50) #select the end rows for equal sample size estimates
CCRA <- estq123_CCRA[r,]
r <- which(estq123_CCRB$m==50) #select the end rows for equal sample size estimates
CCRB <- estq123_CCRB[r,]
r <- which(estq123_CCRC$m==50) #select the end rows for equal sample size estimates
CCRC<- estq123_CCRC[r,]
r <- which(estq123_MXDA$m==50) #select the end rows for equal sample size estimates
MXDA <- estq123_MXDA[r,]
r <- which(estq123_MXDB$m==50) #select the end rows for equal sample size estimates
MXDB <- estq123_MXDB[r,]
r <- which(estq123_MXDC$m==50) #select the end rows for equal sample size estimates
MXDC<- estq123_MXDC[r,]
r <- which(estq123_MINA$m==50) #select the end rows for equal sample size estimates
MINA <- estq123_MINA[r,]
r <- which(estq123_MINB$m==50) #select the end rows for equal sample size estimates
MINB <- estq123_MINB[r,]
r <- which(estq123_MINC$m==50) #select the end rows for equal sample size estimates
MINC<- estq123_MINC[r,]

Estqs <- rbind(BAA,BAB,BAC,AFA,AFB,AFC,SFA,SFB,SFC,CCRA,CCRB,CCRC,MXDA,MXDB,MXDC,MINA,MINB,MINC)
Site <- c(rep("BA-A",4),rep("BA-B",4),rep("BA-C",4),rep("AF-A",4),rep("AF-B",4),rep("AF-C",4),rep("SF-A",4),rep("SF-B",4),rep("SF-C",4),rep("CCR-A",4),rep("CCR-B",4),rep("CCR-C",4),rep("MXD-A",4),rep("MXD-B",4),rep("MXD-C",4),rep("MIN-A",4),rep("MIN-B",4),rep("MIN-C",4))
Ests <- cbind(Estqs,Site)

Site <- c("BA-A","BA-B","BA-C","AF-A","AF-B","AF-C","SF-A","SF-B","SF-C","CCR-A","CCR-B","CCR-C","MXD-A","MXD-B","MXD-C","MIN-A","MIN-B","MIN-C")

Est0 <- Ests[Ests$order==0,4]
Est05 <- Ests[Ests$order==0.5,4]
Est1 <- Ests[Ests$order==1,4]
Est2 <- Ests[Ests$order==2,4]
Est_eqSS <- as.data.frame(cbind(Site=as.character(Site),Est0_ss=as.numeric(Est0),Est05_ss=as.numeric(Est05),Est1_ss=as.numeric(Est1),Est2_ss=as.numeric(Est2)))
# 
#write.csv(Est_eqSS,"C://Data/PhD/Processed_data/Orchidbees/ob_estqs_EqSS_inext.csv")

##/////////////////////////
# # code for 95SC comparison----
# m <- c(50, 100, 200,300,400,500,600) # series of sample sizes for extrapolation (if size=NULL, just goes to doubel sample size, 
# # or otherwise can specify a signle number. for q=0 only reliable up to double sample size, but for 
# # q=1 and 2, can be much higher without problems.)
# 
# #calculate up to 600 individuals for q=1-3, but only double sample size for q=0. qINF just estimates at 1, so not useful
# iN123 <- iNEXT(ob.mat, q=c(1,2,3), datatype="abundance", size=NULL, endpoint=600, knots=80, se=TRUE, conf=0.95, nboot=50)
# iN0 <- iNEXT(ob.mat, q=0, datatype="abundance", size=NULL, endpoint=100, knots=40, se=TRUE, conf=0.95, nboot=50)
# 
# asy.q123 <- as.data.frame(iN123$AsyEst)
# asy.q0 <- as.data.frame(iN0$AsyEst)
# 
# # for many sites, observed samples are already at 90-95% completeness,
# # or even higher. Therefore, 95% completeness was chosen at the 
# # point at which to compare diversity estimates across sites. 
# 
# #extract the dataframes for each site
# estq123_BAA <- as.data.frame(iN123$iNextEst$`1.BA-A`)
# estq123_BAB <- as.data.frame(iN123$iNextEst$`1.BA-B`)
# estq123_BAC <- as.data.frame(iN123$iNextEst$`1.BA-C`)
# estq123_AFA <- as.data.frame(iN123$iNextEst$`2.AF-A`)
# estq123_AFB <- as.data.frame(iN123$iNextEst$`2.AF-B`)
# estq123_AFC <- as.data.frame(iN123$iNextEst$`2.AF-C`)
# estq123_SFA <- as.data.frame(iN123$iNextEst$`3.SF-A`)
# estq123_SFB <- as.data.frame(iN123$iNextEst$`3.SF-B`)
# estq123_SFC <- as.data.frame(iN123$iNextEst$`3.SF-C`)
# estq123_CCRA <- as.data.frame(iN123$iNextEst$`4.CCR-A`)
# estq123_CCRB <- as.data.frame(iN123$iNextEst$`4.CCR-B`)
# estq123_CCRC <- as.data.frame(iN123$iNextEst$`4.CCR-C`)
# estq123_MXDA <- as.data.frame(iN123$iNextEst$`5.MXD-A`)
# estq123_MXDB <- as.data.frame(iN123$iNextEst$`5.MXD-B`)
# estq123_MXDC <- as.data.frame(iN123$iNextEst$`5.MXD-C`)
# estq123_MINA <- as.data.frame(iN123$iNextEst$`6.MIN-A`)
# estq123_MINB <- as.data.frame(iN123$iNextEst$`6.MIN-B`)
# estq123_MINC <- as.data.frame(iN123$iNextEst$`6.MIN-C`)
# 
# estq0_BAA <- as.data.frame(iN0$iNextEst$`1.BA-A`)
# estq0_BAB <- as.data.frame(iN0$iNextEst$`1.BA-B`)
# estq0_BAC <- as.data.frame(iN0$iNextEst$`1.BA-C`)
# estq0_AFA <- as.data.frame(iN0$iNextEst$`2.AF-A`)
# estq0_AFB <- as.data.frame(iN0$iNextEst$`2.AF-B`)
# estq0_AFC <- as.data.frame(iN0$iNextEst$`2.AF-C`)
# estq0_SFA <- as.data.frame(iN0$iNextEst$`3.SF-A`)
# estq0_SFB <- as.data.frame(iN0$iNextEst$`3.SF-B`)
# estq0_SFC <- as.data.frame(iN0$iNextEst$`3.SF-C`)
# estq0_CCRA <- as.data.frame(iN0$iNextEst$`4.CCR-A`)
# estq0_CCRB <- as.data.frame(iN0$iNextEst$`4.CCR-B`)
# estq0_CCRC <- as.data.frame(iN0$iNextEst$`4.CCR-C`)
# estq0_MXDA <- as.data.frame(iN0$iNextEst$`5.MXD-A`)
# estq0_MXDB <- as.data.frame(iN0$iNextEst$`5.MXD-B`)
# estq0_MXDC <- as.data.frame(iN0$iNextEst$`5.MXD-C`)
# estq0_MINA <- as.data.frame(iN0$iNextEst$`6.MIN-A`)
# estq0_MINB <- as.data.frame(iN0$iNextEst$`6.MIN-B`)
# estq0_MINC <- as.data.frame(iN0$iNextEst$`6.MIN-C`)
# 
# #find the row where completeness is closest to 0.95
# com <- 0.95 # sampling completeness level
# 
# #q=1
# q1BAA <- estq123_BAA[estq123_BAA$order==1,] # select only rows for q=1
# r <- which.min(abs(q1BAA$SC - com)) # which row has desired SC
# BAA <- q1BAA[r,] # select for q=1 which has desired SC
# q1BAB <- estq123_BAB[estq123_BAB$order==1,] # select only rows for q=1
# r <- which.min(abs(q1BAB$SC - com)) # which row has desired SC
# BAB <- q1BAB[r,] # select for q=1 which has desired SC
# q1BAC <- estq123_BAC[estq123_BAC$order==1,] # select only rows for q=1
# r <- which.min(abs(q1BAC$SC - com)) # which row has desired SC
# BAC <- q1BAC[r,] # select for q=1 which has desired SC
# q1AFA <- estq123_AFA[estq123_AFA$order==1,] # select only rows for q=1
# r <- which.min(abs(q1AFA$SC - com)) # which row has desired SC
# AFA <- q1AFA[r,] # select for q=1 which has desired SC
# q1AFB <- estq123_AFB[estq123_AFB$order==1,] # select only rows for q=1
# r <- which.min(abs(q1AFB$SC - com)) # which row has desired SC
# AFB <- q1AFB[r,] # select for q=1 which has desired SC
# q1AFC <- estq123_AFC[estq123_AFC$order==1,] # select only rows for q=1
# r <- which.min(abs(q1AFC$SC - com)) # which row has desired SC
# AFC <- q1AFC[r,] # select for q=1 which has desired SC
# q1SFA <- estq123_SFA[estq123_SFA$order==1,] # select only rows for q=1
# r <- which.min(abs(q1SFA$SC - com)) # which row has desired SC
# SFA <- q1SFA[r,] # select for q=1 which has desired SC
# q1SFB <- estq123_SFB[estq123_SFB$order==1,] # select only rows for q=1
# r <- which.min(abs(q1SFB$SC - com)) # which row has desired SC
# SFB <- q1SFB[r,] # select for q=1 which has desired SC
# q1SFC <- estq123_SFC[estq123_SFC$order==1,] # select only rows for q=1
# r <- which.min(abs(q1SFC$SC - com)) # which row has desired SC
# SFC <- q1SFC[r,] # select for q=1 which has desired SC
# q1CCRA <- estq123_CCRA[estq123_CCRA$order==1,] # select only rows for q=1
# r <- which.min(abs(q1CCRA$SC - com)) # which row has desired SC
# CCRA <- q1CCRA[r,] # select for q=1 which has desired SC
# q1CCRB <- estq123_CCRB[estq123_CCRB$order==1,] # select only rows for q=1
# r <- which.min(abs(q1CCRB$SC - com)) # which row has desired SC
# CCRB <- q1CCRB[r,] # select for q=1 which has desired SC
# q1CCRC <- estq123_CCRC[estq123_CCRC$order==1,] # select only rows for q=1
# r <- which.min(abs(q1CCRC$SC - com)) # which row has desired SC
# CCRC <- q1CCRC[r,] # select for q=1 which has desired SC
# q1MXDA <- estq123_MXDA[estq123_MXDA$order==1,] # select only rows for q=1
# r <- which.min(abs(q1MXDA$SC - com)) # which row has desired SC
# MXDA <- q1MXDA[r,] # select for q=1 which has desired SC
# q1MXDB <- estq123_MXDB[estq123_MXDB$order==1,] # select only rows for q=1
# r <- which.min(abs(q1MXDB$SC - com)) # which row has desired SC
# MXDB <- q1MXDB[r,] # select for q=1 which has desired SC
# q1MXDC <- estq123_MXDC[estq123_MXDC$order==1,] # select only rows for q=1
# r <- which.min(abs(q1MXDC$SC - com)) # which row has desired SC
# MXDC <- q1MXDC[r,] # select for q=1 which has desired SC
# q1MINA <- estq123_MINA[estq123_MINA$order==1,] # select only rows for q=1
# r <- which.min(abs(q1MINA$SC - com)) # which row has desired SC
# MINA <- q1MINA[r,] # select for q=1 which has desired SC
# q1MINB <- estq123_MINB[estq123_MINB$order==1,] # select only rows for q=1
# r <- which.min(abs(q1MINB$SC - com)) # which row has desired SC
# MINB <- q1MINB[r,] # select for q=1 which has desired SC
# q1MINC <- estq123_MINC[estq123_MINC$order==1,] # select only rows for q=1
# r <- which.min(abs(q1MINC$SC - com)) # which row has desired SC
# MINC <- q1MINC[r,] # select for q=1 which has desired SC
# 
# q1est <- rbind(BAA,BAB,BAC,AFA,AFB,AFC,SFA,SFB,SFC,CCRA,CCRB,CCRC,MXDA,MXDB,MXDC,MINA,MINB,MINC)
# Site <- c("BA-A","BA-B","BA-C","AF-A","AF-B","AF-C","SF-A","SF-B","SF-C","CCR-A","CCR-B","CCR-C","MXD-A","MXD-B","MXD-C","MIN-A","MIN-B","MIN-C")
# Eq1 <- cbind(q1est,Site)
# 
# #q=2
# q2BAA <- estq123_BAA[estq123_BAA$order==2,] # select only rows for q=2
# r <- which.min(abs(q2BAA$SC - com)) # which row has desired SC
# BAA <- q2BAA[r,] # select for q=2 which has desired SC
# q2BAB <- estq123_BAB[estq123_BAB$order==2,] # select only rows for q=2
# r <- which.min(abs(q2BAB$SC - com)) # which row has desired SC
# BAB <- q2BAB[r,] # select for q=2 which has desired SC
# q2BAC <- estq123_BAC[estq123_BAC$order==2,] # select only rows for q=2
# r <- which.min(abs(q2BAC$SC - com)) # which row has desired SC
# BAC <- q2BAC[r,] # select for q=2 which has desired SC
# q2AFA <- estq123_AFA[estq123_AFA$order==2,] # select only rows for q=2
# r <- which.min(abs(q2AFA$SC - com)) # which row has desired SC
# AFA <- q2AFA[r,] # select for q=2 which has desired SC
# q2AFB <- estq123_AFB[estq123_AFB$order==2,] # select only rows for q=2
# r <- which.min(abs(q2AFB$SC - com)) # which row has desired SC
# AFB <- q2AFB[r,] # select for q=2 which has desired SC
# q2AFC <- estq123_AFC[estq123_AFC$order==2,] # select only rows for q=2
# r <- which.min(abs(q2AFC$SC - com)) # which row has desired SC
# AFC <- q2AFC[r,] # select for q=2 which has desired SC
# q2SFA <- estq123_SFA[estq123_SFA$order==2,] # select only rows for q=2
# r <- which.min(abs(q2SFA$SC - com)) # which row has desired SC
# SFA <- q2SFA[r,] # select for q=2 which has desired SC
# q2SFB <- estq123_SFB[estq123_SFB$order==2,] # select only rows for q=2
# r <- which.min(abs(q2SFB$SC - com)) # which row has desired SC
# SFB <- q2SFB[r,] # select for q=2 which has desired SC
# q2SFC <- estq123_SFC[estq123_SFC$order==2,] # select only rows for q=2
# r <- which.min(abs(q2SFC$SC - com)) # which row has desired SC
# SFC <- q2SFC[r,] # select for q=2 which has desired SC
# q2CCRA <- estq123_CCRA[estq123_CCRA$order==2,] # select only rows for q=2
# r <- which.min(abs(q2CCRA$SC - com)) # which row has desired SC
# CCRA <- q2CCRA[r,] # select for q=2 which has desired SC
# q2CCRB <- estq123_CCRB[estq123_CCRB$order==2,] # select only rows for q=2
# r <- which.min(abs(q2CCRB$SC - com)) # which row has desired SC
# CCRB <- q2CCRB[r,] # select for q=2 which has desired SC
# q2CCRC <- estq123_CCRC[estq123_CCRC$order==2,] # select only rows for q=2
# r <- which.min(abs(q2CCRC$SC - com)) # which row has desired SC
# CCRC <- q2CCRC[r,] # select for q=2 which has desired SC
# q2MXDA <- estq123_MXDA[estq123_MXDA$order==2,] # select only rows for q=2
# r <- which.min(abs(q2MXDA$SC - com)) # which row has desired SC
# MXDA <- q2MXDA[r,] # select for q=2 which has desired SC
# q2MXDB <- estq123_MXDB[estq123_MXDB$order==2,] # select only rows for q=2
# r <- which.min(abs(q2MXDB$SC - com)) # which row has desired SC
# MXDB <- q2MXDB[r,] # select for q=2 which has desired SC
# q2MXDC <- estq123_MXDC[estq123_MXDC$order==2,] # select only rows for q=2
# r <- which.min(abs(q2MXDC$SC - com)) # which row has desired SC
# MXDC <- q2MXDC[r,] # select for q=2 which has desired SC
# q2MINA <- estq123_MINA[estq123_MINA$order==2,] # select only rows for q=2
# r <- which.min(abs(q2MINA$SC - com)) # which row has desired SC
# MINA <- q2MINA[r,] # select for q=2 which has desired SC
# q2MINB <- estq123_MINB[estq123_MINB$order==2,] # select only rows for q=2
# r <- which.min(abs(q2MINB$SC - com)) # which row has desired SC
# MINB <- q2MINB[r,] # select for q=2 which has desired SC
# q2MINC <- estq123_MINC[estq123_MINC$order==2,] # select only rows for q=2
# r <- which.min(abs(q2MINC$SC - com)) # which row has desired SC
# MINC <- q2MINC[r,] # select for q=2 which has desired SC
# 
# q2est <- rbind(BAA,BAB,BAC,AFA,AFB,AFC,SFA,SFB,SFC,CCRA,CCRB,CCRC,MXDA,MXDB,MXDC,MINA,MINB,MINC)
# Eq2 <- cbind(q2est,Site)
# 
# #q=3
# q3BAA <- estq123_BAA[estq123_BAA$order==3,] # select only rows for q=3
# r <- which.min(abs(q3BAA$SC - com)) # which row has desired SC
# BAA <- q3BAA[r,] # select for q=3 which has desired SC
# q3BAB <- estq123_BAB[estq123_BAB$order==3,] # select only rows for q=3
# r <- which.min(abs(q3BAB$SC - com)) # which row has desired SC
# BAB <- q3BAB[r,] # select for q=3 which has desired SC
# q3BAC <- estq123_BAC[estq123_BAC$order==3,] # select only rows for q=3
# r <- which.min(abs(q3BAC$SC - com)) # which row has desired SC
# BAC <- q3BAC[r,] # select for q=3 which has desired SC
# q3AFA <- estq123_AFA[estq123_AFA$order==3,] # select only rows for q=3
# r <- which.min(abs(q3AFA$SC - com)) # which row has desired SC
# AFA <- q3AFA[r,] # select for q=3 which has desired SC
# q3AFB <- estq123_AFB[estq123_AFB$order==3,] # select only rows for q=3
# r <- which.min(abs(q3AFB$SC - com)) # which row has desired SC
# AFB <- q3AFB[r,] # select for q=3 which has desired SC
# q3AFC <- estq123_AFC[estq123_AFC$order==3,] # select only rows for q=3
# r <- which.min(abs(q3AFC$SC - com)) # which row has desired SC
# AFC <- q3AFC[r,] # select for q=3 which has desired SC
# q3SFA <- estq123_SFA[estq123_SFA$order==3,] # select only rows for q=3
# r <- which.min(abs(q3SFA$SC - com)) # which row has desired SC
# SFA <- q3SFA[r,] # select for q=3 which has desired SC
# q3SFB <- estq123_SFB[estq123_SFB$order==3,] # select only rows for q=3
# r <- which.min(abs(q3SFB$SC - com)) # which row has desired SC
# SFB <- q3SFB[r,] # select for q=3 which has desired SC
# q3SFC <- estq123_SFC[estq123_SFC$order==3,] # select only rows for q=3
# r <- which.min(abs(q3SFC$SC - com)) # which row has desired SC
# SFC <- q3SFC[r,] # select for q=3 which has desired SC
# q3CCRA <- estq123_CCRA[estq123_CCRA$order==3,] # select only rows for q=3
# r <- which.min(abs(q3CCRA$SC - com)) # which row has desired SC
# CCRA <- q3CCRA[r,] # select for q=3 which has desired SC
# q3CCRB <- estq123_CCRB[estq123_CCRB$order==3,] # select only rows for q=3
# r <- which.min(abs(q3CCRB$SC - com)) # which row has desired SC
# CCRB <- q3CCRB[r,] # select for q=3 which has desired SC
# q3CCRC <- estq123_CCRC[estq123_CCRC$order==3,] # select only rows for q=3
# r <- which.min(abs(q3CCRC$SC - com)) # which row has desired SC
# CCRC <- q3CCRC[r,] # select for q=3 which has desired SC
# q3MXDA <- estq123_MXDA[estq123_MXDA$order==3,] # select only rows for q=3
# r <- which.min(abs(q3MXDA$SC - com)) # which row has desired SC
# MXDA <- q3MXDA[r,] # select for q=3 which has desired SC
# q3MXDB <- estq123_MXDB[estq123_MXDB$order==3,] # select only rows for q=3
# r <- which.min(abs(q3MXDB$SC - com)) # which row has desired SC
# MXDB <- q3MXDB[r,] # select for q=3 which has desired SC
# q3MXDC <- estq123_MXDC[estq123_MXDC$order==3,] # select only rows for q=3
# r <- which.min(abs(q3MXDC$SC - com)) # which row has desired SC
# MXDC <- q3MXDC[r,] # select for q=3 which has desired SC
# q3MINA <- estq123_MINA[estq123_MINA$order==3,] # select only rows for q=3
# r <- which.min(abs(q3MINA$SC - com)) # which row has desired SC
# MINA <- q3MINA[r,] # select for q=3 which has desired SC
# q3MINB <- estq123_MINB[estq123_MINB$order==3,] # select only rows for q=3
# r <- which.min(abs(q3MINB$SC - com)) # which row has desired SC
# MINB <- q3MINB[r,] # select for q=3 which has desired SC
# q3MINC <- estq123_MINC[estq123_MINC$order==3,] # select only rows for q=3
# r <- which.min(abs(q3MINC$SC - com)) # which row has desired SC
# MINC <- q3MINC[r,] # select for q=3 which has desired SC
# 
# q3est <- rbind(BAA,BAB,BAC,AFA,AFB,AFC,SFA,SFB,SFC,CCRA,CCRB,CCRC,MXDA,MXDB,MXDC,MINA,MINB,MINC)
# Eq3 <- cbind(q3est,Site)
# 
# 
# #q=0
# r <- which.min(abs(estq0_BAA$SC - com)) # which row has desired SC
# BAA <- estq0_BAA[r,] # select for q=0 which has desired SC
# r <- which.min(abs(estq0BAB$SC - com)) # which row has desired SC
# BAB <- estq0_BAB[r,] # select for q=0 which has desired SC
# r <- which.min(abs(estq0_BAC$SC - com)) # which row has desired SC
# BAC <- estq0_BAC[r,] # select for q=0 which has desired SC
# r <- which.min(abs(estq0_AFA$SC - com)) # which row has desired SC
# AFA <- estq0_AFA[r,] # select for q=0 which has desired SC
# r <- which.min(abs(estq0_AFB$SC - com)) # which row has desired SC
# AFB <- estq0_AFB[r,] # select for q=0 which has desired SC
# r <- which.min(abs(estq0_AFC$SC - com)) # which row has desired SC
# AFC <- estq0_AFC[r,] # select for q=0 which has desired SC
# r <- which.min(abs(estq0_SFA$SC - com)) # which row has desired SC
# SFA <- estq0_SFA[r,] # select for q=0 which has desired SC
# r <- which.min(abs(estq0_SFB$SC - com)) # which row has desired SC
# SFB <- estq0_SFB[r,] # select for q=0 which has desired SC
# r <- which.min(abs(estq0_SFC$SC - com)) # which row has desired SC
# SFC <- estq0_SFC[r,] # select for q=0 which has desired SC
# r <- which.min(abs(estq0_CCRA$SC - com)) # which row has desired SC
# CCRA <- estq0_CCRA[r,] # select for q=0 which has desired SC
# r <- which.min(abs(estq0_CCRB$SC - com)) # which row has desired SC
# CCRB <- estq0_CCRB[r,] # select for q=0 which has desired SC
# r <- which.min(abs(estq0_CCRC$SC - com)) # which row has desired SC
# CCRC <- estq0_CCRC[r,] # select for q=0 which has desired SC
# r <- which.min(abs(estq0_MXDA$SC - com)) # which row has desired SC
# MXDA <- estq0_MXDA[r,] # select for q=0 which has desired SC
# r <- which.min(abs(estq0_MXDB$SC - com)) # which row has desired SC
# MXDB <- estq0_MXDB[r,] # select for q=0 which has desired SC
# r <- which.min(abs(estq0_MXDC$SC - com)) # which row has desired SC
# MXDC <- estq0_MXDC[r,] # select for q=0 which has desired SC
# r <- which.min(abs(estq0_MINA$SC - com)) # which row has desired SC
# MINA <- estq0_MINA[r,] # select for q=0 which has desired SC
# r <- which.min(abs(estq0_MINB$SC - com)) # which row has desired SC
# MINB <- estq0_MINB[r,] # select for q=0 which has desired SC
# r <- which.min(abs(estq0_MINC$SC - com)) # which row has desired SC
# MINC <- estq0_MINC[r,] # select for q=0 which has desired SC
# 
# q0est <- rbind(BAA,BAB,BAC,AFA,AFB,AFC,SFA,SFB,SFC,CCRA,CCRB,CCRC,MXDA,MXDB,MXDC,MINA,MINB,MINC)
# Eq0 <- cbind(q0est,Site)
# 
# # 95% CI
# colnames(Eq0)[4] <- "e95q0"
# colnames(Eq1)[4] <- "e95q1"
# colnames(Eq2)[4] <- "e95q2"
# colnames(Eq3)[4] <- "e95q3"
# 
# r <- which(asy.q123$Diversity=="Shannon diversity") #select Shannon asymtotic estimates
# as.q1 <- asy.q123[r,c(3:7)] #extract shannon rows
# colnames(as.q1)[1] <- "obs.q1"
# colnames(as.q1)[2] <- "asEst.q1"
# 
# r <- which(asy.q123$Diversity=="Simpson diversity") #select Shannon asymtotic estimates
# as.q2 <- asy.q123[r,c(3:7)] #extract shannon rows
# colnames(as.q2)[1] <- "obs.q2"
# colnames(as.q2)[2] <- "asEst.q2"
# 
# Est.qs <- cbind(Site=Eq0$Site,Eq0[,c(1,2,4,5,6,7)],Eq1[,c(1,2,4,5,6,7)],Eq2[,c(1,2,4,5,6,7)],Eq3[,c(1,2,4,5,6,7)],as.q1,as.q2)
# 
# # write.csv(Est.qs,"C://Data/PhD/Processed_data/Orchidbees/ob_estqs_inext.csv")
# 

# iNEXT plots ----
iN0 <- iNEXT(ob.mat, q=0, datatype="abundance", size=NULL, endpoint=250, knots=25, se=TRUE, conf=0.95, nboot=1000)
tiff("C://Data/PhD/Outputs/Orchidbees/Figures/Est_div_q0.tiff",width=300,height=190,units="mm",res=500, pointsize=14)  
gc <- ggiNEXT(iN0, type=1, facet.var="site",grey=TRUE)
gc +  theme_bw(base_size = 18) +
       theme(legend.position="none",
        axis.text=element_text(size=12,colour = "black"),axis.title=element_text(size=12),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))
dev.off()


# Sample completeness curves ----
iN0 <- iNEXT(ob.mat, q=0, datatype="abundance", size=NULL, endpoint=100, knots=50, se=TRUE, conf=0.95, nboot=1000)

gc <- ggiNEXT(iN0, type=2, facet.var="none")
#tiff("C://Data/PhD/Outputs/Orchidbees/Figures/samplecompletness_q0.tiff",width=190,height=130,units="mm",res=500, pointsize=9)  
#png(file="C://Data/PhD/Outputs/Orchidbees/Figure/OB_samplecompletness_q0_nobg.png",width=15,height=10,units="in",res=400)  
gc + scale_color_manual(values=c('darkred','darkred','darkred','firebrick2','firebrick2','firebrick2','darksalmon','darksalmon','darksalmon','darkslategray3','darkslategray3','darkslategray3','dodgerblue2','dodgerblue2','dodgerblue2','navyblue','navyblue','navyblue')) +
  scale_fill_manual(values=c('darkred','darkred','darkred','firebrick2','firebrick2','firebrick2','darksalmon','darksalmon','darksalmon','darkslategray3','darkslategray3','darkslategray3','dodgerblue2','dodgerblue2','dodgerblue2','navyblue','navyblue','navyblue'))+
  scale_shape_manual(values=c(rep(20,18)))+
  theme_bw(base_size = 18) +
  guides(shape=FALSE,lty = guide_legend(order = 2))+
  theme(legend.position=c(0.6,0.3),legend.direction = "horizontal",
        legend.title=element_blank(),legend.text=element_text(size=9),
        axis.text=element_text(size=9,colour = "black"),axis.title=element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))
dev.off()




# multipanel sample coverage plot
#png("C://Data/PhD/Outputs/Orchidbees/Figures/OB_samplecompletness_q0_panels.png",width=25,height=11,units="in",res=500)  
tiff("C://Data/PhD/Outputs/Orchidbees/Figures/Ecolind/FigS3_2column_samplecompletness_q0.tiff",width=190,height=130,units="mm",res=500, pointsize=9)  
ggiNEXT(iN0, type=2,facet.var="site",grey=TRUE)+
  theme_bw(base_size = 10) + 
  theme(legend.position="none")
dev.off()


# multipanel species richness plot
#png("C://Data/PhD/Outputs/Orchidbees/Figures/OB_samplecompletness_q0_panels.png",width=25,height=11,units="in",res=500)  
tiff("C://Data/PhD/Outputs/Orchidbees/Figures/Ecolind/FigS4_2column_speciesacummulation_q0.tiff",width=190,height=130,units="mm",res=500, pointsize=9)  
ggiNEXT(iN0, type=1,facet.var="site",grey=TRUE)+
  theme_bw(base_size = 10) + 
  theme(legend.position="none")
dev.off()

iN0
# resample bootstrapped diversity values -----

# figure out good sample size to use
iN0 <- iNEXT(ob.mat, q=0, datatype="abundance", size=NULL, endpoint=NULL, knots=40, se=TRUE, conf=0.95, nboot=50)

# png(file="C://Data/PhD/Outputs/Orchidbees/Diversity/iNEXT/OB_samplecompletness100_site.png",width=60,height=5,units="in",res=180)  
# ggiNEXT(iN0, type=2, facet.var="site")
# dev.off()

# samples size of 50 reaches ~90%+ coverage for all samples (or very close) without massively exceeding the double sample size rule for the smallest samples.

# iNEXT resampling estimates FROM HERE ----
# extracted only the bits of code I need to run, once the functions have allbeen loaded above.
# load functions from iNEXT_bgcode file for dungbeetles!#
#source("C://Data/PhD/Analysis/Diversity/iNEXT_bg_code.r")

ssize <- 50 # sample size at which to compare
q <- 2# q value
nboot <- 1000 # number of bootstrappings
conf = 0.95 # confidence interval

plotdivs <- NULL # reset memory of plotsdivs
plotdivs <- c(1:nboot) # set up a column for labelling each row of bootstraps

for(p in 1:18){
  obsub <- ob.mat[,p] # select just the minC communtiy for testing code
  Spec <- obsub
  
  
  m <- iNEXT.Ind.1(Spec=obsub,q,endpoint=ssize,knots=40) # determines sample sizes (m) at which to estimate diversity
  
  #Dq.hat <- Dqhat.Ind(Spec, q, m) # diversity at q=0 (defined earlier) for each size of m
  #C.hat <- Chat.Ind(Spec, m) 
  
  Prob.hat <- EstiBootComm.Ind(Spec) # this line calculated the probabilities of finding each species
  Abun.Mat <- rmultinom(nboot, size=length(Spec), Prob.hat) # produces 200 possible assemblages
  
  
  Divboot <- NULL
  
  for(s in 1:nboot){
    sub <- Abun.Mat[,s] # select one of the possible communities
    subD <- Dqhat.Ind(sub,q,m=max(m)) # calculate diversity of this one random community  at highest sample size you are interested in
    Divboot <- rbind(Divboot,subD)
  }
  
  plotdivs <- as.data.frame(cbind(plotdivs,Divboot))
}

plotdivs

#rename plotdivs columns with site names
#edit(plotdivs)
names(plotdivs) <- c("nboot", "BA-A","BA-B","BA-C","AF-A","AF-B","AF-C","SF-A","SF-B","SF-C","CCR-A","CCR-B","CCR-C","MXD-A","MXD-B","MXD-C","MIN-A","MIN-B","MIN-C")

#
################
# random sample estimates ----
# compare best and worst site, and see what proportion of estimates are higher in best site

mean(plotdivs[,2:4]<plotdivs[,17:19])


# gather together the three plots per habitat
head(plotdivs)

BAests <- c(plotdivs[,2],plotdivs[,3],plotdivs[,4])
AFests <- c(plotdivs[,5],plotdivs[,6],plotdivs[,7])
SFests <- c(plotdivs[,8],plotdivs[,9],plotdivs[,10])
CCests <- c(plotdivs[,11],plotdivs[,12],plotdivs[,13])
MXests <- c(plotdivs[,14],plotdivs[,15],plotdivs[,16])
MDests <- c(plotdivs[,17],plotdivs[,18],plotdivs[,19])

mean(plotdivs[,2:4]<plotdivs[,17:19])

meandiffs <- c(mean(BAests<MDests),mean(AFests<MDests),mean(SFests<MDests),mean(CCests<MDests),
               mean(MXests<MDests),mean(BAests<MXests),mean(AFests<MXests),mean(SFests<MXests),
               mean(CCests<MXests),mean(BAests<CCests),mean(AFests<CCests),mean(SFests<CCests),
               mean(BAests<SFests),mean(AFests<SFests),mean(BAests<AFests))
rankdiff <- c(6-1,6-2,6-3,6-4,6-5,5-1,5-2,5-3,5-4,4-1,4-2,4-3,3-1,3-2,2-1)

inr2 <- as.data.frame(cbind(meandiffs,rankdiff))

#write.csv(inr2,"C://Data/PhD/Outputs/Orchidbees/OBq2ests_rsampled-1708.csv")

mdr <- inr0
res <- cor.test(mdr$meandiffs, mdr$rankdiff, method = "spearman",exact=F) 
cbind(res$statistic,res$p.value,res$estimate)

# bootstrapped 95% CI
quantile(
  replicate(10000, {
    boot.i <- sample(length(mdr$rankdiff), replace = TRUE)
    cor(mdr$rankdiff[boot.i],mdr$meandiffs[boot.i], method = "spearman")
  }), 
  c(0.025, 0.975))

#iNEXT resample plots----m
inr0 <- read.csv("C://Data/PhD/Outputs/Orchidbees/OBq0ests_rsampled.csv")
inr1 <- read.csv("C://Data/PhD/Outputs/Orchidbees/OBq1ests_rsampled.csv")
inr2 <- read.csv("C://Data/PhD/Outputs/Orchidbees/OBq2ests_rsampled.csv")

png("C://Data/PhD/Outputs/Orchidbees/Figures/inextresampling2-jitter-abline.png",width=190,height=70,units="mm",res=500, pointsize=12)
#tiff(file="C://Data/PhD/Outputs/Orchidbees/Figures/inextresampling2-jitter-hr.tiff",width=190,height=70,units="mm",res=1000, pointsize=12)  
tiff(file="C://Data/PhD/Outputs/Orchidbees/Figures/Ecolind/Fig3-2columns-diversitydifference.tiff",width=190,height=80,units="mm",res=1000, pointsize=14)  
par(mfrow=c(1,3))
par(mar=c(5.1,5.1,4.1,2.1))
plot(inr0$meandiffs~inr0$rankdiff,pch=21,cex=1.5,cex.axis=1,cex.lab=1,col='black',bty="n",ylab= parse(text='p^q*D'),
     xlab="Difference in disturbance rank",ylim=c(0,1))
#abline(lm(inr0$meandiffs~inr0$rankdiff))
mtext(side = 3, line = 1,adj=0,"a",font=2,cex=1.1)
plot(inr1$meandiffs~inr1$rankdiff,pch=21,cex=1.5,cex.axis=1,cex.lab=1,col='black',bty="n",ylab="",
     xlab="Difference in disturbance rank",ylim=c(0,1))
#abline(lm(inr1$meandiffs~inr1$rankdiff))
mtext(side = 3, line = 1,adj=0,"b",font=2,cex=1.1)
plot(inr2$meandiffs~inr2$rankdiff,pch=21,cex=1.5,cex.axis=1,cex.lab=1,col='black',bty="n",ylab="",
     xlab="Difference in disturbance rank",ylim=c(0,1))
#abline(lm(inr2$meandiffs~inr2$rankdiff))
mtext(side = 3, line = 1,adj=0,"c",font=2,cex=1.1)
dev.off()



# pollination data ----
poll<- read.csv("C://Data/PhD/Rawdata/Orchidbees/pollination_data_raw.csv",check.names = F)

str(poll)
head(poll)
use <- c(1,3,5,6,7,9,10,11) #columns to include
poll <- poll[,use]
poll$Site <- as.factor(as.character(poll$Site))
levels(poll$Site)

levels(poll$Site)[levels(poll$Site)=="MinD-B"] <- "MIN-B"
levels(poll$Site)[levels(poll$Site)=="MinD-A"] <- "MIN-A"
levels(poll$Site)[levels(poll$Site)=="MinD-C"] <- "MIN-C"
levels(poll$Site)[levels(poll$Site)=="AF - C"] <- "AF-C"
levels(poll$Site)[levels(poll$Site)=="CCR - C"] <- "CCR-C"
levels(poll$Site)[levels(poll$Site)=="minD-B"] <- "MIN-B"
levels(poll$Site)[levels(poll$Site)=="minD-A"] <- "MIN-A"
levels(poll$Site)[levels(poll$Site)=="MinD - C"] <- "MIN-C"
levels(poll$Site)[levels(poll$Site)=="SF - B"] <- "SF-B"
levels(poll$Site)[levels(poll$Site)=="BA-A "] <- "BA-A"
levels(poll$Site)[levels(poll$Site)=="BA - C"] <- "BA-C"

colnames(poll)[3] <- "Session"
colnames(poll)[4] <- "Start"
colnames(poll)[5] <- "End"
colnames(poll)[7] <- "Visits"
colnames(poll)[8] <- "OBvisits"

Visits <- tapply(poll$Visits,poll$Site, sum) 
OBvisits <- tapply(poll$OBvisits,poll$Site, sum)  
pollinatorvisits <- as.data.frame(cbind(Site=names(Visits),Visits,OBvisits))
pollinatorvisits$Visits <- as.numeric(as.character(pollinatorvisits$Visits))
pollinatorvisits$OBvisits <- as.numeric(as.character(pollinatorvisits$OBvisits))
pollinatorvisits$Site <- as.factor(as.character(pollinatorvisits$Site))
pollvis <- pollinatorvisits

# calculate abundance ----

ob <- read.csv("C://Data/PhD/Processed_data/Orchidbees/OBcountmat.csv",row.names=1,check.names = F)

head(ob)
abund <- colSums(ob)
names(abund)


# Preparing combined dataframe ----

# 
 veg <- read.csv("C://Data/PhD/Processed_data/Vegetation/veg_pca_out.csv")
 alpha <- read.csv("C://Data/PhD/Processed_data/Orchidbees/Diversity/obalpha.csv")
 site_data <- read.csv("C://Data/PhD/Processed_data/Site_data/sites_elev_dist_coords.csv")
 Estqs <- read.csv("C://Data/PhD/Processed_data/Orchidbees/ob_estqs_EqSS_inext.csv")
 abundance <- as.data.frame(cbind(Site=names(abund),abund))
 rho <- read.csv("C://Data/PhD/Processed_data/Orchidbees/Diversity/obrho.csv")
 rhobar <- read.csv("C://Data/PhD/Processed_data/Orchidbees/Diversity/obrho_b.csv")
 

colnames(veg)[1] <- "Site"
colnames(alpha)[1] <- "Site"
colnames(rho)[1] <- "Site"
colnames(rhobar)[1] <- "Site"
colnames(rho)[2:6] <- c("rq0","rq05","rq1","rq2","rqInf")
colnames(rhobar)[2:6] <- c("rbq0","rbq05","rbq1","rbq2","rbqInf")
library(stringr)
newalphasite <- str_split_fixed(alpha$Site, ".",3) # split Site column, to get site values without Rank. infront
alpha$Site <- newalphasite[,3]
alpha$Site <- as.factor(alpha$Site)
levels(alpha$Site)

newabundsite <- str_split_fixed(abundance$Site, ".",3) # split Site column, to get site values without Rank. infront
abundance$Site <- newabundsite[,3]
abundance$Site <- as.factor(abundance$Site)
levels(abundance$Site)

newsite <- str_split_fixed(rho$Site, ".",3) # split Site column, to get site values without Rank. infront
rho$Site <- newsite[,3]
rho$Site <- as.factor(rho$Site)
rhobar$Site <- newsite[,3]
rhobar$Site <- as.factor(rhobar$Site)
levels(rho$Site)

merge1 <- merge(alpha,site_data,by="Site")
merge2 <- merge(merge1,veg,by="Site")
merge3 <- merge(merge2,abundance,by="Site")
merge4 <- merge(merge3,Estqs,by="Site")
merge5 <- merge(merge4,rho,by="Site")
merge6 <- merge(merge5,rhobar,by="Site")
merge7 <- merge(merge6,pollvis,by="Site")
str(merge7)
# write.csv(merge7,"C://Data/PhD/Processed_data/Orchidbees/OB_div_allvars.csv") # first file used, which used Estimated div at 95%SC
# write.csv(merge7,"C://Data/PhD/Processed_data/Orchidbees/OB_div_allvars_EqSS.csv") # replacement file, using EstDiv at equal sample size (n=50)
# write.csv(merge7,"C://Data/PhD/Processed_data/Orchidbees/OB_div_allvars_EqSS.csv") # replacement file, using EstDiv at equal sample size (n=50)









