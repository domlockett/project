library(ggplot2)
library(catSurv)
library(scales)
library(dplyr)
library(plyr)
library(latex2exp)
#load("~/Summer 2018/Montgomery/catSurv_dl/troubleshooting.RData")

## Plotting data

# Set a threshold 
grm_cat@lengthThreshold<-4

# Define answer profiles and thetas

#first 100 rows -4, 2nd 100 -3, etc.
#respprof<-apply(thetaValue,1,  simulateRespondents_test, cat=grm_cat, n=100)
#respprof<-ldply(respprof, rbind)
data(nfc)
grmCat(nfc)
respprof<-nfc
thetaValue<-estimateThetas(grm[[1]],respprof)
  
# Various cat methods

grm_KL<-grm_cat
setSelection(grm_KL) <- "KL"

grm_EPV<-grm_cat
setSelection(grm_EPV) <- "EPV"

grm_MFI<-grm_cat
setSelection(grm_MFI) <- "MFI"

grm_MEI<-grm_cat
setSelection(grm_MEI) <- "MEI"

grm_LKL<-grm_cat
setSelection(grm_LKL) <- "LKL"

grm_PKL<-grm_cat
setSelection(grm_PKL) <- "PKL"

grm_MLWI<-grm_cat
setSelection(grm_MLWI) <- "MLWI"

grm_MFII<-grm_cat
setSelection(grm_MFII) <- "MFII"


grm_RAND<-grm_cat
setSelection(grm_RAND) <- "RANDOM"

grm<-list(grm_KL,grm_EPV,grm_MFI,grm_MEI,grm_LKL,grm_PKL,grm_MLWI,grm_MFII,grm_RAND)

2

#We get the true theta for reference
grm_TRUE<-estimateThetas(grm_cat, responses=respProf)

#define fixed battery by the highest level of discrimination
respProf<-respprof
#respprof<-respProf
names(grm[[1]]@discrimination)<-1:length(grm[[1]]@discrimination)
dif<-as.integer(names(grm[[1]]@discrimination[order(grm[[1]]@discrimination)[1:4]]))
dif
respprof[,c(1,2,4,5,6,7,8,9,11,12,13,16,17,18)]<-NA
fixed<-estimateThetas(grm[[1]], respprof)

#find the average estimates of the oracle function for these respondents

orac<-allOrac(grm[[1]], thetaValue, respProf[1:1000,],4) # be sure to have the 'n' of oracle same as lengthThreshold of catObjs
orac<-rbind(orac,allOrac(grm[[1]], thetaValue, respProf[1001:2000,],4)) 
orac<-rbind(orac,allOrac(grm[[1]], thetaValue, respProf[2001:3000,],4)) 
orac<-rbind(orac,allOrac(grm[[1]], thetaValue, respProf[3001:4000,],4)) 
orac<-rbind(orac,allOrac(grm[[1]], thetaValue, respProf[4001:4043,],4)) 


orac[1:2,]

allEst(grm, respProf[1,])
#Now let's get our estimates for these thousand observations
##########################################
##ISSUES: loads forever
##########################################
allEst(grm, respProf[1:100,]) #takes 30 seconds to load
allEst(grm, respProf[1:200,])#start:5:39:09 killed function:6:17
allEst(list(grm[[1]]), respProf[1:200,])#killed after 2minutes
## an error is returned
#########################################
estimates<-allEst(grm, respProf[1:100,])
estimates<-rbind(estimates,allEst(grm, respProf[201:300,]))
estimates<-rbind(estimates,allEst(grm, respProf[301:400,]))
estimates<-rbind(estimates,allEst(grm, respProf[401:500,]))
estimates<-rbind(estimates,allEst(grm, respProf[501:600,]))
estimates<-rbind(estimates,allEst(grm, respProf[601:700,]))
estimates<-rbind(estimates,allEst(grm, respProf[701:800,]))
estimates<-rbind(estimates,allEst(grm, respProf[801:900,]))
#########################################
"KL method of selctItem() not applicable when no questions asked"
#every method used thus far estimates the "KL"== grm[[1]]@selection
#after 4 rows it starts shooting KL errors
allEst(grm, respProf[801:804,])
allEst(list(grm_EPV),respProf[1:200,])#>45sec; killed
allEst(list(grm_KL), respProf[1:200,])#>45sec;killed
allEst(list(grm_EPV),respProf[1:100,])# Completed in 7 seconds
allEst(list(grm_KL),respProf[1:100,])# Completed in 5 seconds
#the nature of these issues is similar to the issues which
#arose with the WLE problems. Too much information and it won't run. 
#abitrary and/or random issues arise at certain values
#########################################

estimates<-rbind(estimates,allEst(grm, respProf[901:1000,]))
estimates<-rbind(estimates,allEst(grm, respProf[1001:1100,]))
estimates<-rbind(estimates,allEst(grm, respProf[1101:1200,]))
estimates<-rbind(estimates,allEst(grm, respProf[1201:1300,]))
estimates<-rbind(estimates,allEst(grm, respProf[1301:1400,]))
estimates<-rbind(estimates,allEst(grm, respProf[1401:1500,]))
estimates<-rbind(estimates,allEst(grm, respProf[1501:1600,]))
estimates<-rbind(estimates,allEst(grm, respProf[1601:1700,]))
estimates<-rbind(estimates,allEst(grm, respProf[1701:1800,]))
estimates<-rbind(estimates,allEst(grm, respProf[1801:1900,]))
estimates<-rbind(estimates,allEst(grm, respProf[1901:2000,]))
estimates<-rbind(estimates,allEst(grm, respProf[2001:2100,]))
estimates<-rbind(estimates,allEst(grm, respProf[2101:2200,]))
estimates<-rbind(estimates,allEst(grm, respProf[2201:2300,]))
estimates<-rbind(estimates,allEst(grm, respProf[2301:2400,]))
estimates<-rbind(estimates,allEst(grm, respProf[2401:2500,]))
estimates<-rbind(estimates,allEst(grm, respProf[2501:2600,]))
estimates<-rbind(estimates,allEst(grm, respProf[2601:2700,]))
estimates<-rbind(estimates,allEst(grm, respProf[2701:2800,]))
estimates<-rbind(estimates,allEst(grm, respProf[2801:2900,]))
estimates<-rbind(estimates,allEst(grm, respProf[2901:3000,]))
estimates<-rbind(estimates,allEst(grm, respProf[3001:3100,]))
estimates<-rbind(estimates,allEst(grm, respProf[3101:3200,]))
estimates<-rbind(estimates,allEst(grm, respProf[3201:3300,]))
estimates<-rbind(estimates,allEst(grm, respProf[3301:3400,]))
estimates<-rbind(estimates,allEst(grm, respProf[3401:3500,]))
estimates<-rbind(estimates,allEst(grm, respProf[3501:3600,]))
estimates<-rbind(estimates,allEst(grm, respProf[3601:3700,]))
estimates<-rbind(estimates,allEst(grm, respProf[3701:3800,]))
estimates<-rbind(estimates,allEst(grm, respProf[3801:3900,]))
estimates<-rbind(estimates,allEst(grm, respProf[3901:4000,]))
estimates<-rbind(estimates,allEst(grm, respProf[4001:4043,]))



estimates<-estimates[,1:9]
estimates[1,]
estimates<-cbind(estimates, orac, fixed)
ncol(estimates)

colnames(estimates)<-c("KL", "EPV", "MFI", "MEI", "LKL", 'PKL', 'MLWI', 'MFII', 'RANDOM', "Oracle", "Fixed")


#Calculate the bias for each cat Object
bias<-apply(estimates, 2, function(x){abs(grm_TRUE-x)})
bias[1,]

grm_TRUE[1]

#colnames(bias)<-c("KL", "EPV", "MFI", "MEI", "LKL", 'PKL', 'MLWI', 'MFII', 'RANDOM')

#attach the thetas to the corresponding respondent to find average by theta
bias<-cbind(bias, rep(-4:5, each=100))
colnames(bias)<-c("KL", "EPV", "MFI", "MEI", "LKL", 'PKL', 'MLWI', 'MFII', 'RANDOM', "Oracle", "Fixed",'Theta')
bias[1,]
#Calculate mean by theta

avgBias<-apply(bias[,1:11], 2,function(x){tapply(x, bias[,12], mean, na.rm=T)} )
avgBias<-cbind(avgBias, c(-4:5))
colnames(avgBias)<-c("KL", "EPV", "MFI", "MEI", "LKL", 'PKL', 'MLWI', 'MFII', 'RANDOM', "Oracle", "Fixed",'Theta')
avgBias<-as.data.frame(avgBias)

#Plot it
linecol<- c( "#339966", "#0066CC",
             "#ffcccc", "#660066", "#CC3300", 
             "#003333", "#660000", "#666666", "#99cccc", "#cc9900")
ltys<-c(2:11)

par(mar=c(3,3,2,9),mgp=c(1.5,0,0))
plot(avgBias$Theta, avgBias$Oracle, yaxt="n",main=TeX("Level of Bias by Average Value of $\\theta$"),tck=F, "l", xlab= expression(theta), ylab="Bias", ylim=c(0.2,1), col="#003366", lty=1, lwd=3)
axis(2, at=seq(0,1, by=.2),labels=seq(0,1, by=.2), col.axis="black", las=2, tick=F)

sapply(1:(ncol(avgBias[,c(1:9, 11)])),function(i){lines(avgBias$Theta,avgBias[,c(1:9, 11)][,i],col=alpha(linecol[i], .4),lty=ltys[i], lwd=2)})

legend("left",  inset=c(1,1.2), xpd=TRUE, bty="n", col = c("black", linecol),legend=c( "Oracle","KL", "EPV", "MFI", "MEI", "LKL", 'PKL', 'MLWI', 'MFII', 'RANDOM', "Fixed"), lty=c(1,ltys))
rug(grm_TRUE)


#now we want to know the average information instead of Bias
# allEst should also return the fisherInfo data
fish<- allFish(grm, respProf)
fish<- cbind(fish, rep(-4:5, each=100))
fish[1,]


fishAvg<-apply(fish[,1:11], 2,function(x){tapply(x, fish[,12], mean, na.rm=T)} )


colnames(fishAvg)<-c()#"MAP", "EAP", "KL", "Random", "Theta")


#Plot it
plot(fishAvg$Theta, fishAvg[,10], yaxt="n",main=TeX("Fisher Information by Average Value of $\\theta$"),tck=F, "l", xlab= expression(theta), ylab="Fish", ylim=c(0.2,1), col="#003366", lty=1, lwd=3)
sapply(1:(ncol(fishAvg[,c(1:9, 11)])),function(i){lines(fishAvg$Theta,fishAvg[,c(1:9, 11)][,i],col=alpha(linecol[i], .4),lty=ltys[i], lwd=2)})



legend("topright", bty="n", col = linecol[i], lty= c(2,3,4),legend=c("MAP", "EAP", "KL", "Random"))

## Unit test for allEst

grm<-list( grm_cat, ltm_cat)
grm[[1]]@selection
allEst(grm, respProf[1,])
length(grm)
