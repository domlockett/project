##New project

# Get data from YouGov and Qualtrics
  # Fit these responses to grm objects
# Using *example.R* compare how the adaptive batt compares to fixed
# Walk simulated and real respondents through methods
## Start with big 5 on YouGov
    #randomly sample 0f 50,000 yougov and fb data
    #100 questions about 20 questions per trati, 5 traits.
      #that means five different cat objs


#Parse out which questions belong to which trait





###Jacob no fixed so just plot the adaptive
# compare to simulate 


library(ggplot2)
library(catSurv)
library(scales)
library(dplyr)
library(plyr)
library(latex2exp)
library(purrr)
library(haven)
library(dplyr)
library(foreign)
library(catSurv)
library(readxl)
library(fuzzyjoin)
data<-read_sav("C:/Users/dl0ck/OneDrive/Documents/Summer 2018/Montgomery/catSurv_dl/big 5/raw_data/yougov.R" )
type<-read_xlsx("C:/Users/dl0ck/OneDrive/Documents/Summer 2018/Montgomery/catSurv_dl/big 5/fb_yg/big5_type.xlsx" )
type<-as.data.frame(type)
yougov<- data %>%
  select(BIGFIVE_1:BIGFIVE_100)
type<- type %>%
  mutate(question=as.factor(type[,1]))


ques<-yougov  %>% map_chr(~attributes(.)$label)

ques<-as.data.frame(ques)

ques<-apply(ques, 1,function(i){gsub("Describe self:", "",i)})

ques<-as.data.frame(ques)

ques<- cbind(ques, rownames(ques))

colnames(ques)<- c("question", "number")

ques$question<-paste0(ques[,1], ".")
type$question<-paste0(" ", type[,1])
ques[3,1]<-" Don't mind being the center of attention."
type[45,1]<- " Avoid contact with others."

dividers<-inner_join(type, ques )
agree_ques<-c(96,72,2,62,52,42,92,32,22,9,82,13,76,56,46,36,26,66,6,86) 

consci_ques<-c(65,8,68,48,58,38,20,78,98,88,28,35,45,25,5,55,95,75,85,15)
extra_ques<-c(49,69,79,99,18,29,59,89,14,39,23,3,43,33,93,53,63,73,10,83)
neuro_ques<-c(60,57,87,47,77,27,97,19,67,11,37,40,17,100,90,50,70,80,30,12)
open_ques<-c(64,54,84,94,4,44,34,7,74,24,71,81,16,21,31,61,41,51,1,91)


#agreeableness:
agree<-yougov[,agree_ques]
grm_agree<-grmCat(agree)
respProf<-agree
#conscienciousnes:
consci<-yougov[,consci_ques]
grm_consci<-grmCat(consci)
respProf<-consci

#extraversion:
extra<-yougov[,extra_ques]
grm_extra<-grmCat(extra)
respProf<-extra

#############

###################
#error occurs when estimating thetas for catObj:
#thetaValue<-estimateThetas(grmList[[2]],respProf)
#Error in estimateThetas(grmList[[2]], respProf) : 
#Theta value too extreme for numerical routines to provide reliable calculations.  
#This usually happens when the probabilities of answering in different response categories can no longer be distinguished.
## WORKS WITH MAP BUT NOT WITH EAP ##

#####################
#error occurs when estimating oracle
#got NAs for catObj2 grmEAP:
#Theta value too extreme for numerical routines to provide reliable calculations. 
#This usually happens when the probabilities of answering in different response categories can no longer be distinguished.  Try using less extreme values for theta.  If using MAP estimation, try EAP instead.

####
#error occurs in allEst only in grm_EAP:
#Theta value too extreme for numerical routines to provide reliable calculations. 
#This usually happens when the probabilities of answering in different response categories can no longer be distinguished.  
#Try using less extreme values for theta.  If using MAP estimation, try EAP instead.
###############
#neuroticism:

neuro<-yougov[,neuro_ques]
grm_neuro<-grmCat(neuro)
respProf<-neuro
#openness:
open<-yougov[,open_ques]
grm_open<-grmCat(open)
respProf<-open

#######
grm_neuro@lengthThreshold<-3


grm_MAP<-grm_neuro
grm_MAP@estimation<-"MAP"
grm_EAP<-grm_neuro
grm_EAP@estimation<-"EAP"
grmList<- list(grm_MAP, grm_EAP )


empty<-function(resp){
  for (i in 1:nrow(resp)){
    if (sum(is.na(resp[i,]))==18){
      print(i)
    }
  }
}

empty(respProf)

sum(is.na(respProf))
respProf[is.na(respProf)]<- -1
thetaValue<-estimateThetas(grm_MAP,respProf)
orac<-allOrac(grmList[[1]], thetaValue, respProf, n=3)
estimates<-allEst(list(grm_MAP), respProf)
 estimates[1,]
estimates<-cbind(estimates, orac)
bias<-apply(estimates, 2, function(x){abs(thetaValue-x)})
bias<-as.data.frame(cbind(bias, thetaValue))
colnames(bias)<-c("MAP", 'Oracle',"Theta")
linecol<- c( "#339966", "#0066CC",
             "#ffcccc", "#660066", "#CC3300", 
             "#003333", "#660000", "#666666", "#99cccc", "#cc9900")
ltys<-c(2:11)
par(mar=c(4,3,2,9),mgp=c(1.5,0,0))
plot(bias$Theta,y=c(1:length(thetaValue)), yaxt="n",tck=F, xlab= expression(theta), ylab="Bias", ylim=c(0,1.5), col="white", lty=1, lwd=1)
axis(2, at=seq(0,1.5, by=.2),labels=seq(0,1.5, by=.2), col.axis="black", las=2, tick=F)

lines(lowess(bias$Theta, bias[,1]),col=alpha(linecol[2], .4),lty=ltys[2], lwd=2)
#NAs#:lines(lowess(bias$Theta, bias[,2]),col=alpha(linecol[4], .4),lty=ltys[4], lwd=2)
lines(lowess(bias$Theta, bias[,2]),col=alpha(linecol[5], .4),lty=ltys[5], lwd=2)
#noFixed#:lines(lowess(bias$Theta, bias[,4]),col=alpha(linecol[6], .4),lty=ltys[6], lwd=2)
sapply(1:length(quantile(bias$Theta, c(0.95, 0.9, 0.1, 0.05))), function(i){abline(v=quantile(bias$Theta, c(0.95, 0.9, 0.1, 0.05))[i])})

legend("left",  inset=c(1,1.2), xpd=TRUE, bty="n", col = linecol[c(2,5)],legend=c( "MAP",  'Oracle' ), lty=ltys[c(2,5)])

title(main=TeX("YouGov Respondents'(nuero) bias "),sub= "*Vertical lines represent quantiles at 0.95, 0.9, 0.1, 0.05" , cex.sub=.7, cex.main=.8)
#################
fish<- allFish(list(grm_MAP), thetaValue,respProf)
fish<- cbind(fish, thetaValue)


fish<-as.data.frame(fish)
colnames(fish)<-c("MAP",'Theta')
fish[1,]

par(mar=c(4,3,2,9),mgp=c(1.3,0,0))
plot(fish$Theta,y=c(1:length(thetaValue)), yaxt="n",tck=F, xlab= expression(theta), ylab="Fisher_test", ylim=c(1,2.5), col="white", lty=1, lwd=1)
axis(2, at=seq(1,2.5, by=.5),labels=seq(1,2.5, by=.5), col.axis="black", las=2, tick=F)

lines(lowess(fish$Theta, fish[,1]),col=alpha(linecol[2], .4),lty=ltys[2], lwd=2)
#lines(lowess(fish$Theta, fish[,2]),col=alpha(linecol[4], .4),lty=ltys[4], lwd=2)
sapply(1:length(quantile(fish$Theta, c(0.95, 0.9, 0.1, 0.05))), function(i){abline(v=quantile(fish$Theta, c(0.95, 0.9, 0.1, 0.05))[i])})
legend("left",  inset=c(1,1.2), xpd=TRUE, bty="n", col = linecol[c(2)],legend=c( "MAP"), lty=ltys[c(2)])
title(main=TeX("YouGov Respondents' (neuro) Fisher info "),sub= "*Vertical lines represent quantiles at 0.95, 0.9, 0.1, 0.05" , cex.sub=.7, cex.main=.8)

