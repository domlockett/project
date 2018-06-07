# live tests
library(catSurv)


####catPlot####
#test
catPlot(ltm_cat,item=8,model="ICC")
catPlot(ltm_cat,item=8,model="IRF")
catPlot(ltm_cat,item=8,model="IIF")
catPlot(grm_cat,item=8,model="ICC")
catPlot(grm_cat,item=8,model="IRF")
catPlot(grm_cat,item=8,model="IIF")


####thetaBasic####
#sample answers
ltm_answers<-sample(c(0:1),40,replace=T)
grm_answers<-sample(c(1:4),20,replace=T)
gpcm_answers<-sample(c(1:4),10, replace=T)
tpm_answers<-sample(c(0:1),20,replace=T)

# set some thresholds so we have checkStopRules
ltm_cat@lengthThreshold<-8
grm_cat@lengthThreshold<-8
gpcm_cat@lengthThreshold<-4
tpm_cat@lengthThreshold<-19

#test function
thetaBasic(ltm_cat, ltm_answers)
thetaBasic(grm_cat, grm_answers)
thetaBasic(gpcm_cat, gpcm_answers)
thetaBasic(tpm_cat, tpm_answers)
 
 
####allEst####
#sample answers
respondents_grm<-rbind(sample(c(1:4),20,replace=T),sample(c(1:4),20,replace=T),sample(c(1:4),20,replace=T),sample(c(1:4),20,replace=T),sample(c(1:4),20,replace=T))
respondents_gpcm<-rbind(sample(c(1:4),10, replace=T),sample(c(1:4),10, replace=T),sample(c(1:4),10, replace=T),sample(c(1:4),10, replace=T),sample(c(1:4),10, replace=T))
respondents_ltm<-rbind(sample(0:1,40, replace=T),sample(0:1,40, replace=T),sample(0:1,40, replace=T),sample(0:1,40, replace=T),sample(0:1,40, replace=T))
respondents_tpm<-rbind(sample(0:1,20, replace=T),sample(0:1,20, replace=T),sample(0:1,20, replace=T),sample(0:1,20, replace=T),sample(0:1,20, replace=T))
rownames(respondents_grm)<-c("r1","r2","r3","r4","r5") 
rownames(respondents_gpcm)<-c("r1","r2","r3","r4","r5") 
rownames(respondents_ltm)<-c("r1","r2","r3","r4","r5") 
rownames(respondents_tpm)<-c("r1","r2","r3","r4","r5") 



# multiple catObjects
grm_MAP<-grm_cat
  grm_MAP@estimation<-"MAP"
grm_EAP<-grm_cat
  grm_EAP@estimation<-"EAP"
 
grm<-list(grm_MAP, grm_EAP)

gpcm_MAP<-gpcm_cat
  gpcm_MAP@estimation<-"MAP"
gpcm_EAP<-gpcm_cat
  gpcm_EAP@estimation<-"EAP"

gpcm<-list(gpcm_MAP, gpcm_EAP)

ltm_MAP<-ltm_cat
  ltm_MAP@estimation<-"MAP"
ltm_EAP<-ltm_cat
  ltm_EAP@estimation<-"EAP"
ltm_WLE<-ltm_cat
  ltm_WLE@estimation<-"WLE"

ltm<-list(ltm_MAP, ltm_EAP, ltm_WLE)


tpm_MAP<-tpm_cat
  tpm_MAP@estimation<-"MAP"
tpm_EAP<-tpm_cat
  tpm_EAP@estimation<-"EAP"
tpm_WLE<-tpm_cat
  tpm_WLE@estimation<-"WLE"

tpm<-list(tpm_MAP, tpm_EAP, tpm_WLE)

#test

allEst(respondents_ltm,ltm)
allEst(respondents_grm, grm)
allEst(respondents_tpm, tpm)
allEst(respondents_gpcm, gpcm)
apply(resp[1:3,],1, thetaBasic)
# #makeTree##
grm_cat@answers
ltm_cat@lengthThreshold<-3
catSurv::makeTree(ltm_cat,flat=T)
makeTree(ltm_cat,flat=T)
attr(grm_cat)
