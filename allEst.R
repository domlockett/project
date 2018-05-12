
library(catSurv)
############LTM CAT doesn't shoot an error when its fed non-binary data of incorrect length#############################


ltm_cat@lengthThreshold<-4 # set some thresholds so we have checkStopRules
grm_cat@lengthThreshold<-4
gpcm_cat@lengthThreshold<-4
tpm_cat@lengthThreshold<-4

theta_basic<- function(catObj, answers=c()){
  cat<-storeAnswer(catObj,item=selectItem(catObj)$next_item, answers[selectItem(catObj)$next_item])
  #call on an catObj with no answers to know which question is first; store it 
  while (checkStopRules(cat)==F) {
    #rerun the next 2 lines until the rules we set are met
  cat<-storeAnswer(cat, item=selectItem(cat)$next_item, answers[selectItem(cat)$next_item])
  # now call on the catObj with 1 question/answer
  }
  theta_est<-(estimateTheta(cat)) 
  #theta_est<-list(estimateTheta(cat), getAnswers(cat) )
  #we can verify the estimates by adding answers to our output we would then plug those answers into ltm_cat and check with estimateTheta
  return(theta_est)
}

#test data
answers<-rbind(sample(c(1:4),40,replace=T),sample(c(1:4),40,replace=T))
binary_answers<-sample(c(0:1),40,replace=T)
tpm_answers<-sample(c(0:1),40,replace=T)
#test function
theta_basic(ltm_cat, binary_answers)
theta_basic(grm_cat, answers)
theta_basic(gpcm_cat, answers)
theta_basic(tpm_cat, tpm_answers)

#Tests to ensure accuracy

#answ<-theta_basic(ltm_cat, answers)
#test<-(answ[[2]])
#ltm_cat@answers<-test
##################SAMPLE DATA##########################################################
respondents<-rbind(sample(c(1:4),40,replace=T),sample(c(1:4),40,replace=T),sample(c(1:4),40,replace=T),sample(c(1:4),40,replace=T),sample(c(1:4),40,replace=T))
respondents_ltm<-rbind(sample(0:1,40, replace=T),sample(0:1,40, replace=T),sample(0:1,40, replace=T),sample(0:1,40, replace=T),sample(0:1,40, replace=T))
respondents_tpm<-rbind(sample(0:1,20, replace=T),sample(0:1,20, replace=T),sample(0:1,20, replace=T),sample(0:1,20, replace=T),sample(0:1,20, replace=T))
rownames(respondents)<-c("r1","r2","r3","r4","r5")
rownames(respondents_tpm)<-c("r1","r2","r3","r4","r5")
rownames(respondents_ltm)<-c("r1","r2","r3","r4","r5")


grm_MAP<-grm_cat
grm_MAP@estimation<-"MAP"

grm_EAP<-grm_cat
grm_EAP@estimation<-"MAP"

grm_WLE<-grm_cat
grm_WLE@estimation<-"MAP"

grm<-list(grm_MAP, grm_EAP, grm_WLE)

gpcm_MAP<-gpcm_cat
gpcm_MAP@estimation<-"MAP"

gpcm_EAP<-gpcm_cat
gpcm_EAP@estimation<-"EAP"

gpcm_WLE<-gpcm_cat
gpcm_WLE@estimation<-"MAP"

gpcm<-list(grm_MAP, grm_EAP, grm_WLE)

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
######################################SAMPLE DATA##################




allEst<-function(respondents, catObjs=list()){
  store<-NULL
    for (i in 1:length(catObjs)){
          store<-(cbind(apply(respondents,1,  theta_basic, catObj=catObjs[[i]]),store))
    }
  colnames(store)<-  paste("Theta", 1:length(catObjs), sep = " ")
  return(store)
}

allEst(respondents_ltm,ltm)
allEst(respondents, grm)
allEst(respondents_tpm, tpm)
allEst(respondents, gpcm)
