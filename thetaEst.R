
library(catSurv)

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
answers<-sample(c(1:4),40,replace=T)
tpm_answers<-sample(c(0:1),20,replace=T)
#test function
theta_basic(ltm_cat, answers)
theta_basic(grm_cat, answers)
theta_basic(gpcm_cat, answers)
theta_basic(tpm_cat, tpm_answers)

#Tests to ensure accuracy

#answ<-theta_basic(ltm_cat, answers)
#test<-(answ[[2]])
#ltm_cat@answers<-test
#estimateTheta(ltm_cat)


#static sample data
r1 <-c(1, 3, 4, 3, 1, 3, 1, 3, 2, 3, 2, 2, 2, 1, 3, 3, 2 ,4 ,4 ,3, 4, 1, 4, 3, 4, 3, 1, 3, 3, 3, 1, 2, 1, 1, 4, 1, 4, 2, 2, 1)
r2<-c(3, 1, 3, 3, 4, 1, 4 ,2, 4 ,1 ,3 ,3 ,2 ,2 ,3 ,1 ,2 ,1 ,3 ,2 ,3 ,4 ,1 ,1 ,4 ,4 ,4 ,2 ,3 ,1 ,1 ,4 ,4 ,1 ,1 ,4 ,3 ,2 ,1,4)
r3<-c(4 ,2 ,2 ,1 ,2 ,4 ,3 ,4 ,4 ,1 ,3 ,1 ,2 ,2 ,4 ,2 ,1 ,1 ,1 ,2 ,4 ,4 ,2 ,1 ,4 ,3 ,3 ,2 ,2 ,2 ,2 ,2 ,3 ,2 ,1 ,4 ,2 ,3 ,3, 3)
r4<-c(1 ,1 ,3 ,3 ,1 ,3 ,1 ,1 ,3 ,1 ,1 ,4 ,4 ,4 ,4, 4, 3, 4, 2 ,4 ,4 ,2 ,3 ,4 ,2 ,4 ,2 ,4 ,1 ,1, 2, 3, 4, 4, 2, 1, 1, 2, 4, 4)
r5<-c(1 ,2 ,3 ,3 ,4 ,1 ,4 ,3, 2 ,1 ,2 ,1 ,4 ,2 ,3 ,2 ,2 ,2 ,4 ,1 ,4 ,2 ,2 ,2 ,4 ,1 ,1 ,1 ,2, 4 ,2 ,4 ,1 ,4 ,4 ,2 ,3 ,3 ,1 ,2)

#make some fake data
respondents<-rbind(r1,r2,r3,r4,r5)
respondents_tpm<-rbind(sample(0:1,20, replace=T),sample(0:1,20, replace=T),sample(0:1,20, replace=T),sample(0:1,20, replace=T),sample(0:1,20, replace=T))
rownames(respondents_tpm)<-c("r1","r2","r3","r4","r5")

theta_est<-function(respondents, respondents_tpm){
    if(missing(respondents_tpm)){
#each catObj gets a different apply to perform the theta_basic function on multiple respondents
grm<-(apply(respondents,1,  theta_basic, catObj=grm_cat))
  #turn it into a dataframe
    grm<-(as.data.frame(grm))
      #identify the class
        colnames(grm)<-"GRM"
ltm<-(apply(respondents, 1, theta_basic, catObj=ltm_cat))
  ltm<-(as.data.frame(ltm))
    colnames(ltm)<-"LTM"
gpcm<-apply(respondents,1, theta_basic, catObj=gpcm_cat)
  gpcm<-(as.data.frame(gpcm))
    colnames(gpcm)<-"GPCM"
     return(cbind(grm,ltm,gpcm))
   } else {
    
     grm<-(apply(respondents,1,  theta_basic, catObj=grm_cat))
      grm<-(as.data.frame(grm))
       colnames(grm)<-"GRM"
     ltm<-(apply(respondents, 1, theta_basic, catObj=ltm_cat))
       ltm<-(as.data.frame(ltm))
         colnames(ltm)<-"LTM"
     gpcm<-apply(respondents,1, theta_basic, catObj=gpcm_cat)
      gpcm<-(as.data.frame(gpcm))
        colnames(gpcm)<-"GPCM"
     tpm<-apply(respondents_tpm,1, theta_basic, catObj=tpm_cat)
       tpm<-(as.data.frame(tpm))
        colnames(tpm)<-"TPM" 
        return(cbind(grm,ltm,gpcm, tpm))
  
        }
}
theta_est(respondents)
theta_est(respondents, respondents_tpm)

tpm_cat


#NOT WORKING EXAMPLE
??tmp_cat
theta_basic(tpm_cat,answers)
t<-(apply(respondents, theta_basic, catObj=tpm_cat))
