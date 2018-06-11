##Plot theta estimates using simThetas, oracle, allEst
library(plyr)
library(catSurv)
load("C:/Users/dl0ck/OneDrive/Documents/Summer 2018/Montgomery/catSurv_dl/environments with functions.RData")
grm_cat@lengthThreshold<-4
thetaValue<-data.frame(1)
sr<-apply(as.data.frame(1:5),1, simulateRespondents_test, cat=grm_cat, n= 10)
sr<-ldply(sr, rbind)
sr<-sr[,-1]
grm<-grm_cat


?oracle
th<-estimateThetas(grm_EAP, responses=sr)


realThetas<-rep(1, 10)
  grm[[1]]
allEst(grm,sr)
allOrac<-function(catObj,theta,resp, n){
  oracle<-rep(NA, nrow(resp))
  for (i in 1:nrow(resp)){
    oracle[i]<-oracle_test(catObj=catObj,theta=theta[i],x=resp[i,],n=n)$theta_est
  }
  return(as.data.frame(oracle))
  }

orac_results<-allOrac(resp=sr, catObj=grm_cat,theta=realThetas, n=5)

oracle_test(catObj=grm_cat,theta=th[1],x=sr[1,], n=2)

cat_results<-allEst(grm,sr)

cbind(th[1], orac_results)




plot(th, cat_results[,1], pch=19)

points(th, orac_results$oracle, col='green')

?nextItem


## option to estimate oracle###
## mulitiple catObj (dif parameters)
## unit test..? 1 case when we no positively it is correct
## roxygen documentation
## test at the begininning to make sure they are the same model
## all objects in catList should be in same model otherwise terminate
## an option for a fixed battery which uses specific question if they always asked questions in that order
## take simulate respondents out and  
## write a summary function that evaluate()** that takes argument and
## .
sr<-apply(thetaValue,2, simulateRespondents_test, cat=grm_cat, n= 10)
but<-allEst(grm_cat,sr$X1)



#############################
# Here we are going to start using tryCatch() to stop allEst from breaking computers:
allEst<-function(catObjs=list(),resp){
  
  #the multiple cat objects must be of the same class and be in list form
  store<-NULL
  obj<-NULL
  for (i in 1:length(catObjs)){
    obj<-c(obj, catObjs[[i]]@model)
    if (length(unique(obj))!=1) { #USE THE @MODEL SECTION
      stop("List of Cat objects must be of the same type*")
    }
    if(length(resp) != length(catObjs[[i]]@answers)){
      stop("Response profile is not compatible with Cat object.")
    } 
    #apply theta basic to a set or respondents and return the values for every catObj provided by the user.
    store<-(rbind(store,apply(resp,1,  function(catObj, answers=c()){
      if(length(resp) != length(catObj@answers)){
        stop("Response profile is not compatible with Cat object.")
      }
      tryCatch(cat<-storeAnswer(catObj,item=selectItem(catObj)$next_item, answers[selectItem(catObj)$next_item]), 
               error= function(e) {print("Unsupported values of theta [MAP, WLE]"); NaN})
      #^call on an catObj with no answers to know which question is first; store it 
      while (checkStopRules(cat)==F) {
        #^rerun the next 2 lines until the rules we set are met
      tryCatch(cat<-storeAnswer(cat, item=selectItem(cat)$next_item, answers[selectItem(cat)$next_item]), 
               error= function(e) {print("Unsupported values of theta [MAP, WLE]"); NaN})
        # ^now call on the catObj with 1 question/answer and rerun until checkStopRules completed
      }
      tryCatch(theta_est<-estimateTheta(cat), error= function(e) {print("Unsupported values of theta [MAP, WLE]"); NaN})
      #theta_est<-list(estimateTheta(cat), getAnswers(cat) )
      return(theta_est)
    }, catObj=catObjs[[i]])))
    
  }
  # colnames(store)<-  sapply(catObjs, function(x) catObjs@estimation))
  
  return(t(store))
}

r<-allEst(list(grm_MAP), respProf[1:1000,])

grm_MAP<-grm_cat
grm_MAP@estimation<-"MAP"
grm_WLE<-grm_cat
grm_WLE@estimation<-"WLE"

grm<-list(grm_MAP, grm_EAP)