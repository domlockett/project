allEst<-function(catObjs=list(),resp){
  
  #the multiple cat objects must be of the same class and be in list form
  store<-NULL
  #colnames(store)<-ncol(1:length(catObjs))
  if (length(catObjs)==1){
    if(length(resp) != length(catObjs@answers)){
      stop("Response profile is not compatible with Cat object.")
    }
    if (length(unique(catObjs@answers))!=1)  {
      stop("List of Cat objects must be of the same type*")
    }
    store<-(cbind(apply(resp,1,  function(catObj, answers=c()){
      if(length(resp) != length(catObj@answers)){
        stop("Response profile is not compatible with Cat object.")
      }
      cat<-storeAnswer(catObj,item=selectItem(catObj)$next_item, answers[selectItem(catObj)$next_item])
      #^call on an catObj with no answers to know which question is first; store it 
      while (checkStopRules(cat)==F) {
        #^rerun the next 2 lines until the rules we set are met
        cat<-storeAnswer(cat, item=selectItem(cat)$next_item, answers[selectItem(cat)$next_item])
        # ^now call on the catObj with 1 question/answer and rerun until checkStopRules completed
      }
      theta_est<-(estimateTheta(cat)) 
      #theta_est<-list(estimateTheta(cat), getAnswers(cat) )
      #we can verify the estimates by adding answers to our output we would then plug those answers into ltm_cat and check with estimateTheta
      return(theta_est)
    }, catObj=catObjs),store))
    #colnames(store)<- (catObjs@estimation)
    return(store)
  } else
    if(length(resp) != length(catObjs[[1]]@answers)){
      stop("Response profile is not compatible with Cat object.")
    }
  if (length(unique(catObjs[[1]]@answers))!=1)  {
    stop("List of Cat objects must be of the same type*")
  }
  for (i in 1:length(catObjs)){
    
    #apply theta basic to a set or respondents and return the values for every catObj provided by the user.
    store<-(cbind(apply(resp,1,  function(catObj, answers=c()){
      if(length(resp) != length(catObj@answers)){
        stop("Response profile is not compatible with Cat object.")
      }
      cat<-storeAnswer(catObj,item=selectItem(catObj)$next_item, answers[selectItem(catObj)$next_item])
      #^call on an catObj with no answers to know which question is first; store it 
      while (checkStopRules(cat)==F) {
        #^rerun the next 2 lines until the rules we set are met
        cat<-storeAnswer(cat, item=selectItem(cat)$next_item, answers[selectItem(cat)$next_item])
        # ^now call on the catObj with 1 question/answer and rerun until checkStopRules completed
      }
      theta_est<-(estimateTheta(cat)) 
      #theta_est<-list(estimateTheta(cat), getAnswers(cat) )
      #we can verify the estimates by adding answers to our output we would then plug those answers into ltm_cat and check with estimateTheta
      return(theta_est)
    }, catObj=catObjs[[i]]),store))
    
  }
  # colnames(store)<-  sapply(catObjs, function(x) catObjs@estimation))
  
  return(store)
}
simulateRespondents_test <- function(cat, theta, n){
  if(sum(!is.na(cat@answers)) != 0){
    stop("Cat object should not have respondent specific answers.")
  }
  
  ans_profiles <- matrix(nrow = n, ncol = length(cat@answers))
  for(respondent in 1:n){
    for(i in 1:length(cat@answers)){
      probs <- probability(catObj = cat, theta = theta, item = i)
      
      ## need to calculate answer probabilities from cumulative probabilities
      if(cat@model == "grm"){
        probs <- diff(probs)
      }
      ## need to append probability of answering a 0
      if(cat@model == "ltm" | cat@model == "tpm"){
        probs <- c(1 - probs, probs)
      }
      ## gpcm is fine
      
      ## now generate answers with those probabilities
      ans_profiles[respondent, i] <- sample(1:(length(cat@difficulty[[i]])+1), 1, prob = probs)
    }
  }
  return(as.data.frame(ans_profiles))
}
grm_cat@lengthThreshold<-2

# Define answer profiles and thetas
thetaValue<-as.data.frame(-4:4)

#first 100 rows -4, 2nd 100 -3, etc.
respProf<-apply(thetaValue,1,  simulateRespondents_test, cat=grm_cat, n=100)
respProf<-ldply(respProf, rbind)
# Various cat methods

grm_MAP<-grm_cat
grm_MAP@estimation<-"MAP"

grm_WLE<-grm_cat
grm_WLE@estimation<-"WLE"

grm_RAND<-grm_cat
#why am i generating random with all est?
setSelection(grm_RAND) <- "RANDOM"

grm_TRUE<-estimateThetas(grm_cat, responses=respProf)
grm<-list(MAP=grm_MAP,WLE=grm_WLE,RAND= grm_RAND )
##############
# not getting all thetas bc wont run
# Error in selectItem(cat) : 
#Theta value too extreme for numerical routines. 

a<-allEst(grm, respProf)
