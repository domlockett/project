#' A function that estimates the theta values of multiple cat-objects
#'
#' Takes in a full collection of answers from multiple respondents and multiple cat-objects and returns a set of theta estimates defined by the user
#'
#' @param respondents A matrix of respondent answers
#' @param catObjs Any number of \code{Cat} objects of the same class (using different estimation parameters)
#'
#'
#' @details The function takes a \code{Cat} object and generates an estimation for \code{theta} using \code{MAP}, \code{EAP}, etc. 
#' The user must concatenate all \code{cat} objects into a single list to be analyzed.
#' 
#' @return The function \code{allEst} returns a vector.  Each \code{Cat} object returns a column and each respondent returns a row.
#' 
#' 
#' This function is to allow users to look at data computed before \code{Cat} technologies were availabe and and analyze multpile types of analyses on mulptile resondents to achieve a theta estimate.
#' 
#' @seealso \code{\link{Cat-class}}, \code{\link{apply}}, \code{\link{selectItem}}, \code{\link{basic_theta}}
#' 
#' @examples 
#' 
#'  ## Loading ltm Cat object
#' data(grm_cat)
#'    
#' ## load sample data
#'  
#'respondentsapply(thetaValue, 2, simulateRespondents_test, cat=grm_cat, n=10)
#' 
#' #Choose your \code{cat} object and the types of estimation you would like to use
#'  grm_MAP<-grm_cat
#'  grm_MAP@estimation<-"MAP"
#'  grm_EAP<-grm_cat
#'  grm_EAP@estimation<-"EAP"
#' 
#' 
#' grm<-list(grm_MAP, grm_EAP)
#' ## Create a threshold for the estimation
#' 
#' ## You must set some sort of CheckStopRule
#' grm_cat@lengthThreshold<-4
#' 
#' ## Run 
#' 
#' allEst(respondents, grm)
#' 
#' @author Haley Acevedo, Ryden Butler, Josh W. Cutler, Matt Malis, Jacob M. Montgomery, Tom Wilkinson, Erin Rossiter, Min Hee Seo, Alex Weil, Jaerin Kim, Dominique Lockett 
#' 
#' @rdname allEst
#' 
#' @export
 allEst <- function(catObjs, resp){
UseMethod("allEst", catObj)
}
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
       cat<-storeAnswer(catObj,item=selectItem(catObj)$next_item, answers[selectItem(catObj)$next_item])
        #^call on an catObj with no answers to know which question is first; store it 
        while (checkStopRules(cat)==F) {
          #^rerun the next 2 lines until the rules we set are met
          cat<-storeAnswer(cat, item=selectItem(cat)$next_item, answers[selectItem(cat)$next_item])
          # ^now call on the catObj with 1 question/answer and rerun until checkStopRules completed
        }
        theta_est<-estimateTheta(cat)
        #theta_est<-list(estimateTheta(cat), getAnswers(cat) )
        return(theta_est)
      }, catObj=catObjs[[i]])))
      
    }
 # colnames(store)<-  sapply(catObjs, function(x) catObjs@estimation))
  
  return(t(store))
}



allEst(list(grm_RAND), respProf[1:5,])
allEst(list(grm_EAP), respProf[1:5,])
allEst(grm, respProf[1:5,])
