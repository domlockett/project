#' A function that estimates multiple cat-objects
#'
#' Takes in a full collection of answers from multiple respondents and a cat-object and returns a set of theta estimates defined by the user
#'
#' @param respondents A matrix of respondent answers
#' @param catObjs Any number of \code{Cat} objects of the same class (using different estimation parameters)
#'
#'
#' @details The function takes a \code{Cat} object and generates an estimation for \code{theta} using \code{MAP}, \code{EAP}, etc. 
#' The user must concatenate all \code{cat} objects into a single list to be analyzed.
#' 
#' @return The function \code{allEst} returns a vector.  Each \code{cat} object returns a column and each respondent returns a row.
#' 
#' 
#' This function is to allow users to look at data computed before \code{cat} technologies were availabe and and analyze multpile types of analyses on mulptile resondents to achieve a theta estimate.
#' 
#' @seealso \code{\link{Cat-class}}, \code{\link{apply}}, \code{\link{selectItem}}, \code{\link{basic_theta}}
#' 
#' @examples 
#'   
#' ## load sample data
#'  
#' respondents<-rbind(sample(c(1:4),40,replace=T),sample(c(1:4),40,replace=T),sample(c(1:4),40,replace=T),sample(c(1:4),40,replace=T),sample(c(1:4),40,replace=T))
#' rownames(respondents)<-c("r1","r2","r3","r4","r5") 
#' 
#' #Choose your \code{cat} object and the types of estimation you would like to use
#'  grm_MAP<-grm_cat
#'  grm_MAP@estimation<-"MAP"
#'  grm_EAP<-grm_cat
#'  grm_EAP@estimation<-"EAP"
#' 
#' grm<-list(grm_MAP, grm_EAP)
#' 
#' ## Create a threshold for the estimation
#' grm_cat@lengthThreshold<-4
#' 
#' ## Run 
#' 
#' allEst(resondents, grm)
#' 
#' @author Haley Acevedo, Ryden Butler, Josh W. Cutler, Matt Malis, Jacob M. Montgomery, Tom Wilkinson, Erin Rossiter, Min Hee Seo, Alex Weil, Jaerin Kim, Dominique Lockett 
#' 
#' @rdname allEst
#' 
#' @export
allEst<-function(respondents, catObjs=list()){
  #the multiple cat objects must be of the same class and be in list form
  store<-NULL
    for (i in 1:length(catObjs)){
      #apply theta basic to a set or respondents and return the values for every catObj provided by the user.
          store<-(cbind(apply(respondents,1,  theta_basic, catObj=catObjs[[i]]),store))
    }
  colnames(store)<-  paste("Theta", 1:length(catObjs), sep = " ")
  return(store)
}
