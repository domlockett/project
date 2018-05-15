#' A function that estimates theta
#'
#' Takes in a \code{cat} object
#'
#' @param catObj A matrix of respondent answers
#' @param catObjs any number of cat-objects of the same class (using different estimation parameters)
#'
#'
#' @details The function takes a \code{Cat} object and generates an estimation for \code{theta} using the various types of estimation associated with teh \code{cat} object
#' The function takes in a full set of answers and feeds them into the \code{cat} object as though it were getting the answers in real time.
#' 
#' @return The function \code{thetaBasic} returns a single \eqn{\theta} value. 
#' 
#' @note This function is computationally expensive.  If there are \eqn{k} response options and the researcher wants a complete branching scheme to include \eqn{n} items, \eqn{k^{n-1}} complete branching schemes will be calculated.  Setting \eqn{n} is done via the \code{lengthThreshold} slot in the \code{Cat} object.  See \strong{Examples}.
#' 
#' This function is to allow users to use old data using \code{catSurv} methods
#' 
#' 
#' @seealso \code{\link{Cat-class}}, \code{\link{selectItem}}
#'
#' @examples 
#' 
#' #test data
#' answers<-rbind(sample(c(1:4),40,replace=T),sample(c(1:4),40,replace=T))
#' binary_answers<-sample(c(0:1),40,replace=T)
#' tpm_answers<-sample(c(0:1),40,replace=T)
#' 
#' #test function
#' theta_basic(ltm_cat, binary_answers)
#' theta_basic(grm_cat, answers)
#' theta_basic(gpcm_cat, answers)
#' theta_basic(tpm_cat, tpm_answers)
#' 
#' 
#' @author Haley Acevedo, Ryden Butler, Josh W. Cutler, Matt Malis, Jacob M. Montgomery, Tom Wilkinson, Erin Rossiter, Min Hee Seo, Alex Weil, Jaerin Kim, Dominique Lockett 
#' 
#' @rdname thetaBasic
#' 
#' @export
thetaBasic <- function(catObj){
UseMethod("thetaBasic", catObj)
}
thetaBasic<- function(catObj, answers=c()){
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
}
