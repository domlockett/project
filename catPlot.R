#' A function that plots catObjects given specific parameters
#'
#' Takes in a \code{cat} object 
#'
#' @param catObj An object of class \code{cat}
#' @param item Any number of \code{Cat} objects of the same class (using different estimation parameters)
#' @param model
#'
#' @details The first function is catPlot which takes in a catObject, an item number, and a plot type and returns a plot of the output.
#' 
#' @return The function \code{plotCat} returns a plot.  
#' 
#' 
#' 
#' 
#' @seealso \code{\link{Cat-class}}, \code{\link{probability}}, \code{\link{fisherInfo}}, \code{\link{basic_theta}}
#' 
#' @examples 
#' 
#' ## Loading ltm Cat object
#' 
#' data(ltm_cat)
#'   
#'catPlot(ltm_cat,item=8,model="ICC")
#'catPlot(ltm_cat,item=2,model="IRF")
#'catPlot(ltm_cat,item=3,model="IIF")

#' 
#' @author Haley Acevedo, Ryden Butler, Josh W. Cutler, Matt Malis, Jacob M. Montgomery, Tom Wilkinson, Erin Rossiter, Min Hee Seo, Alex Weil, Jaerin Kim, Dominique Lockett 
#' 
#' @rdname catPlot
#' 
#' @export
catPlot <- function(catObj){
UseMethod("catPlot", catObj)
}

catPlot<-function(catObj,item=1,model="ICC"){
  
linecolors<-c(col=rgb(51/255, 153/255, 102/255), rgb(0/255, 102/255, 204/255),
                rgb(204/255, 102/255, 153/255), col= rgb(102/255, 0/255, 102/255),
                col=rgb(204/255, 51/255, 0/255),
                ## Above is the customizable part. Codes below return randomized colors.
                sapply(c(1:1000),function(i){return(col=rgb(
                  sample(100:255,1)/255,sample(100:255,1)/255,sample(100:255,1)/255))}))
  ltys<-c(1:16)
  x<-seq(-5,5,.1)
  if(model=="IIF"){ipr<-sapply(x,fisherInf, catObj=catObj, item=item)
  par(mar=c(3,3,2,9),mgp=c(1.5,0,0))
  plot(x,ipr,"l", main="Item Information Function",col=linecolors[3],
       ylab=expression(I(theta)),xlab=expression(theta), lwd=2,
       xlim=c(-5,5), ylim=c(0,max(ipr)), tck=F,cex.axis=.90, las=1)
  legend("left",  inset=c(1,1.2), xpd=TRUE, bty="n", legend=paste("Item",item),col=linecolors[3], lty=1, cex=0.8)}
  else{ipr<-as.matrix(sapply(x,probability, catObj=catObj, item))
  if((catObj@model=="grm")|(catObj@model=="gpcm")){
    ipr<-t(ipr)[,2:(nrow(ipr)-1)]}
  if(model=="ICC"){ipr<-as.matrix(sapply(1:nrow(ipr),function(i)weighted.mean(ipr[i,],1:ncol(ipr))))}
  if(model=="IRF"){x<-x[2:length(x)]
  ipr<-10*as.matrix(sapply(1:(nrow(ipr)-1),function(i){return(abs(ipr[i+1,]-ipr[i,]))}))
  if(catObj@model=="grm"|catObj@model=="gpcm"){ipr<-t(ipr)}}
  par(mar=c(3,3,2,9),mgp=c(1.5,0,0))
  plot(c(),c(),"l", main=paste(model,"Plot"),ylab="Probability", xlab=expression(theta),
       lwd=2, xlim=c(-5,5), ylim=c(0,max(ipr)), las=1, tck=F,cex.axis=.90)
  sapply(1:(ncol(ipr)),function(i){lines(x,ipr[,i],col=linecolors[i],lty=ltys[i], lwd=2)})
  if(model=="ICC"){legend("left",  inset=c(1,1.2), xpd=TRUE, bty="n", legend=paste("Item",item), col=linecolors[1], lty=1 , cex=0.8)}}
  if(model=="IRF"){legend("left",  inset=c(1,1.2), xpd=TRUE, bty="n",legend=sapply(1:ncol(ipr),function(i)paste("Response", i,sep=" ")), col=linecolors[1:ncol(ipr)], lty=ltys[1:ncol(ipr)] , cex=0.8)}} 
