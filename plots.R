## Generalized CAT plot
## Arguments for the model: "IRF", "ICC", "IIF" (the default is ICC)

library(catSurv)
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
  if(model=="IRF"){legend("left",  inset=c(1,1.2), xpd=TRUE, bty="n",legend=sapply(1:ncol(ipr),function(i)paste("Response", i,sep=" ")), col=linecolors[1:ncol(ipr)], lty=ltys[1:ncol(ipr)] , cex=0.8)}} col=linecolors[1:ncol(ipr)], lty=ltys[1:ncol(ipr)] , cex=0.8)}}

## Examples
catPlot(ltm_cat,item=8,model="ICC")
catPlot(ltm_cat,item=8,model="IRF")
catPlot(ltm_cat,item=8,model="IIF")
catPlot(grm_cat,item=8,model="ICC")
catPlot(grm_cat,item=8,model="IRF")
catPlot(grm_cat,item=8,model="IIF")
