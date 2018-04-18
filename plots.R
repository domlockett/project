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
  
  x<-seq(-5,5,.1)
  if(model=="IIF"){ipr<-sapply(x,fisherInf, catObj=catObj, item=item)
  plot(x,ipr,"l", main="Item Information Function",col=linecolors[1],
       ylab=expression(I(theta)),xlab=expression(theta), lwd=2,
       xlim=c(-5,5), ylim=c(0,max(ipr)), tick=F,cex.axis=.90)
  legend("topright", legend=paste("Item",item),col=linecolors[1], lty=1, cex=0.8)}
  else{ipr<-as.matrix(sapply(x,probability, catObj=catObj, item))
  if((catObj@model=="grm")|(catObj@model=="gpcm")){
    ipr<-t(ipr)[,2:(nrow(ipr)-1)]}
  if(model=="IRF"){x<-x[2:length(x)]
  ipr<-10*as.matrix(sapply(1:(nrow(ipr)-1),function(i){return(abs(ipr[i+1,]-ipr[i,]))}))
  if(catObj@model=="grm"|catObj@model=="gpcm"){ipr<-t(ipr)}}
  plot(c(),c(),"l", main=paste(model,"Plot"),ylab="Probability", xlab=expression(theta),
       lwd=2, xlim=c(-5,5), ylim=c(0,max(ipr)), tick=F,cex.axis=.90)
  sapply(1:(ncol(ipr)),function(i){lines(x,ipr[,i],col=linecolors[i],lwd=2)})
  legend("left", legend=sapply(1:ncol(ipr),function(i)paste("R",i,sep="")), 
         col=linecolors[1:ncol(ipr)], lty=1, cex=0.8)}}

## Examples
catPlot(ltm_cat,item=8,model="ICC")
catPlot(ltm_cat,item=8,model="IRF")
catPlot(ltm_cat,item=8,model="IIF")
catPlot(grm_cat,item=8,model="ICC")
catPlot(grm_cat,item=8,model="IRF")
catPlot(grm_cat,item=8,model="IIF")


# Old codes

# data(grm_cat)
# data(ltm_cat)
# #Bare bones IRF function 
#  #Needs legend and pazzaz
# plot_irf<- function(cat, item){
#   x<-seq(-5,5,.01)
#   y<-sapply(x,probability, catObj=ltm_cat, item=1)
#   # create a vector of x values a bunch of different theta values
#   #plug x value
#   #use apply to loop through the vector of 
#   plot(x,y,main="Item Response Function",  ,"l",col =rgb(51/255, 153/255, 102/255), axis=F,cex.axis=.90)
#   axis(side, at=, labels=, pos=, lty=, col=, las=, tck=, ...)
#   
#   
#   y2<-(1-y)
#   lines(x,y2, col=rgb(204/255, 102/255, 153/255), lty=2, axis=F,cex.axis=.90)
#   legend("left", legend=c("Probability Correct", "Probability Incorrect"), col=c(rgb(51/255, 153/255, 102/255), rgb(204/255, 102/255, 153/255)), lty=c(1,2), cex=0.8)
#   
# }
# plot_irf(ltm_cat)
#  
# #Bare bones ICC function
# 
# plot_icc<- function(cat, item){
#   x<-seq(-5,5,.01)
#   y<-sapply(x,probability, catObj=ltm_cat, item=1)
#   exp<-sum(x*y)
#   z<-sapply(y,exp)
#   # create a vector of x values a bunch of different theta values
#   #plug x value
#   #use apply to loop through the vector of 
#   plot(x,z, main="Item Characteristic Curve", tick=F, cex.axis=.90)
# }
# plot_icc(ltm_cat)
# 
# ## Setting colors of each line. Customizable.
# 
# 
# 
# 
# # IRFplot<-function(catObj,item=1){
# #   x<-seq(-5,5,.1)
# #   irffn<-as.matrix(abs(diff(sapply(x,probability, catObj=catObj, item))))
# #   if((catObj@model=="grm")|(catObj@model=="gpcm")){
# #   irffn<-t(irffn)}
# #   else{x<-x[2:101]}
# #   plot(c(),c(),"l", main="IRF Plot",ylab="Probability", xlab=expression(theta),
# #        lwd=2, xlim=c(-5,5), ylim=c(0,max(irffn)), tick=F,cex.axis=.90)
# #   sapply(1:(ncol(irffn)),function(i){lines(x,irffn[,i],col=linecolors[i],lwd=2)})
# #   legend("left", legend=sapply(1:ncol(irffn),function(i)paste("R",i,sep="")), 
# #        col=linecolors[1:ncol(irffn)], lty=c(1,2), cex=0.8)
# # }
# # IRFplot(grm_cat)
# # IRFplot(ltm_cat)
# # 
# # 
# # ## Generalized ICC plot (working)
# # 
# # ICCplot<-function(catObj,item=1){
# #   x<-seq(-5,5,.1)
# #   iccfn<-as.matrix(sapply(x,probability, catObj=catObj, item))
# #   if((catObj@model=="grm")|(catObj@model=="gpcm")){
# #     iccfn<-t(iccfn)}
# #   plot(c(),c(),"l", main="ICC Plot",ylab="Probability", xlab=expression(theta),
# #        lwd=2, xlim=c(-5,5), ylim=c(0,max(iccfn)), tick=F,cex.axis=.90)
# #   sapply(1:(ncol(iccfn)),function(i){lines(x,iccfn[,i],col=linecolors[i],lwd=2)})
# #   legend("left", legend=sapply(1:ncol(iccfn),function(i)paste("R",i,sep="")), 
# #          col=linecolors[1:ncol(iccfn)], lty=c(1,2), cex=0.8)
# # }
# # 
# # ICCplot(ltm_cat)
# # ICCplot(grm_cat,item=5)
# # 
# # 
# # #old GPCM Plot
# # 
# # ##plot_gpcm<-function(cat){
# # ##  x<-seq(-5,5,.01)
# # ##  one<-sapply(x,probability, catObj=gpcm_cat, item=1)
# # #  Probability<-t(one)
# #  # x<-1:length(Probability[,1])
# # #
# #  # plot(x, Probability[,1] ,"l", main="IRF Plot",tick=F, ylab="Probability", xlab=  expression(theta), lwd=2, ylim=c(0,1),xlim=c(0, max(x)), col=rgb(51/255, 153/255, 102/255), lty=1)
# #   #lines(x, Probability[,2], col= rgb(0/255, 102/255, 204/255),lwd=2, lty=2 )
# #   #lines(x, Probability[,3], col=rgb(204/255, 102/255, 153/255),lwd=2, lty=3)
# #   #lines(x, Probability[,4], col=rgb(102/255, 0/255, 102/255), lwd=2, lty=4)
# #   #lines(x, Probability[,5], col=rgb(204/255, 51/255, 0/255),lwd=2, lty=5)
# #   #legend("topright", inset=c(-0.2,0), legend=c("Probability of Answering 1", "Probability of Answering 2", "Probability of Answering 3", "Probability of Answering 4", "Probability of Answering 5"),col=c(rgb(51/255, 153/255, 102/255),rgb(0/255, 102/255, 204/255),rgb(204/255, 102/255, 153/255),rgb(102/255, 0/255, 102/255),rgb(204/255, 51/255, 0/255))
# # #, lty=c(1,2,3,4,5))
# # #}
# # #plot_gpcm(gpcm_cat)
# # 
# 
# 
# 
# 
# ##Now we are gonna do this ish with the IIF
# #Maybe discuss this plot with Erin 
# #it uses the fisher Infor
# 
# lines(smooth.spline(x, Probability[,2]))
# plot_iif<- function(cat, item=1){
# 
#   x<-seq(-5,5,.01)
#   
#   # create a vector of x values a bunch of different theta values
#   #plug x value
#   #use apply to loop through the vector of 
# 
#   plot(x,y, main="Item Information Function")
# 
#   plot(x,y, main="Item Information Function","l", col=rgb(204/255, 102/255, 153/255),lwd=2,ylab="Probability", xlab=  expression(theta))
# 
# }
# plot_iif(ltm_cat)
# 
