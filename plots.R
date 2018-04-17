# hi
#THIS IS THE PLOT IRF
library(catSurv)
data(grm_cat)
data(ltm_cat)
#Bare bones IRF function 
 #Needs legend and pazzaz
plot_irf<- function(cat, item){
  x<-seq(-5,5,.01)
  y<-sapply(x,probability, catObj=ltm_cat, item=1)
  # create a vector of x values a bunch of different theta values
  #plug x value
  #use apply to loop through the vector of 
  plot(x,y,main="Item Response Function",  ,"l",col =rgb(51/255, 153/255, 102/255), axis=F,cex.axis=.90)
  axis(side, at=, labels=, pos=, lty=, col=, las=, tck=, ...)
  
  
  y2<-(1-y)
  lines(x,y2, col=rgb(204/255, 102/255, 153/255), lty=2, axis=F,cex.axis=.90)
  legend("left", legend=c("Probability Correct", "Probability Incorrect"), col=c(rgb(51/255, 153/255, 102/255), rgb(204/255, 102/255, 153/255)), lty=c(1,2), cex=0.8)
  
}
plot_irf(ltm_cat)
 
#Bare bones ICC function

plot_icc<- function(cat, item){
  x<-seq(-5,5,.01)
  y<-sapply(x,probability, catObj=ltm_cat, item=1)
  exp<-sum(x*y)
  z<-sapply(y,exp)
  # create a vector of x values a bunch of different theta values
  #plug x value
  #use apply to loop through the vector of 
  plot(x,z, main="Item Characteristic Curve", tick=F, cex.axis=.90)
}
plot_icc(ltm_cat)

#NEXT GRM_cat. 
##Question, jacob said that these plots needn't take on
# only five values. Was he misunderstood abou

plot_grm<-function(cat){
  x<-seq(-5,5,.1)
  one<-sapply(x,probability, catObj=grm_cat, item=1)
  two<-diff(one)
  three<-t(two)
  x<-1:length(three[,1])
  plot(x, three[,1],"l", main="IRF Plot",ylab="Probability", xlab=  expression(theta), lwd=2, ylim=c(0,1), col=rgb(51/255, 153/255, 102/255), tick=F,cex.axis=.90)
  lines(x, three[,2], col=rgb(0/255, 102/255, 204/255),lwd=2 )
  lines(x, three[,3],col=rgb(204/255, 102/255, 153/255), lwd=2 )
  lines(x, three[,4], col= rgb(102/255, 0/255, 102/255),lwd=2 )
  lines(x, three[,5], col=rgb(204/255, 51/255, 0/255),lwd=2 )
  legend("left", legend=c("Probability Correct", "Probability Incorrect"), col=c(rgb(51/255, 153/255, 102/255), rgb(204/255, 102/255, 153/255)), lty=c(1,2), cex=0.8)
  
}


plot_grm(grm_cat)
######

y<-sapply(x,probability, catObj=ltm_cat, item=1)
exp<-sum(x*y)


#GPCM Plot

plot_gpcm<-function(cat){
  x<-seq(-5,5,.01)
  one<-sapply(x,probability, catObj=gpcm_cat, item=1)
  Probability<-t(one)
  x<-1:length(Probability[,1])

  plot(x, Probability[,1] ,"l", main="IRF Plot",tick=F, ylab="Probability", xlab=  expression(theta), lwd=2, ylim=c(0,1),xlim=c(0, max(x)), col=rgb(51/255, 153/255, 102/255), lty=1)
  lines(x, Probability[,2], col= rgb(0/255, 102/255, 204/255),lwd=2, lty=2 )
  lines(x, Probability[,3], col=rgb(204/255, 102/255, 153/255),lwd=2, lty=3)
  lines(x, Probability[,4], col=rgb(102/255, 0/255, 102/255), lwd=2, lty=4)
  lines(x, Probability[,5], col=rgb(204/255, 51/255, 0/255),lwd=2, lty=5)
  legend("topright", inset=c(-0.2,0), legend=c("Probability of Answering 1", "Probability of Answering 2", "Probability of Answering 3", "Probability of Answering 4", "Probability of Answering 5"),col=c(rgb(51/255, 153/255, 102/255),rgb(0/255, 102/255, 204/255),rgb(204/255, 102/255, 153/255),rgb(102/255, 0/255, 102/255),rgb(204/255, 51/255, 0/255))
, lty=c(1,2,3,4,5))
}

plot_gpcm(gpcm_cat)

##Now we are gonna do this ish with the IIF
#Maybe discuss this plot with Erin 
#it uses the fisher Infor

plot_iif<- function(cat, item){
  x<-seq(-5,5,.01)
  y<-sapply(x,fisherInf, catObj=ltm_cat, item=1)
  # create a vector of x values a bunch of different theta values
  #plug x value
  #use apply to loop through the vector of 
  plot(x,y, main="Item Information Function","l", col=rgb(204/255, 102/255, 153/255),lwd=2,ylab="Probability", xlab=  expression(theta))
}
plot_iif(ltm_cat)

