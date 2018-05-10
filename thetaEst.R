theta_est<- function(catObj, answ=c()){
  while (checkStopRules==F) {
  q1<-selectItem(catObj)$next_item
  cat<-storeAnswer(q=q1, answ[q1])
  }
  return(cat)
}

student<-seq(1,40,2)
theta_est(ltm_cat, student)
  

cat<-storeAnswer(grm_cat@answers@q1)
ltm_cat@answers[10]
ltm_cat@discrimination[1]
estimateTheta(ltm_cat)

answers<-sample(0:1, 40, replace=T)
apply