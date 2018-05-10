treeList<-function(catObj){
  catlist<-list() ## catlist is the output=tree list
  calcat<-catObj  ## catObj is the original cat-object; calcat is its manipulation
  currentnode<-function(nextq){
    sapply(1:(length(catObj))
  }
}

treeList <- function(catObj){
  catlist<-list() ## catlist is the output=tree list
  calcat<-catObj  ## catObj is the original cat-object; calcat is its manipulation
  for (i in length(catObj@difficulty)){ ## The maximum possible length of tree=#questions
    # Consider 'i' to be a dimension
    nextq<-selectItem(calcat)$next_item ## nextq is the question to be asked
  for (j in length(catObj@difficulty[[nextq]])){  ## For possible answers
  calcat<-storeAnswer(catObj=catObj,item=nextq,answer=j)
  checkStopRules(calcat)
}


 ## For each question
  
  for (j in length(catObj@difficulty[[i]])) ## For each answer
  {storeAnswer(catObj=catObj,item=i,answer=j)
    
  
  }
    
}
}

