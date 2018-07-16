##New project

# Get data from YouGov and Qualtrics
  # Fit these responses to grm objects
# Using *example.R* compare how the adaptive batt compares to fixed
# Walk simulated and real respondents through methods
## Start with big 5 on YouGov
    #randomly sample 0f 50,000 yougov and fb data
    #100 questions about 20 questions per trati, 5 traits.
      #that means five different cat objs


#Parse out which questions belong to which trait





###Jacob no fixed so just plot the adaptive
# compare to simulate 


library(ggplot2)
library(catSurv)
library(scales)
library(dplyr)
library(plyr)
library(latex2exp)
library(purrr)
library(haven)
library(dplyr)
library(foreign)
library(catSurv)
library(readxl)
library(fuzzyjoin)
data<-read_sav("C:/Users/dl0ck/OneDrive/Documents/Summer 2018/Montgomery/catSurv_dl/big 5/raw_data/yougov.R" )
type<-read_xlsx("C:/Users/dl0ck/OneDrive/Documents/Summer 2018/Montgomery/catSurv_dl/big 5/fb_yg/big5_type.xlsx" )
type<-as.data.frame(type)
yougov<- data %>%
  select(BIGFIVE_1:BIGFIVE_100)
type<- type %>%
  mutate(question=as.factor(type[,1]))


ques<-yougov  %>% map_chr(~attributes(.)$label)

ques<-as.data.frame(ques)

ques<-apply(ques, 1,function(i){gsub("Describe self:", "",i)})

ques<-as.data.frame(ques)

ques<- cbind(ques, rownames(ques))

colnames(ques)<- c("question", "number")

ques$question<-paste0(ques[,1], ".")
type$question<-paste0(" ", type[,1])
ques[3,1]<-" Don't mind being the center of attention."
type[45,1]<- " Avoid contact with others."

dividers<-inner_join(type, ques )
agree_ques<-c(96,72,2,62,52,42,92,32,22,9,82,13,76,56,46,36,26,66,6,86) 
consci_ques<-c(65,8,68,48,58,38,20,78,98,88,28,35,45,25,5,55,95,75,85,15)
extra_ques<-c(49,69,79,99,18,29,59,89,14,39,23,3,43,33,93,53,63,73,10,83)
neuro_ques<-c(60,57,87,47,77,27,97,19,67,11,37,40,17,100,90,50,70,80,30,12)
open_ques<-c(64,54,84,94,4,44,34,7,74,24,71,81,16,21,31,61,41,51,1,91)


#agreeableness:
agree<-yougov[,agree_ques]
respprof<-agree
#conscienciousnes:
consci<-yougov[,consci_ques]
#extraversion:
extra<-yougov[,extra_ques]
#neuroticism:
neuro<-yougov[,neuro_ques]
#openness:
open<-yougov[,open_ques]



#######
grm_cat@lengthThreshold<-3


grm_MAP<-grm_cat
grm_MAP@estimation<-"MAP"
grm_EAP<-grm_cat
grm_EAP@estimation<-"EAP"
grmList<- list(grm_MAP, grm_EAP )
