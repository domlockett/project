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

ques<- cbind(ques, rownames(ques))

colnames(ques)<- c("question", "number")

ques$question<-paste0(ques[,1], ".")
ques$question<-paste0(" ", ques[,1])
ques[3,1]<-" Don't mind being the center of attention."
type[45,1]<- " Avoid contact with others."

dividers<-inner_join(type, ques )





