#exlpore location information from the text
library(stringr)
library(rebus)
location1<-str_subset(HH9_4$text,pattern = "at" %R% ANY_CHAR)

#for location, seperate state and city
library(tidyr)
library(dplyr)
#delete location information that is useless,seperate the dataset based on states.

cHH9_4<-separate(cHH9_4,location,c("city","state"),sep=",")

no_state<-filter(cHH9_4,str_count(cHH9_4$state,pattern="USA")==1
                 |is.na(state))
y_state<-filter(cHH9_4,str_count(cHH9_4$state,pattern="USA")==0
                 &!is.na(state))

state_num<-summary(y_state$state)
head(state_num)
y_state_t<-y_state
