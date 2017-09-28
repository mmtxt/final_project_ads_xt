#exlpore location information from the text
library(stringr)
library(rebus)
location1<-str_subset(HH9_4$text,pattern = "at" %R% ANY_CHAR)

#for location, seperate state and city
library(tidyr)
library(dplyr)
#delete location information that is useless,seperate the dataset based on states.
cHH9_4<-separate(cHH9_4,location,c("city","state"),sep=",")

cHH9_4$state<-toupper(cHH9_4$state)
cHH9_4$city<-toupper(cHH9_4$city)

cHH9_4$state<-as.factor(str_replace_all(cHH9_4$state,pattern=" ",replacement=""))
cHH9_4$city<-as.factor(str_replace_all(cHH9_4$city,pattern=" ",replacement=""))

cHH9_4$city<-as.factor(str_replace_all(cHH9_4$city,pattern=" ",replacement=""))

no_state<-filter(cHH9_4,state=="USA"
                 |is.na(state))
y_state<-filter(cHH9_4,state!="USA"
                 &!is.na(state))

#remain those have Tx

y_state_t<-y_state
state_num<-summary(y_state$state)
head(state_num)
y_state_t<-filter(y_state_t,state=="TX"|state=="TEXAS")
city_c<-summary(y_state_t$city)

#plot the location information












