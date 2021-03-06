#exlpore location information from the text
library(stringr)
library(rebus)

#for location, seperate state and city
library(tidyr)
library(dplyr)
library(ggplot2)
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


state_num<-summary(y_state$state,maxsum = 500)

y_state<-filter(y_state,state=="TX"|state=="TEXAS")
city_c<-summary(y_state$city,maxsum = 500)
name_city<-names(city_c)

#change summary of state and city into data frame

city_ystate<-data.frame(city_c)
city_ystate$city<-as.factor(name_city)

#clear nonsense data$rename
city_ystate<-filter(city_ystate,city_c>0)
vector<-c("number","city")
citylist<-city_ystate$city

colnames(city_ystate)<-vector
rownames(city_ystate)<-citylist

#for those without state
#use citylist to find same city and add to count

city_nstate<-no_state[ ,"city"]
city_nstate<-
  
no_state$city %in% city_ystate$city
#bind city together:
city_total<-city_ystate+city_nstate



#plot the location information

#make sure the city list from the smallest to highest
city_ystate$city <- factor(city_ystate$city, levels=unique(city_ystate$city))
ggplot(city_ystate,aes(x=city,y=number))+
  geom_bar(stat = "identity")+
  coord_flip()+
  labs(title="city count")
  
for(i in 4:8){
  locat[i]<-i
  print(location[i])
  }

assign(paste("i","n",sep=""),separate(get(paste(cHH9_5,location,c("city","state"),sep=","))
