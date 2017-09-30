#Getting data:

install.packages(c("devtools", "rjson", "bit64", "httr"))
#follow the instruction and restart R
library(devtools)
install_github("geoffjentry/twitteR")

library(twitteR)
setup_twitter_oauth("gLvw86UAzsVGG4lNoBTCttQjg", 
                    "gbBf9OW1PMz2KXMZx9Wo3neKgXNSpvG3abgt3A3TxcBWxefmjm",
                    "770639700839333888-foZXS5jtNXc7bWdpNnUR18Gb5fPQSp9",
                    "n8lL1J7HlAhupOGdOVJFyLcNObxWtyxkffTGAnytyqjY1")
#selection:
1

#Get started!:
sample_1000<- searchTwitter('#HurricaneHarvey since:2017-8-25 until:2017-9-8', n=1000)
head(sample_1000)
sample_10000<-searchTwitter('#HurricaneHarvey until:2017-9-8', n=10000)
HH1<-twListToDF(sample_10000)

#Load packages
library(twitteR)
#data at 9-4
sample_9_4<-searchTwitter('#HurricaneHarvey until:2017-9-5', n=10000)
#data at 9-5
sample_9_5<-searchTwitter('#HurricaneHarvey until:2017-9-6', n=10000)
#data at 9-6
sample_9_6<-searchTwitter('#HurricaneHarvey until:2017-9-7', n=10000)
#data at 9-7
sample_9_7<-searchTwitter('#HurricaneHarvey until:2017-9-8', n=10000)
#data at 9-8
sample_9_8<-searchTwitter('#HurricaneHarvey until:2017-9-9', n=10000)
HH9_4<-twListToDF(sample_9_4)
HH9_5<-twListToDF(sample_9_5)
HH9_6<-twListToDF(sample_9_6)
HH9_7<-twListToDF(sample_9_7)
HH9_8<-twListToDF(sample_9_8)

#collect correction data(using a as a key word to search)
HH9_29<-searchTwitter('a until:2017-9-30 ',n=10000)
HH9_29<-twListToDF(HH9_29)




#cleaning the data:
#load tidytext and dplyr
library(tidytext)
library(dplyr)

#delete the data without location information
rm(sample_9_4)
rm(sample_9_5)
rm(sample_9_6)
rm(sample_9_7)
rm(sample_9_8)

#select data that have location information

HH9_4$location[HH9_4$location==""]<-NA  
HH9_4$location<-as.factor(HH9_4$location)
cHH9_4<-filter(HH9_4,is.na(location))


HH9_5$location[HH9_5$location==""]<-NA  
HH9_5$location<-as.factor(HH9_5$location)
cHH9_5<-filter(HH9_5,is.na(location))


HH9_6$location[HH9_6$location==""]<-NA  
HH9_6$location<-as.factor(HH9_6$location)
cHH9_6<-filter(HH9_6,is.na(location))


HH9_7$location[HH9_7$location==""]<-NA  
HH9_7$location<-as.factor(HH9_7$location)
cHH9_7<-filter(HH9_7,is.na(location))


HH9_8$location[HH9_8$location==""]<-NA  
HH9_8$location<-as.factor(HH9_8$location)
cHH9_8<-filter(HH9_8,is.na(location))

HH9_29$location[HH9_29$location==""]<-NA  
HH9_29$location<-as.factor(HH9_29$location)
cHH9_29<-filter(HH9_29,!is.na(location))

#export data:
write.csv(HH9_4,"9-4.csv")
write.csv(HH9_5,"9-5.csv")
write.csv(HH9_6,"9-6.csv")
write.csv(HH9_7,"9-7.csv")
write.csv(HH9_8,"9-8.csv")
write.csv(HH9_29,"9-29.csv")
#visualize data:

dim1<-dim(cHH9_4)
dim2<-dim(cHH9_5)
dim3<-dim(cHH9_6)
dim4<-dim(cHH9_7)
dim5<-dim(cHH9_8)
dim6<-dim(cHH9_29)
dim<-c(dim1,dim2,dim3,dim4,dim5,dim6)
col<-c("observation_num","variable_num")
row<-c("9-4","9-5","9-6","9-7","9-8","9-29")
dim_max<-matrix(dim,byrow = TRUE,nrow=6)
rownames(dim_max)<-row
colnames(dim_max)<-col
dim_max<-data.frame(dim_max)
write.csv(dim_max,"dim.csv")

dim<-read.csv("dim.csv")
colnames(dim)[1]<-"dates"
dim

#seperate the state and city

#exlpore location information from the text
library(stringr)
library(rebus)
library(tidyr)
library(dplyr)
library(ggplot2)
#delete location information that is useless,seperate the dataset based on states.
cHH9_5<-separate(cHH9_5,location,c("city","state"),sep=",")

cHH9_5<-filter(cHH9_5,language=="en")
cHH9_5$state<-toupper(cHH9_5$state)
cHH9_5$city<-toupper(cHH9_5$city)

cHH9_5$state<-as.factor(str_replace_all(cHH9_5$state,pattern=" ",replacement=""))
cHH9_5$city<-as.factor(str_replace_all(cHH9_5$city,pattern=" ",replacement=""))

cHH9_5$city<-as.factor(str_replace_all(cHH9_5$city,pattern=" ",replacement=""))

no_state<-filter(cHH9_4,state=="USA"
                 |is.na(state))
y_state<-filter(cHH9_4,state!="USA"
                &!is.na(state))

#remain those have Tx and LA,MA AND NV for reference.


state_num<-summary(y_state$state,maxsum = 500)

y_state_1<-filter(y_state,state=="TX"|state=="TEXAS")
y_state_2<-filter(y_state,state=="LA"|state=="LOUISIANA")
y_state_3<-filter(y_state,state=="MA"|state=="MASSACHUSETTS")
y_state_4<-filter(y_state,state=="NV"|state=="NEVADA")
                  
city_c_1<-summary(y_state_1$city,maxsum = 500)
city_c_2<-summary(y_state_2$city,maxsum = 500)
city_c_3<-summary(y_state_3$city,maxsum = 500)
city_c_4<-summary(y_state_4$city,maxsum = 500)
name_city_1<-names(city_c_1)
name_city_2<-names(city_c_2)
name_city_3<-names(city_c_3)
name_city_4<-names(city_c_4)
#change summary of state and city into data frame


city_ystate_1<-data.frame(city_c_1)
city_ystate_2<-data.frame(city_c_2)
city_ystate_3<-data.frame(city_c_3)
city_ystate_4<-data.frame(city_c_4)
city_ystate_1$city<-as.factor(name_city_1)
city_ystate_2$city<-as.factor(name_city_2)
city_ystate_3$city<-as.factor(name_city_3)
city_ystate_4$city<-as.factor(name_city_4)

city_ystate_1$state<-as.factor("TX")
city_ystate_2$state<-as.factor("LA")
city_ystate_3$state<-as.factor("MA")
city_ystate_4$state<-as.factor("NV")

vector<-c("number","city","state")

colnames(city_ystate_1)<-vector
colnames(city_ystate_2)<-vector
colnames(city_ystate_3)<-vector
colnames(city_ystate_4)<-vector

city_ystate<-rbind(city_ystate_1,city_ystate_2,city_ystate_3,city_ystate_4)
#clear nonsense data$rename
city_ystate<-filter(city_ystate,number>0)
#reorder
city_ystate<-city_ystate[order(-city_ystate$number), ]

write.csv(city_ystate,"citys.csv")

#plot the location information

#make sure the city list from the smallest to highest
city_ystate$city <- factor(city_ystate$city, levels=unique(city_ystate$city))
ggplot(head(city_ystate,20),aes(x=city,y=number,fill=state))+
  geom_bar(stat = "identity")+
  coord_flip()+
  labs(title="city count")

#geo
library(ggmap)
library(mapproj)

location<-city_ystate
location$longi<-NA
location$latti<-NA

for(i in 1:nrow(location)){
    location[i, 4:5] <- geocode(as.character(location[i,2])) %>% as.numeric
}

#for those not fitted in the first loop

for(i in 1:nrow(location)){
  if(is.na(location[i, 4])){
  location[i, 4:5] <- geocode(as.character(location[i,2])) %>% as.numeric
  }
  else{}
}
location<-filter(location,!is.na(longi))
location<-filter(location,longi<=-60&longi>=-140&latti>=20&latti<=50)
write.csv(location,"location.csv")
#compare with the other state:

usa_center <- as.numeric(geocode("United States"))
USAMap <- ggmap(get_googlemap(center=usa_center, zoom=4, maptype = "roadmap"), 
                extent="normal")

USAMap +  geom_point(aes(x=longi, y = latti), data = location,
             alpha=0.6, col="orange",size = location$number*0.1)+
  geom_point(aes(x = longi, y = latti), data = location, 
             alpha = 0.3, col = "blue", size = 1) +
  scale_size_continuous(range = range(location$number))

#compare within their own state


statemap<-get_map(location='Texas',zoom=6,maptype="toner")
statemap<-ggmap(statemap)

texas<-filter(location,state=="TX"|state=="LA")
statemap +  geom_point(aes(x=longi, y = latti), data = texas,
                     alpha=0.2, col="yellow",size = texas$number*0.7)+
  geom_point(aes(x = longi, y = latti), data = texas, 
             alpha = 0.6, col = "red", size = 2) +
  scale_size_continuous(range = range(texas$number))


