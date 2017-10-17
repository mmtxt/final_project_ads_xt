#loop one

library(dplyr)
library(stringr)
library(rebus)
library(tidyr)
library(ggplot2)
library(ggmap)
library(mapproj)
library(twitteR)

#read in database

library(curl)


download.file("https://www.dropbox.com/s/26fciujm2iw3umu/9-4.csv?dl=1","9-4.csv")
download.file("https://www.dropbox.com/s/h58yrmd1cy0mwgb/9-5.csv?dl=1","9-5.csv")
download.file("https://www.dropbox.com/s/vzselkdqr597ykk/9-6.csv?dl=1","9-6.csv")
download.file("https://www.dropbox.com/s/l4i5ufbxemu241b/9-7.csv?dl=1","9-7.csv")
download.file("https://www.dropbox.com/s/fb2hv24etxytxgl/9-8.csv?dl=1","9-8.csv")
download.file("https://www.dropbox.com/s/xqb3bqnmv87olh0/9-29.csv?dl=1","9-9.csv")


for(i in c(4:8,29)){
  assign(paste("HH9",i,sep="_"),
         read.csv(paste("9-",i,".csv",sep=""))[,-1])
  
}
#select data that have location information and save as cHHs
for(i in c(4:8,29)){
 cHHs<-get(paste("HH9",i,sep="_"))
 cHHs$location[cHHs$location==""]<-NA  
 cHHs$location<-as.factor(cHHs$location)
 cHHs<-filter(cHHs,!is.na(location))
 write.csv(cHHs,paste("c9-",i,".csv",sep=""))
 assign(paste("cHH9",i,sep="_"),cHHs)
 rm(cHHs)
}

#raw data
for (i in c(4:8,29)){
  dim<-get(paste("cHH9",i,sep="_"))
  assign(paste("dim",i,sep=""),dim(dim))
}
#first step data visualing:
dim<-c(dim4,dim5,dim6,dim7,dim8,dim29)
col<-c("observation_num","variable_num")
row<-c("9-4","9-5","9-6","9-7","9-8","9-29")
dim_max<-matrix(dim,byrow = TRUE,nrow=6)
rownames(dim_max)<-row
colnames(dim_max)<-col
(dim_max<-data.frame(dim_max))
write.csv(dim_max,"dim.csv")

#seperate the location information into state and city,export those data with state information
for(i in c(4:8,29)){
  temp<-get(paste("cHH9",i,sep="_"))
  temp<-separate(temp,location,c("city","state"),sep=",")
  temp$state<-toupper(temp$state)
  temp$city<-toupper(temp$city)
  temp$state<-as.factor(str_replace_all(temp$state,pattern=" ",replacement=""))
  temp$city<-as.factor(str_replace_all(temp$city,pattern=" ",replacement=""))
  assign(paste("cHH9",i,sep="_"),temp)
}

#extract number of each city and extract the dataset(STATE INCLUDE TX,LA. MA AND NV as reference)

for(i in c(4:8,29)){
  
  temp<-get(paste("cHH9",i,sep="_"))
  y_state<-filter(temp,state!="USA"
                  &!is.na(state))
  y_state_1<-filter(y_state,state=="TX"|state=="TEXAS")
  y_state_2<-filter(y_state,state=="LA"|state=="LOUISIANA")
  y_state_3<-filter(y_state,state=="MA"|state=="MASSACHUSETTS")
  y_state_4<-filter(y_state,state=="NV"|state=="NEVADA")
                  
  for(a in 1:4){
    assign(paste("city_c",a,sep="_"),
           summary(get(paste("y_state",a,sep="_"))$city,
                   maxsum = 1000))
    assign(paste("name_city",a,sep="_"),
           names(get(paste("city_c",a,sep="_"))))
    assign(paste("city_ystate",a,sep="_"),
           data.frame(get(paste("city_c",a,sep="_"))))
    temp2<-get(paste("city_ystate",a,sep="_"))
    temp2$city<-as.factor(get(paste("name_city",a,sep="_")))
    
    #add colnames
    colnames(temp2)<-c("number","city")
    assign(paste("city_ystate",a,sep="_"),
           temp2)
  }
  city_ystate_1$state<-as.factor("TX")
  city_ystate_2$state<-as.factor("LA")
  city_ystate_3$state<-as.factor("MA")
  city_ystate_4$state<-as.factor("NV")
  city_ystate<-rbind(city_ystate_1,city_ystate_2,city_ystate_3,city_ystate_4)
  
#clear nonsense data$rename
  city_ystate<-filter(city_ystate,number>0)  
#calculate frequency
  city_ystate$freq<-city_ystate[ ,"number"]/sum(city_ystate[ ,"number"])
#add date value
  city_ystate$date<-paste("9",i,sep="-")
#reorder
  city_ystate<-city_ystate[order(-city_ystate$number), ]
#save the data
  write.csv(city_ystate,paste("citys_9_",i,".csv",sep=""))
  rm(city_ystate)
  rm(city_ystate_1)
  rm(city_ystate_2)
  rm(city_ystate_3)
  rm(city_ystate_4)
  rm(temp)
  rm(temp2)
  rm(y_state)
  rm(y_state_1)
  rm(y_state_2)
  rm(y_state_3)
  rm(y_state_4)
}
#read in the dataset

for(i in c(4:8,29)){
  
  temp<-read.csv(paste("citys_9_",i,".csv",sep=""))
  temp<-temp[ ,-1]
  assign(paste("citys_9",i,sep="_"),temp)
  
}
#statiscal inference:
#raw data adjusting:
#extract first 40 cities and adjust them with 9_29:

for (i in c(4:8,29)){
  
  head_city<-head(get(paste("citys_9",i,sep="_")),40)
  head_city$freq<-head_city[ ,"number"]/sum(head_city[ ,"number"])
  assign(paste("head",i,sep=""),head_city)
}
for (i in 4:8){
  
  head<-get(paste("head",i,sep=""))
  head_re<-merge(x=head,y=head29,by="city",all.x=TRUE)
  head_cl<-head_re %>%
    select(city,freq.x,freq.y,state.x) %>%
    arrange(desc(freq.x))
  head_cl[is.na(head_cl)]<-0
  head_cl<-head_cl %>%
    mutate(freq_ad=freq.x-freq.y) %>%
    filter(freq_ad>0) %>%
    arrange(desc(freq_ad))
  write.csv(head_cl,paste("adcity_9_",i,".csv",sep=""))
}


#raw data without adjusting
#make sure the city list from the smallest to highest
a<-0
for (i in c(4:8,29)){
  a<-a+1
  city_ystate<-get(paste("citys_9",i,sep="_"))
  city_ystate$city<-factor(city_ystate$city, levels=unique(city_ystate$city))
  assign(paste("p",a,sep=""),ggplot(head(city_ystate,10),aes(x=city,y=freq,fill=state))+
         geom_bar(stat = "identity")+
         labs(title=paste("Sep/",i,"th",sep=""))+
           ylab("freqency")+
          theme(axis.title.x = element_text(size = 10),
                axis.text.y=element_text(size = 5))+
           coord_flip())
}

p1
multiplot(p1, p2, p3, p4,p5,p6, cols=2)

#data with the adjusting

#ggmap
#big one
library(ggmap)
library(mapproj)

#read in the dataset:
for (i in 4:8){
  assign(paste("adcity_9_",i,sep=""),read.csv(paste("adcity_9_",i,".csv",sep=""))[,-1])
}

#fit geo information:
for (i in 4:8){
  location<-get(paste("adcity_9",i,sep="_"))
  
  location$longi<-NA
  location$latti<-NA

  for(a in 1:nrow(location)){
    location[a, 6:7] <- geocode(as.character(location[a,"city"])) %>% as.numeric
  }
  
  #for those not fitted in the first loop
  for(b in 1:nrow(location)){
    if(is.na(location[b, 7])){
      location[b, 6:7] <- geocode(as.character(location[b,"city"])) %>% as.numeric
    }
    else{}
  }
  location<-filter(location,!is.na(longi))
  location<-filter(location,longi<=-60&longi>=-140&latti>=20&latti<=50)
  assign(paste("adcity_9",i,sep="_"),location)
  write.csv(location,paste("loc_9_",i,".csv",sep=""))
} 

#read in the datasete
  for(i in 4:8) {
    
    loc<-read.csv(paste("loc_9_",i,".csv",sep=""))
    loc<-loc[ ,-1]
    assign(paste("loc_9",i,sep="_"),loc)
  }

#draw the zoom one:
statemap<-get_map(location='Texas',zoom=6,maptype="toner")
statemap<-ggmap(statemap)

b<-0
for (i in 4:8){
  b<-b+1
  location<-get(paste("loc_9",i,sep="_"))
  texas<-filter(location,state.x=="TX"|state.x=="LA")
  assign(paste("gb",b,sep=""),
         statemap +  geom_point(aes(x=longi, y = latti), data = texas,
                                alpha=0.2, col="yellow",size = texas$freq_ad*200)+
                     geom_point(aes(x = longi, y = latti), data = texas, 
                                alpha = 0.6, col = "red", size = 0.5) +
                     theme(axis.title.x = element_text(size = 5),
                           axis.text.x=element_text(size = 5),
                           axis.title.y = element_text(size = 5),
                           axis.text.y=element_text(size = 5),
                           text = element_text(size = 10),
                           plot.title=element_text(size=4))+
                     labs(title=paste("Sep/",i,"th",sep=""))+
                     scale_size_continuous(range = range(texas$number)))
                     
}
plot_grid(gb1, gb2, gb3, gb4,gb5, cols=2)
ggsave("map_move.png",limitsize = FALSE)


#the data before adjustment:
#read in the dataset
for(i in 4:8){
  assign(paste("adcity_9_",i,sep=""),read.csv(paste("adcity_9_",i,".csv",sep=""))[ ,-1])
}


a<-0
for (i in c(4:8,29)){
  a<-a+1
  city_ystate<-get(paste("citys_9",i,sep="_"))
  city_ystate$city<-factor(city_ystate$city, levels=unique(city_ystate$city))
  assign(paste("p",a,sep=""),ggplot(head(city_ystate,20),aes(x=city,y=freq,fill=state))+
           geom_bar(stat = "identity")+
           labs(title=paste("Sep/",i,"th",sep=""))+
           ylab("freqency")+
           theme(axis.title.x = element_text(size = 10),
                 axis.text.y=element_text(size = 5))+
           coord_flip())
}
p1

b<-0
for (i in c(4:8)){
  b<-b+1
  city_ystate<-get(paste("adcity_9",i,sep="_"))
  city_ystate$city<-factor(city_ystate$city, levels=unique(city_ystate$city))
  assign(paste("e",b,sep=""),ggplot(head(city_ystate,10),aes(x=city,y=freq_ad,fill=state.x))+
           geom_bar(stat = "identity")+
           labs(title=paste("September ",i,"th",sep=""))+
           ylab("freqency")+
           theme(axis.title.x = element_text(size = 5),
                 axis.title.y=element_text(size=5),
                 axis.text.y=element_text(size = 4),
                 axis.text.x=element_text(size=4),
                 plot.title=element_text(size=5),
                 legend.key.size=unit(.045,"inches"),
                 legend.key.height=unit(.05,"inches"),
                 legend.key.width = unit(.05,"inches"),
                 legend.title=element_text(size=4),
                 legend.text=element_text(size=4))+
           coord_flip())
}
e1
plot_grid(e1,e2,e3,e4,e5, ncol = 2)
ggsave("adjust_plot.png")


#exclude Huston and Austin.
library(cowplot)
for (i in c(4:8)){
  city_ex<-get(paste("adcity_9_",i,sep=""))
  city_ex<-city_ex[-c(1:2), ]
  assign(paste("cityex9_",i,sep=""),city_ex)
}
#draw the map again
statemap<-get_map(location='Texas',zoom=6,maptype="toner")
statemap<-ggmap(statemap)

b<-0
for (i in 4:8){
  b<-b+1
  location<-get(paste("cityex9_",i,sep=""))
  texas<-filter(location,state.x=="TX"|state.x=="LA")
  assign(paste("gb",b,sep=""),
         statemap +  geom_point(aes(x=longi, y = latti), data = texas,
                                alpha=0.4, col="yellow",size = texas$freq_ad*1000)+
           geom_point(aes(x = longi, y = latti), data = texas, 
                      alpha = 0.6, col = "red", size = 0.5) +
           theme(axis.title.x = element_text(size = 5),
                 axis.text.x=element_text(size = 5),
                 axis.title.y = element_text(size = 5),
                 axis.text.y=element_text(size = 5),
                 text = element_text(size = 10))+
           labs(title=paste("Sep/",i,"th",sep=""))+
           scale_size_continuous(range = range(texas$number)))
  
}
gb1
plot_grid(gb1, gb2, gb3, gb4,gb5, cols=2)
ggsave("map_move")






#ten high cities based on different dates:
#raw data before adjusting
for (i in 4:8){
  rawrow<-get(paste("citys_9_",i,sep=""))
  rawrow<-rawrow$city[c(1:20)]
  assign(paste("rawrow",i,sep=""),as.character(rawrow))
}

city<-c(rawrow4,rawrow5,rawrow6,rawrow7,rawrow8)
col<-c("citys","date")
row<-c("9-4","9-5","9-6","9-7","9-8")
city_max<-matrix(city,byrow = FALSE,ncol=5)
colnames(city_max)<-row
write.csv(city_max,"rawcity.csv")
raw_city_list<-read.csv("rawcity.csv")

#raw data after adjusting
for (i in 4:8){
  adrow<-get(paste("adcity_9_",i,sep=""))
  adrow<-adrow$city[c(1:20)]
  assign(paste("adrow",i,sep=""),as.character(adrow))
}

#first step data visualing:
city<-c(adrow4,adrow5,adrow6,adrow7,adrow8)
col<-c("citys","date")
row<-c("9-4","9-5","9-6","9-7","9-8")
city_max<-matrix(city,byrow = FALSE,ncol=5)
colnames(city_max)<-row
write.csv(city_max,"adcity.csv")
adjust_city_list<-read.csv("adcity.csv")



