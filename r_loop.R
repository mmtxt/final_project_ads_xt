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
for(i in c(4:8,29)){
  assign(paste("HH9",i,sep="_"),
         read.csv(paste("9-",i,".csv",sep="")))
  
}
#select data that have location information and save as cHHs
for(i in c(4:8,29)){
 cHHs<-get(paste("HH9",i,sep="_"))
 cHHs$location[cHHs$location==""]<-NA  
 cHHs$location<-as.factor(cHHs$location)
 cHHs<-filter(cHHs,!is.na(location))
 write.csv(cHHs,paste("c9-",i,".csv",sep=""))
 assign(paste("cHH9",i,sep="_"),cHHs)
 
}
#raw data
for (i in c(4:8,29)){
  dim<-get(paste("cHH9",i,sep="_"))
  assign(paste("dim",i,sep=""),dim(dim))
}

dim<-c(dim4,dim5,dim6,dim7,dim8,dim29)
col<-c("observation_num","variable_num")
row<-c("9-4","9-5","9-6","9-7","9-8","9-29")
dim_max<-matrix(dim,byrow = TRUE,nrow=6)
rownames(dim_max)<-row
colnames(dim_max)<-col
dim_max<-data.frame(dim_max)
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
  
}
#read in the dataset

for(i in c(4:8,29)){
  
  temp<-read.csv(paste("citys_9_",i,".csv",sep=""))
  temp<-temp[ ,-1]
  assign(paste("citys_9",i,sep="_"),temp)
  
}

#summary statistic
#joint in a big dataset
citys<-rbind(head(citys_9_4,10),
             head(citys_9_5,10),
             head(citys_9_6,10),
             head(citys_9_7,10),
             head(citys_9_8,10),
             head(citys_9_29,10))

#top ten city based on different dates:

#graphs:
#1.bars
#1.1seperated by dates.
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

#ggmap
#big one
library(ggmap)
library(mapproj)


#fit geo information:
for (i in c(4:8,29)){
  location<-get(paste("citys_9",i,sep="_"))
  location$longi<-NA
  location$latti<-NA

  for(a in 1:nrow(location)){
    location[a, 6:7] <- geocode(as.character(location[a,2])) %>% as.numeric
  }
  
  #for those not fitted in the first loop
  for(b in 1:nrow(location)){
    if(is.na(location[b, 4])){
      location[b, 4:5] <- geocode(as.character(location[i,2])) %>% as.numeric
    }
    else{}
  }
  location<-filter(location,!is.na(longi))
  location<-filter(location,longi<=-60&longi>=-140&latti>=20&latti<=50)
  assign(paste("citys_9",i,sep="_"),location)
  write.csv(location,paste("loc_9_",i,".csv",sep=""))
} 

#read in the datasete
  for(i in c(4:8,29)){
    
    loc<-read.csv(paste("loc_9_",i,".csv",sep=""))
    loc<-loc[ ,-1]
    assign(paste("loc_9",i,sep="_"),loc)
  }
#draw the whole map one
usa_center <- as.numeric(geocode("United States"))
USAMap <- ggmap(get_googlemap(center=usa_center, zoom=4, maptype = "roadmap"), 
                extent="normal")
b<-0
for (i in c(4:8,29)){
  b<-b+1
  location<-get(paste("loc_9",i,sep="_"))
  assign(paste("g",b,sep=""),USAMap +  geom_point(aes(x=longi, y = latti), data = location,
                       alpha=0.6, col="orange",size = location$number*0.1)+
    geom_point(aes(x = longi, y = latti), data = location, 
               alpha = 0.3, col = "blue", size = 1) +
    scale_size_continuous(range = range(location$number))
  )

}
#draw the zoom one:
statemap<-get_map(location='Texas',zoom=6,maptype="toner")
statemap<-ggmap(statemap)

b<-0
for (i in 4:8){
  b<-b+1
  location<-get(paste("loc_9",i,sep="_"))
  texas<-filter(location,state=="TX"|state=="LA")
  assign(paste("gb",b,sep=""),
         statemap +  geom_point(aes(x=longi, y = latti), data = texas,
                                alpha=0.4, col="yellow",size = texas$number*0.1)+
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

multiplot(gb1, gb2, gb3, gb4,gb5, cols=2)
gb1
gb2
gb3
gb4
gb5



