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

HH9_4$location[HH9_4$location==""]<-"NA"  
HH9_4$location<-as.factor(HH9_4$location)
cHH9_4<-filter(HH9_4,location!="NA")
  

HH9_5$location[HH9_5$location==""]<-"NA"  
HH9_5$location<-as.factor(HH9_5$location)
cHH9_5<-filter(HH9_5,location!="NA")


HH9_6$location[HH9_6$location==""]<-"NA"  
HH9_6$location<-as.factor(HH9_6$location)
cHH9_6<-filter(HH9_6,location!="NA")


HH9_7$location[HH9_7$location==""]<-"NA"  
HH9_7$location<-as.factor(HH9_7$location)
cHH9_7<-filter(HH9_7,location!="NA")


HH9_8$location[HH9_8$location==""]<-"NA"  
HH9_8$location<-as.factor(HH9_8$location)
cHH9_8<-filter(HH9_8,location!="NA")
