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

HH9_29$location[HH9_29$location=="NA"]<-NA  
HH9_29$location<-as.factor(HH9_29$location)
cHH9_29<-filter(HH9_29,!is.na(location))

#visualize data:

dim1<-dim(cHH9_4)
dim2<-dim(cHH9_5)
dim3<-dim(cHH9_6)
dim4<-dim(cHH9_7)
dim5<-dim(cHH9_8)
dim<-c(dim1,dim2,dim3,dim4,dim5)
col<-c("observation_num","variable_num")
row<-c("cHH9_4","cHH9_5","cHH9_6","cHH9_7","cHH9_8")
dim_max<-matrix(dim,byrow = TRUE,nrow=5)
rownames(dim_max)<-row
colnames(dim_max)<-col
dim_max<-data.frame(dim_max)
write.csv(dim_max,"dim.csv")

dim<-read.csv("dim.csv")
colnames(dim)[1]<-"dates"
dim


