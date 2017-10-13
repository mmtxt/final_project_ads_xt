#useful package and command
HH9_4 %>%
  filter(isRetweet==FALSE)%>%
  rowwise()  %>%
  mutate(houston = grepl("Houston|HTX", text, ignore.case=TRUE) %>%
           filter(houston==TRUE)
#
ggplot
gmcall
libbon
ggmap

no_state$city %in% city_ystate$city
left_join()

readin

location <- data.frame(loc = location[!is.na(location)], stringsAsFactors =  F) %>%
  group_by(loc) %>%
  tally() %>%
  arrange(desc(n)) %>%
  as.data.frame() %>%
  mutate(longi = NA, latti = NA)

for(i in 1:4){
  rm(get(paste("city_c","i",sep="_")))
}
get(paste("city_c","1",sep="_"))---
  
  
for(i in 4:8){
  get(paste("HH9","i",sep="_"))$location[get(paste("HH9","i",sep="_"))$location==""]<-NA
  get(paste("HH9","i",sep="_"))$location<-as.factor(get(paste("HH9","i",sep="_"))$location)
  get(paste("cHH9","i",sep="_"))<-filter(get(paste("HH9","i",sep="_")),is.na(location))
}  
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

if (!require("devtools")) install.packages("devtools")
devtools::install_github("rstudio/rmarkdown")



#git commit under shell
git add --all && git commit -m "comment"