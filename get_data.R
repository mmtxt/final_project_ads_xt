#getting the data: install twitterR since install.pacakge("twitterR")is not avialble
#let me try another one
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
