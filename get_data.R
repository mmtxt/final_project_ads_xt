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

