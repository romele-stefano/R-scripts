#################
#   LIBRARIES   #
#################

# Twitter API
library(twitteR)
library(plyr)
# For wordcloud
library(wordcloud)
# For transformation functions
library(tm)
# For working with strings
library(stringr)
# For plotting
library(ggplot2)



##############
#   SET WD   #
##############

setwd("C:/Users/ste/Documents/R/data/twitter-sentiment")



#####################
#   SETUP TWITTER   #
#####################


key = "Insert your Consumer Key"
secret = "Insert your Consumer Secret"
setup_twitter_oauth(key, secret)



