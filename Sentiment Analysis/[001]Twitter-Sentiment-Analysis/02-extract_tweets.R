######################
#   EXTRACT TWEETS   #
######################

# Add since='yyyy-mm-dd' and until="yyyy-mm-dd" to limit search
# if wanted are result of a specific string, use searchTwitter
trump_tweets <- userTimeline("realDonaldTrump", n = 500)
clinton_tweets <- userTimeline("HillaryClinton", n = 500)

# make data frame of tweets
dft <- do.call("rbind", lapply(trump_tweets, as.data.frame))
dfc <- do.call("rbind", lapply(clinton_tweets, as.data.frame))

# extract only text
text_trump <- dft[,1]
text_clinton <- dfc[,1]


# Create a dataframe of tweet text
text.dft <- data.frame(text_trump)
text.dfc <- data.frame(text_clinton)

# Remove emojii etc.
text_trump <- sapply(dft$text,function(row) iconv(row, "latin1", "ASCII", sub=""))
text_clinton <- sapply(dfc$text,function(row) iconv(row, "latin1", "ASCII", sub=""))

