# Extract tweets based on hashtag
topic_tweets <- searchTwitter("#DataScience", n = 500)

# Transform data extracted from twitter
dft <- do.call("rbind", lapply(topic_tweets, as.data.frame))

# Extract only text and user from tweet
text_topic <- dft[,c(1,11)]
text.dft <- data.frame(text_topic)

mention <- 0
user <- 0
# Extract mentions
for (i in 1:nrow(text.dft)){
	# Extract mention in tweet
	mention[i] <- str_extract_all(text.dft[i,1], "@\\S+")
	# Extract user of tweet
	user[i] <- text.dft[i,2]
}

# Remove if there isn't a mention in the tweet
user <- user[lapply(mention,length)>0]
mention <- mention[lapply(mention,length)>0]

# Add for network purposes
list <- paste(mention, user)

# Remove punctuation
list <- gsub("[[:punct:]]", "", list) 