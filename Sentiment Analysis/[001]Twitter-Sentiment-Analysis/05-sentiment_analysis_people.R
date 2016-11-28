# New York City vs San Francisco
nyc <- searchTwitter("Trump", n=3200, geocode='40.729932,-73.936190,30km')
sf <- searchTwitter("Trump", n=3200, geocode='37.773972,-122.431297,9km')

# make data frame of tweets
nycdf <- do.call("rbind", lapply(nyc, as.data.frame))
sfdf <- do.call("rbind", lapply(sf, as.data.frame))


# extract only text
text_nyc <- nycdf[,1]
text_sf <- sfdf[,1]

# Create a dataframe of tweet text
text.nycdf <- data.frame(text_nyc)
text.sfdf <- data.frame(text_sf)


# Remove emojii etc.
text_nyc <- sapply(nycdf$text,function(row) iconv(row, "latin1", "ASCII", sub=""))
text_sf <- sapply(sfdf$text,function(row) iconv(row, "latin1", "ASCII", sub=""))


#################
#   WORDCLOUD   #
#################

### NEW YORK CITY ###

# Eliminate character
text_nyc <- gsub("&amp;", "", text_nyc)
text_nyc <- gsub("http.*", "", text_nyc)
text_nyc <- gsub("@.*", "", text_nyc)

# create wordcloud
corpus=Corpus(VectorSource(text_nyc))

# Convert to lower-case
corpus=tm_map(corpus,tolower)

# Remove stopwords
corpus=tm_map(corpus,function(x) removeWords(x,stopwords()))

# convert corpus to a Plain Text Document
corpus=tm_map(corpus,PlainTextDocument)

# display.brewer.all()  to see possible colors!

col=brewer.pal(8,"Paired")
wordcloud(corpus, min.freq=3, scale=c(5,1),rot.per = 0.25,
          random.color=T, max.word=250, random.order=F,colors=col)


### SAN FRANCISCO ###

# Eliminate character
text_sf <- gsub("&amp;", "", text_sf)
text_sf <- gsub("http.*", "", text_sf)
text_sf <- gsub("@.*", "", text_sf)

# create wordcloud
corpus=Corpus(VectorSource(text_sf))

# Convert to lower-case
corpus=tm_map(corpus,tolower)

# Remove stopwords
corpus=tm_map(corpus,function(x) removeWords(x,stopwords()))

# convert corpus to a Plain Text Document
corpus=tm_map(corpus,PlainTextDocument)

# display.brewer.all()  to see possible colors!

col=brewer.pal(8,"Paired")
wordcloud(corpus, min.freq=3, scale=c(5,.8),rot.per = 0.25,
          random.color=T, max.word=250, random.order=F,colors=col)


##########################
#   SENTIMENT ANALYSIS   #
##########################

### NEW YORK ###

### Find positive/negative words used ###

positives= readLines("wordbanks/positive-words.txt")
negatives = readLines("wordbanks/negative-words.txt")

# Create a list of words
word_list_nyc = str_split(text_nyc, "\\s+")

# Unlist words
words_nyc = unlist(word_list_nyc)

# compare words to the dictionaries of positive & negative terms
positive_matches_nyc = match(words_nyc, positives)
negative_matches_nyc = match(words_nyc, negatives)

# Find list of positive/negative word used
p_nyc <- words_nyc[!is.na(positive_matches_nyc)]
n_nyc <- words_nyc[!is.na(negative_matches_nyc)]

# Find frequency of positive/negative words
p_nyc_count <- table(p_nyc)
n_nyc_count <- table(n_nyc)

# If used RegExp before, than do this. If not, skip
# Eliminate first value of ""
p_nyc_count <- p_nyc_count[-1]
n_nyc_count <- n_nyc_count[-1]

# Select most frequent words only
most_used_p_nyc <- subset(p_nyc_count, p_nyc_count>0)
most_used_n_nyc <- subset(n_nyc_count, n_nyc_count>0)

# Create dataframe with most used word (for plotting)
# Use colnames to allow the cbind to work
most_used_p_nyc <- data.frame(most_used_p_trump)
most_used_n_nyc <- data.frame(most_used_n_trump)
colnames(most_used_p_nyc) <- c("word", "freq")
colnames(most_used_n_nyc) <- c("word", "freq")
most_used_nyc_df <- rbind(most_used_p_nyc, most_used_n_nyc)


# Add column with positive/negative type
type <- 0
for (i in 1:nrow(most_used_nyc_df)){
	if (most_used_nyc_df[i,1] %in% p_nyc){
		type[i] <- "positive"
	} else if (most_used_nyc_df[i,1] %in% n_nyc){
		type[i] <- "negative"
	} else {
		type[i] <- "neutral"
	}
}

# Merge values
most_used_nyc_df <- cbind(most_used_nyc_df, type)

most_used_nyc_df <- subset(most_used_nyc_df, type != "neutral")


# Create barplot with ggplot2
ggplot(data=most_used_nyc_df, aes(x=word, y=freq, fill=type))+
geom_bar(stat="identity")+
ggtitle("NYC: Most Used Words in last 3200 tweets") +
labs(x="Words",y="Frequency") +
scale_y_continuous("Frequency", breaks=seq(0,8,1),limits=c(0,8))+
theme(plot.margin=unit(c(1,1,.5,.5),"cm"))+
theme(axis.text.x = element_text(angle = 90, hjust = 1, margin=margin(20,0,0,0)))+
theme(axis.title.y=element_text(margin=margin(0,20,0,0)))+
theme(plot.title=element_text(margin=margin(0,0,20,0)))+
coord_flip()+
theme(axis.text.x = element_text(angle = 0, hjust = 1, margin=margin(20,0,0,0)))


### SAN FRANCISCO ###

### Find positive/negative words used ###

positives= readLines("wordbanks/positive-words.txt")
negatives = readLines("wordbanks/negative-words.txt")

# Create a list of words
word_list_sf = str_split(text_sf, "\\s+")

# Unlist words
words_sf = unlist(word_list_sf)

# compare words to the dictionaries of positive & negative terms
positive_matches_sf = match(words_sf, positives)
negative_matches_sf = match(words_sf, negatives)

# Find list of positive/negative word used
p_sf <- words_sf[!is.na(positive_matches_sf)]
n_sf <- words_sf[!is.na(negative_matches_sf)]

# Find frequency of positive/negative words
p_sf_count <- table(p_sf)
n_sf_count <- table(n_sf)

# If used RegExp before, than do this. If not, skip
# Eliminate first value of ""
p_sf_count <- p_sf_count[-1]
n_sf_count <- n_sf_count[-1]

# Select most frequent words only
most_used_p_sf <- subset(p_sf_count, p_sf_count>1)
most_used_n_sf <- subset(n_sf_count, n_sf_count>1)

# Create dataframe with most used word (for plotting)
# Use colnames to allow the cbind to work
most_used_p_sf <- data.frame(most_used_p_sf)
most_used_n_sf <- data.frame(most_used_n_sf)
colnames(most_used_p_sf) <- c("word", "freq")
colnames(most_used_n_sf) <- c("word", "freq")
most_used_sf_df <- rbind(most_used_p_sf, most_used_n_sf)


# Add column with positive/negative type
type <- 0
for (i in 1:nrow(most_used_sf_df)){
	if (most_used_sf_df[i,1] %in% p_sf){
		type[i] <- "positive"
	} else if (most_used_sf_df[i,1] %in% n_sf){
		type[i] <- "negative"
	} else {
		type[i] <- "neutral"
	}
}

# Merge values
most_used_sf_df <- cbind(most_used_sf_df, type)
most_used_sf_df <- subset(most_used_sf_df, type != "neutral")


# Create barplot with ggplot2
ggplot(data=most_used_sf_df, aes(x=word, y=freq, fill=type))+
geom_bar(stat="identity")+
ggtitle("San Francisco: Most Used Words in last 3200 tweets") +
labs(x="Words",y="Frequency") +
scale_y_continuous("Frequency", breaks=seq(0,9,1),limits=c(0,9))+
theme(plot.margin=unit(c(1,1,.5,.5),"cm"))+
theme(axis.text.x = element_text(angle = 90, hjust = 1, margin=margin(20,0,0,0)))+
theme(axis.title.y=element_text(margin=margin(0,20,0,0)))+
theme(plot.title=element_text(margin=margin(0,0,20,0)))+
coord_flip()+
theme(axis.text.x = element_text(angle = 0, hjust = 1, margin=margin(20,0,0,0)))



