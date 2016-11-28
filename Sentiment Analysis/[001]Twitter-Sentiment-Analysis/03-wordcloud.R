#################
#   WORDCLOUD   #
#################

### TRUMP ###

# Eliminate character
text_trump <- gsub("&amp;", "", text_trump)
text_trump <- gsub("http.*", "", text_trump)
text_trump <- gsub("@.*", "", text_trump)

# create wordcloud
corpus=Corpus(VectorSource(text_trump))

# Convert to lower-case
corpus=tm_map(corpus,tolower)

# Remove stopwords
corpus=tm_map(corpus,function(x) removeWords(x,stopwords()))

# convert corpus to a Plain Text Document
corpus=tm_map(corpus,PlainTextDocument)

# display.brewer.all()  to see possible colors!

col=brewer.pal(8,"Paired")
wordcloud(corpus, min.freq=5, scale=c(4,.7),rot.per = 0.25,
          random.color=T, max.word=250, random.order=F,colors=col)


### CLINTON ###

# Eliminate character
text_clinton <- gsub("&amp;", "", text_clinton)
text_clinton <- gsub("http.*", "", text_clinton)
text_clinton <- gsub("@.*", "", text_clinton)

# create wordcloud
corpus=Corpus(VectorSource(text_clinton))

# Convert to lower-case
corpus=tm_map(corpus,tolower)

# Remove stopwords
corpus=tm_map(corpus,function(x) removeWords(x,stopwords()))

# convert corpus to a Plain Text Document
corpus=tm_map(corpus,PlainTextDocument)

# display.brewer.all()  to see possible colors!
col=brewer.pal(8,"Paired")
wordcloud(corpus, min.freq=5, scale=c(5,1),rot.per = 0.25,
          random.color=T, max.word=100, random.order=F,colors=col)
