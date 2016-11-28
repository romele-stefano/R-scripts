##########################
#   SENTIMENT ANALYSIS   #
##########################

### TRUMP ###

### Find positive/negative words used ###

positives= readLines("wordbanks/positive-words.txt")
negatives = readLines("wordbanks/negative-words.txt")

# Create a list of words
word_list_trump = str_split(text_trump, "\\s+")

# Unlist words
words_trump = unlist(word_list_trump)

# compare words to the dictionaries of positive & negative terms
positive_matches_trump = match(words_trump, positives)
negative_matches_trump = match(words_trump, negatives)

# Find list of positive/negative word used
p_trump <- words_trump[!is.na(positive_matches_trump)]
n_trump <- words_trump[!is.na(negative_matches_trump)]

# Find frequency of positive/negative words
p_trump_count <- table(p_trump)
n_trump_count <- table(n_trump)

# If used RegExp before, than do this. If not, skip
# Eliminate first value of ""
p_trump_count <- p_trump_count[-1]
n_trump_count <- n_trump_count[-1]

# Select most frequent words only
most_used_p_trump <- subset(p_trump_count, p_trump_count>1)
most_used_n_trump <- subset(n_trump_count, n_trump_count>1)

# Create dataframe with most used word (for plotting)
# Use colnames to allow the cbind to work
most_used_p_trump <- data.frame(most_used_p_trump)
most_used_n_trump <- data.frame(most_used_n_trump)
colnames(most_used_p_trump) <- c("word", "freq")
colnames(most_used_n_trump) <- c("word", "freq")
most_used_trump_df <- rbind(most_used_p_trump, most_used_n_trump)


# Add column with positive/negative type
type <- 0
for (i in 1:nrow(most_used_trump_df)){
	if (most_used_trump_df[i,1] %in% p_trump){
		type[i] <- "positive"
	} else if (most_used_trump_df[i,1] %in% n_trump){
		type[i] <- "negative"
	}
}

# Merge values
most_used_trump_df <- cbind(most_used_trump_df, type)



# Create barplot with ggplot2
# use aes(x=reorder(word,freq) if want plotted in order (mixed positive/negative)
ggplot(data=most_used_trump_df, aes(x=word, y=freq, fill=type))+
geom_bar(stat="identity")+
ggtitle("Trump: Most Used Words in last 500 tweets") +
labs(x="Words",y="Frequency") +
scale_y_continuous("Frequency", breaks=seq(0,8,1),limits=c(0,8))+
theme(plot.margin=unit(c(1,1,.5,.5),"cm"))+
theme(axis.text.x = element_text(angle = 90, hjust = 1, margin=margin(20,0,0,0)))+
theme(axis.title.y=element_text(margin=margin(0,20,0,0)))+
theme(plot.title=element_text(margin=margin(0,0,20,0)))+
coord_flip()+
theme(axis.text.x = element_text(angle = 0, hjust = 1, margin=margin(20,0,0,0)))




### CLINTON ###

### Find positive/negative words used ###

positives= readLines("wordbanks/positive-words.txt")
negatives = readLines("wordbanks/negative-words.txt")

# Create a list of words
word_list_clinton = str_split(text_clinton, "\\s+")

# Unlist words
words_clinton = unlist(word_list_clinton)

# compare words to the dictionaries of positive & negative terms
positive_matches_clinton = match(words_clinton, positives)
negative_matches_clinton = match(words_clinton, negatives)

# Find list of positive/negative word used
p_clinton <- words_clinton[!is.na(positive_matches_clinton)]
n_clinton <- words_clinton[!is.na(negative_matches_clinton)]

# Find frequency of positive/negative words
p_clinton_count <- table(p_clinton)
n_clinton_count <- table(n_clinton)

# If used RegExp before, than do this. If not, skip
# Eliminate first value of ""
p_clinton_count <- p_clinton_count[-1]
n_clinton_count <- n_clinton_count[-1]

# Select most frequent words only
most_used_p_clinton <- subset(p_clinton_count, p_clinton_count>1)
most_used_n_clinton <- subset(n_clinton_count, n_clinton_count>1)

# Create dataframe with most used word (for plotting)
# Use colnames to allow the cbind to work
most_used_p_clinton <- data.frame(most_used_p_clinton)
most_used_n_clinton <- data.frame(most_used_n_clinton)
colnames(most_used_p_clinton) <- c("word", "freq")
colnames(most_used_n_clinton) <- c("word", "freq")
most_used_clinton_df <- rbind(most_used_p_clinton, most_used_n_clinton)


# Add column with positive/negative type
type <- 0
for (i in 1:nrow(most_used_clinton_df)){
	if (most_used_clinton_df[i,1] %in% p_clinton){
		type[i] <- "positive"
	} else if (most_used_clinton_df[i,1] %in% n_clinton){
		type[i] <- "negative"
	}
}

# Merge values
most_used_clinton_df <- cbind(most_used_clinton_df, type)



# Create barplot with ggplot2
ggplot(data=most_used_clinton_df, aes(x=word, y=freq, fill=type))+
geom_bar(stat="identity")+
ggtitle("Clinton: Most Used Words in last 500 tweets") +
labs(x="Words",y="Frequency") +
scale_y_continuous("Frequency", breaks=seq(0,8,1),limits=c(0,8))+
theme(plot.margin=unit(c(1,1,.5,.5),"cm"))+
theme(axis.text.x = element_text(angle = 90, hjust = 1, margin=margin(20,0,0,0)))+
theme(axis.title.y=element_text(margin=margin(0,20,0,0)))+
theme(plot.title=element_text(margin=margin(0,0,20,0)))+
coord_flip()+
theme(axis.text.x = element_text(angle = 0, hjust = 1, margin=margin(20,0,0,0)))

