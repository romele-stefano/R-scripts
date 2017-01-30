# 1) Find beer reviewed by a specific user
# 2) Extract the % of beer reviewed by type in order to find probability of kind of beer
# 3) From the probabilities (2), select one beer style
# 4) Find all beer with the style (3)
# 5) Remove from (4) the beer already tried by the user
# 6) Select only 3 most favourite beer by the user (greater overall score)
# 7) Find other user who tired the same beers
# 8) Find avg value for each beer
# 9) Find distances
# 10) Find closest beer
# 11) Extract all closest beer reviews
# 12) Extract choice pool of beers
# 13) Recommend a beer



# 1) extract one user data
user_name <- "flexabull"
name <- subset(data, review_profilename == user_name)


# 2) extract probabilities
beer_perc <- table(name$beer_style)/nrow(name)
beer_perc <- beer_perc[beer_perc > 0]
beer_type <- sample(beer_perc, size = 1, prob = beer_perc)


# 3) Select one beer style (manually at the moment, then use probabilities point 2)
user_beer <- subset(name, beer_style == names(beer_type))
user_beer_name <- user_beer$beer_name


# 4) Find all beer with the style selected
all_type_beer <- subset(data, beer_style == names(beer_type))


# 5) Find beer not tried by the user
not_tried <- subset(all_type_beer, !(beer_name %in% user_beer_name))


# 6) Find user most liked beer
beer_order <- user_beer[order(user_beer$review_overall, decreasing=T), ]
most_liked <- beer_order[1:3, ]
most_liked <- most_liked[order(most_liked$beer_id, decreasing=T), ]
most_liked_name <- most_liked$beer_name


# 7) Find common user
all_reviews <- subset(data, beer_name %in% most_liked_name)
all_other_user_reviews <- subset(all_reviews, !(review_profilename == user_name))


# 8) Find avg value for each beer
mean_score <- aggregate(all_other_user_reviews[ ,c(4,5,6,9,10)],
                        list(all_other_user_reviews$beer_id), mean)
mean_score <- round(mean_score, 2)

# Format mean_score data
colnames(mean_score)[1] <- "beer_id"
mean_score <- mean_score[order(mean_score$beer_id, decreasing=T), ]
beer_name_id_and_abv <- unique(all_other_user_reviews[,c(11,12,13)])

mean_score <- merge(mean_score, beer_name_id_and_abv, by="beer_id")


# 9) Find distances for the same beers (maybe user to fine tuning predictions)
distances <- 0
distances <- as.data.frame(distances)
for (i in 1:nrow(most_liked)){
	distances[i,1] <- sqrt((most_liked[i,4] - mean_score[i,2])^2 +
                              (most_liked[i,5] - mean_score[i,3])^2 +
                              (most_liked[i,6] - mean_score[i,4])^2 +
                              (most_liked[i,9] - mean_score[i,5])^2 +
                              (most_liked[i,10] - mean_score[i,6])^2
			         )
	distances[i,2] <- most_liked[i,13]
	distances[i,3] <- most_liked[i,11]
}

colnames(distances) <- c("distance", "beer_id", "beer_name")

# 10) Find closest beer from other users review
distances <- distances[order(distances$distance, decreasing=F), ]
closest <- distances[1, ]


# 11) Extract all reviews for closest beer
all_closest <- all_other_user_reviews[all_other_user_reviews$beer_id == closest$beer_id, ]


# 12) All reviews from all_closest user of the same beer style
names <- all_closest$review_profilename
choice_pool <- subset(all_type_beer, review_profilename %in% names)
choice_pool <- subset(choice_pool, !(beer_name %in% closest$beer_name))
choice_pool <- choice_pool[order(choice_pool$beer_id),]

# Find average score for choice pool
choice_pool_mean_score <- aggregate(choice_pool[ ,c(4,5,6,9,10)],
                        list(choice_pool$beer_id), mean)
choice_pool_mean_score <- round(choice_pool_mean_score, 2)

names(choice_pool_mean_score)[1] <- "beer_id"

choice_pool_mean_score <- choice_pool_mean_score[order(choice_pool_mean_score$beer_id, decreasing=T), ]
choice_beer_name_id_and_abv <- unique(choice_pool[,c(11,12,13)])

# Add beer name and other info to choice_pool
choice_pool_mean_score <- merge(choice_pool_mean_score, choice_beer_name_id_and_abv, by="beer_id")


# 13) Find distances between choice_pool and user most closest

# Extract user closest
user_closest <- subset(most_liked, beer_name == closest$beer_name)

closest_distances <- 0
closest_distances <- as.data.frame(closest_distances)
for (i in 1:nrow(choice_pool_mean_score)){
	closest_distances[i,1] <- sqrt((user_closest[1,4] - choice_pool_mean_score[i,2])^2 +
                                      (user_closest[1,5] - choice_pool_mean_score[i,3])^2 +
                                      (user_closest[1,6] - choice_pool_mean_score[i,4])^2 +
                                      (user_closest[1,9] - choice_pool_mean_score[i,5])^2 +
                                      (user_closest[1,10] - choice_pool_mean_score[i,6])^2
			                 )
	closest_distances[i,2] <- choice_pool[i,13]
	closest_distances[i,3] <- choice_pool[i,11]
}

colnames(closest_distances) <- c("distance", "beer_id", "beer_name")


# 14) Extract closest beer
recommended_beer <- closest_distances[order(closest_distances$distance), ]
recommended_beer <- recommended_beer[1, ]
