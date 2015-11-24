## There are 4 deliverables for this assignment. Each deliverable represents a different analysis of the data provided to you. For each deliverable, you will submit a list of the top 5 movies as ranked by a particular metric. 

# Set path where is located the CSV file
# setwd('...')

# Read data into raw
raw <- read.csv("A1Ratings.csv")

# Separate movie rates from id of users (first column)
raw2 <- raw[, 2:21]


##1. Mean Rating: Calculate the mean rating for each movie, order with the highest rating listed first, and submit the top 5.

# apply mean function over the columns
means <- sapply(raw2, FUN = mean, na.rm=TRUE)
# order result
means <- means[order(means, decreasing = TRUE)]
print(means[1:5])


##2. % of ratings 4+: Calculate the percentage of ratings for each movie that are 4 or higher. Order with the highest percentage first, and submit the top 5.

# function for calculating the percentage of movies with ratings above or equal to 4
calcPercentGE4 <- function(x) {
  sum(!is.na(x[x>=4])) / sum(!is.na(x))
}
percentGE4 <- apply(raw2, MARGIN = 2, FUN = calcPercentGE4)
# round down, as in the example
percentGE4 <- floor(percentGE4 * 1000)/10
# order result
percentGE4 <- percentGE4[order(percentGE4, decreasing = TRUE)]
print(percentGE4[1:5])


##3. Rating Count: Count the number of ratings for each movie, order with the most number of ratings first, and submit the top 5.

# get dataset of TRUE and FALSE corresponding to the fact of if there is rating or not
data_has_rate <- !is.na(raw2)
# count number of ratings (TRUE is 1 and FALSE is 0)
count_ratings <- apply(data_has_rate, MARGIN = 2, FUN=sum, na.rm=FALSE)
# order result
count_ratings <- count_ratings[order(count_ratings, decreasing = TRUE)]
print(count_ratings[1:5])


##4. Top 5 Star Wars: Calculate movies that most often occur with Star Wars: Episode IV - A New Hope (1977) using the (x+y)/x method described in class. In other words, for each movie, calculate the percentage of Star Wars raters who also rated that movie. Order with the highest percentage first, and submit the top 5.

# starwars
names(raw2)[1]

#vetor of occurrences of star wars iv rates
sw_notNA <- !is.na(raw2[, 1]) 
#function that calculates the probability of occurrences of star wars 
#together with each movie
calcOccurWithSW <- function(x) {
    # (x + y) / x
    #where x is the occurrences of star wars
    #and y is the occurrences of that other movie
    col_notNA <- !is.na(x)
    prob_sw_and_col <- sum(sw_notNA & col_notNA)
    prob_sw <- sum(sw_notNA)
    prob <- prob_sw_and_col / prob_sw
    #rounding result
    round(prob * 100, digits = 1)
}
#data without star war movie
data_noSW <- raw2[, 2:ncol(raw2)]
#apply the function to the whole dataset
occurWithSW <- apply(data_noSW, MARGIN = 2, FUN = calcOccurWithSW)
#ordering and cropping
occurWithSW <- occurWithSW[order(occurWithSW, decreasing = TRUE)]
print(occurWithSW[1:5])
