# Matthew Miesle
# CUNY SPS
# IS 607 SECT 01
# Week 4 Quiz
# Due 9/18/2014 EOD

filelocation <- "C:/Users/MattM/Downloads/movies.tab.gz"
movie.data <- read.table(filelocation, sep="\t", header=TRUE, quote="", comment="", stringsAsFactors = FALSE)

#### Prob 1 ####
# 1. Show an appropriate visualization that displays the total number 
# of movies for each decade.
func1 <- function()
{
    movies.by.decade <- c()
    i <- 0
    while (i < 12)
    {
        movies.by.decade[i+1] <- sum(ifelse(movie.data$year >= (1890 + i * 10) & movie.data$year <=
                                      (1899 + i * 10), 1, 0))
        i = i + 1
    }

#     movies.1890s <- length(movie.data$year[movie.data$year >= 1890 & movie.data$year <= 1899])
#     could try to vectorize it

    names(movies.by.decade) <- c("1890s", "1900s", "1910s", "1920s", "1930s", "1940s", "1950s", "1960s",
                                 "1970s", "1980s", "1990s", "2000s")

    barplot(movies.by.decade, xlab="Decades", ylab="Number of movies released",
            main="Movie release volume by decade", ylim=c(0,14000))

}

#### Prob 2 ####
# 2. Show the average IMDB user rating for different genres of 
# movies. Has this changed over time?
func2 <- function()
{

#     is this where a melt() would be useful?
#     Action Animation Comedy Drama Documentary Romance Short   
#     avg.rating.genres <- melt(movie.data, id=c("rating", "Action", "Animation",
#                               "Comedy", "Drama", "Documentary", 
#                               "Romance", "Short"))
    
    avg.rating.genre <- c()
    avg.rating.genre[1] <- mean(movie.data$rating[movie.data$Action == 1])
    avg.rating.genre[2] <- mean(movie.data$rating[movie.data$Animation == 1])
    avg.rating.genre[3] <- mean(movie.data$rating[movie.data$Comedy == 1])
    avg.rating.genre[4] <- mean(movie.data$rating[movie.data$Drama == 1])
    avg.rating.genre[5] <- mean(movie.data$rating[movie.data$Documentary == 1])
    avg.rating.genre[6] <- mean(movie.data$rating[movie.data$Romance == 1])
    avg.rating.genre[7] <- mean(movie.data$rating[movie.data$Short == 1])
    
    names(avg.rating.genre) <- c("Action", "Animation", "Comedy", "Drama",
                                 "Documentary", "Romance", "Short")

    barplot(avg.rating.genre, xlab="Genres", ylab="Average rating",
            main="Average IMDb user rating of movies by genre", ylim=c(0,10))
    
#     part 2 - has this changed over time?
    
#     is this where a melt() would be useful?
#     Action Animation Comedy Drama Documentary Romance Short   
#     avg.rating.genres <- melt(movie.data, id=c("rating", "Action", "Animation",
#                               "Comedy", "Drama", "Documentary", 
#                               "Romance", "Short"))
#     create an array by year???


#     use one of the apply functions!

    avg.rating.genre.yr <- array(ncol = 7, nrow = 113)
    i = 0
    while (i < 113)
    {
        avg.rating.genre.yr[i + 1, 1] <- mean(movie.data$rating[movie.data$Action == 1 & movie.data$year == 1893 + i])
        avg.rating.genre.yr[i + 1, 2] <- mean(movie.data$rating[movie.data$Animation == 1 & movie.data$year == 1893 + i])
        avg.rating.genre.yr[i + 1, 3] <- mean(movie.data$rating[movie.data$Comedy == 1 & movie.data$year == 1893 + i])
        avg.rating.genre.yr[i + 1, 4] <- mean(movie.data$rating[movie.data$Drama == 1 & movie.data$year == 1893 + i])
        avg.rating.genre.yr[i + 1, 5] <- mean(movie.data$rating[movie.data$Documentary == 1 & movie.data$year == 1893 + i])
        avg.rating.genre.yr[i + 1, 6] <- mean(movie.data$rating[movie.data$Romance == 1 & movie.data$year == 1893 + i])
        avg.rating.genre.yr[i + 1, 7] <- mean(movie.data$rating[movie.data$Short == 1 & movie.data$year == 1893 + i])
        i = i + 1
    }
    rownames(avg.rating.genre.yr) <- c(1893:2005)
    colnames(avg.rating.genre.yr) <- c("Action", "Animation", "Comedy", "Drama",
                                 "Documentary", "Romance", "Short")
    

#     melted.argy <- melt(avg.rating.genre.yr)

# line plots for each genre with x-axis being the year
# y -axis being the rating
# and a different line for each genre

# difficulty getting this line plot to work

# g <- ggplot(data.frame(avg.rating.genre.yr), aes(x=rownames(avg.rating.genre.yr), y=colnames(avg.rating.genre.yr)))
# g + geom_line()

# http://docs.ggplot2.org/current/geom_line.html
# code below taken from ggplot2 documentation working with movie data
# how to modify this to take genre into account???
# mry <- do.call(rbind, by(movie.data, round(movie.data$rating), function(df) {
#     nums <- tapply(df$length, df$year, length)
#     data.frame(rating=round(df$rating[1]), year = as.numeric(names(nums)), number=as.vector(nums))
# }))
# p <- ggplot(mry, aes(x=year, y=number, group=rating))
# p + geom_line()


#     interpreting problem statement "over time" as being year of production
#     the time of user rating is not recorded
#     the rating is as of the data of data capture
#     we can only say that users prefer movies of a given genre at the point of data capture
#     we can't say that users prefered one genre over another at any point in time
    
}

#### Prob 3 ####
# 3. Is there a relationship between length of movie and movie rating?
func3 <- function(x, y)
{
#     movie rating is being interpreted as the IMDb user rating
#     and not meaning the MPAA rating
#     still tried to interpret both though
    
    with(movie.data, plot(length[length < 180], rating[length < 180], xlab = "Movie Length in Mins",
                          ylab = "User Rating", main = "User Rating vs. Movie Length"))
#     limited the plot to movies less than 150minutes in length
#     not as many movies longer than that so it's better to view more detail
#     on the bulk of the plot instead of the few outliers causing a dark
#     cloud followed by a lot of empty space and then a few points far out to the right
#     http://ww2.coastal.edu/kingw/statistics/R-tutorials/simplelinear.html

#     movie.rating.vs.length <- lm(movie.data$rating ~ movie.data$length)
#     Coefficients:
#     (Intercept)  movie.data$length  
#     6.021471          -0.001076     
#     summary(movie.rating.vs.length)
# http://www.montefiore.ulg.ac.be/~kvansteen/GBIO0009-1/ac20092010/Class8/Using%20R%20for%20linear%20regression.pdf
    print("Correlation coefficient for Movie Rating and Movie Length:")
    print(cor(movie.data$rating, movie.data$length))
#     -0.03073441
    print ("Because the correlation coefficient is close to 0")  
    print ("there is little linear correlation between the 2 variables")
# http://www.r-tutor.com/elementary-statistics/numerical-measures/correlation-coefficient

#     also looked at MPAA Rating vs. Movie Length
#     but commented it out since it didn't seem applicable
#     movie.length.mpaa <- data.frame(movie.data$length, movie.data$mpaa)
#     names(movie.length.mpaa) <- c("length", "mpaa")
#     movie.length.mpaa$mpaa <- replace(movie.length.mpaa$mpaa, which(movie.length.mpaa$mpaa == ""), NA)
# #     ggplot(movie.data, aes(y = length, x = mpaa)) + geom_boxplot()
#     boxplot(formula = length ~ mpaa, data = movie.length.mpaa, xlab = "MPAA Rating",
#             ylab = "Movie Length in Mins", main = "MPAA Rating vs. Movie Length")
# 
# #     it doesn't appear that there's any sort of relationship between MPAA Rating and Movie length
}


#### Prob 4 ####
# 4. Is there a relationship between length of movie and genre?
func4 <- function()
{
#     box plots that show side by side
#     ggplot(Actionlength, aes(y = Actionlength)) + geom_boxplot()
#     Actionlength <- movie.data$length[movie.data$Action ==1]
    genrelength <- movie.data[18:24] * movie.data$length
#     trouble getting apply to work properly with replace and which
#     apply(genrelength, FUN = replace, , MARGIN = 2, list = which(genrelength == 0), values = NA)
    genrelength$Action <- replace(genrelength$Action, which(genrelength$Action == 0), NA)
    genrelength$Animation <- replace(genrelength$Animation, which(genrelength$Animation == 0), NA)
    genrelength$Comedy <- replace(genrelength$Comedy, which(genrelength$Comedy == 0), NA)
    genrelength$Drama <- replace(genrelength$Drama, which(genrelength$Drama == 0), NA)
    genrelength$Documentary <- replace(genrelength$Documentary, which(genrelength$Documentary == 0), NA)
    genrelength$Romance <- replace(genrelength$Romance, which(genrelength$Romance == 0), NA)
    genrelength$Short <- replace(genrelength$Short, which(genrelength$Short == 0), NA)
    
    boxplot(genrelength, xlab = "Genre", ylab = "Movie Length in Mins", 
            main = "Boxplots of Movie Length by Genre")
    # http://www.r-bloggers.com/box-plot-with-r-tutorial/

    
    print ("Appropriately named Shorts tend to be shorter, and Animation tends to be shorter as well")
    print ("Otherwise, other genres seem to be about the same length, accept Documentaries vary more")

}


#### Prob 5 ####
# 5. Which other variable best predicts total number of votes that a movie received.
func5 <- function(x, y)
{
#     started with a pairs plot to get a general idea of relationships
#     but it doesn't seem necessary to plot this for you
#     pairs(movie.data[2:6])
#     http://ww2.coastal.edu/kingw/statistics/R-tutorials/simplelinear.html
#     it appears that year of release and movie rating have more defined plots
#     and movie length has some outliers that make it difficult to view so it was
#     reviewed in more detail as well
    
    with(movie.data, plot(rating, votes, main = "Number of Votes vs. User Rating", 
         xlab = "User Rating", ylab = "Number of Votes"))
    with(movie.data, plot(year, votes, main = "Number of Votes vs. Year of Release", 
         xlab = "Year of Movie Release", ylab = "Number of Votes"))
    with(movie.data, plot(length[length > 50 & length < 180], votes[length > 50 & length < 180],
         main = "Number of Votes vs. Length of Movie", xlab ="Length of Movie in Mins",
         ylab = "Number of Votes"))
#     if more time available would have put into one plot to view simultaneously
    
#     movie.votes.vs.rating <- lm(movie.data$votes ~ movie.data$rating)
#     summary(movie.votes.vs.rating)
    print("Correlation coefficient for User Rating and Movie Votes:")
    print(cor(movie.data$rating, movie.data$votes))
    
#     movie.votes.vs.year <- lm(movie.data$votes ~ movie.data$year)
#     summary(movie.votes.vs.year)
    print("Correlation coefficient for Year of Release and Movie Votes:")
    print(cor(movie.data$rating, movie.data$year))

#     movie.votes.vs.length <- lm(movie.data$votes ~ movie.data$length)
#     summary(movie.votes.vs.length)
    print("Correlation coefficient for Movie Length in Mins and Movie Votes:")
    print(cor(movie.data$length, movie.data$votes))

    print("Because the correlation coefficient is highest between Movie Length")
    print("and Number of Votes, I would say Movie Length is the best predictor")
    print("for the number of votes received.")

#     would using the entropy function from project#1 to see where infogain is
#     be appropriate?

}



