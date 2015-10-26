# set options to cache twitter oauth so that it doesn´t prompt every time
options(httr_oauth_cache = TRUE)
# setwd("~/Desktop/IA/Twitter/SMFeedback")

# required packages
library(twitteR)
library(httr)
library(dplyr)
library(magrittr)
library(stringr)
library(RJSONIO)
library(RCurl)
source("hashgrep.R")
source("cleanPosts.R")
source("getUserMedia.R")

# lists with positive and negative words
pos <- scan("poswords.txt", what = "character")
neg <- scan("negwords.txt", what = "character")

#' @post /score
getScore <- function(twittername = NULL, igname = NULL) {
    
    if (!is.null(twittername) & is.null(igname)) {
        # twitter OAuth, token = my_oauth
        load("twitterOauthSurvey.Rdata")
        consumer_key <- my_oauth$consumerKey
        consumer_secret <- my_oauth$consumerSecret
        access_token <- my_oauth$oauthKey[[1]]
        access_secret <- my_oauth$oauthSecret[[1]]
        setup_twitter_oauth(consumer_key = consumer_key, consumer_secret = consumer_secret,
                            access_token = access_token, access_secret = access_secret)
        1
        
        # get most recent tweets
        user_tweets <- userTimeline(twittername, n = 100, includeRts = FALSE)
        # get the text\\
        dirty_tweets <- twListToDF(user_tweets) %>%
            data.frame(text = unlist(lapply(user_tweets, function(t)t$getText())))
        
        # clean tweets
        clean_tweets <- cleanPosts(dirty_tweets)
        
        # determine polarity of tweets
        str_tweets <- str_split(clean_tweets$text, "\\s+")
        scores_pos <- lapply(str_tweets, match, table = pos) %>%
            lapply(., function(row) !is.na(row)) %>%
            lapply(., function(row) sum(unlist(row)))
        scores_neg <- lapply(str_tweets, match, table = neg) %>%
            lapply(., function(row) !is.na(row)) %>%
            lapply(., function(row) sum(unlist(row)))
        
        # sum scores and compute percentage of positive, negative and neutral posts
        scores <- as.numeric(scores_pos)-as.numeric(scores_neg)
        list(length(which(scores > 0))/length(scores), 
             length(which(scores < 0))/length(scores), 
             length(which(scores == 0))/length(scores))
        
    } else if (is.null(twittername) & !is.null(igname)) {
        # instagram OAuth
        load("ig_oauth_ia")
        token <- ig_oauth_ia$token
        
        # get user media
        user_media <- getUserMedia(igname, token = token)
        
        # clean media
        clean_ig <- cleanPosts(user_media)
        
        # determine polarity
        str_ig <- str_split(clean_ig$text, "\\s+")
        scores_pos <- lapply(str_ig, match, table = pos) %>%
            lapply(., function(row) !is.na(row)) %>%
            lapply(., function(row) sum(unlist(row)))
        scores_neg <- lapply(str_ig, match, table = neg) %>%
            lapply(., function(row) !is.na(row)) %>%
            lapply(., function(row) sum(unlist(row)))
        
        # sum scores and compute percentage of positive, negative and neutral posts
        scores <- as.numeric(scores_pos)-as.numeric(scores_neg)
        ig_pos_perc <- length(which(scores > 0))/length(scores)
        ig_neg_perc <- length(which(scores < 0))/length(scores)
        ig_neu_perc <- length(which(scores == 0))/length(scores)
        list(ig_pos_perc, ig_neg_perc, ig_neu_perc)
        
    } else if (!is.null(twittername) & !is.null(igname)) {
        
        ################################ TWITTER ###################################
        
        # twitter OAuth, token = my_oauth
        load("twitterOauthSurvey.Rdata")
        consumer_key <- my_oauth$consumerKey
        consumer_secret <- my_oauth$consumerSecret
        access_token <- my_oauth$oauthKey[[1]]
        access_secret <- my_oauth$oauthSecret[[1]]
        setup_twitter_oauth(consumer_key = consumer_key, consumer_secret = consumer_secret,
                            access_token = access_token, access_secret = access_secret)
        1
        
        # get most recent tweets
        user_tweets <- userTimeline(twittername, n = 100, includeRts = TRUE)
        # get the text
        dirty_tweets <- twListToDF(user_tweets) 
        dirty_tweets <- data.frame(text = unlist(lapply(user_tweets, function(t)t$getText())))
        
        # clean tweets
        clean_tweets <- cleanPosts(dirty_tweets)
        
        # determine polarity of tweets
        str_tweets <- str_split(clean_tweets$text, "\\s+")
        scores_pos <- lapply(str_tweets, match, table = pos) %>%
            lapply(., function(row) !is.na(row)) %>%
            lapply(., function(row) sum(unlist(row)))
        scores_neg <- lapply(str_tweets, match, table = neg) %>%
            lapply(., function(row) !is.na(row)) %>%
            lapply(., function(row) sum(unlist(row)))
        
        # sum scores and compute percentage of positive, negative and neutral posts
        tw_scores <- as.numeric(scores_pos)-as.numeric(scores_neg)
        tw_pos_perc <- length(which(tw_scores > 0))/length(tw_scores)
        tw_neg_perc <- length(which(tw_scores < 0))/length(tw_scores)
        tw_neu_perc <- length(which(tw_scores == 0))/length(tw_scores)
    
        
        ############################ INSTAGRAM #####################################
        
        # instagram OAuth
        load("ig_oauth_ia")
        token <- ig_oauth_ia$token
        
        # get user media
        user_media <- getUserMedia(igname, token = token)
        
        # clean media
        clean_ig <- cleanPosts(user_media)
        
        # determine polarity
        str_ig <- str_split(clean_ig$text, "\\s+")
        ig_scores_pos <- lapply(str_ig, match, table = pos) %>%
            lapply(., function(row) !is.na(row)) %>%
            lapply(., function(row) sum(unlist(row)))
        ig_scores_neg <- lapply(str_ig, match, table = neg) %>%
            lapply(., function(row) !is.na(row)) %>%
            lapply(., function(row) sum(unlist(row)))
        
        # sum scores and compute percentage of positive, negative and neutral posts
        ig_scores <- as.numeric(ig_scores_pos)-as.numeric(ig_scores_neg)
        ig_pos_perc <- length(which(ig_scores > 0))/length(ig_scores)
        ig_neg_perc <- length(which(ig_scores < 0))/length(ig_scores)
        ig_neu_perc <- length(which(ig_scores == 0))/length(ig_scores)
        
        # sum up twitter & ig percentages and return them
        list((tw_pos_perc+ig_pos_perc)/2, (tw_neg_perc+ig_neg_perc)/2, 
             (tw_neu_perc+ig_neu_perc)/2)
        
    } else 
        print("Sorry, we can´t provide feedback as you didn´t indicate any social media profile")
    
}

# try it out
# score <- getScore(twittername = "parishilton", igname = "parishilton")
# score2 <- getScore(twittername = "parishilton")
# score3 <- getScore(igname = "parishilton")

# library(plumber)
# r <- plumb("evaluation.R")
# r$run(port=1234, host="0.0.0.0")

###################### WORK IN PROGRESS #######################

# check for language of tweets
#tweets_text <- as.vector(tweets_text)
#EnglishWordComparisonList <- as.vector(fread("englishDict.txt", header = FALSE))
#Englishinator <- function(tweet) {
    #TWTS <- which((EnglishWordComparisonList %in% tweets_text)/length(tweets_text)>.06)
    #tweet[TWTS]
#}

#perc_eng <- lapply(tweets_text, FUN = Englishinator)
# add if statement: if perc_eng < 0,6 -> return "Sorry, you don´t have enough english tweets to process"
