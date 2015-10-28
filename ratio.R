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

#' @post /ratio
getRatio <- function(twittername = NULL, igname = NULL) {
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
        
        # get user data
        user <- getUser(twittername)
        # compute ratio
        user$getFollowersCount() / user$getFriendsCount()
        
    } else if (is.null(twittername) & !is.null(igname)) {
        # instagram OAuth
        load("ig_oauth_ia")
        token <- ig_oauth_ia$token
        
        # get user data
        content <- fromJSON(getURL(paste('https://api.instagram.com/v1/users/search?q=', 
                                         igname,'&access_token=', token, sep="")), 
                            unexpected.escape = "keep")
        userid <- as.numeric(content$data[[1]][[3]])
        content <- fromJSON(getURL(paste("https://api.instagram.com/v1/users/", 
                                         userid, "/?access_token=", token, sep="")))
    
        # compute ratio
        content$data$counts$followed_by / content$data$counts$follows
        
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
        
        # get user data
        user <- getUser(twittername)
        
        ############################ INSTAGRAM #####################################
        
        # instagram OAuth
        load("ig_oauth_ia")
        token <- ig_oauth_ia$token
        
        # get user data
        content <- fromJSON(getURL(paste('https://api.instagram.com/v1/users/search?q=', 
                                         igname,'&access_token=', token, sep="")), 
                            unexpected.escape = "keep")
        userid <- as.numeric(content$data[[1]][[3]])
        content <- fromJSON(getURL(paste("https://api.instagram.com/v1/users/", 
                                         userid, "/?access_token=", token, sep="")))
        
        # sum up twitter & ig percentages and return them
        (user$getFollowersCount()/user$getFriendsCount()) + 
            (content$data$counts$followed_by/content$data$counts$follows) / 2
        
    } else 
        print("Sorry, we can´t provide feedback about your follower/friends ratio as you didn´t indicate any social media profile")
    
}
