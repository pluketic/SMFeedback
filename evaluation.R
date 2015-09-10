# set options to cahce twitter oauth so that it doesn´t prompt every time
options(httr_oauth_cache = TRUE)
# setwd("~/Desktop/IA/Twitter/SMFeedback")

#' @post /score
getScore <- function(twittername = NULL, igname = NULL) {
    
    # required packages
    library(twitteR)
    library(httr)
    library(dplyr)
    library(magrittr)
    library(stringr)
    library(rjson)
    library(RCurl)
    load("hashgrep.R")
    
    if (!is.null(twittername) & is.null(igname)) {
        # twitter OAuth, token = my_oauth
        load("twitterOauthSurvey.Rdata")
        consumer_key <- my_oauth$consumerKey
        consumer_secret <- my_oauth$consumerSecret
        access_token <- my_oauth$oauthKey
        access_secret <- my_oauth$oauthSecret
        setup_twitter_oauth(consumer_key = consumer_key, consumer_secret = consumer_secret,
                            access_token = access_token, access_secret = access_secret)
        1
        
        # lists with positive and negative words
        # download lists with positive and negative words
        #fileURL2 <- "https://raw.githubusercontent.com/mjhea0/twitter-sentiment-analysis/master/wordbanks/negative-words.txt"
        #download.file(fileURL2, destfile = "negwords.txt", method = "curl")
        #fileURL3 <- "https://raw.githubusercontent.com/mjhea0/twitter-sentiment-analysis/master/wordbanks/positive-words.txt"
        #download.file(fileURL3, destfile = "poswords.txt", method = "curl")
        pos <- scan("poswords.txt", what='character')
        neg <- scan("negwords.txt", what='character')
        
        # get most recent tweets
        user_tweets <- userTimeline(twittername, n = 100, includeRts = TRUE)
        # get the text\\
        dirty_tweets <- twListToDF(user_tweets) 
        dirty_tweets <- data.frame(unlist(lapply(user_tweets, function(t)t$getText())))
        names(dirty_tweets) <- "text"
        
        # cleaning pipeline
        clean_tweets <- data.frame(sapply(dirty_tweets$text, function(row) 
            iconv(row, "latin1", "ASCII", sub="")))
        names(clean_tweets) <- "text"
        clean_tweets <- clean_tweets$text %>%
            gsub("&amp;", "", .) %>% # remove &
            gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", .) %>% # remove retweet entities
            gsub("@\\w+", "", .) %>% # remove at people
            #gsub("([[:upper:]][[:lower:]])", " \\1", .) %>% # split group of words at uppercase letter and add a white space
            hashgrep %>%
            gsub("[[:punct:]]", "", .) %>% # remove punctuation
            gsub("[[:digit:]]", "", .) %>% # remove digits
            gsub("http\\w+", "", .) %>% # remove html links
            iconv(from = "latin1", to = "ASCII", sub="") %>% # remove emoji and bizarre signs
            gsub("[ \t]{2,}", " ", .) %>% # remove unnecessary spaces
            gsub("^\\s+|\\s+$", "", .) %>% # remove unnecessary spaces
            subset(., . != "") %>% # remove empty lines
            tolower %>%
            data.frame(., stringsAsFactors=FALSE)
        names(clean_tweets) <- "text"
        
        ################## Compute polarity ################
        
        # determine polarity of tweets
        str_tweets <- str_split(clean_tweets$text, "\\s+")
        pos.matches <- lapply(str_tweets, match, table = pos)
        neg.matches <- lapply(str_tweets, match, table = neg)
        pos.matches <- lapply(pos.matches, function(row) !is.na(row))
        neg.matches <- lapply(neg.matches, function(row) !is.na(row))
        scores.pos <- lapply(pos.matches, function(row) length(pos.matches[row == TRUE]))
        scores.neg <- lapply(neg.matches, function(row) length(neg.matches[row == TRUE]))
        ## if statement zur überprüfung von polarität (<0 ist negativ, >0 ist positiv, = 0 ist neutral)
        
        # sum scores to total score
        score <- sum(sapply(pos.matches, sum) - sapply(neg.matches, sum))
        score
        
    } else if (is.null(twittername) & !is.null(igname)) {
        # instagram OAuth
        load("ig_oauth_ia")
        token <- ig_oauth_ia$token
        
        # lists with positive and negative words
        pos <- scan("poswords.txt", what='character')
        neg <- scan("negwords.txt", what='character')
        
        # get user media
        getUserMedia <- function(igname, token, n=100){
            
            # search for username and get user id
            content <- fromJSON(getURL(paste('https://api.instagram.com/v1/users/search?q=', 
                                             igname,'&access_token=', token, sep="")), 
                                unexpected.escape = "keep")
            userid <- as.numeric(content$data[[1]]$id)
            
            # get user media 
            content <- fromJSON(getURL(paste("https://api.instagram.com/v1/users/", 
                                             userid, "/media/recent?access_token=", 
                                             token, "&count=100", sep="")))
            
            # put text in data frame for the first 20 media
            df <- data.frame(no = 1:length(content$data))
            for(i in 1:length(content$data))
            {
                #text
                df$text[i] <- content$data[[i]]$caption$text
            }
            
            # iterate over the next 80 media to have 100  
            l <- length(content$data)
            next_url <- content$pagination$next_url
            while (l<n & length(content$data)>0 && (length(content$pagination)!=0) &&
                       !is.null(content$pagination['next_url'])){
                
                content <- fromJSON(getURL(next_url))
                l <- l + length(content$data)
                
                new.df <- data.frame(no = 1:length(content$data))
                for(i in 1:length(content$data))
                {
                    #text
                    new.df$text[i] <- content$data[[i]]$caption$text
                }
                
                df <- rbind(df, new.df)
            }
            
            return(data.frame(df$text))
        }
        
        user_media <- getUserMedia(igname, token = token)
        
        # function to split hashtags apart by uppercase letter
        hashgrep <- function(text) {
            hg <- function(text) {
                result <- ""
                while(text != result) {
                    result <- text
                    text <- gsub("#[[:alpha:]]+\\K([[:upper:]]+)", " \\1", 
                                 text, perl = TRUE)
                }
                return(text)
            }
            unname(sapply(text, hg))
        }
        
        # cleaning pipeline
        clean_ig <- data.frame(sapply(user_media$df.text, function(row) 
            iconv(row, "latin1", "ASCII", sub="")))
        names(clean_ig) <- "text"
        clean_ig <- clean_ig$text %>%
            gsub("&amp;", "", .) %>% # remove &
            gsub("@\\w+", "", .) %>% # remove at people
            #gsub("([[:upper:]][[:lower:]])", " \\1", .) %>% # split group of words at uppercase letter and add a white space
            hashgrep %>%
            gsub("[[:punct:]]", "", .) %>% # remove punctuation
            gsub("[[:digit:]]", "", .) %>% # remove digits
            gsub("http\\w+", "", .) %>% # remove html links
            iconv(from = "latin1", to = "ASCII", sub="") %>% # remove emoji and bizarre signs
            gsub("[ \t]{2,}", " ", .) %>% # remove unnecessary spaces
            gsub("^\\s+|\\s+$", "", .) %>% # remove unnecessary spaces
            subset(., . != "") %>% # remove empty lines
            tolower %>%
            data.frame(., stringsAsFactors=FALSE)
        names(clean_ig) <- "text"
        
        # determine polarity
        str_ig <- str_split(clean_ig$text, "\\s+")
        pos.matches <- lapply(str_ig, match, table = pos)
        neg.matches <- lapply(str_ig, match, table = neg)
        pos.matches <- lapply(pos.matches, function(row) !is.na(row))
        neg.matches <- lapply(neg.matches, function(row) !is.na(row))
        scores.pos <- lapply(pos.matches, function(row) length(pos.matches[row == TRUE]))
        scores.neg <- lapply(neg.matches, function(row) length(neg.matches[row == TRUE]))
        
        # sum scores to total score
        score <- sum(sapply(pos.matches, sum) - sapply(neg.matches, sum))
        score
        
    } else if (!is.null(twittername) & !is.null(igname)) {
        
        ################################ TWITTER ###################################
        
        # twitter OAuth, token = my_oauth
        load("twitterOauthSurvey.Rdata")
        consumer_key <- my_oauth$consumerKey
        consumer_secret <- my_oauth$consumerSecret
        access_token <- my_oauth$oauthKey
        access_secret <- my_oauth$oauthSecret
        setup_twitter_oauth(consumer_key = consumer_key, consumer_secret = consumer_secret,
                            access_token = access_token, access_secret = access_secret)
        1
        
        # lists with positive and negative words
        pos <- scan("poswords.txt", what='character')
        neg <- scan("negwords.txt", what='character')
        
        # get most recent tweets
        user_tweets <- userTimeline(twittername, n = 100, includeRts = TRUE)
        # get the text
        dirty_tweets <- twListToDF(user_tweets) 
        dirty_tweets <- data.frame(unlist(lapply(user_tweets, function(t)t$getText())))
        names(dirty_tweets) <- "text"
        
        # cleaning pipeline
        clean_tweets <- data.frame(sapply(dirty_tweets$text, function(row) 
            iconv(row, "latin1", "ASCII", sub="")))
        names(clean_tweets) <- "text"
        clean_tweets <- clean_tweets$text %>%
            gsub("&amp;", "", .) %>% # remove &
            gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", .) %>% # remove retweet entities
            gsub("@\\w+", "", .) %>% # remove at people
            #gsub("([[:upper:]][[:lower:]])", " \\1", .) %>% # split group of words at uppercase letter and add a white space
            hashgrep %>%
            gsub("[[:punct:]]", "", .) %>% # remove punctuation
            gsub("[[:digit:]]", "", .) %>% # remove digits
            gsub("http\\w+", "", .) %>% # remove html links
            iconv(from = "latin1", to = "ASCII", sub="") %>% # remove emoji and bizarre signs
            gsub("[ \t]{2,}", " ", .) %>% # remove unnecessary spaces
            gsub("^\\s+|\\s+$", "", .) %>% # remove unnecessary spaces
            subset(., . != "") %>% # remove empty lines
            tolower %>%
            data.frame(., stringsAsFactors=FALSE)
        names(clean_tweets) <- "text"
        
        
        ################## Compute polarity ################
        
        # determine polarity of tweets
        str_tweets <- str_split(clean_tweets$text, "\\s+")
        pos.matches <- lapply(str_tweets, match, table = pos)
        neg.matches <- lapply(str_tweets, match, table = neg)
        pos.matches <- lapply(pos.matches, function(row) !is.na(row))
        neg.matches <- lapply(neg.matches, function(row) !is.na(row))
        scores.pos <- lapply(pos.matches, function(row) length(pos.matches[row == TRUE]))
        scores.neg <- lapply(neg.matches, function(row) length(neg.matches[row == TRUE]))
        ## if statement zur überprüfung von polarität (<0 ist negativ, >0 ist positiv, = 0 ist neutral)
        
        # sum scores to total score
        twitterscore <- sum(sapply(pos.matches, sum) - sapply(neg.matches, sum))
        
        
        ############################ INSTAGRAM #####################################
        
        # instagram OAuth
        load("ig_oauth_ia")
        token <- ig_oauth_ia$token
        
        # get user media
        getUserMedia <- function(igname, token, n=100){
            
            # search for username and get user id
            content <- fromJSON(getURL(paste('https://api.instagram.com/v1/users/search?q=', 
                                             igname,'&access_token=', token, sep="")), 
                                unexpected.escape = "keep")
            userid <- as.numeric(content$data[[1]]$id)
            
            # get user media 
            content <- fromJSON(getURL(paste("https://api.instagram.com/v1/users/", 
                                             userid, "/media/recent?access_token=", 
                                             token, "&count=100", sep="")))
            
            # put text in data frame for the first 20 media
            df <- data.frame(no = 1:length(content$data))
            for(i in 1:length(content$data))
            {
                #text
                df$text[i] <- content$data[[i]]$caption$text
            }
            
            # iterate over the next 80 media to have 100  
            l <- length(content$data)
            next_url <- content$pagination$next_url
            while (l<n & length(content$data)>0 && (length(content$pagination)!=0) &&
                       !is.null(content$pagination['next_url'])){
                
                content <- fromJSON(getURL(next_url))
                l <- l + length(content$data)
                
                new.df <- data.frame(no = 1:length(content$data))
                for(i in 1:length(content$data))
                {
                    #text
                    new.df$text[i] <- content$data[[i]]$caption$text
                }
                
                df <- rbind(df, new.df)
            }
            
            return(data.frame(df$text))
        }
        
        user_media <- getUserMedia(igname, token = token)
        
        # function to split hashtags apart by uppercase letter
        hashgrep <- function(text) {
            hg <- function(text) {
                result <- ""
                while(text != result) {
                    result <- text
                    text <- gsub("#[[:alpha:]]+\\K([[:upper:]]+)", " \\1", 
                                 text, perl = TRUE)
                }
                return(text)
            }
            unname(sapply(text, hg))
        }
        
        # cleaning pipeline
        clean_ig <- data.frame(sapply(user_media$df.text, function(row) 
            iconv(row, "latin1", "ASCII", sub="")))
        names(clean_ig) <- "text"
        clean_ig <- clean_ig$text %>%
            gsub("&amp;", "", .) %>% # remove &
            gsub("@\\w+", "", .) %>% # remove at people
            #gsub("([[:upper:]][[:lower:]])", " \\1", .) %>% # split group of words at uppercase letter and add a white space
            hashgrep %>%
            gsub("[[:punct:]]", "", .) %>% # remove punctuation
            gsub("[[:digit:]]", "", .) %>% # remove digits
            gsub("http\\w+", "", .) %>% # remove html links
            iconv(from = "latin1", to = "ASCII", sub="") %>% # remove emoji and bizarre signs
            gsub("[ \t]{2,}", " ", .) %>% # remove unnecessary spaces
            gsub("^\\s+|\\s+$", "", .) %>% # remove unnecessary spaces
            subset(., . != "") %>% # remove empty lines
            tolower %>%
            data.frame(., stringsAsFactors=FALSE)
        names(clean_ig) <- "text"
        
        # determine polarity
        str_ig <- str_split(clean_ig$text, "\\s+")
        pos.matches <- lapply(str_ig, match, table = pos)
        neg.matches <- lapply(str_ig, match, table = neg)
        pos.matches <- lapply(pos.matches, function(row) !is.na(row))
        neg.matches <- lapply(neg.matches, function(row) !is.na(row))
        scores.pos <- lapply(pos.matches, function(row) length(pos.matches[row == TRUE]))
        scores.neg <- lapply(neg.matches, function(row) length(neg.matches[row == TRUE]))
        
        # sum scores to total score
        igscore <- sum(sapply(pos.matches, sum) - sapply(neg.matches, sum))
        
        # sum up twitter score & ig score
        score <- (igscore + twitterscore) / 2
        score
        
    } else 
        print("Sorry, we can´t provide feedback as you didn´t indicate any social media profile")
    
}

# try it out
score <- getScore(twittername = "parishilton", igname = "parishilton")
score2 <- getScore(twittername = "parishilton")
score3 <- getScore(igname = "parishilton")

library(plumber)
r <- plumb("evaluation2.R")
r$run(port=8000, host="0.0.0.0")

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
