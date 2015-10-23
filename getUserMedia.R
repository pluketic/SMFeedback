getUserMedia <-
function(igname, token, n=100){
            
            # search for username and get user id
            content <- fromJSON(getURL(paste('https://api.instagram.com/v1/users/search?q=', 
                                             igname,'&access_token=', token, sep="")), 
                                unexpected.escape = "keep")
            userid <- as.numeric(content$data[[1]][[3]])
            
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
            next_url <- content$pagination[[1]]
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
