cleanPosts <-
function(df) {
    clean_posts <- data.frame(text = sapply(df$text, function(row) 
        iconv(row, "latin1", "ASCII", sub="")))
    clean_posts <- clean_posts$text %>%
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
        data.frame(text = ., stringsAsFactors=FALSE)
    return(clean_posts)
}
