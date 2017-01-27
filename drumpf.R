require("twitteR")
require("tm")
require("wordcloud")
require("stringi")
require('lubridate')
require('ggplot2')
require("tidyr")
require("dplyr")
require('reshape')
require('lattice')

# connect to twitter api application
my_api_key <- "sWJkLwgYL5fhIc6x6LZVkX5VD"
my_api_secret <- "hOfDdRhJcdVf8MRZpfjlYxNwBgfFKR18tVWLp7CD5Hhqk2L20U"
my_access_token <- "355668182-JGPRgBxyNtoalUvorC7ki73hNRWc7p2dLHkfTJn1" # avoids the browser authentication limbo
my_access_token_secret <- "UwvQHkMANm9D9P7wyNCvGxxBlB0c5qw2w5o21NGnOXBeq" # avoids the browser authentication limbo

setup_twitter_oauth(my_api_key, my_api_secret,my_access_token, my_access_token_secret)


tl <- userTimeline('realdonaldtrump', n=3000) # will only bring recent tweets
df <- do.call(rbind, lapply(tl, function(x) x$toDataFrame())) # convert timelinedata to dataframe

#save(df,file="20170105.Rda")

#load("20170105.Rda")


df[1] <- lapply(df[1], gsub, pattern = "&amp;", replacement = "", fixed = TRUE) 
df[1] <- lapply(df[1], gsub, pattern = "\n", replacement = " ", fixed = TRUE)


android <- df[df$statusSource== "<a href=\"http://twitter.com/download/android\" rel=\"nofollow\">Twitter for Android</a>",]
ios <- df[df$statusSource== "<a href=\"http://twitter.com/download/iphone\" rel=\"nofollow\">Twitter for iPhone</a>",]

## finding average fav
aFav <- (android$favoriteCount)
iFav <- (ios$favoriteCount)
mean(aFav)
mean(iFav)

## finding average rt
aRT <- (android$retweetCount)
iRT <- (ios$retweetCount)
mean(aRT)
mean(iRT)

## finding tweet hours
aTime <- android$created
iTime <- ios$created

aTime <- strptime(aTime, "%Y-%m-%d %H:%M:%S")
iTime <- strptime(iTime, "%Y-%m-%d %H:%M:%S")

aTime <- hour(aTime)
iTime <- hour(iTime)

aHours <- as.data.frame(table(aTime))
iHours <- as.data.frame(table(iTime))

totalHours <-merge(aHours,iHours, by.x= c("aTime"),by.y = c("iTime") )
totalHours <- totalHours[order(totalHours$aTime),]

# plot both timings
xyplot( totalHours$Freq.x + totalHours$Freq.y ~ totalHours$aTime, data=totalHours,
        grid = TRUE,type = "b",xlab = "Hours",ylab = "# tweets") #,panel = panel.smoothScatter)

# get tweet text
aText <- android$text
iText <- ios$text

# clean up
aText = gsub("https\\w+", "", aText)
aText = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", aText)
aText = gsub("@\\w+", "", aText)
aText = gsub("[[:punct:]]", " ", aText)
aText = gsub("[[:digit:]]", "", aText)

iText = gsub("http\\w+", "", iText)
iText = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", iText)
iText = gsub("@\\w+", "", iText)
iText = gsub("[[:punct:]]", " ", iText)
iText = gsub("[[:digit:]]", "", iText)

aCorpus <- Corpus(VectorSource(aText))
aCorpus <- tm_map(aCorpus,tolower)
aCorpus = tm_map(aCorpus, removeWords, c(stopwords("english"),"will","https","trump"))
aCorpus = tm_map(aCorpus, stripWhitespace)

iCorpus <- Corpus(VectorSource(iText))
iCorpus <- tm_map(iCorpus,tolower)
iCorpus = tm_map(iCorpus, removeWords, c(stopwords("english"),"will","https","trump"))
iCorpus = tm_map(iCorpus, stripWhitespace)

aWordCloud <- tm_map(aCorpus, PlainTextDocument) #convert the data into a plain txt document (removes special attributes)
iWordCloud <- tm_map(iCorpus, PlainTextDocument) #convert the data into a plain txt document (removes special attributes)

# wordclouds
pal2 <- brewer.pal(8,"Dark2")
wordcloud(aWordCloud,max.words = 150, random.order = FALSE,colors=pal2) #display word cloud
wordcloud(iWordCloud,max.words = 150, random.order = FALSE,colors=pal2) #display word cloud


