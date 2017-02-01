require("jsonlite")

trump_2009 <- fromJSON(paste(readLines("http://trumptwitterarchive.com/data/realdonaldtrump/2009.json"), collapse=""))
trump_2010 <- fromJSON(paste(readLines("http://trumptwitterarchive.com/data/realdonaldtrump/2010.json"), collapse=""))
trump_2011 <- fromJSON(paste(readLines("http://trumptwitterarchive.com/data/realdonaldtrump/2011.json"), collapse=""))
trump_2012 <- fromJSON(paste(readLines("http://trumptwitterarchive.com/data/realdonaldtrump/2012.json"), collapse=""))
trump_2013 <- fromJSON(paste(readLines("http://trumptwitterarchive.com/data/realdonaldtrump/2013.json"), collapse=""))
trump_2014 <- fromJSON(paste(readLines("http://trumptwitterarchive.com/data/realdonaldtrump/2014.json"), collapse=""))
trump_2015 <- fromJSON(paste(readLines("http://trumptwitterarchive.com/data/realdonaldtrump/2015.json"), collapse=""))
trump_2016 <- fromJSON(paste(readLines("http://trumptwitterarchive.com/data/realdonaldtrump/2016.json"), collapse=""))
trump_2017 <- fromJSON(paste(readLines("http://trumptwitterarchive.com/data/realdonaldtrump/2017.json"), collapse=""))

trump_df <- rbind.pages(
  list(trump_2009, 
       trump_2010, 
       trump_2011,
       trump_2012,
       trump_2013,
       trump_2014,
       trump_2015,
       trump_2016,
       trump_2017)
)


trump_no_retweets = trump_df_android <- subset(trump_df,is_retweet == FALSE)
trump_df_android <- subset(trump_no_retweets,source == "Twitter for Android")
trump_df_iphone <- subset(trump_no_retweets,source == "Twitter for iPhone")
