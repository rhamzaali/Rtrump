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
require('sqldf')

# connect to twitter api application
my_api_key <- "sWJkLwgYL5fhIc6x6LZVkX5VD"
my_api_secret <- "hOfDdRhJcdVf8MRZpfjlYxNwBgfFKR18tVWLp7CD5Hhqk2L20U"
my_access_token <- "355668182-JGPRgBxyNtoalUvorC7ki73hNRWc7p2dLHkfTJn1" # avoids the browser authentication limbo
my_access_token_secret <- "UwvQHkMANm9D9P7wyNCvGxxBlB0c5qw2w5o21NGnOXBeq" # avoids the browser authentication limbo

setup_twitter_oauth(my_api_key, my_api_secret,my_access_token, my_access_token_secret)

#############################################################################################
# get most recent tweets after election day
tl <- userTimeline('realdonaldtrump', n=3200) # will only bring recent tweets
df <- do.call(rbind, lapply(tl, function(x) x$toDataFrame())) # convert timelinedata to dataframe
df$date <- as.Date( as.character(df$created), "%Y-%m-%d %H:%M:%S")
after <- subset(df,df$date > as.Date("2016-11-08"))


# load old data including and before election day since december 2015
load("trump_tweets_df.rda")
old <- trump_tweets_df
old$date <- as.Date( as.character(old$created), "%Y-%m-%d %H:%M:%S")

#get old data from newer data and append to before election data frame
temp <-subset(df,df$date < as.Date("2016-11-08"))
old <-rbind(old,temp)

before <- subset(old,old$date < as.Date("2016-11-08"))


temp$date

#combine the data to get some other statistics
all <- rbind(before,after)
#############################################################################################




#############################################################################################
#tweet hours changes (before, after)
bfTime <- before$created
afTime <- after$created

bfTime <- strptime(bfTime, "%Y-%m-%d %H:%M:%S")
afTime <- strptime(afTime, "%Y-%m-%d %H:%M:%S")

bfTime <- hour(bfTime)
afTime <- hour(afTime)

bfHours <- as.data.frame(table(bfTime))
afHours <- as.data.frame(table(afTime))

totalHours <-merge(bfHours,afHours, by.x= c("bfTime"),by.y = c("afTime") )
totalHours <- totalHours[order(totalHours$bfTime),]

# plot both timings
xyplot( totalHours$Freq.x + totalHours$Freq.y ~ totalHours$bfTime, data=totalHours,
        grid = TRUE,type = "b",xlab = "Hours",ylab = "# tweets") #,panel = panel.smoothScatter)
#############################################################################################




df$month <- format(as.Date(df$date),"%Y-%m")

after$month <- format(as.Date(after$date), "%Y-%m")
all$yearmonth <- format(as.Date(all$date), "%Y-%m")

allData <- all



fav<-sqldf('SELECT yearmonth, AVG(favoriteCount) AS avgFav FROM allData GROUP BY yearmonth')


rt<-sqldf('SELECT yearmonth, AVG(retweetCount) AS avgRT FROM allData GROUP BY yearmonth')


barplot(fav$avgFav, names.arg=fav$yearmonth, ylim=c(0,90000),xlab = "months",ylab = "avg favorited Tweets")



barplot(rt$avgRT, names.arg=fav$yearmonth, ylim=c(0,30000),xlab = "months",ylab = "avg RT'd Tweets")



#############################################################################################


chinatweets <- allData[grep("China",allData$text),]
chinatweets[1]


hillstweets <- allData[grep("Hillary",allData$text),]
hillstweets


fav<-sqldf('SELECT yearmonth, COUNT(*) AS avgFav FROM hillstweets GROUP BY yearmonth')
# 324/2125 15.2%


barplot(fav$avgFav, names.arg=fav$yearmonth, ylim=c(0,200),xlab = "months",ylab = "tweets with Hillary mentioned")

sample <- allData[sample(nrow(allData),150),]

sampleHills <- sample[grep("Hillary",sample$text),]


totalavg = 0
for(i in c(1:10))
{
  sample <- allData[sample(nrow(allData),150),]
  sampleHills <- sample[grep("Hillary",sample$text),]
  sampleCount<-sqldf('SELECT yearmonth, COUNT(*) AS avgFav FROM sampleHills GROUP BY yearmonth')
  totalavg = totalavg + (sum(sampleCount$avgFav)/150)
}
print(totalavg/10)
# 21/150 14%


#############################################################################################


job <- allData[grep("!",allData$text),]

length(job$text)



fav<-sqldf('SELECT yearmonth, COUNT(*) AS avgFav FROM hillstweets GROUP BY yearmonth')
# 324/2125 15.2%






