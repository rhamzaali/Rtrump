require("rJava")
require("NLP")
require("openNLP")
require("magrittr")
require("twitteR")
require("tm")
require("googleVis")
require("rjson")
require("plyr")

trump_2009 <- fromJSON(paste(readLines("http://trumptwitterarchive.com/data/realdonaldtrump/2009.json"), collapse=""))
trump_2010 <- fromJSON(paste(readLines("http://trumptwitterarchive.com/data/realdonaldtrump/2010.json"), collapse=""))
trump_2011 <- fromJSON(paste(readLines("http://trumptwitterarchive.com/data/realdonaldtrump/2011.json"), collapse=""))
trump_2012 <- fromJSON(paste(readLines("http://trumptwitterarchive.com/data/realdonaldtrump/2012.json"), collapse=""))
trump_2013 <- fromJSON(paste(readLines("http://trumptwitterarchive.com/data/realdonaldtrump/2013.json"), collapse=""))
trump_2014 <- fromJSON(paste(readLines("http://trumptwitterarchive.com/data/realdonaldtrump/2014.json"), collapse=""))
trump_2015 <- fromJSON(paste(readLines("http://trumptwitterarchive.com/data/realdonaldtrump/2015.json"), collapse=""))
trump_2016 <- fromJSON(paste(readLines("http://trumptwitterarchive.com/data/realdonaldtrump/2016.json"), collapse=""))
trump_2017 <- fromJSON(paste(readLines("http://trumptwitterarchive.com/data/realdonaldtrump/2017.json"), collapse=""))

# combine all the json blobs
trump_combine <- trump_2009
trump_combine <- append(trump_combine,trump_2010)
trump_combine <- append(trump_combine,trump_2011)
trump_combine <- append(trump_combine,trump_2012)
trump_combine <- append(trump_combine,trump_2013)
trump_combine <- append(trump_combine,trump_2014)
trump_combine <- append(trump_combine,trump_2015)
trump_combine <- append(trump_combine,trump_2016)
trump_combine <- append(trump_combine,trump_2017)

trump_df <- do.call(rbind.data.frame, trump_combine) # convert timelinedata to dataframe
trump_no_retweets = trump_df_android <- subset(trump_df,is_retweet == FALSE)

trump_df_android <- subset(trump_no_retweets,source == "Twitter for Android")
trump_df_iphone <- subset(trump_no_retweets,source == "Twitter for iPhone")


entities = function(doc,kind)
{
  s = doc$content
  a = annotations(doc)[[1]]
  if (hasArg(kind))
  {
    k = sapply(a$features,'[[',"kind")
    s[a[k == kind]]
  }
  else
  {
    s[a[a$type == "entity"]]
  }
}

word_ann = Maxent_Word_Token_Annotator()
sent_ann = Maxent_Sent_Token_Annotator()

person_ann = Maxent_Entity_Annotator(kind = "person")
location_ann = Maxent_Entity_Annotator(kind = "location")
date_ann = Maxent_Entity_Annotator(kind = "date")

#write(toJSON(unname(split(trump_df_android, 1:nrow(trump_df_android)))), file = "trump_android.json")


#df.tweets <- trump_df_android
df.people <- data.frame(id=numeric(),people=character()) 

tweet_sentemential = function(tweets)
{
  people <- data.frame(id=numeric(),people=character())
  for(t in tweets)
  {
    text_annotation = annotate(t["text"],list(sent_ann,word_ann,person_ann,location_ann))
    for(p_entry in entities(AnnotatedPlainTextDocument(t["text"],text_annotation),"person"))
    {
      people =rbind(tweets.people, data.frame(id=t["id_str"],people=p_entry))
    }
    
    #x["person"] <-  as.String(entities(AnnotatedPlainTextDocument(x["text"],text_annotation),"person"))
    #x["location"] <- as.String(entities(AnnotatedPlainTextDocument(x["text"],text_annotation),"location"))
  }
  return(people)
}



trump_df_android.tweet <- tweet_sentemential(trump_df_iphone)

#x["person"] <-  as.String()
#x["location"] <- as.String(entities(AnnotatedPlainTextDocument(x["text"],text_annotation),"location"))
df = trump_df[1:10,1:5]

df.people <- data.frame(id=numeric(),people=character()) 
apply(df,1,function(x){
  text_annotation = annotate(x["text"],list(sent_ann,word_ann,person_ann,location_ann))
  for(peeps in entities(AnnotatedPlainTextDocument(x["text"],text_annotation),"person"))
  {
    message(x["id_str"])
    df.people = rbind(df.people, data.frame(id=x["id_str"],people=peeps))
  }
})
  





