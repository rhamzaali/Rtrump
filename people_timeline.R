require("rJava")
require("NLP")
require("openNLP")
require("magrittr")
require("twitteR")
require("tm")
require("googleVis")
require("jsonlite")

install.packages("openNLPmodels.en",repos="http://datacube.wu.ac.at/")


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
pos_ann = Maxent_POS_Tag_Annotator()

person_ann = Maxent_Entity_Annotator(kind = "person")
location_ann = Maxent_Entity_Annotator(kind = "location")
date_ann = Maxent_Entity_Annotator(kind = "date")

df = trump_no_retweets

result <- list(nrow(df))
sapply(1:nrow(df), function(i){
  message(paste(toString(nrow(df)), "/", toString(i)))
  row = df[i,]
  text_annotation = annotate(row["text"],list(sent_ann,word_ann,person_ann,location_ann))
  text_doc = AnnotatedPlainTextDocument(row["text"],text_annotation)
  
  result[[i]] <<- list(
    people = strsplit(toString(entities(text_doc,"person")),",")[[1]],
    location = strsplit(toString(entities(text_doc,"location")),",")[[1]],
    id_str = row[["id_str"]],
    text =  row[["text"]],
    created_at = row[["created_at"]])
})
write(toJSON(result),file="trump_df_no_retweets.json")


