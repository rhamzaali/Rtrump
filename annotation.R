require(rJava)
require(NLP)
require(openNLP)
require(magrittr)
require("twitteR")
require("tm")
require("googleVis")

#install.packages("openNLPmodels.en",repos="http://datacube.wu.ac.at/")


# connect to twitter api application
my_api_key <- "sWJkLwgYL5fhIc6x6LZVkX5VD"
my_api_secret <- "hOfDdRhJcdVf8MRZpfjlYxNwBgfFKR18tVWLp7CD5Hhqk2L20U"
my_access_token <- "355668182-JGPRgBxyNtoalUvorC7ki73hNRWc7p2dLHkfTJn1" # avoids the browser authentication limbo
my_access_token_secret <- "UwvQHkMANm9D9P7wyNCvGxxBlB0c5qw2w5o21NGnOXBeq" # avoids the browser authentication limbo

setup_twitter_oauth(my_api_key, my_api_secret,my_access_token, my_access_token_secret)


tl <- userTimeline('realdonaldtrump', n=50) # will only bring recent tweets
df <- do.call(rbind, lapply(tl, function(x) x$toDataFrame())) # convert timelinedata to dataframe

text = as.String(df$text)


word_ann = Maxent_Word_Token_Annotator()
sent_ann = Maxent_Sent_Token_Annotator()
pos_ann = Maxent_POS_Tag_Annotator()

pos_annotation = annotate(text, list(sent_ann,word_ann,pos_ann))
text_annotation = annotate(text,list(sent_ann,word_ann))
head(text_annotation)

text_doc = AnnotatedPlainTextDocument(text,text_annotation)
words(text_doc) %>% head(10)


person_ann = Maxent_Entity_Annotator(kind = "person")
location_ann = Maxent_Entity_Annotator(kind = "location")
date_ann = Maxent_Entity_Annotator(kind = "date")


pipeline = list(sent_ann,word_ann,person_ann,location_ann,date_ann)

text_annotation = annotate(text,pipeline)
text_doc = AnnotatedPlainTextDocument(text,text_annotation)
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

#entities(text_doc, kind = "person")
#entities(text_doc, kind = "location")
#entities(text_doc, kind = "date")

dframe = data.frame(table(entities(text_doc, kind = "location")))
ctype = gvisColumnChart(dframe)
plot(ctype)

