require("rJava")
require("NLP")
require("openNLP")
require("magrittr")
require("twitteR")
require("tm")
require("googleVis")
require("jsonlite")
library(tm)
require("wordcloud")
install.packages("openNLPmodels.en",repos="http://datacube.wu.ac.at/")

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

df = trump_df_android

result <- list(nrow(df))
people <- Corpus(VectorSource(""))


sapply(1:nrow(df), function(i){
  message(paste(toString(nrow(df)), "/", toString(i)))
  row = df[i,]
  text_annotation = annotate(row["text"],list(sent_ann,word_ann,person_ann,location_ann))
  text_doc = AnnotatedPlainTextDocument(row["text"],text_annotation)
  
  crp = Corpus(VectorSource(strsplit(toString(entities(text_doc,"person")),",")[[1]]))
  people <<- c(people,crp)
 # result[[i]] <<- list(
#    people = strsplit(toString(entities(text_doc,"person")),",")[[1]],
#    location = strsplit(toString(entities(text_doc,"location")),",")[[1]],
#    id_str = row[["id_str"]],
#    text =  row[["text"]],
#    created_at = row[["created_at"]])
})
people <- tm_map(people, tolower)
people <- tm_map(people, removeWords, c("donald","trump", "donald trump"),lazy=TRUE)

peopleCloud <- tm_map(people, PlainTextDocument,lazy=TRUE)
pal2 <- brewer.pal(8,"Dark2")
wordcloud(peopleCloud, random.order = FALSE,colors=pal2)

write(toJSON(result),file="trump_df_no_retweets.json")


