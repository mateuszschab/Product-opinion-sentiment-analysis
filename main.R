#Magazine_Subscriptions.json
#opinions.json
library(plyr)
library(igraph)
library(sentimentr)
library(ndjson)
library(lexicon)
library(ggplot2)
library(dplyr)


setwd("Current_folder")


# Wyznaczyæ œrednie wartoœci sentymentu dla wartoœci overal reView 1,2,3,4,5

df = stream_in("Magazine_Subscriptions.json")
head(df)
sentiment=sentiment_by(df$reviewText,by=df$overall)
View(sentiment)

#-------------------------------------------------------------------------
#  Wyznaczyæ œrednie wartoœci sentymentu dla wartoœci overal reView 1,2,3,4,5 dla ró¿nych
# s³owników (3) i parametrów valence_shifters (3) i porównaæ ró¿nice.

text1 <-c(
  'I do not like it.',
  'I really like it.',
  'I hardly like it.')

text2 <-c(
  'Not much of a fan',
  'be keen on',
  'It’s not my thing',
  'have a soft spot for',
  'It drives me crazy')


dict = c("lexicon::hash_sentiment_jockers_rinker",
              "lexicon::hash_sentiment_jockers",
              "lexicon::emojis_sentiment")

# Dla s³ownika 1
for (lex in 1: 3)
{
sent = sentiment(df$reviewText, polarity_dt = lexicon::hash_sentiment_jockers_rinker,
                 valence_shifters_dt = lexicon::hash_valence_shifters[y==lex], hyphen = "", amplifier.weight = 0.8,
                 n.before = 5, n.after = 2, question.weight = 1, 
          adversative.weight = 0.25, neutral.nonverb.like = FALSE,
          missing_value = 0)
sentyment = sentiment_by(sent,by=df$overall)

if (lex==1)
{
  dic1 = sentyment
}
else
{
  sentyment = sentyment[,-2]
  dic1 <- merge(dic1,sentyment,by.x="overall",by.y="overall")
  
}
}
  # Dla s³ownika 2
for (lex in 1: 3)
{
  sent = sentiment(df$reviewText, polarity_dt =  lexicon::hash_sentiment_jockers,
                   valence_shifters_dt = lexicon::hash_valence_shifters[y==lex], hyphen = "", amplifier.weight = 0.8,
                   n.before = 5, n.after = 2, question.weight = 1, 
                   adversative.weight = 0.25, neutral.nonverb.like = FALSE,
                   missing_value = 0)
  sentyment = sentiment_by(sent,by=df$overall)
  
  if (lex==1)
  {
    dic2 = sentyment
  }
  else
  {
    sentyment = sentyment[,-2]
    dic2 <- merge(dic2,sentyment,by.x="overall",by.y="overall")
    
  }
}
  # Dla s³ownika 3
for (lex in 1: 3)
{
  sent = sentiment(df$reviewText,  polarity_dt = lexicon::hash_sentiment_nrc,
                   valence_shifters_dt = lexicon::hash_valence_shifters[y==lex], hyphen = "", amplifier.weight = 0.8,
                   n.before = 5, n.after = 2, question.weight = 1, 
                   adversative.weight = 0.25, neutral.nonverb.like = FALSE,
                   missing_value = 0)
  sentyment = sentiment_by(sent,by=df$overall)
  
  if (lex==1)
  {
    dic3 = sentyment
  }
  else
  {
    sentyment = sentyment[,-2]
    dic3 <- merge(dic3,sentyment,by.x="overall",by.y="overall")
    
  }
}


View(dic1)
View(dic2)
View(dic3)


#-------------------------------------------------------------------------
#Porównaæ wartoœci sentymentu,
#histogramy dla dwóch wybranych, konkurencyjnych produktów

produkt1 = df[asin=='B00005N7P0']
produkt2 = df[asin=='B00005N7PS']

sent1=sentiment_by(produkt1$reviewText,by=df$overall)
sent1=sentiment(produkt1$reviewText, emotion_dt = lexicon::hash_sentiment_sentiword)
View(sent1)
qplot(sent1$sentiment, geom="histogram",binwidth=0.1,main="Histogram")

sent2=sentiment_by(produkt2$reviewText,by=df$overall)
sent2=sentiment(produkt2$reviewText, emotion_dt = lexicon::hash_sentiment_sentiword)
View(sent2)
qplot(sent2$sentiment, geom="histogram",binwidth=0.1,main="Histogram")

#------------------------------------------------------------------------
#Wyznaczyæ udzia³ poszczególnych emocji w zbiorze komentarzy
#dla dwóch wybranych produktów

produkt1 = df[asin=='B00005N7P0']
produkt2 = df[asin=='B00005N7PS']

em <- emotion(produkt1$reviewText) 
View(em)
em <- emotion_by(produkt1$dialogue,by=list( produkt1$reviewerID,produkt1$reviewTime))
plot(em)

em <- emotion(produkt2$reviewText) 
View(em)
em <- emotion_by(produkt2$dialogue,by=list( produkt2$reviewerID,produkt2$reviewTime))
plot(em)
#------------------------------------------------------------------------



produkt1 = df[asin=='B00005N7PN']
# Select data 2017
produkt1 = produkt1[39:141]

for (i in 1: nrow(produkt1))
{

temp = produkt1[i,reviewTime]
new_value = strtrim(temp,2)

produkt1$reviewTime[i] <- new_value
#produkt1 <- replace(produkt1[i],produkt1$reviewTime,new_value)
rm(temp,new_value)
}


sentTime=sentiment_by(produkt1$reviewText,by=produkt1$reviewTime)

View(sentTime)
plot(sentTime)




