#OBTENCIÓN Y PROCESAMIENTO INICIAL DE LOS DATOS

#Descargamos las librerías necesarias

install.packages("academictwitteR")
install.packages("tidyr")
install.packages("ggplot2")

library(academictwitteR)
library(tidyverse)

library(data.table)
library(textclean)
library(tm)
library(stringr)
library(dplyr)
library(academictwitteR)
library(tidyr)
library(rtweet)
library(ggplot2)
set_bearer()

#Tenemos que añadir un bearer que es la clave para poder hacer la descarga desde el API de los datos, esta clave es privada y única para cada administrador 
#Reiniciamos R
Session --> Restart R

#Obtenemos y guardamos los datos seleccionando el periodo y el idioma que queremos obtener
tweets <-
  get_all_tweets(
    query = c("shein"),
    #users = c("Shein_oficial", "Shein"),
    start_tweets = "2017-01-01T00:00:00Z",
    end_tweets = "2022-11-22T00:00:00Z",
    bearer_token = get_bearer(),
    bind_tweets=FALSE,
    country = "ES", 
    #lang = "es",
    #bbox= c(-3.838321,40.298529,-3.479205,40.518644),
    file = "tweets_shein",
    data_path="data_complete_shein/",
    n = 1000000,
  )

tweets_shein <- bind_tweets(data_path = "data_complete_shein/")

#Nos quedamos solo con las columnas que necesitemos 
df<-data.frame(author_id=tweets_shein$author_id, text=tweets_shein$text, fecha=tweets_shein$created_at, lang=tweets_shein$lang, retweet_number=tweets_shein$public_metrics$retweet_count, reply_number=tweets_shein$public_metrics$reply_count,like_number=tweets_shein$public_metrics$like_count)
df$hashtags=lapply(tweets_shein$entities$hashtags, function(x) if (length(x) == 0) NA else  toString(unlist(`[`(x, c('tag')))) )
df$users=lapply(tweets_shein$entities$mentions, function(x) if (length(x) == 0) NA else  toString(unlist(`[`(x, c('username')))) )
df$fecha <- as.Date(df$fecha, format = "%Y-%m-%d")
df$year <- format(df$fecha, "%Y")
df$month <- format(df$fecha, "%m")

#Descripción de los datos 
sapply(df, class)
sapply(tweets_shein,class)

#Estudiamos diferentes aspectos de los datos 

install.packages("dplyr")
library(dplyr)

#Por ejemplo el número de tweets por fecha y por idioma
number_tweets_bydate<-df %>% 
  group_by(fecha) %>% 
  summarise(count = n())

ggplot(data = number_tweets_bydate, aes(x = fecha, y = count, fill=fecha)) +
  geom_col() +
  xlab("Fecha") +
  ylab("Número de Tweets") +
  ggtitle("Número de Tweets por fecha")
scale_fill_brewer(type = "qual", palette = "Set1")

number_tweets_bylang<-df %>% 
  group_by(lang) %>% 
  summarise(count = n())

#Obtenemos un listado con los hashtahs 

all_hashtags <- unlist( # flattten word list from individual strings into one vector
  regmatches(df$hashtags,  gregexpr('\\w+', df$hashtags)))

freq_count <- as.data.frame(table(all_hashtags))

freq_count<-freq_count[order(freq_count$Freq, decreasing = TRUE),]
total_different_hashtags <- nrow(freq_count)

#Para quitar los que contienen shein
freq_count <- freq_count[!grepl("\\bShein\\b|\\bshein\\b|\\bSHEIN\\b", freq_count$all_hashtags),]

#nos quedamos con los 6 principales (mayor frecuencia)
top10<-head(freq_count,10)

# Frecuencias de emojis
devtools::install_github("hadley/emo")

library(emo)
emojis<-df %>%
  mutate(emoji = ji_extract_all(text)) %>%
  unnest(cols = c(emoji)) %>%
  count(emoji, sort = TRUE) %>%
  top_n(20)

#Podemos guardar los datos en csv para tener el dataset completo

df <- apply(df,2,as.character)
write.csv2(df,"df_completo.csv", row.names = FALSE)
df<-as.data.frame(df)


#Hacemos un primer preprocesamiento

custom_rules <- "ñ > \\~;
                 Ñ > \\^;
                 ::Latin-ASCII;
                 \\~ > ñ;
                 \\^ > Ñ"

#Eliminamos stopwords
library(tm)
stopw<- stopwords("spanish")
stop<- c(stopw,"shein","sera") # a?adimos las stopwords que se hayn pasado y la palabra shein porque la mayoria de tweets tienen esta palabra



clean <- function(x){
  if (!(is.na(x))){x <- gsub("http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+", "", x)} #Remove URLs
  if (!(is.na(x))){x <- gsub("@\\w+", "", x)} #Remove mentions}
  if (!(is.na(x))){x <- gsub("#\\w+", "", x)} #Remove hashtags}
  if (!(is.na(x))){x <- gsub("\\d+\\w*\\d*", "", x)}
  
  if (!(is.na(x))){x<- stringi::stri_trans_general(x, id = custom_rules, rules = TRUE)}
  if (!(is.na(x))){x <- gsub("[[:punct:]]", " ", x) } #Remove punctuation
  if (!(is.na(x))){x <-tolower(x)}
  if (!(is.na(x))){x <-iconv(x, "latin1", "ASCII", sub="")}
  
  if (!(is.na(x))){x <-removeNumbers(x)}
  if (!(is.na(x))){ x <-removeWords(x,stop)}
  if (!(is.na(x))){x <-gsub('\\b+RT', '', x)} ## Remove RT}
  
  if (!(is.na(x))){x <- gsub('\\b\\w{1,2}\\s','',x)}
  if (!(is.na(x))){x <- str_replace(gsub("\\s+", " ", str_trim(x)), "B", "b")} #multiple spaces to single space
  
  return(x)}

df$cleaned_text<-clean(df$text)
length(df$cleaned_text)
df_filtered<-df[!(is.na(df$cleaned_text) | df$cleaned_text==""), ]

#Guardamos los datos filtrados habiendo quitado las stopwords 

drop <- c("hashtags","users")
df_filtered = df_filtered[,!(names(df_filtered) %in% drop)]
write.csv2(df_filtered,"df_filtered2.csv", row.names = FALSE)
