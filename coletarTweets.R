

#instalar
#install.packages("tm")
#install.packages("wordcloud")
#install.packages("RColorBrewer")
#install.packages("rtweet")
#install.packages("stringr")
#install.packages("dplyr")
#install.packages("SnowballC")

#carregar
library("tm")
library("wordcloud")
library("RColorBrewer")
library("rtweet")
library("stringr")
library("dplyr")
library("SnowballC")
library("rvest")
library("jsonlite")


getOldTweets= function(termoBusca, liguagem, dtInicio, dtFim) {
  startdate =  dtInicio
  enddate = dtFim
  language = liguagem
  searchTerm <- termoBusca
  searchbox <- URLencode(searchTerm)
  
  
  #converte para url
  temp_url <- paste0("https://twitter.com/i/search/timeline?f=tweets&q=",searchbox,"%20since%3A",startdate,"%20until%3A",enddate,"&l=",language,"&src=typd&max_position=")
  #temp_url <- paste0("https://api.twitter.com/1.1/search/tweets.json?q=%20",searchbox,"%20since%3A",startdate,"%20until%3A",enddate,"&l=",language,"&src=typd&max_position=")
  webpage <-fromJSON(temp_url)
  
  
  if(webpage$new_latent_count>0){
    tweet_ids <- read_html(webpage$items_html) %>% html_nodes('.js-stream-tweet') %>% html_attr('data-tweet-id')
    
    while (webpage$has_more_items == T) {
      tryCatch({
        min_position <- webpage$min_position
        next_url <- paste0(temp_url, min_position)
        webpage <- fromJSON(next_url)
        next_tweet_ids <- read_html(webpage$items_html) %>% html_nodes('.js-stream-tweet') %>% html_attr('data-tweet-id')
        next_tweet_ids <- next_tweet_ids[!is.na(next_tweet_ids)]
        tweet_ids <- unique(c(tweet_ids,next_tweet_ids))
      },
      error=function(cond) {
        message(paste("URL does not seem to exist:", next_url))
        message("Here's the original error message:")
        message(cond)
      })
    }
    tweets <- lookup_tweets(tweet_ids, parse = TRUE, token = NULL)
  } else {
    paste0("Nao existem tweets sobre o termo da busca!")
  }
  
  return(tweets)
}


incrementarDias = function (data, qtdDias) {
  ano= strtoi(str_sub(data, end=4), 10)
  mes= strtoi(str_sub(data, start= 6, end=7), 10)
  dia= strtoi(str_sub(data, start=9), 10)
  
  m1= c(1,3,5,7,8,10,12)
  m2= c(4,6,9,11)
  
  if(mes %in% m1) {
    #ate dia 31
    if((dia + qtdDias) > 31) {
      mes= mes + 1
      dia= (dia + qtdDias) - 31
    }
    else {
      dia= dia + qtdDias
    }
  }
  else if(mes == 2) {
    #ate dia 28
    if((dia + qtdDias) > 28) {
      mes= mes + 1
      dia= (dia + qtdDias) - 28
    }
    else {
      dia= dia + qtdDias
    }
  }
  else if(mes %in% m2) {
    if((dia + qtdDias) > 30) {
      mes= mes + 1
      dia= (dia + qtdDias) - 30
    }
    else {
      dia= dia + qtdDias
    }
  }
  
  if(mes > 12) {
    ano= ano + 1
    mes= 1
  }
  
  aux= c(1,2,3,4,5,6,7,8,9)
  if(dia %in% aux) {
    dia= str_c("0", dia)
  }
  
  if(mes %in% aux) {
    mes= str_c("0", mes)
  }
  
  resultado= str_c(ano,"-",mes,"-",dia)
  
  return(resultado)
}


#Chaves de autenticacao
app= "TccGuilhermeMineracaoDados"
consumer_key= "ZScZy7d1yuLtJHte7qpnoqgN6"
consumer_secret= "snO33GlADCvVUjLdxPGgJg0llgoys02Jmqsz2aqGoY1TYfyqcd"
access_token= "1224651897027616771-NDquLaYkiIZzUCwbyqqwnBu57GEGzj"
access_secret= "MAE6TzcgKfEyf1RObU2ETbXZkX699WnLPg6gAJVzBmwpj"

#Autenticacao com o Twitter
token= create_token(app, consumer_key, consumer_secret, access_token, access_secret)

#Consultando o Twitter
dtInicio= "2019-01-01"
dtFim= "2019-01-02"
termoBusca= "stf"
liguagem= "pt"
ano=1
mesAnterior= strtoi(str_sub(dtFim, start= 6, end=7), 10)

tweets= getOldTweets(termoBusca, liguagem, dtInicio, dtFim)

while(!ano>2019) {
  dtInicio= incrementarDias(dtInicio, 2)
  dtFim= incrementarDias(dtFim, 2)
  
  print(paste("Obtendo Tweets do periodo: ", dtInicio, " ate ", dtFim))
  
  aux= getOldTweets(termoBusca, liguagem, dtInicio, dtFim)
  
  tweets= rbind.data.frame(tweets, aux)
  
  #Sys.sleep(60*15)
  ano= strtoi(str_sub(dtFim, end=4), 10)
  mes= strtoi(str_sub(dtFim, start= 6, end=7), 10)
  if(mes>mesAnterior) {
    mesAnterior= mes
    print("Aguardando...")
    Sys.sleep(60*15)
  }
}


write.csv(tweets, file="dataframe PARTE1.csv", row.names=TRUE)
save.image("~/dataframe PARTE1.RData")
