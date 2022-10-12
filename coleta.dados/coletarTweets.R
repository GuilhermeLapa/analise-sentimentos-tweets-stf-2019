

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
library("rtweet")
library("stringr")
library("rvest")
library("jsonlite")


getOldTweets <- function(termoBusca, liguagem, dtInicio, dtFim) {
	#parâmetros da consulta
	startdate <-  dtInicio
	enddate <- dtFim
	language <- liguagem
	searchbox <- URLencode(termoBusca)
  
	#concatena os parâmetros da consulta e estrutura a URL para chamada do serviço do Search Tweets
	temp_url <- paste0("https://twitter.com/i/search/timeline?f=tweets&q=",searchbox,"%20since%3A",startdate,"%20until%3A",enddate,"&l=",language,"&src=typd&max_position=")
	webpage <-fromJSON(temp_url)
  
	if(webpage$new_latent_count>0) {
		#1- Acessamos a página HTML contida em webpage&items_html e obtemos um XML correspondente à página
		#2- Buscamos as tags HTML '.js-stream-tweet' pelo CSS no XML e então obtemos um vetor delas
		#3- Extraimos os valores dos atributos 'data-tweet-id' dessas tags da lista
		#4- Armazenamos em um vetor tweet_ids os valores dos ids extraidos
		tweet_ids <- read_html(webpage$items_html) %>% html_nodes('.js-stream-tweet') %>% html_attr('data-tweet-id')
		
		while (webpage$has_more_items == TRUE) {
			tryCatch({
				#min_position parâmetro de controle para iterar para as próximas páginas
				#guardamos o min_position
				min_position <- webpage$min_position
				#estruturamos a próxima URL agora considerando o min_position
				next_url <- paste0(temp_url, min_position)
				#serão retornados tweets com posição maior do que o min_position
				webpage <- fromJSON(next_url)
				next_tweet_ids <- read_html(webpage$items_html) %>% html_nodes('.js-stream-tweet') %>% html_attr('data-tweet-id')
				next_tweet_ids <- next_tweet_ids[!is.na(next_tweet_ids)]
				#armazenamos os novos ids extraidos sem duplicatas
				tweet_ids <- unique(c(tweet_ids,next_tweet_ids))
			},
			error=function(cond) {
				message(paste("Essa URL nao existe:", next_url))
				message("Menssagem de erro original:")
				message(cond)
			})
		}
		
		#obtém Tibble(dataframe) dos tweets com base nos ids passados no vetor tweet_ids
		tweets <- lookup_tweets(tweet_ids, parse = TRUE, token = NULL)
	} else {
		paste0("Nao existem tweets sobre o termo da busca!")
	}
  
	return(tweets)
}


incrementarDias <- function (data, qtdDias) {
	ano <- strtoi(str_sub(data, end=4), 10)
	mes <- strtoi(str_sub(data, start= 6, end=7), 10)
	dia <- strtoi(str_sub(data, start=9), 10)
	
	meses31dias <- c(1,3,5,7,8,10,12)
	meses30dias <- c(4,6,9,11)
	
	#se o mês têm 31 dias
	if(mes %in% meses31dias) {
		#se o incremento supera 31 dias
		if((dia + qtdDias) > 31) {
			mes <- mes + 1
			dia <- (dia + qtdDias) - 31
		}
		else {
			#se o incremento NÃO supera 31 dias
			dia <- dia + qtdDias
		}
	}
	else if(mes == 2) { #se é fevereiro
		#se o incremento supera 28 dias
		if((dia + qtdDias) > 28) {
			mes <- mes + 1
			dia <- (dia + qtdDias) - 28
		}
		else {
			#se o incremento NÃO supera 28 dias
			dia <- dia + qtdDias
		}
	}
	else if(mes %in% meses30dias) { #se o mês têm 30 dias
		#se o incremento supera 30 dias
		if((dia + qtdDias) > 30) {
			mes <- mes + 1
			dia <- (dia + qtdDias) - 30
		}
		else {
			#se o incremento NÃO supera 30 dias
			dia <- dia + qtdDias
		}
	}
	
	#mudança de ano
	if(mes > 12) {
		ano <- ano + 1
		mes <- 1
	}
	
	#ajustamos para incluir o zero antes dos dias 1 até 9
	aux <- c(1,2,3,4,5,6,7,8,9)
	if(dia %in% aux) {
		dia <- str_c("0", dia)
	}
	
	#ajustamos para incluir o zero antes dos meses 1 até 9
	if(mes %in% aux) {
		mes <- str_c("0", mes)
	}
	
	#formatamos a string da data para ficar aaaa-mm-dd
	resultado <- str_c(ano,"-",mes,"-",dia)
	
	return(resultado)
}

#Chaves de autenticação para o Twitter
app <- "TccGuilhermeMineracaoDados"
consumer_key <- "ZScZy7d1yuLtJHte7qpnoqgN6"
consumer_secret <- "snO33GlADCvVUjLdxPGgJg0llgoys02Jmqsz2aqGoY1TYfyqcd"
access_token <- "1224651897027616771-NDquLaYkiIZzUCwbyqqwnBu57GEGzj"
access_secret <- "MAE6TzcgKfEyf1RObU2ETbXZkX699WnLPg6gAJVzBmwpj"

#Autenticação com o Twitter
token <- create_token(app, consumer_key, consumer_secret, access_token, access_secret)

rm(app)
rm(consumer_key)
rm(consumer_secret)
rm(access_token)
rm(access_secret)

#Inicializando parâmetros da busca
dtInicio <- "2019-01-01"
dtFim <- "2019-01-02"
termoBusca <- "stf"
liguagem <- "pt"
ano <- 1
mesAnterior <- strtoi(str_sub(dtFim, start= 6, end=7), 10)

tweets <- getOldTweets(termoBusca, liguagem, dtInicio, dtFim)

while(!ano>2019) {
	dtInicio <- incrementarDias(dtInicio, 2)
	dtFim <- incrementarDias(dtFim, 2)
	
	print(paste("Obtendo Tweets do periodo: ", dtInicio, " ate ", dtFim))
	
	aux <- getOldTweets(termoBusca, liguagem, dtInicio, dtFim)
	
	tweets <- rbind.data.frame(tweets, aux)
	
	ano <- strtoi(str_sub(dtFim, end=4), 10)
	mes <- strtoi(str_sub(dtFim, start= 6, end=7), 10)
	if(mes>mesAnterior) {
		mesAnterior <- mes
		print("Aguardando...")
		Sys.sleep(60*15)
	}
}

rm(dtInicio)
rm(dtFim)
rm(termoBusca)
rm(liguagem)
rm(ano)
rm(mes)
rm(mesAnterior)
rm(aux)
rm(getOldTweets)
rm(incrementarDias)
rm(token)

#armazenamos o dataframe dos tweets em uma base de dados
write.csv(apply(tweets,2,as.character), file="dataframe PARTE1.csv", row.names=TRUE)
save.image("~/dataframe PARTE1.RData")
