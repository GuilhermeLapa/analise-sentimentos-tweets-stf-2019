

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


preProcessamentoTweets <- function(corpus) {
	#Coloca tudo em minusculo
	corpus <- tm_map(corpus, tolower)
	#Remove pontuacao
	corpus <- tm_map(corpus, removePunctuation)
	#Remove numeros
	corpus <- tm_map(corpus, removeNumbers);
	#Remove espacos extras em branco
	corpus <- tm_map(corpus, stripWhitespace)
	#Remove palavras ruido
	corpus <- tm_map(corpus, removeWords, stopwords('portuguese'))


	#remove URLs
	removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
	corpus <- tm_map(corpus, removeURL)
	#remove qualquer coisa que nÃ£o sejam letras em portuguÃªs e espaÃ§o.
	removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
	corpus <- tm_map(corpus, content_transformer(removeNumPunct))
	
	rm(removeURL)
	rm(removeNumPunct)
	
	return(corpus)
}


#separamos as variaveis com maior relevancia
tweets_selecionados <- select(tweets_selecionados, 
                            user_id,
                            status_id,
                            created_at,
                            screen_name,
                            text,
                            source,
                            display_text_width,
                            reply_to_status_id,
                            reply_to_user_id,
                            reply_to_screen_name,
                            is_quote,
                            is_retweet,
                            favorite_count,
                            retweet_count, 
                            location)

write.csv(tweets_selecionados, file="tweets selecionados PARTE2.csv", row.names=TRUE)
save.image("~/dataframe PARTE2.RData")


#Colapsando todos os tweets em um vetor de uma posicao.
tweets_t <- paste(tweets_selecionados$text, collapse = " ")

#criando o corpus
tweets_S <- VectorSource(tweets_t)
corpus <- Corpus(tweets_S)


corpus <- preProcessamentoTweets(corpus)


#Cria a matriz
dtm <- TermDocumentMatrix(corpus)
dtm <- removeSparseTerms(dtm, 0.99)
dtmMatriz <- as.matrix(dtm)
#Fornece a frequencia de cada palavra
frequencia <- sort(rowSums(dtmMatriz),decreasing=TRUE)
DTF_frequencia <- data.frame(palavra = names(frequencia),frequencia=frequencia)

write.csv(dtmMatriz, file="dtmMatriz PARTE3.csv", row.names=TRUE)
save.image("~/dataframe PARTE3.RData")
rm(dtmMatriz)


#cria a nuvem de palavras
wordcloud(corpus, min.freq = 1, max.words=Inf, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))


########################### Remocao manual de palavras ###########################
novas_stopwords_pt <- c(stopwords("pt"), 
                        "aaa",
                        "aaaa",
                        "aaaaa",
                        "aaaaaa",
                        "aaaaaaa",
                        "aaaaaaaa",
                        "aaaaaaaaaa",
                        "aaaaaaaaaaa",
                        "aaaaaaaaaaaaaaaa",
                        "aaaaaaaaaaaaaaaaaaaa",
                        "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
                        "aaaaaaaaaaaaaah",
                        "aaaaaaaah",
                        "aaaaaah",
                        "aaaaaahhhhhj",
                        "aaaahh",
                        "aaaahhh",
                        "aaaahhhh",
                        "aaaaahhhhh",
                        "aaaaaheu",
                        "aaaaah",
                        "aaah",
                        "aaahh",
                        "aaahhhh",
                        "aaahq",
                        "pra",
                        "tá",
                        "desses",
                        "dessas",
                        "dessa",
                        "desse",
                        "sempre",
                        "pois",
                        "assim",
                        "lá",
                        "aí",
                        "porque",
                        "vcs",
                        "outro",
                        "sai",
                        "etc",
                        "vamo",
                        "vamos",
                        "vai",
                        "ser",
                        "todo",
                        "agora",
                        "dia",
                        "via",
                        "ter",
                        "fazer",
                        "quer",
                        "cara",
                        "faz",
                        "ver",
                        "deve",
                        "favor",
                        "hora",
                        "gente",
                        "dar",
                        "então",
                        "acho",
                        "fez",
                        "dá",
                        "outra",
                        "tão",
                        "após",
                        "kkkkk",
                        "aqui",
                        "diz",
                        "vão",
                        "onde",
                        "aqui",
                        "vez",
                        "todos",
                        "sendo",
                        "querem",
                        "ainda",
                        "pro",
                        "deveria")
corpus <- tm_map(corpus, removeWords, novas_stopwords_pt)

corpus <- preProcessamentoTweets(corpus)


#Cria a matriz
dtm <- TermDocumentMatrix(corpus)
dtm <- removeSparseTerms(dtm, 0.99)
dtmMatriz <- as.matrix(dtm)
#Fornece a frequencia de cada palavra
frequencia <- sort(rowSums(dtmMatriz),decreasing=TRUE)
DTF_frequencia <- data.frame(palavra = names(frequencia),frequencia=frequencia)

write.csv(dtmMatriz, file="dtmMatriz PARTE4.csv", row.names=TRUE)
save.image("~/dataframe PARTE4.RData")


#desconsideramos para a nuvem de palavras as palavras descriminativas(OUTLIERS), aquelas que ocorrem com muita frequencia ou raramente
View(frequencia)
filter(DTF_frequencia, frequencia == max(DTF_frequencia$frequencia))
count(filter(DTF_frequencia, frequencia == 1))
#Removemos as palavras com maior frequencia
removerParaWordcloud <- c('stf','stfoficial')
corpus <- tm_map(corpus, removeWords, removerParaWordcloud)

#REFAZEMOS A DTM,DTMMATRIZ, FREQUENCIA E DTF_frequencia APÓS A RETIRADA DOS OUTLIERS
dtm <- TermDocumentMatrix(corpus)
dtm <- removeSparseTerms(dtm, 0.99)
dtmMatriz <- as.matrix(dtm)
frequencia <- sort(rowSums(dtmMatriz),decreasing=TRUE)
DTF_frequencia <- data.frame(palavra = names(frequencia),frequencia=frequencia)


#nova nuvem de palavras ; min.freq = 2 desconsideramos as com frequencia == 1
wordcloud(corpus, min.freq = 2, max.words=Inf, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))


#STEMMING
#O corpus apos o stemming deve ser guardado em nova variavel para nao afetar os graficos
stemmed_corpus <- tm_map(corpus, stemDocument, language = "portuguese")
stemmed_dtm <- TermDocumentMatrix(stemmed_corpus)
stemmed_dtm_mt <- as.matrix(stemmed_dtm)
stemmed_frequencia <- sort(rowSums(stemmed_dtm_mt),decreasing=TRUE)
stemmed_DTF <- data.frame(palavra = names(stemmed_frequencia),frequencia=stemmed_frequencia)


rm(tweets_t)
rm(tweets_S)
rm(dtmMatriz)
rm(removerParaWordcloud)
rm(stemmed_dtm)
rm(stemmed_dtm_mt)
rm(stemmed_frequencia)
rm(preProcessamentoTweets)
